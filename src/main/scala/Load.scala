


import java.awt.event.{ActionEvent, ActionListener, AdjustmentEvent, AdjustmentListener, KeyEvent, KeyListener}
import java.awt.{Color, Component, Dimension, GridBagConstraints, Insets}
import java.io.{File, FileInputStream, FileReader, InputStream, InputStreamReader, Reader}
import java.nio.charset.{Charset, StandardCharsets}
import java.util.Locale
import java.util.concurrent.Executors
import java.util.regex.Pattern

import Load.{semanticClass, sus}
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.table.TableCellRenderer
import javax.swing.text.{DefaultStyledDocument, StyleConstants, StyleContext}
import javax.swing.{JLabel, JTable, JTextPane, SwingUtilities}
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}

import scala.collection.mutable
import scala.concurrent.Promise
import scala.io.Codec
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
//import scala.xml.pull.{EvElemEnd, XMLEventReader}
//import slick.jdbc.H2Profile.api._

import scala.collection.mutable.{ListBuffer, HashMap => Hashtable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
//import scala.xml.pull.{EvElemStart, EvText}

object Load extends App {


  def swing[T](body: => T) = {
    val promise = Promise[T]
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = promise success body
    }
    )
    promise.future
  }

  val executor = Executors.newSingleThreadExecutor()

  def load(file: File) = executor.submit(new Runnable {
    override def run(): Unit = {
      swing(Screen.search.setText(s"Loading $file"))
      if (file.getName.toLowerCase.endsWith(".xml")) loadXML(file)
      else loadCSV(file)

    }
  })

  var runner: java.util.concurrent.Future[_] = _

  Screen.search.setFocusTraversalKeysEnabled(false)
  Screen.search.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = e.getKeyChar match {
      case KeyEvent.VK_TAB =>
        more()
      case KeyEvent.VK_ENTER =>
        Screen.search.setText("")
        submit("")
      case _ =>
    }

    override def keyPressed(e: KeyEvent): Unit = {}

    override def keyReleased(e: KeyEvent): Unit = {}

  })
  val vscroll = Screen.scroll.getVerticalScrollBar
  //  vscroll.addAdjustmentListener(new AdjustmentListener {
  //    var active = false
  //    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
  //      if (vscroll.isVisible && e.getValue + vscroll.getVisibleAmount >= vscroll.getMaximum) {
  //       if(active) {
  //         active = false
  //         more()
  //         println("Rock bottom")
  //       }
  //      } else if(!e.getValueIsAdjusting) {
  //        println("rearming")
  //        active = true
  //      }
  //    }
  //  })
  Screen.search.getDocument.addDocumentListener(new DocumentListener {
    override def insertUpdate(e: DocumentEvent): Unit = keyTyped()

    override def removeUpdate(e: DocumentEvent): Unit = keyTyped()

    override def changedUpdate(e: DocumentEvent): Unit = keyTyped()
  })


  swing {
    Screen.search.setEditable(false)
    Screen.search.setEnabled(false)
    Screen.search.setText("Drop memory to window")
  }

  args.flatMap { f: String => Try(new File(f)).toOption
  }.foreach(load)

  object Semicolon extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  val langIds = new Hashtable[String,Int]
  val dictionaries = new Hashtable[String, Hashtable[String, Int]]
  var segmentIndex = -1
  var semantics = new mutable.HashMap[Int, Int]() // map from segment to semantic class
  var translations = new mutable.HashMap[Int, List[Int]] withDefaultValue Nil // members of semantic class
  var duplicates = 0
  var transitives = 0
  var semanticClass = -1
  var texts = new Array[String](segmentIndex + 1)
  var lang = new Array[Int](segmentIndex + 1)
  def inputStream(file:File) : (InputStream, Option[Charset]) = {
    val is = new FileInputStream(file)
    val bom = new Array[Byte](2)
    is.read(bom)

    def eq(index: Int, const: Int) = bom(index) == const.toByte

    if (eq(0, 0xef) && eq(1, 0xbb) && is.read() == 0xbf) { //UTF-8
      (is, Some(StandardCharsets.UTF_8))
    } else if (eq(0, 0xff) && eq(1, 0xfe))
      (is, Some(StandardCharsets.UTF_16LE))
    else if (eq(1, 0xff) && eq(0, 0xfe))
      (is, Some(StandardCharsets.UTF_16BE))
    else {
      is.close
      (new FileInputStream(file), None)
    }
  }
  def makeReader(file:File):Reader = {
    val (is, cs) = inputStream(file)
    cs match {
      case Some(charset) =>
        new InputStreamReader(is,charset)
      case None =>
        new InputStreamReader(is)
    }
  }
  def merge(ids:List[Int]):Unit = {
    val sus = ids.flatMap(semantics.get)
    sus match {
      case Nil =>
        // new semantics
        semanticClass += 1
        for (id <- ids) {
          translations(semanticClass) ::= id
          semantics(id) = semanticClass
        }
      case List(semanticClass) =>
        for (id <- ids) {
          translations(semanticClass) ::= id
          semantics(id) = semanticClass
        }
      case List(semanticClass, equivalent) =>
        if (semanticClass == equivalent) {
          //println(s"Duplicate $entry")
          duplicates += 1
        } else {
          for (id <- ids) {
            translations(semanticClass) ::= id
            semantics(id) = semanticClass
          }
          // uh-oh
          // println(s"Transitive classes: $sus")
          transitives += 1

          for (id <- translations(equivalent)) {
            translations(semanticClass) ::= id
            semantics(id) = semanticClass
          }
          translations -= equivalent

        }
    }
  }

  def loadCSV(file: File): Unit = {
    val semanticClasses = semanticClass
    val csv = CSVReader.open(makeReader(file))(Semicolon)
    val stream = try csv.toStream catch {
      case NonFatal(e) =>
        println(e)
        throw e
    }
    val languages = stream.head.map{longForm =>
      Try{
        val m = "(.*) [(](.*)[)]".r.findAllMatchIn(longForm).next()
        val language = m.group(1)
        val country = m.group(2)
        val cc = Locale.getISOCountries.map(new Locale("",_)).find(_.getDisplayCountry(Locale.US) == country)
        val lc = Locale.getISOLanguages.map(new Locale(_)).find(_.getDisplayLanguage(Locale.US)==language)
        lc.get.getLanguage +"-"+ cc.get.getCountry
      }.getOrElse(longForm)
    }
    for (language <- languages if !dictionaries.contains(language)) {
      dictionaries += language -> new Hashtable[String, Int]
      langIds += language -> langIds.size
    }
    val dicts = languages.map(dictionaries)
    for (entry <- stream.tail if entry.toSet.size > 1) {
      // not counting transparent translations
      val ids = for ((dict, text) <- dicts zip entry) yield {
        val id = dict.get(text) match {
          case Some(id) => id
          case None =>
            segmentIndex += 1
            dict += text -> segmentIndex
            segmentIndex
        }
        id
      }
      merge(ids)
    }
    consolidate(semanticClasses)
  }
    def consolidate(semanticClasses:Int) :Unit = {
    swing(Screen.search.setText(s"Loaded ${semanticClass-semanticClasses} (${semanticClasses+1})"))

    val offset = texts.size
    texts = new Array[String](segmentIndex+1)
    lang = new Array[Int](segmentIndex+1)

    for (dict <- dictionaries.values; (text, index) <- dict) texts(index) = text

    for ((language, lid) <- langIds; dict = dictionaries(language); index <- dict.values) lang(index) = lid
    swing(Screen.search.requestFocus())
  }




//  def scans(regExp: Regex, from: Int = 0): Iterator[Int] = new Iterator[Int] {
//    var last = from - 1
//    var nextIndex = last
//    var exhausted = false
//
//    override def hasNext: Boolean = if (exhausted) false else if (last == nextIndex) {
//      nextIndex = texts.indexWhere(regExp.findFirstIn(_).isDefined, last + 1)
//      exhausted = nextIndex < 0
//      !exhausted
//    } else nextIndex >= 0
//
//    override def next(): Int = if (hasNext) {
//      last = nextIndex
//      nextIndex
//    } else throw new NoSuchElementException
//  }

  class Scanner(var from: Int, to: Int, r: Pattern) {
    val slice = texts.slice(from, to)

    def next(): Future[Int] = Future {
      def index(): Int = if (from < to) {
        while (from < to) {
          if(r.matcher(texts(from)).find())  {
              from += 1
              return from - 1
          }
          from += 1
        }
        throw new NoSuchElementException

      } else throw new NoSuchElementException


      index()
    }
  }

  def scan(regExp: Pattern, from: Int = 0): Iterator[Int] = new Iterator[Int] {

    import concurrent.ExecutionContext.Implicits.global

    val parallelism = Runtime.getRuntime.availableProcessors()
    val stride = texts.size / parallelism
    val scanners = Array.tabulate(parallelism)(instance =>
      new Scanner(stride * instance, if (parallelism == instance + 1) texts.size else stride * (instance + 1), regExp)
    )
    var futures = (0 until parallelism).map(part => part -> scanners(part).next()).toMap
    var nextIndex = Option.empty[Int]

    override def hasNext: Boolean = {
      while (nextIndex.isEmpty && futures.nonEmpty) {

        Try(Await.result(Future.firstCompletedOf(futures.values), Duration.Inf)) match {
          case Failure(_) =>
            // remove all failures
            futures = futures.filterNot(_._2.value match {
              case Some(Failure(_)) =>
                true
              case _ => false
            })
          case Success(nextId) =>
            for ((id, future) <- futures; res <- future.value if res == Success(nextId)) {
              futures += id -> scanners(id).next()
            }
            nextIndex = Some(nextId)
        }
      }
      nextIndex.isDefined
    }

    override def next(): Int = if (hasNext) {
      val ret = nextIndex.get
      nextIndex = None
      ret
    } else
      throw new NoSuchElementException
  }


  swing {
    Screen.search.setEditable(true)
    Screen.search.setEnabled(true)

  }

//  def loadXML(file:File) = {
//    val semanticClasses = semanticClass
//    val (is,cs) = inputStream(file)
//    implicit val codec =  Codec(cs getOrElse StandardCharsets.UTF_8)
//    val reader = new XMLEventReader(Source.fromInputStream(is))
//    var insertText = false
//    var insertValue = false
//    var langText = false
//    var langValue = false
//    var keys :mutable.HashMap[String,Int] = null
//    var values :mutable.HashMap[String,Int] = null
//    var key:String = null
//    var value:String = null
//    for (event <- reader) {
//      event match {
//        case EvElemStart(_, "source_language", _, _) =>
//          langText = true
//        case EvElemStart(_, "target_language", _, _) =>
//          langValue = true
//
//        case EvElemStart(_, "source_segment", _, _) =>
//          insertText = true
//        case EvElemStart(_, "target_segment", _, _) =>
//          insertValue = true
//        case EvText(text) if langText =>
//          if(!dictionaries.contains(text)) {
//            dictionaries+= text -> new mutable.HashMap[String,Int]()
//            langIds += text -> langIds.size
//          }
//          keys= dictionaries(text)
//          langText = false
//        case EvText(text) if langValue =>
//          if(!dictionaries.contains(text)){
//            dictionaries+= text -> new mutable.HashMap[String,Int]()
//            langIds += text -> langIds.size
//          }
//          values= dictionaries(text)
//          langValue = false
//        case EvText(text) if insertText =>
//          key = text
//          insertText = false
//        case EvText(text) if insertValue =>
//          value = text
//          insertValue = false
//        case EvElemEnd(_, "Translation_Memory") =>
//          if (key!=value) {
//            val inserted = new ListBuffer[Int]
//            keys.get(key) match {
//              case Some(si) => inserted append si
//              case None =>
//                segmentIndex+=1
//                keys += key -> segmentIndex
//                inserted append segmentIndex
//            }
//            values.get(value) match {
//              case Some(si) => inserted append si
//              case None =>
//                segmentIndex+=1
//                values += value -> segmentIndex
//                inserted append segmentIndex
//            }
//
//            merge(inserted.toList)
//          }
//
//        case _ =>
//        // skip
//      }
//
//    }
//    consolidate(semanticClasses)
//  }
  val factory  = XMLInputFactory.newInstance
  def loadXML(file:File) = {
    val semanticClasses = semanticClass
    val (is,cs) = inputStream(file)
    implicit val codec =  Codec(cs getOrElse StandardCharsets.UTF_8)
    val reader = factory.createXMLStreamReader(is,codec.name)
    var insertText = false
    var insertValue = false
    var langText = false
    var langValue = false
    var keys :mutable.HashMap[String,Int] = null
    var values :mutable.HashMap[String,Int] = null
    var key:String = null
    var value:String = null
    while (reader.hasNext) {
      val event = reader.next()
      import XMLStreamConstants._
      event match {
        case START_ELEMENT =>
          reader.getLocalName match {
            case "source_language" =>
              langText = true
            case "target_language" =>
              langValue = true

            case "source_segment" =>
              insertText = true
            case "target_segment" =>
              insertValue = true
            case _ =>
          }
        case CHARACTERS  if (langText||langValue||insertText||insertValue) && !reader.isWhiteSpace=>
          val text = reader.getText
          if ( (langText || langValue) && !dictionaries.contains(text)) {
            dictionaries += text -> new mutable.HashMap[String, Int]()
            langIds += text -> langIds.size
          }
          if (langText) {
            keys = dictionaries(text)
            langText = false
          }
          if (langValue) {
            values = dictionaries(text)
            langValue = false
          }
          if (insertText) {
            key = text
            insertText = false
          }
          if (insertValue) {
            value = text
            insertValue = false
          }
        case END_ELEMENT if reader.getLocalName == "Translation_Memory" =>
          if (key!=value) {
            val inserted = new ListBuffer[Int]
            keys.get(key) match {
              case Some(si) => inserted append si
              case None =>
                segmentIndex+=1
                keys += key -> segmentIndex
                inserted append segmentIndex
            }
            values.get(value) match {
              case Some(si) => inserted append si
              case None =>
                segmentIndex+=1
                values += value -> segmentIndex
                inserted append segmentIndex
            }

            merge(inserted.toList)
          }

        case _ =>
        // skip
      }

    }
    consolidate(semanticClasses)
  }

  def comp(regexp: Pattern, texts: String*): Component = {
    def decorate(string: String) = {
      val buffer = new StringBuffer()
      var start = 0
      val m = regexp.matcher(string)
      while (m find start) {
        if (m.start > start) buffer append string.substring(start, m.start)
        buffer append "<b>"
        buffer append string.substring(m.start, m.end)
        buffer append "</b>"
        start = m.end
      }
      buffer append string.substring(start)
      buffer.toString
    }

    val label = new JLabel(texts.map(decorate).mkString("<html>", "<hr/>", "</html>"))
    label.setBackground(Color.WHITE)
    label.setOpaque(true)
    import java.awt.Font
    val f = label.getFont
    label.setFont(f.deriveFont(f.getStyle & ~Font.BOLD))
    label
  }

  var regexp = Option.empty[Pattern]
  var scans = Option.empty[Iterator[Int]]
  var sus = Set.empty[Int]
  def render[T](regexp: Pattern,means:Int)(ret: =>T) = {
    sus += means
    val phrases = translations(means)
    val t = phrases.map(texts).groupBy(regexp.matcher(_).find()).view.mapValues(_.toSet.toSeq)
    val (left,right) = if (t contains false) (t(true), t(false)) else {
      val list = t(true).toList
      list.splitAt((list.size+1)/2)
    }
    //println(s"Found $regexp ${texts(key)} ${t(false).head},  ${translations(key)}")
    swing {
      val pos = Screen.pane.getComponents.size / 2
      val c = new GridBagConstraints()
      c.gridx = 0
      c.insets = new Insets(4, 4, 0, 0)
      c.gridy = pos
      c.weightx = 0.5
      c.anchor = GridBagConstraints.PAGE_START
      c.fill = GridBagConstraints.HORIZONTAL
      Screen.pane.add(comp(regexp,left:_*), c)
      c.gridx = 1
      Screen.pane.add(comp(regexp,right:_*), c)
      Screen.pane.revalidate()
      ret
    }
  }
  def more() = regexp foreach { pattern =>

    runner = executor.submit(new Runnable {
        override def run(): Unit = {


        val view = Screen.pane.getVisibleRect


        def findAndDisplay(f: Try[Int]): Future[Int] = f match {
          case Success(n) if n > 0 =>
            scans.flatMap {
              _.find(key => !sus.contains(semantics(key)))
            } match {
              case None =>
                scans = None
                Future successful 0
              case Some(key) =>
                val means = semantics(key)
                if (Thread.interrupted()) {
                  println("interrupted")
                  Future successful 0
                } else {
                  render(pattern,means)(n - 1).transformWith(findAndDisplay)
                }
            }
          case _ => Future.successful(0)
        }

        Await.ready(Future.successful(5).transformWith(findAndDisplay), Duration.Inf)

        //        swing{
        ////            Screen.scroll.revalidate()
        ////            val dim = Screen.pane.getSize()
        ////            view.y = dim.height
        ////            Screen.pane.scrollRectToVisible(view)
        //           // Screen.scroll.revalidate()
        ////          new AdjustmentListener() {
        ////            override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
        ////              val adjustable = e.getAdjustable
        ////              adjustable.setValue(adjustable.getMaximum)
        ////              vscroll.removeAdjustmentListener(this)
        ////            }
        ////          })
        //          vscroll.revalidate()
        //          vscroll.setValue(vscroll.getMaximum+vscroll.getVisibleAmount)
        //        }
        //      }
      }
    }
    )
  }
  def submit(searchText: String) = {
    if (runner != null) runner.cancel(true)
    runner = executor.submit(new Runnable {
      sus = Set.empty
      val search = searchText

      def clear() = swing {
        Screen.pane.removeAll()
        Screen.pane.revalidate()
        Screen.pane.repaint()
        false
      }
      override def run(): Unit = {
        val drawn = clear()
        if (search.size<2) {
          regexp = None
          return
        }
        val pattern = Pattern.compile(search,Pattern.CASE_INSENSITIVE|Pattern.CASE_INSENSITIVE)
        regexp = Some(pattern) // for more()
        scans = Some(scan(pattern))
        scans.get.hasNext


        def findAndDisplay(f: Try[Boolean]): Future[Boolean] = f match {
          case Success(false) =>
            scans.flatMap {
              _.find(key => !sus.contains(semantics(key)))
            } match {
              case None =>
                scans = None
                Future successful true
              case Some(key) =>
                val means = semantics(key)
                if (Thread.interrupted()) {
                  println("interrupted")
                  Future successful true
                } else {
                  render(pattern,means)(vscroll.isVisible).transformWith(findAndDisplay)
                }
            }
          case _ => Future.successful(true)
        }

        Await.ready(drawn.transformWith(findAndDisplay), Duration.Inf)
      }
    }
    )
  }

  def keyTyped(): Unit = {

    submit(Screen.search.getText())


  }


}


