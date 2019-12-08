import java.awt._
import java.awt.datatransfer.DataFlavor
import java.awt.dnd.{DnDConstants, DropTargetAdapter, DropTargetDropEvent}
import java.io.File

import javax.sound.midi.MidiSystem
import javax.swing.ScrollPaneConstants.{HORIZONTAL_SCROLLBAR_NEVER, VERTICAL_SCROLLBAR_AS_NEEDED}
import javax.swing.{JButton, JFrame, JList, JScrollPane, JTextField, TransferHandler}


object Screen extends JFrame("Memory Search") {
  val shouldFill = true
  val shouldWeightX = true
  val RIGHT_TO_LEFT = false
  var button: JButton = new JButton("Button 1")
  val pane = new ScrollablePanel(new GridBagLayout())
  pane.setScrollableWidth(ScrollablePanel.ScrollableSizeHint.FIT)
  pane.setScrollableHeight(ScrollablePanel.ScrollableSizeHint.NONE)


  val scroll = new JScrollPane(pane, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_NEVER)
  val search = new JTextField("")
//  search.setDragEnabled(true)
//  search.setTransferHandler(new TransferHandler(null))

  import javax.swing.JOptionPane
  import java.awt.dnd.DropTarget


  def addComponentsToPane(panel: Container): Unit = {


    new DropTarget(panel, new DropTargetAdapter {
      override def drop(dtde: DropTargetDropEvent): Unit = {
        dtde.acceptDrop(DnDConstants.ACTION_COPY)
        val transferable = dtde.getTransferable()
        transferable.getTransferData(DataFlavor.javaFileListFlavor) match {
          case evt: java.util.List[File] @unchecked =>
            val iter = evt.iterator()
            while (iter.hasNext) {
             Load.load(iter.next())
              }
            }
        }
      }
    )
    panel.setLayout(new GridBagLayout())

    panel.setBackground(Color.DARK_GRAY)

    val c = new GridBagConstraints
    c.fill = GridBagConstraints.HORIZONTAL
    c.weightx = 0.5
    c.gridx = 0
    c.gridy = 0

    c.anchor = GridBagConstraints.PAGE_START
    panel.add(search, c)
    c.gridy = 1
    c.weighty = 1
    c.fill = GridBagConstraints.BOTH


    pane.setBackground(Color.LIGHT_GRAY)
    panel.add(scroll, c)

  }

  /**
   * Create the GUI and show it.  For thread safety,
   * this method should be invoked from the
   * event-dispatching thread.
   */
  private def createAndShowGUI(): Unit = { //Create and set up the window.

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    //Set up the content pane.
    val content = addComponentsToPane(getContentPane)
    //Display the window.
    setSize(500, 500)
    setVisible(true)

  }


  //creating and showing this application's GUI.
  javax.swing.SwingUtilities.invokeLater(new Runnable() {
    override def run(): Unit = {
      createAndShowGUI()
    }
  })

}