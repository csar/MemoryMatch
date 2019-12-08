enablePlugins(UniversalPlugin)
enablePlugins(JavaAppPackaging)

name := "MemoryMatch"

version := "0.3"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/com.typesafe.slick/slick
//libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.2"
//
//libraryDependencies += "com.h2database" % "h2" % "1.4.197"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
//libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
//
//libraryDependencies += "com.typesafe" % "config" % "1.3.2"

//libraryDependencies +=
//  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

//libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.0"
//libraryDependencies += "com.michaelpollmeier" %% "scala-collection-contrib" % "0.2.1"
//resolvers += Resolver.bintrayRepo("mpollmeier", "maven")

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.6"

assemblyJarName in assembly := s"tmsearch-${version.value}.jar"

mainClass in assembly := Some("Load")