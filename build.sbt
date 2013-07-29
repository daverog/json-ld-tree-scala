name := "json-ld-tree-scala"

version := "1.0.0"

description := "Library to generate stable and predictable JSON-LD from RDF either automatically or using a frame"

scalaVersion := "2.9.1"

pomIncludeRepository := { _ => false }

makePomConfiguration ~= { _.copy(file = file("pom.xml")) }

onLoad in Global ~= (_ compose { Project.runTask(makePom, _).get._1 })

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "org.joda" % "joda-convert" % "1.1",
  "org.scalaj" % "scalaj-collection_2.9.1" % "1.2",
  "org.scalaj" %% "scalaj-time" % "0.6",
  "net.liftweb" %% "lift-json" % "2.5",
  "com.hp.hpl.jena" % "arq" % "2.8.8",
  "com.ning" % "async-http-client" % "1.7.18",
  "com.samskivert" % "jmustache" % "1.5"
)
