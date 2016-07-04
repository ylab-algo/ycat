organization := "ru.biocad"
name := "ycat-aligner"
version := "1.0"
scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies ++= Seq("ch.qos.logback" % "logback-classic" % "1.1.3",
                            "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0")