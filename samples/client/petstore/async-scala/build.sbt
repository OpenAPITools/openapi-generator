organization := ""

name := "-client"

libraryDependencies ++= Seq(
  "io.swagger" %% "swagger-async-httpclient" % "0.3.5",
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.0.13" % "provided",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)
