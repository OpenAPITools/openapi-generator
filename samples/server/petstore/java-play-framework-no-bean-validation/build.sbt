name := """swagger-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
javaJdbc,
cache,
javaWs,
"org.webjars" % "swagger-ui" % "2.2.10-1")
