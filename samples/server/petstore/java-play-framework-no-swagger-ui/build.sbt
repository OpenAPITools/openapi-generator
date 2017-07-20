name := """swagger-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
javaJdbc,
cache,
javaWs,
"javax.validation" % "validation-api" % "1.1.0.Final"
)
