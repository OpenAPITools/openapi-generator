name := """openapi-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.12.6"

libraryDependencies += "javax.validation" % "validation-api" % "1.1.0.Final"
libraryDependencies += guice
