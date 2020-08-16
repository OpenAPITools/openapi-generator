name := """openapi-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.12.6"

libraryDependencies += "org.webjars" % "swagger-ui" % "3.23.5"
libraryDependencies += guice
