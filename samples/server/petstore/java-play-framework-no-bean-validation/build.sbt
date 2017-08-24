name := """swagger-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.12.2"

libraryDependencies += "org.webjars" % "swagger-ui" % "3.1.5"
libraryDependencies += guice
