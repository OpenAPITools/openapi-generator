name := """swagger-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.7"

libraryDependencies += "org.webjars" % "swagger-ui" % "2.2.10-1"
