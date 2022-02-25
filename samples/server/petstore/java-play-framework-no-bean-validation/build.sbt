name := """openapi-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.12.6"

libraryDependencies += "org.webjars" % "swagger-ui" % "3.32.5"
libraryDependencies += guice
libraryDependencies += "com.auth0" % "java-jwt" % "3.18.1"
libraryDependencies += "com.auth0" % "jwks-rsa" % "0.19.0"
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.6"
