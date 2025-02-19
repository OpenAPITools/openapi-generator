version := "1.0.0"
name := "openapi-scala-akka-http-server"
organization := "org.openapitools"
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.5.21",
  "com.typesafe.akka" %% "akka-http" % "10.1.10"
)
