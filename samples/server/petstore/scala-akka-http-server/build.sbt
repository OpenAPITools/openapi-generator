version := "1.0.0"
name := "openapi-scala-akka-http-server"
organization := "org.openapitools"
scalaVersion := "2.13.16"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.6.21",
  "com.typesafe.akka" %% "akka-http" % "10.2.9"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
)
