version := "1.0.0"
name := "openapi-scala-pekko-http-server"
organization := "org.openapitools"
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-stream" % "1.0.0",
  "org.apache.pekko" %% "pekko-http" % "1.0.0"
)
