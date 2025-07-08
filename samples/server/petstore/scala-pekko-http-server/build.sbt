version := "1.0.0"
name := "openapi-scala-pekko-http-server"
organization := "org.openapitools"
scalaVersion := "2.13.16"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-stream" % "1.1.3",
  "org.apache.pekko" %% "pekko-http" % "1.1.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
)
