version := "1.0.0"
name := "&#39;scala-sttp-petstore&#39;"
organization := "org.openapitools"

scalaVersion := "2.13.0"

crossScalaVersions := Seq(scalaVersion.value, "2.12.10",  "2.11.12")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % "2.0.0",
  "com.softwaremill.sttp.client" %% "json4s" % "2.0.0",
  "org.json4s" %% "json4s-jackson" % "3.6.7",
  // test dependencies
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "junit" % "junit" % "4.13" % "test"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false