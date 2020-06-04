version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.2"

crossScalaVersions := Seq(scalaVersion.value, "2.12.10", "2.11.12")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % "2.1.5",
  "com.softwaremill.sttp.client" %% "json4s" % "2.1.5",
  "org.json4s" %% "json4s-jackson" % "3.6.7"
)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

publishArtifact in (Compile, packageDoc) := false
