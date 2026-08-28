version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "2.13.18"
crossScalaVersions := Seq(scalaVersion.value, "2.12.20")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core" % "4.0.15",
  "com.softwaremill.sttp.client4" %% "json4s" % "4.0.15",
  "org.json4s" %% "json4s-jackson" % "4.0.6"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
