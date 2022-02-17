version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.5"
crossScalaVersions := Seq(scalaVersion.value, "2.12.13")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.3.18",
  "com.softwaremill.sttp.client3" %% "json4s" % "3.3.18",
  "org.json4s" %% "json4s-jackson" % "3.6.11"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
