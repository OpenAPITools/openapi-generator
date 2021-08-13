version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.5"
crossScalaVersions := Seq(scalaVersion.value, "2.12.13")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % "2.2.9",
  "com.softwaremill.sttp.client" %% "json4s" % "2.2.9",
  "org.json4s" %% "json4s-jackson" % "3.6.11"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
