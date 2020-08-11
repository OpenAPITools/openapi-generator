version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.2"
crossScalaVersions := Seq(scalaVersion.value, "2.12.10")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % "2.2.0",
  "com.softwaremill.sttp.client" %% "json4s" % "2.2.0",
  "org.json4s" %% "json4s-jackson" % "3.6.8"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
