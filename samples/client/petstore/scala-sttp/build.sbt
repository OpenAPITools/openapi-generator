version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.4"
crossScalaVersions := Seq(scalaVersion.value, "2.12.13")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % "3.0.0",
  "com.softwaremill.sttp.client" %% "json4s" % "3.0.0",
  "org.json4s" %% "json4s-jackson" % "3.6.10"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
