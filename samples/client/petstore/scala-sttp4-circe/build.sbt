version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "2.13.16"
crossScalaVersions := Seq(scalaVersion.value, "2.12.20")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core" % "4.0.15",
  "com.softwaremill.sttp.client4" %% "circe" % "4.0.15",
  "io.circe" %% "circe-generic" % "0.14.15",
  "io.circe" %% "circe-generic-extras" % "0.14.4",
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
