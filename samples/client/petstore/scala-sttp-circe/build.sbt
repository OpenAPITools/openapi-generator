version := "1.0.0"
name := "scala-sttp-petstore"
organization := "org.openapitools"

scalaVersion := "2.13.16"
crossScalaVersions := Seq(scalaVersion.value, "2.12.20")

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.3.18",
  "com.softwaremill.sttp.client3" %% "circe" % "3.3.18",
  "io.circe" %% "circe-generic" % "0.14.1"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
