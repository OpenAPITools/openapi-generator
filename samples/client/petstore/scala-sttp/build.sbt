version := "1.0.0"
name := "scala-akka-petstore-client"
organization := "org.openapitools"
scalaVersion := "2.12.8"

val sttp = "2.0.0"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client" %% "core" % sttp,
  "com.softwaremill.sttp.client" %% "json4s" % sttp,

  "joda-time" % "joda-time" % "2.10.1",
  "org.json4s" %% "json4s-jackson" % "3.6.5",
  "org.json4s" %% "json4s-ext" % "3.6.5",

  // test dependencies
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "junit" % "junit" % "4.13" % "test"
)

resolvers ++= Seq(Resolver.mavenLocal)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false