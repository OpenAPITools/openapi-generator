version := "1.0.0"
name := "scala-akka-petstore-client"
organization := "org.openapitools"

scalaVersion := "2.13.2"
crossScalaVersions := Seq(scalaVersion.value, "2.12.10")

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  "com.typesafe.akka" %% "akka-actor" % "2.5.24",
  "com.typesafe.akka" %% "akka-stream" % "2.5.24",
  "com.typesafe.akka" %% "akka-http" % "10.1.9",
  "org.json4s" %% "json4s-jackson" % "3.6.7",
  "org.json4s" %% "json4s-ext" % "3.6.7",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.27.0",
  // test dependencies
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "junit" % "junit" % "4.13.1" % "test"
)

resolvers ++= Seq(Resolver.mavenLocal)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false
