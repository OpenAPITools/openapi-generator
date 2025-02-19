version := "1.0.0"
name := "scala-akka-petstore-client"
organization := "org.openapitools"

scalaVersion := "2.12.13"
crossScalaVersions := Seq(scalaVersion.value, "2.13.4")


libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  "com.typesafe.akka" %% "akka-actor" % "2.6.12",
  "com.typesafe.akka" %% "akka-stream" % "2.6.12",
  "com.typesafe.akka" %% "akka-http" % "10.2.3",
  "org.json4s" %% "json4s-jackson" % "3.6.7",
  "org.json4s" %% "json4s-ext" % "3.6.7",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.27.0",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.4.1",
  // test dependencies
  "org.scalatest"     %% "scalatest"  % "3.2.3"   % "test",
  "org.scalatestplus" %% "junit-4-13" % "3.2.3.0" % "test"
)

resolvers ++= Seq(Resolver.mavenLocal)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false
