version := "1.0.0"
name := "scala-akka-petstore-client"
organization := "org.openapitools"

scalaVersion := "2.13.13"
crossScalaVersions := Seq(scalaVersion.value, "2.13.4")


libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.3",
  "com.typesafe.akka" %% "akka-actor" % "2.8.5",
  "com.typesafe.akka" %% "akka-stream" % "2.8.5",
  "com.typesafe.akka" %% "akka-http" % "10.5.3",
  "org.json4s" %% "json4s-jackson" % "4.0.7",
  "org.json4s" %% "json4s-ext" % "4.0.7",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.39.2",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0",
  // test dependencies
  "org.scalatest"     %% "scalatest"  % "3.2.3"   % "test",
  "org.scalatestplus" %% "junit-5-10" % "3.2.18.0" % "test"
)

resolvers ++= Seq(Resolver.mavenLocal)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false
