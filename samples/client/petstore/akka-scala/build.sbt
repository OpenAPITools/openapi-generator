version       := "1.0.0"

name          := "scala-akka-petstore-client"

organization  := "io.swagger"

scalaVersion  := "2.11.8"

libraryDependencies ++= Seq(
  "io.swagger" % "swagger-core" % "1.5.15",
  "com.typesafe" % "config" % "1.2.1",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.3.9",
  "io.spray" % "spray-client" % "1.3.1",
  "joda-time" % "joda-time" % "2.2",
  "org.joda" % "joda-convert" % "1.2",
  "org.json4s" % "json4s-jackson_2.10" % "3.2.11",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "junit" % "junit" % "4.8.1" % "test"
)

resolvers ++= Seq(
  Resolver.mavenLocal
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

publishArtifact in (Compile, packageDoc) := false

