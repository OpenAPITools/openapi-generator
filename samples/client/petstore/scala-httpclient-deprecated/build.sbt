version := "1.0.0"
name := "scala-legacy-petstore"
organization := "org.openapitools"
scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.9",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.9.9",
  "com.sun.jersey" % "jersey-core" % "1.19.4",
  "com.sun.jersey" % "jersey-client" % "1.19.4",
  "com.sun.jersey.contribs" % "jersey-multipart" % "1.19.4",
  "org.jfarcand" % "jersey-ahc-client" % "1.0.5",
  "io.swagger" % "swagger-core" % "1.5.8",
  "joda-time" % "joda-time" % "2.9.9",
  "org.joda" % "joda-convert" % "1.9.2",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "junit" % "junit" % "4.13" % "test",
  "com.wordnik.swagger" %% "swagger-async-httpclient" % "0.3.5"
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
