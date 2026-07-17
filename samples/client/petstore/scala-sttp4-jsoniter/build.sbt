version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "3.3.8"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core"                          % "4.0.26",
  "com.softwaremill.sttp.client4" %% "jsoniter"                      % "4.0.26",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.39.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.39.1" % "compile-internal",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe"  % "2.39.1"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)