version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "3.3.9"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core"                          % "4.0.27",
  "com.softwaremill.sttp.client4" %% "jsoniter"                      % "4.0.27",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.42.0",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.42.0" % "compile-internal",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe"  % "2.42.0"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)