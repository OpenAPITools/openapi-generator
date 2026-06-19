version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "3.3.7"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core"                          % "4.0.23",
  "com.softwaremill.sttp.client4" %% "jsoniter"                      % "4.0.23",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.38.12",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.38.12" % "compile-internal",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe"  % "2.38.12"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)