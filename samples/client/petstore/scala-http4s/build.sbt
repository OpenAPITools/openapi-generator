scalaVersion := "3.3.3"

version := "1.0.0"
name := "scala-http4s-client"
organization := "org.openapitools"

val CirceVersion   = "0.14.9"
val Http4sVersion  = "0.23.26"

libraryDependencies ++= Seq(
  "org.http4s"   %% "http4s-ember-client" % Http4sVersion,
  "org.http4s"   %% "http4s-circe"        % Http4sVersion,
  "org.http4s"   %% "http4s-dsl"          % Http4sVersion,
  "org.http4s"   %% "http4s-client"       % Http4sVersion,
  "io.circe" %% "circe-core"    % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser"  % CirceVersion,
  "org.scalatest" %% "scalatest"  % "3.2.19"   % "test"
)

scalacOptions := Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:existentials,experimental.macros,higherKinds,implicitConversions,postfixOps,adhocExtensions",
  "-Yretain-trees",
  "-Xmax-inlines:100",
  "-Ykind-projector:underscores",
  "-source:future"
)

Compile / publishArtifact := false