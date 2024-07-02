version       := "1.0.0"

name          := "scala-lagom-server"

organization  := "org.openapitools"

scalaVersion  := "2.13.13"

val playJsonDerivedCodecs = "org.julienrf" %% "play-json-derived-codecs" % "3.3"

libraryDependencies ++= Seq(
lagomScaladslApi,
playJsonDerivedCodecs
)
