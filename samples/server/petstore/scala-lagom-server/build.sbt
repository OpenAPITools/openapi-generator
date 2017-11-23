version       := "1.0.0"

name          := "scala-lagom-server"

organization  := "io.swagger"

scalaVersion  := "2.11.8"

val playJsonDerivedCodecs = "org.julienrf" %% "play-json-derived-codecs" % "3.3"

libraryDependencies ++= Seq(
lagomScaladslApi,
playJsonDerivedCodecs
)


