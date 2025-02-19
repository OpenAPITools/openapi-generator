scalariformSettings

organization := "org.openapitools"

name := "finch-sample"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.3"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "TM" at "https://maven.twttr.com"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases/"

Defaults.itSettings

lazy val circeVersion         = "0.8.0"
lazy val finagleVersion       = "6.45.0"
lazy val finchVersion         = "0.15.1"
lazy val scalaTestVersion     = "3.0.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint",
  "-Ywarn-unused-import",
  "-language:postfixOps"
)

lazy val `it-config-sbt-project` = project.in(file(".")).configs(IntegrationTest)

libraryDependencies ++= Seq(
  "com.github.finagle"      %% "finch-core"                     % finchVersion,
  "com.github.finagle"      %% "finch-circe"                    % finchVersion,
  "io.circe"                %% "circe-generic"                  % circeVersion,
  "io.circe"                %% "circe-java8"                    % circeVersion,
  "com.twitter"             %% "util-core"                      % finagleVersion,
  "com.github.finagle"      %% "finch-test"                     % finchVersion      % "test",
  "org.scalacheck"          %% "scalacheck"                     % "1.13.4" % "test",
  "org.scalatest"           %% "scalatest"                      % scalaTestVersion      % "test"
)

assemblyMergeStrategy in assembly := {
  case "application.conf"  => MergeStrategy.concat
  case "about.html"     => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
