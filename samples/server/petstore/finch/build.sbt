scalariformSettings

organization := "io.swagger"

name := "finch-sample"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "TM" at "http://maven.twttr.com"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

Defaults.itSettings

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
//  "-Ywarn-unused-import",
  "-language:postfixOps"
)

lazy val `it-config-sbt-project` = project.in(file(".")).configs(IntegrationTest)

libraryDependencies ++= Seq(
  "com.github.finagle"      %% "finch-core"                     % "0.12.0",
  "com.github.finagle"      %% "finch-circe"                    % "0.12.0",
  "io.circe"                %% "circe-generic"                  % "0.7.0",
  "io.circe"                %% "circe-java8"                    % "0.7.0",
  "com.twitter"             %% "util-core"                      % "6.40.0",
  "com.github.finagle"      %% "finch-test"                     % "0.12.0"      % "test",
  "org.scalacheck"          %% "scalacheck"                     % "1.13.4"     % "test",
  "org.scalatest"           %% "scalatest"                      % "3.0.0"      % "test"
)

assemblyMergeStrategy in assembly := {
  case "application.conf"  => MergeStrategy.concat
  case "about.html"     => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
