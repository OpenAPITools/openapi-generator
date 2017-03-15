import AssemblyKeys._ // put this at the top of the file

import NativePackagerKeys._

packageArchetype.java_server

assemblySettings

scalariformSettings

organization := "io.swagger"

seq(webSettings :_*)

mainClass in assembly := Some("JettyMain")

name := "scalatra-sample"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions += "-language:postfixOps"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.9.2-SNAPSHOT" changing(),
  "com.github.finagle" %% "finch-argonaut" % "0.9.2-SNAPSHOT" changing(),
  "io.argonaut" %% "argonaut" % "6.1",
  "com.github.finagle" %% "finch-test" % "0.9.2-SNAPSHOT" % "test,it" changing(),
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test,it",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test,it"
)

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "TM" at "http://maven.twttr.com"


ivyXML := <dependencies>
    <exclude module="slf4j-log4j12"/>
    <exclude module="grizzled-slf4j_2.9.1"/>
    <exclude module="jsr311-api" />
  </dependencies>

mergeStrategy in assembly <<= (mergeStrategy in assembly) {
  (old) => {
    case "about.html"     => MergeStrategy.discard
    case x => old(x)
  }
}

net.virtualvoid.sbt.graph.Plugin.graphSettings
