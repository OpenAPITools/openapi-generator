import AssemblyKeys._ // put this at the top of the file

import NativePackagerKeys._

packageArchetype.java_server

assemblySettings

organization := "com.wordnik"

seq(webSettings :_*)

mainClass in assembly := Some("JettyMain")

name := "scalatra-sample"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                      % "2.0"               % "test",
  "org.scalatra"            %% "scalatra"                       % "2.3.0.M1",
  "org.scalatra"            %% "scalatra-scalate"               % "2.3.0.M1",
  "org.scalatra"            %% "scalatra-json"                  % "2.3.0.M1",
  "org.scalatra"            %% "scalatra-swagger"               % "2.3.0.M1",
  "org.scalatra"            %% "scalatra-swagger-ext"           % "2.3.0.M1",
  "org.scalatra"            %% "scalatra-slf4j"                 % "2.3.0.M1",
  "org.json4s"              %% "json4s-jackson"                 % "3.1.0",
  "org.json4s"              %% "json4s-ext"                     % "3.1.0",
  "commons-codec"            % "commons-codec"                  % "1.7",
  "net.databinder.dispatch" %% "dispatch-core"                  % "0.9.5",
  "net.databinder.dispatch" %% "json4s-jackson"                 % "0.9.5",
  "com.typesafe.akka"       %% "akka-actor"                     % "2.1.0",
  "org.eclipse.jetty"        % "jetty-server"                   % "8.1.7.v20120910" % "container;provided",
  "org.eclipse.jetty"        % "jetty-webapp"                   % "8.1.7.v20120910" % "container;provided",
  "org.eclipse.jetty.orbit"  % "javax.servlet"                  % "3.0.0.v201112011016" % "container;compile;provided;test" artifacts (Artifact("javax.servlet", "jar", "jar"))
)

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

ivyXML := <dependencies>
    <exclude module="slf4j-log4j12"/>
    <exclude module="grizzled-slf4j_2.9.1"/>
    <exclude module="jsr311-api" />
  </dependencies>

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html"     => MergeStrategy.discard
    case x => old(x)
  }
}
