import com.github.siasia.PluginKeys._ 

organization := "com.wordnik"

name := "swagger-scalatra-server"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.2"

seq(webSettings :_*)

libraryDependencies ++= Seq(
  "org.scalatra"                 % "scalatra"               % "2.2.0-SNAPSHOT",
  "org.scalatra"                 % "scalatra-scalate"       % "2.2.0-SNAPSHOT",
  "org.scalatra"                 % "scalatra-specs2"        % "2.2.0-SNAPSHOT"     % "test",
  "org.scalatra"                 % "scalatra-swagger"       % "2.2.0-SNAPSHOT",
  "org.scalatra"                 % "scalatra-json"          % "2.2.0-SNAPSHOT",
  "org.json4s"                  %% "json4s-jackson"         % "3.0.0-SNAPSHOT",
  "com.wordnik"                  % "swagger-core_2.9.1"     % "1.1.1-SNAPSHOT",
  "ch.qos.logback"               % "logback-classic"        % "1.0.6" % "runtime",
  "org.eclipse.jetty"            % "jetty-webapp"           % "8.1.5.v20120716"     % "container",
  "org.eclipse.jetty.orbit"      % "javax.servlet"          % "3.0.0.v201112011016" % "container;provided;test" artifacts (Artifact("javax.servlet", "jar", "jar")))


resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

apps in container.Configuration <<= (deployment in Compile) map (d => Seq("/api" -> d))

ivyXML := <dependencies><exclude module="slf4j-log4j12"/></dependencies>
