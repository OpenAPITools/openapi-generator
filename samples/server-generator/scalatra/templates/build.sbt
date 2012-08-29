organization := "com.wordnik"

name := "swagger-scalatra-server"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory.identity

seq(webSettings :_*)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.0.0" % "compile;container;test;runtime",
  "org.scalatra" % "scalatra" % "2.2.0-SNAPSHOT",
  "org.scalatra" % "scalatra-scalate" % "2.2.0-SNAPSHOT",
  "org.scalatra" % "scalatra-specs2" % "2.2.0-SNAPSHOT" % "test",
  "org.scalatra" % "scalatra-swagger" % "2.2.0-SNAPSHOT",
  "com.wordnik" % "swagger-core_2.9.1" % "1.1.0",
  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
  "org.eclipse.jetty" % "jetty-server" % "8.1.3.v20120416" % "compile;container;test;runtime",
  "org.eclipse.jetty" % "jetty-servlet" % "8.1.3.v20120416" % "compile;container;test;runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.3.v20120416" % "compile;container;test;runtime",
  "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided;container;test;runtime"
)

resolvers += Resolver.url("local-ivy", new URL("file://" + Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

ivyXML := <dependencies>
            <exclude org="org.eclipse.jetty.orbit" />
          </dependencies>