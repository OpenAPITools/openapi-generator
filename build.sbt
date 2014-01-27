import xml.Group
import AssemblyKeys._

organization := "com.wordnik"

name := "swagger-codegen"

version := "2.0.13"

scalaVersion := "2.10.0"

crossVersion := CrossVersion.full

javacOptions ++= Seq("-target", "1.6", "-source", "1.6", "-Xlint:unchecked", "-Xlint:deprecation")

scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

//crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.9.3", "2.10.0", "2.10.1", "2.10.2")

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"     % "3.2.5",
  "io.backchat.inflector"       %% "scala-inflector"    % "1.3.5",
  "commons-io"                   % "commons-io"         % "2.3",
  "ch.qos.logback"               % "logback-classic"    % "1.0.13" % "provided",
  "org.rogach"                  %% "scallop"            % "0.9.4",
  "junit"                        % "junit"              % "4.11" % "test",
  "org.scalatest"               %% "scalatest"          % "1.9.1" % "test"
)

libraryDependencies <+= scalaVersion {
  case v if v.startsWith("2.9") => 
    "org.fusesource.scalate" % "scalate-core_2.9" % "1.6.1"
  case v if v.startsWith("2.10") => { 
    "org.fusesource.scalate" %% "scalate-core" % "1.6.1"
  }
}

libraryDependencies ++= {
  scalaVersion.toString match {
    case v if v.startsWith("2.10") => Seq("org.scala-lang" % "scala-reflect" % v)
    case _ => Seq()
  }
}

packageOptions <+= (name, version, organization) map {
  (title, version, vendor) =>
    Package.ManifestAttributes(
      "Created-By" -> "Simple Build Tool",
      "Built-By" -> System.getProperty("user.name"),
      "Build-Jdk" -> System.getProperty("java.version"),
      "Specification-Title" -> title,
      "Specification-Version" -> version,
      "Specification-Vendor" -> vendor,
      "Implementation-Title" -> title,
      "Implementation-Version" -> version,
      "Implementation-Vendor-Id" -> vendor,
      "Implementation-Vendor" -> vendor
    )
}

publishTo <<= (version) { version: String =>
  if (version.trim.endsWith("SNAPSHOT"))
    Some("Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    Some("Sonatype Nexus Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}


publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))


publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

homepage := Some(new URL("https://github.com/wordnik/swagger-codegen"))

parallelExecution in Test := false

startYear := Some(2009)

licenses := Seq(("Apache License 2.0", new URL("http://www.apache.org/licenses/LICENSE-2.0.html")))

pomExtra <<= (pomExtra, name, description) {(pom, name, desc) => pom ++ Group(
  <scm>
    <connection>scm:git:git@github.com:wordnik/swagger-codegen.git</connection>
    <developerConnection>scm:git:git@github.com:wordnik/swagger-codegen.git</developerConnection>
    <url>https://github.com/wordnik/swagger-codegen</url>
  </scm>
  <issueManagement>
    <system>github</system>
    <url>https://github.com/wordnik/swagger-codegen/issues</url>
  </issueManagement>
  <developers>
    <developer>
      <id>rpidikiti</id>
      <name>Ramesh Pidikiti</name>
      <email>ramesh@wordnik.com</email>
    </developer>
    <developer>
      <id>ayush</id>
      <name>Ayush Gupta</name>
      <email>ayush@glugbot.com</email>
    </developer>
    <developer>
      <id>fehguy</id>
      <name>Tony Tam</name>
      <email>fehguy@gmail.com</email>
    </developer>
    <developer>
      <id>casualjim</id>
      <name>Ivan Porto Carrero</name>
      <url>http://flanders.co.nz/</url>
    </developer>
    <developer>
      <id>radius314</id>
      <name>Danny Gershman</name>
      <email>danny.gershman@gmail.com</email>
    </developer>
  </developers>
)}

assemblySettings

jarName in assembly := "swagger-codegen.jar"
