lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "okhttp-jackson3-oneOf",
    version := "1.0.0",
    scalaVersion := "2.13.6",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % "3.0.2",
      "com.squareup.okhttp3" % "okhttp" % "5.4.0",
      "com.squareup.okhttp3" % "logging-interceptor" % "5.4.0",
      "tools.jackson.core" % "jackson-core" % "3.2.1",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.22",
      "tools.jackson.core" % "jackson-databind" % "3.2.1",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.11",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
