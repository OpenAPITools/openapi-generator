lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-okhttp-jsonb-jspecify",
    version := "1.0.0",
    scalaVersion := "2.13.6",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "org.jspecify" % "jspecify" % "1.0.0",
      "com.squareup.okhttp3" % "okhttp" % "5.4.0",
      "com.squareup.okhttp3" % "logging-interceptor" % "5.4.0",
      "jakarta.json.bind" % "jakarta.json.bind-api" % "3.0.1",
      "org.eclipse" % "yasson" % "3.0.4",
      "jakarta.json" % "jakarta.json-api" % "2.1.3",
      "org.eclipse.parsson" % "parsson" % "1.1.7",
      "org.apache.commons" % "commons-lang3" % "3.18.0",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
