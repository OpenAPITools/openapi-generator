lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-okhttp",
    version := "1.0.0",
    scalaVersion := "2.13.6",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-annotations" % "2.2.15",
      "com.google.code.findbugs" % "jsr305" % "3.0.2",
      "com.squareup.okhttp3" % "okhttp" % "5.4.0",
      "com.squareup.okhttp3" % "logging-interceptor" % "5.4.0",
      "com.google.code.gson" % "gson" % "2.10.1",
      "io.gsonfire" % "gson-fire" % "1.9.0",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.11",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
