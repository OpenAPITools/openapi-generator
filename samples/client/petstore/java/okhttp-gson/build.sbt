lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-okhttp-gson",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.24",
      "com.squareup.okhttp3" % "okhttp" % "4.9.1",
      "com.squareup.okhttp3" % "logging-interceptor" % "4.9.1",
      "com.google.code.gson" % "gson" % "2.8.6",
      "org.apache.commons" % "commons-lang3" % "3.10",
      "javax.ws.rs" % "jsr311-api" % "1.1.1",
      "javax.ws.rs" % "javax.ws.rs-api" % "2.0",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.2",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1",
      "io.gsonfire" % "gson-fire" % "1.8.3" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "junit" % "junit" % "4.13.2" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
