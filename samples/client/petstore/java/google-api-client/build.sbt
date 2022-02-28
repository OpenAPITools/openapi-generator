lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-google-api-client",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.22",
      "com.google.api-client" % "google-api-client" % "1.23.0",
      "org.glassfish.jersey.core" % "jersey-common" % "2.25.1",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.12.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.10.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.10.5.1" % "compile",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.9.10" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "junit" % "junit" % "4.13.2" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
