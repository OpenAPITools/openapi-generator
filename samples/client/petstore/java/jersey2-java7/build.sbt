lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-jersey2-java7",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.22",
      "org.glassfish.jersey.core" % "jersey-client" % "2.25.1",
      "org.glassfish.jersey.media" % "jersey-media-multipart" % "2.25.1",
      "org.glassfish.jersey.media" % "jersey-media-json-jackson" % "2.25.1",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.10.4" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.10.4" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.10.4" % "compile",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.9.10" % "compile",
      "com.brsanthu" % "migbase64" % "2.2",
      "junit" % "junit" % "4.13" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
