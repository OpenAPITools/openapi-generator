lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-resteasy",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.18",
      "org.glassfish.jersey.core" % "jersey-client" % "2.22.2",
      "org.glassfish.jersey.media" % "jersey-media-multipart" % "2.22.2",
      "org.glassfish.jersey.media" % "jersey-media-json-jackson" % "2.22.2",
      "org.jboss.resteasy" % "resteasy-client" % "3.1.3.Final",
      "org.jboss.resteasy" % "resteasy-multipart-provider" % "3.1.3.Final",
      "org.jboss.resteasy" % "resteasy-jackson2-provider" % "3.1.3.Final",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.8.6",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.6",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.6",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.6.4",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.7.5",
      "joda-time" % "joda-time" % "2.9.4",
      "com.brsanthu" % "migbase64" % "2.2",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
