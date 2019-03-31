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
      "io.swagger" % "swagger-annotations" % "1.5.21" % "compile",
      "org.jboss.resteasy" % "resteasy-client" % "3.1.3.Final" % "compile",
      "org.jboss.resteasy" % "resteasy-multipart-provider" % "3.1.3.Final" % "compile",
      "org.jboss.resteasy" % "resteasy-jackson2-provider" % "3.1.3.Final" % "compile",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.8.11" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.11" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.11.3" % "compile",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.6.4" % "compile",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.7.5" % "compile",
      "joda-time" % "joda-time" % "2.9.9" % "compile",
      "com.brsanthu" % "migbase64" % "2.2" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
