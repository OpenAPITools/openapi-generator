lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-rest-assured-jackson",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.6.6",
      "io.rest-assured" % "rest-assured" % "4.5.1",
      "io.rest-assured" % "scala-support" % "4.5.1",
      "com.google.code.findbugs" % "jsr305" % "3.0.2",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.13.4",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.13.4",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.13.4.2",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.4",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.13.4.1",
      "com.squareup.okio" % "okio" % "1.17.5" % "compile",
      "jakarta.validation" % "jakarta.validation-api" % "2.0.2" % "compile",
      "org.hibernate" % "hibernate-validator" % "6.0.19.Final" % "compile",
    "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "junit" % "junit" % "4.13.2" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
