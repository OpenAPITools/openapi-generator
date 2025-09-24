lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-rest-assured-jackson",
    version := "1.0.0",
    scalaVersion := "2.11.12",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.6.16",
      "io.rest-assured" % "rest-assured" % "5.5.6",
      "io.rest-assured" % "scala-support" % "5.5.6",
      "com.google.code.findbugs" % "jsr305" % "3.0.2",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.19.2",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.19.2",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.19.2",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.7",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.13.4.1",
      "com.squareup.okio" % "okio" % "1.17.5" % "compile",
      "jakarta.validation" % "jakarta.validation-api" % "3.0.2" % "compile",
      "org.hibernate" % "hibernate-validator" % "6.0.19.Final" % "compile",
    "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
