lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-rest-assured-jackson3",
    version := "1.0.0",
    scalaVersion := "2.11.12",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.6.16",
      "io.rest-assured" % "rest-assured" % "6.0.1",
      "io.rest-assured" % "scala-support" % "6.0.1",
      "com.google.code.findbugs" % "jsr305" % "3.0.2",
      "tools.jackson.core" % "jackson-core" % "3.1.6",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.21",
      "tools.jackson.core" % "jackson-databind" % "3.1.6",
      "org.openapitools" % "jackson-databind-nullable" % "0.2.11",
      "com.squareup.okio" % "okio" % "1.17.5" % "compile",
      "jakarta.validation" % "jakarta.validation-api" % "3.0.2" % "compile",
      "org.hibernate" % "hibernate-validator" % "6.0.19.Final" % "compile",
    "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
