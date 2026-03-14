lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-rest-assured",
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
      "com.google.code.gson" % "gson" % "2.8.9",
      "io.gsonfire" % "gson-fire" % "1.9.0" % "compile",
      "com.squareup.okio" % "okio" % "1.17.5" % "compile",
      "jakarta.validation" % "jakarta.validation-api" % "3.0.2" % "compile",
      "org.hibernate" % "hibernate-validator" % "6.0.19.Final" % "compile",
    "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
