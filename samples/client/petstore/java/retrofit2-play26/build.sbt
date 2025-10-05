lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-java-client-retrofit2-play26",
    version := "1.0.0",
    scalaVersion := "2.11.12",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "com.squareup.retrofit2" % "retrofit" % "2.11.0" % "compile",
      "com.squareup.retrofit2" % "converter-scalars" % "2.11.0" % "compile",
      "com.typesafe.play" % "play-ahc-ws_2.12" % "2.6.7" % "compile",
      "jakarta.validation" % "jakarta.validation-api" % "3.0.2" % "compile",
      "com.squareup.retrofit2" % "converter-jackson" % "2.11.0" % "compile",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.17.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.17.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.1" % "compile",
      "io.swagger" % "swagger-annotations" % "1.5.21" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "io.gsonfire" % "gson-fire" % "1.9.0" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.3" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
