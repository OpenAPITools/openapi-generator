lazy val root = (project in file(".")).
  settings(
    organization := "io.swagger",
    name := "swagger-java-client",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "com.typesafe.play" % "play-java-ws_2.11" % "2.4.11" % "compile",
      "com.squareup.retrofit2" % "retrofit" % "2.1.0" % "compile",
      "com.squareup.retrofit2" % "converter-scalars" % "2.1.0" % "compile",
      "com.squareup.retrofit2" % "converter-gson" % "2.1.0" % "compile",
      "com.squareup.retrofit2" % "converter-jackson" % "2.1.0" % "compile",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.7.5" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.7.5" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.7.5" % "compile",
      "io.swagger" % "swagger-annotations" % "1.5.12" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "joda-time" % "joda-time" % "2.9.4" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
