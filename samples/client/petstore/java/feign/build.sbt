lazy val root = (project in file(".")).
  settings(
    organization := "io.swagger",
    name := "swagger-petstore-feign",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.9" % "compile",
      "com.netflix.feign" % "feign-core" % "8.16.0" % "compile",
      "com.netflix.feign" % "feign-jackson" % "8.17.0" % "compile",
      "com.netflix.feign" % "feign-slf4j" % "8.16.0" % "compile",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.6.4" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "com.brsanthu" % "migbase64" % "2.2" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
