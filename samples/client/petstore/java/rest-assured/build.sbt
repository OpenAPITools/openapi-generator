lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-rest-assured",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.21",
      "io.rest-assured" % "scala-support" % "3.1.0",
      "com.google.code.gson" % "gson" % "2.6.1",
      "io.gsonfire" % "gson-fire" % "1.8.2" % "compile",
      "org.threeten" % "threetenbp" % "1.3.5" % "compile",
      "com.squareup.okio" % "okio" % "1.13.0" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
