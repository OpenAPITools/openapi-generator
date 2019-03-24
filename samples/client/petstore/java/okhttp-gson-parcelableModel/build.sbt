lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-okhttp-gson-parcelableModel",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.21",
      "com.squareup.okhttp3" % "okhttp" % "3.13.1",
      "com.squareup.okhttp3" % "logging-interceptor" % "3.13.1",
      "com.google.code.gson" % "gson" % "2.8.5",
      "org.apache.commons" % "commons-lang3" % "3.8.1",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1",
      "org.threeten" % "threetenbp" % "1.3.5" % "compile",
      "io.gsonfire" % "gson-fire" % "1.8.3" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
