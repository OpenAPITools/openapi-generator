lazy val root = (project in file(".")).
  settings(
    organization := "io.swagger",
    name := "swagger-petstore-retrofit2-rx2",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "com.squareup.retrofit2" % "retrofit" % "2.3.0" % "compile",
      "com.squareup.retrofit2" % "converter-scalars" % "2.3.0" % "compile",
      "com.squareup.retrofit2" % "converter-gson" % "2.3.0" % "compile",
      "com.jakewharton.retrofit" % "retrofit2-rxjava2-adapter" % "1.0.0" % "compile",
      "io.reactivex.rxjava2" % "rxjava" % "2.1.1" % "compile",
      "io.swagger" % "swagger-annotations" % "1.5.15" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "org.threeten" % "threetenbp" % "1.3.5" % "compile",
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
