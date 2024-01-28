lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-retrofit2-rx3",
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
      "com.github.akarnokd" % "rxjava3-retrofit-adapter" % "3.0.0" % "compile",
      "io.reactivex.rxjava3" % "rxjava" % "3.0.4" % "compile",
      "io.swagger" % "swagger-annotations" % "1.5.21" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "io.gsonfire" % "gson-fire" % "1.8.0" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "junit" % "junit" % "4.13.2" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
