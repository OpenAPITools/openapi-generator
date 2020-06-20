lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-feign-10x",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.24" % "compile",
      "io.github.openfeign" % "feign-core" % "9.7.0" % "compile",
      "io.github.openfeign" % "feign-jackson" % "9.7.0" % "compile",
      "io.github.openfeign" % "feign-slf4j" % "9.7.0" % "compile",
      "io.github.openfeign.form" % "feign-form" % "2.1.0" % "compile",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.10.3" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.10.3" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.10.3" % "compile",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.9.10" % "compile",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.9.10" % "compile",
      "org.apache.oltu.oauth2" % "org.apache.oltu.oauth2.client" % "1.0.1" % "compile",
      "com.brsanthu" % "migbase64" % "2.2" % "compile",
      "javax.annotation" % "javax.annotation-api" % "1.3.2" % "compile",
      "junit" % "junit" % "4.13" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
