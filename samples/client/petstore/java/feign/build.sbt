lazy val root = (project in file(".")).
  settings(
    organization := "org.openapitools",
    name := "petstore-feign",
    version := "1.0.0",
    scalaVersion := "2.11.12",
    scalacOptions ++= Seq("-feature"),
    compile / javacOptions ++= Seq("-Xlint:deprecation"),
    Compile / packageDoc / publishArtifact := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.6.11" % "compile",
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % "compile",
      "io.github.openfeign" % "feign-core" % "13.5" % "compile",
      "io.github.openfeign" % "feign-jackson" % "13.5" % "compile",
      "io.github.openfeign" % "feign-slf4j" % "13.5" % "compile",
      "io.github.openfeign.form" % "feign-form" % "3.8.0" % "compile",
      "io.github.openfeign" % "feign-okhttp" % "13.5" % "compile",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.17.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.17.1" % "compile",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.1" % "compile",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.17.1" % "compile",
      "com.github.joschi.jackson" % "jackson-datatype-threetenbp" % "2.15.2" % "compile",
      "com.github.scribejava" % "scribejava-core" % "8.0.0" % "compile",
      "com.brsanthu" % "migbase64" % "2.2" % "compile",
      "jakarta.annotation" % "jakarta.annotation-api" % "1.3.5" % "compile",
      "org.junit.jupiter" % "junit-jupiter" % "5.7.0" % "test",
      "org.junit.jupiter" % "junit-jupiter-params" % "5.7.0" % "test",
      "com.github.tomakehurst" % "wiremock-jre8" % "2.35.1" % "test",
      "org.hamcrest" % "hamcrest" % "2.2" % "test",
      "commons-io" % "commons-io" % "2.16.1" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
