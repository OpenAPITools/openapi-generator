name := """swagger-java-playframework"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
javaJdbc,
cache,
javaWs,
"io.swagger" %% "swagger-play2" % "1.5.3",
"org.webjars" % "swagger-ui" % "2.2.8",
"javax.validation" % "validation-api" % "1.1.0.Final"
)
