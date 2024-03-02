name := "caskgen"
organization:="org.openapitools"
version := "0.0.1-SNAPSHOT"
scalaVersion := "3.3.1"
scalafmtOnCompile := true
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask" % "0.9.2" ,
  "com.lihaoyi" %% "upickle" % "3.2.0"
)
