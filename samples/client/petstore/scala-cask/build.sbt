name := "caskgen"
version := "0.0.1-SNAPSHOT"
scalaVersion := "3.4.0"
scalafmtOnCompile := true
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask" % "0.9.2" ,
  "com.lihaoyi" %% "upickle" % "3.2.0",
  "com.softwaremill.sttp.client3" %% "core" % "4.0.0-M9"
)
