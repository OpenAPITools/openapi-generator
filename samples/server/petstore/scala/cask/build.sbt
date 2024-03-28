name := "caskgen"
organization:="org.openapitools"
version := "0.0.1-SNAPSHOT"
scalaVersion := "3.3.1"
scalafmtOnCompile := true
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask" % "0.9.2" ,
  "com.lihaoyi" %% "upickle" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

publishMavenStyle := true

val githubUser = "aaron"
val githubRepo = "caskgen"
publishTo := Some("GitHub Package Registry" at s"https://maven.pkg.github.com/$githubUser/$githubRepo")

sys.env.get("GITHUB_TOKEN") match {
  case Some(token) if !token.isEmpty =>
    credentials += Credentials(
      "GitHub Package Registry",
      "maven.pkg.github.com",
      githubUser,
      sys.env.getOrElse("GITHUB_TOKEN", {println("GITHUB_TOKEN not set - assuming a local build"); "GITHUB_TOKEN is unset"})
    )
  case _ =>
    println("\n\t\tGITHUB_TOKEN not set - assuming a local build\n\n")
    credentials ++= Nil
}