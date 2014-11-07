val wordnikSnapshots = "Wordnik Snapshots" at "https://ci.aws.wordnik.com/artifactory/m2-snapshots/"

val wordnikReleases = "Wordnik Releases" at "https://ci.aws.wordnik.com/artifactory/m2-releases/"

val wordnikRemoteRepos = "Wordnik Remote Repos" at "https://ci.aws.wordnik.com/artifactory/remote-repos/"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimize", "-Xcheckinit", "-encoding", "utf8")

version := "1.6.1"

publishTo <<= (version) { version: String =>
  val artifactory = "https://ci.aws.wordnik.com/artifactory/m2-"
  if (version.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at artifactory + "snapshots")
  else
    Some("releases"  at artifactory + "releases")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

credentials += Credentials("Artifactory Realm", "ci.aws.wordnik.com", "mavenuser", "DEEaffe987a")

resolvers ++= Seq(wordnikSnapshots, wordnikReleases, wordnikRemoteRepos)