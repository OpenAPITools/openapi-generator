scalaVersion := "2.13.16"
scalacOptions += "-Ymacro-annotations"

val circeVersion = "0.14.5"
def circe(artifact: String): ModuleID = "io.circe" %% s"circe-$artifact" % circeVersion

val http4sVersion = "0.23.23"
def http4s(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sVersion

val refinedVersion = "0.9.29"
val refined = Seq(
  "eu.timepit" %% "refined"      % refinedVersion,
  "eu.timepit" %% "refined-cats" % refinedVersion
)

val catsVersion = "2.10.0"
val cats = Seq("org.typelevel" %% "cats-core" % catsVersion)

lazy val compilerPlugins = Seq(
  compilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.3.1"),
  compilerPlugin("org.typelevel"   %% "kind-projector"     % "0.13.3" cross CrossVersion.full)
)

libraryDependencies ++= (Seq(
  http4s("core"), http4s("ember-server"), http4s("circe"), http4s("dsl"),
  circe("core"), circe("generic"), circe("parser"), circe("refined")
) ++ refined ++ cats ++ compilerPlugins)
