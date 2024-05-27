import mill._, scalalib._, scalafmt._, publish._

// Mill build file (see https://mill-build.com/mill/Intro_to_Mill.html).
// run with:
//
// mill _.compile
// mill _.reformat
// mill _.publishLocal
// mill _.test.test
object scala-cask-petstore extends SbtModule with ScalafmtModule with PublishModule {
  def scalaVersion = "3.4.1"

  def pomSettings = PomSettings(
    description = "scala-cask-petstore",
    organization = "cask.groupId",
    url = "https://github.com/<username>/scala-cask-petstore",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("<username>", "scala-cask-petstore"),
    developers = Seq(
      // Developer("<username>", "<your name>", "https://github.com/<you>")
    )
  )

  def publishVersion: mill.T[String] = T("0.0.1")

  def ivyDeps = Agg(
    ivy"com.lihaoyi::cask:0.9.2" ,
    ivy"com.lihaoyi::upickle:3.2.0"
  )

  override def sources = T.sources(millSourcePath / os.up / "shared" / "src" / "main" / "scala")
  override def resources = T.sources(millSourcePath / os.up / "shared" / "src" / "main" / "resources")

  object test extends SbtModuleTests {
    def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:3.2.18",
      ivy"org.scalatest::scalatest:3.2.18"
    )

    def testFramework = "org.scalatest.tools.Framework"
    override def sources = T.sources(millSourcePath / os.up / "src" / "test" / "scala")
  }
}
