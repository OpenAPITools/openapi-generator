resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)
sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("org.openapitools" % "sbt-openapi-generator" % x)
  case _ =>
    throw new Exception("The system property 'plugin.version' is not defined.")
}