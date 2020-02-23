lazy val `sbt-openapi-generator` = Project("sbt-openapi-generator", file("."))
resolvers ++= Seq(
  Resolver.sbtPluginRepo("snapshots"),
  Resolver.sonatypeRepo("snapshots")
)
scalaVersion := "2.12.10"

name := "sbt-openapi-generator"
description := """
This plugin supports common functionality found in Open API Generator CLI as a sbt plugin.

This gives you the ability to generate client SDKs, documentation, new generators, and to validate Open API 2.0 and 3.x
specifications as part of your build. Other tasks are available as command line tasks.
"""
homepage := Some(url("https://openapi-generator.tech"))

organization := "org.openapitools"
organizationName := "OpenAPI-Generator Contributors"
organizationHomepage := Some(url("https://github.com/OpenAPITools"))

licenses += ("The Apache Software License, Version 2.0", url("https://www.apache.org/licenses/LICENSE-2.0.txt"))

developers += Developer(
  id = "openapitools",
  name = "OpenAPI-Generator Contributors",
  email = "team@openapitools.org",
  url = url("https://github.com/OpenAPITools")
)

scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://github.com/OpenAPITools/openapi-generator"),
    connection = "scm:git:git://github.com/OpenAPITools/openapi-generator.git",
    devConnection = "scm:git:ssh://git@github.com:OpenAPITools/openapi-generator.git")
)

bintrayOrganization := Some("sbt")
bintrayRepository := "sbt-plugins"
bintrayReleaseOnPublish in ThisBuild := false

crossSbtVersions := List("0.13.17", "1.1.5")

libraryDependencies += "org.openapitools" % "openapi-generator" % version.value

enablePlugins(SbtPlugin)

scriptedLaunchOpts := {
  scriptedLaunchOpts.value ++ Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
}
scriptedBufferLog := false
