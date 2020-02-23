scalaVersion := "2.12.10"

enablePlugins(OpenApiGeneratorPlugin)

lazy val generated = project.in(file("generated"))
  .settings(
    inConfig(OpenApiCodegen) {
      Seq(
        inputSpec := "openapi.yaml",
        configFile := "config.yaml",
        validateSpec := SettingDisabled,
        generateModelTests := SettingEnabled,
      )
    }
  )

lazy val root = (project in file("."))
  .settings(
    name := "openapi-generator-example",
  )
  .dependsOn(generated)
  .aggregate(generated)


unmanagedSourceDirectories in Compile += baseDirectory.value / "generated"

