package sbtopenapigenerator

import sbt.{Def, ThisBuild, inConfig}
import sbt.Keys.{baseDirectory, aggregate, _}
import sbt.plugins.JvmPlugin
import sbt.librarymanagement.Configuration
import sbtopenapigenerator.configs.OpenApiCodegen
import sbtopenapigenerator.tasks.{OpenApiGenerateTask, OpenApiGeneratorsTask}

object OpenApiGeneratorPlugin extends sbt.AutoPlugin
  with OpenApiGeneratorsTask
  with OpenApiGenerateTask
  with OpenApiCodegen {
  self =>

  override def requires: JvmPlugin.type = sbt.plugins.JvmPlugin

  override def trigger: sbt.PluginTrigger = allRequirements

  object autoImport extends OpenApiGeneratorKeys {
    val OpenApiCodegen = self.OpenApiCodegen

      def SettingEnabled: Option[Boolean] = Some(true)
      def SettingDisabled: Option[Boolean] = Some(false)

  }

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    outputDir := s"${baseDirectory.in(ThisBuild).value}/generated",
    aggregate in (OpenApiCodegen / openApiGenerators) := false
  )


  private lazy val baseSettings: Seq[sbt.Setting[_]] = Seq[sbt.Setting[_]](
    language := "",
    inputSpec := "",
    outputDir := "",
    configFile := "",
    additionalProperties := Map.empty[String, String],
    systemProperties := Map.empty[String, String],
    verbose := None,
    validateSpec := None,
    generatorName := "",
    templateDir := "",
    auth := "",
    skipOverwrite := None,
    packageName := "",
    apiPackage := "",
    modelPackage := "",
    modelNamePrefix := "",
    modelNameSuffix := "",
    instantiationTypes := Map.empty[String, String],
    typeMappings := Map.empty[String, String],
    serverVariables := Map.empty[String, String],
    languageSpecificPrimitives := List[String](),
    importMappings := Map.empty[String, String],
    invokerPackage := "",
    groupId := "",
    id := "",
    library := "",
    gitHost := "",
    gitUserId := "",
    gitRepoId := "",
    releaseNote := "",
    httpUserAgent := "",
    reservedWordsMappings := Map.empty[String, String],
    ignoreFileOverride := "",
    removeOperationIdPrefix := None,
    apiFilesConstrainedTo := List[String](),
    modelFilesConstrainedTo := List[String](),
    supportingFilesConstrainedTo := List[String](),
    generateModelTests := None,
    generateModelDocumentation := None,
    generateApiTests := None,
    generateApiDocumentation := None,
    withXml := None,
    logToStderr := None,
    enablePostProcessFile := None,
    skipValidateSpec := None,
    generateAliasAsModel := None
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = inConfig(OpenApiCodegen)(Seq[sbt.Setting[_]](
    openApiGenerate := openApiGenerateTask.value,
    openApiGenerators := openApiGeneratorsTask.value,
    sources := openApiGenerate.value
  ) ++ baseSettings)

  override def projectConfigurations: List[Configuration] = OpenApiCodegen :: Nil

}
