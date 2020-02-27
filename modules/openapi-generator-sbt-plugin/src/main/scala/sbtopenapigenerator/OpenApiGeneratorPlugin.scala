package sbtopenapigenerator

import sbt._
import sbt.{Def, ThisBuild}
import sbt.Keys.{baseDirectory, aggregate, _}
import sbt.plugins.JvmPlugin
import sbtopenapigenerator.tasks.{OpenApiGenerateTask, OpenApiGeneratorsTask}

object OpenApiGeneratorPlugin extends sbt.AutoPlugin
  with OpenApiGeneratorsTask
  with OpenApiGenerateTask
 {
  self =>

  override def requires: JvmPlugin.type = sbt.plugins.JvmPlugin

  override def trigger: sbt.PluginTrigger = allRequirements

  object autoImport extends OpenApiGeneratorKeys {

      def SettingEnabled: Option[Boolean] = Some(true)
      def SettingDisabled: Option[Boolean] = Some(false)


  }
   val out = taskKey[Seq[File]]("Out")

  val OpenApiCodegen = config("openApiCodegen")

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    aggregate in openApiGenerators := false
  )

  private lazy val baseSettings: Seq[sbt.Setting[_]] = Seq[sbt.Setting[_]](
    openApiInputSpec := "",
    openApiOutputDir := "",
    openApiConfigFile := "",
    openApiAdditionalProperties := Map.empty[String, String],
    openApiSystemProperties := Map.empty[String, String],
    openApiVerbose := None,
    openApiValidateSpec := None,
    openApiGeneratorName := "",
    openApiTemplateDir := "",
    openApiAuth := "",
    openApiSkipOverwrite := None,
    openApiPackageName := "",
    openApiApiPackage := "",
    openApiModelPackage := "",
    openApiModelNamePrefix := "",
    openApiModelNameSuffix := "",
    openApiInstantiationTypes := Map.empty[String, String],
    openApiTypeMappings := Map.empty[String, String],
    openApiServerVariables := Map.empty[String, String],
    openApiLanguageSpecificPrimitives := List[String](),
    openApiImportMappings := Map.empty[String, String],
    openApiInvokerPackage := "",
    openApiGroupId := "",
    openApiId := "",
    openApiLibrary := "",
    openApiGitHost := "",
    openApiGitUserId := "",
    openApiGitRepoId := "",
    openApiReleaseNote := "",
    openApiHttpUserAgent := "",
    openApiReservedWordsMappings := Map.empty[String, String],
    openApiIgnoreFileOverride := "",
    openApiRemoveOperationIdPrefix := None,
    openApiApiFilesConstrainedTo := List[String](),
    openApiModelFilesConstrainedTo := List[String](),
    openApiSupportingFilesConstrainedTo := List[String](),
    openApiGenerateModelTests := None,
    openApiGenerateModelDocumentation := None,
    openApiGenerateApiTests := None,
    openApiGenerateApiDocumentation := None,
    openApiWithXml := None,
    openApiLogToStderr := None,
    openApiEnablePostProcessFile := None,
    openApiSkipValidateSpec := None,
    openApiGenerateAliasAsModel := None,
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = Seq[sbt.Setting[_]](
    openApiGenerators := openApiGeneratorsTask.value,
    openApiGenerate := openApiGenerateTask.value,
  ) ++ baseSettings

}
