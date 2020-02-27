package sbtopenapigenerator.tasks

import org.openapitools.codegen.{CodegenConstants, DefaultGenerator}
import org.openapitools.codegen.config.{CodegenConfigurator, GlobalSettings}
import sbt.{Def, _}
import sbt.Keys._
import sbtopenapigenerator.OpenApiGeneratorKeys

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait OpenApiGenerateTask extends OpenApiGeneratorKeys {

  protected[this] def openApiGenerateTask: Def.Initialize[Task[Seq[File]]] = Def.task {

    val logger = sbt.Keys.streams.value.log

    if (openApiConfigFile.value.isEmpty && openApiGeneratorName.value.isEmpty) {
      Seq()
    } else {
      val configurator: CodegenConfigurator = if (openApiConfigFile.value.nonEmpty) {
        val config = openApiConfigFile.value
        logger.info(s"read configuration from $config")
        CodegenConfigurator.fromFile(config)
      } else new CodegenConfigurator()

      if (openApiSupportingFilesConstrainedTo.value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, openApiSupportingFilesConstrainedTo.value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.SUPPORTING_FILES)
      }

      if (openApiModelFilesConstrainedTo.value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.MODELS, openApiModelFilesConstrainedTo.value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.MODELS)
      }

      if (openApiApiFilesConstrainedTo.value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.APIS, openApiApiFilesConstrainedTo.value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.APIS)
      }

      openApiGenerateApiDocumentation.value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.API_DOCS, value.toString)
      }

      openApiGenerateModelDocumentation.value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, value.toString)
      }

      openApiGenerateModelTests.value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, value.toString)
      }

      openApiGenerateApiTests.value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.API_TESTS, value.toString)
      }

      openApiWithXml.value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.WITH_XML, value.toString)
      }

      // now override with any specified parameters
      openApiVerbose.value.foreach { value =>
        configurator.setVerbose(value)
      }
      openApiValidateSpec.value.foreach { value =>
        configurator.setValidateSpec(value)
      }

      openApiSkipOverwrite.value.foreach { value =>
        configurator.setSkipOverwrite(value)
      }

      if (openApiInputSpec.value.nonEmpty) {
        configurator.setInputSpec(openApiInputSpec.value)
      }


      if (openApiGeneratorName.value.nonEmpty) {
        configurator.setGeneratorName(openApiGeneratorName.value)
      }

      if (openApiOutputDir.value.nonEmpty) {
        configurator.setOutputDir(openApiOutputDir.value)
      }

      if (openApiAuth.value.nonEmpty) {
        configurator.setAuth(openApiAuth.value)
      }

      if (openApiTemplateDir.value.nonEmpty) {
        configurator.setTemplateDir(openApiTemplateDir.value)
      }

      if (openApiPackageName.value.nonEmpty) {
        configurator.setPackageName(openApiPackageName.value)
      }

      if (openApiApiPackage.value.nonEmpty) {
        configurator.setApiPackage(openApiApiPackage.value)
      }

      if (openApiModelPackage.value.nonEmpty) {
        configurator.setModelPackage(openApiModelPackage.value)
      }

      if (openApiModelNamePrefix.value.nonEmpty) {
        configurator.setModelNamePrefix(openApiModelNamePrefix.value)
      }

      if (openApiModelNameSuffix.value.nonEmpty) {
        configurator.setModelNameSuffix(openApiModelNameSuffix.value)
      }

      if (openApiInvokerPackage.value.nonEmpty) {
        configurator.setInvokerPackage(openApiInvokerPackage.value)
      }

      if (openApiGroupId.value.nonEmpty) {
        configurator.setGroupId(openApiGroupId.value)
      }

      if (openApiId.value.nonEmpty) {
        configurator.setArtifactId(openApiId.value)
      }

      if ((version in openApiGenerate).value.nonEmpty) {
        configurator.setArtifactVersion(version.value)
      }

      if (openApiLibrary.value.nonEmpty) {
        configurator.setLibrary(openApiLibrary.value)
      }

      if (openApiGitHost.value.nonEmpty) {
        configurator.setGitHost(openApiGitHost.value)
      }

      if (openApiGitUserId.value.nonEmpty) {
        configurator.setGitUserId(openApiGitUserId.value)
      }


      if (openApiGitRepoId.value.nonEmpty) {
        configurator.setGitRepoId(openApiGitRepoId.value)
      }

      if (openApiReleaseNote.value.nonEmpty) {
        configurator.setReleaseNote(openApiReleaseNote.value)
      }

      if (openApiHttpUserAgent.value.nonEmpty) {
        configurator.setHttpUserAgent(openApiHttpUserAgent.value)
      }

      if (openApiIgnoreFileOverride.value.nonEmpty) {
        configurator.setIgnoreFileOverride(openApiIgnoreFileOverride.value)
      }

      openApiRemoveOperationIdPrefix.value.foreach { value =>
        configurator.setRemoveOperationIdPrefix(value)
      }

      openApiLogToStderr.value.foreach { value =>
        configurator.setLogToStderr(value)
      }

      openApiEnablePostProcessFile.value.foreach { value =>
        configurator.setEnablePostProcessFile(value)
      }

      openApiSkipValidateSpec.value.foreach { value =>
        configurator.setValidateSpec(!value)
      }

      openApiGenerateAliasAsModel.value.foreach { value =>
        configurator.setGenerateAliasAsModel(value)
      }

      if (openApiSystemProperties.value.nonEmpty) {
        openApiSystemProperties.value.foreach { entry =>
          configurator.addSystemProperty(entry._1, entry._2)
        }
      }

      if (openApiInstantiationTypes.value.nonEmpty) {
        openApiInstantiationTypes.value.foreach { entry =>
          configurator.addInstantiationType(entry._1, entry._2)
        }
      }

      if (openApiImportMappings.value.nonEmpty) {
        openApiImportMappings.value.foreach { entry =>
          configurator.addImportMapping(entry._1, entry._2)
        }
      }

      if (openApiTypeMappings.value.nonEmpty) {
        openApiTypeMappings.value.foreach { entry =>
          configurator.addTypeMapping(entry._1, entry._2)
        }
      }

      if (openApiAdditionalProperties.value.nonEmpty) {
        openApiAdditionalProperties.value.foreach { entry =>
          configurator.addAdditionalProperty(entry._1, entry._2)
        }
      }

      if (openApiServerVariables.value.nonEmpty) {
        openApiServerVariables.value.foreach { entry =>
          configurator.addServerVariable(entry._1, entry._2)
        }
      }

      if (openApiLanguageSpecificPrimitives.value.nonEmpty) {
        openApiLanguageSpecificPrimitives.value.foreach { it =>
          configurator.addLanguageSpecificPrimitive(it)
        }
      }

      if (openApiReservedWordsMappings.value.nonEmpty) {
        openApiReservedWordsMappings.value.foreach { entry =>
          configurator.addAdditionalReservedWordMapping(entry._1, entry._2)
        }
      }

      Try(configurator.toClientOptInput) match {
        case Success(clientOptInput) =>
          Try {
            val gen = new DefaultGenerator().opts(clientOptInput)
            val res = gen.generate().asScala

            logger.out(s"Successfully generated code to ${clientOptInput.getConfig.getOutputDir}")
            res
          } match {
            case Success(value) => value
            case Failure(ex) =>
              throw new Exception("Code generation failed.", ex)
          }
        case Failure(ex) =>
          logger.info(ex.getMessage)
          Seq()
      }
    }
  }
}
