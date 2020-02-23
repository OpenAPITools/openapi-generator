package sbtopenapigenerator.tasks

import org.openapitools.codegen.{CodegenConstants, DefaultGenerator}
import org.openapitools.codegen.config.{CodegenConfigurator, GlobalSettings}
import sbt.{Def, _}
import sbt.Keys._
import sbtopenapigenerator.OpenApiGeneratorKeys
import sbtopenapigenerator.configs.OpenApiCodegen

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait OpenApiGenerateTask extends OpenApiCodegen with OpenApiGeneratorKeys {

  protected[this] def openApiGenerateTask: Def.Initialize[Task[Seq[File]]] = Def.task {

    val logger = sbt.Keys.streams.value.log


    if ((OpenApiCodegen / configFile).value.isEmpty && (OpenApiCodegen / generatorName).value.isEmpty) {
      Seq()
    } else {
      val configurator: CodegenConfigurator = if ((OpenApiCodegen / configFile).value.nonEmpty) {
        val config = (OpenApiCodegen / configFile).value
        logger.info(s"read configuration from $config")
        CodegenConfigurator.fromFile(config)
      } else new CodegenConfigurator()

      if ((OpenApiCodegen / supportingFilesConstrainedTo).value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, (OpenApiCodegen / supportingFilesConstrainedTo).value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.SUPPORTING_FILES)
      }

      if ((OpenApiCodegen / modelFilesConstrainedTo).value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.MODELS, (OpenApiCodegen / modelFilesConstrainedTo).value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.MODELS)
      }

      if ((OpenApiCodegen / apiFilesConstrainedTo).value.nonEmpty) {
        GlobalSettings.setProperty(CodegenConstants.APIS, (OpenApiCodegen / apiFilesConstrainedTo).value.mkString(","))
      } else {
        GlobalSettings.clearProperty(CodegenConstants.APIS)
      }

      (OpenApiCodegen / generateApiDocumentation).value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.API_DOCS, value.toString)
      }

      (OpenApiCodegen / generateModelDocumentation).value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, value.toString)
      }

      (OpenApiCodegen / generateModelTests).value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, value.toString)
      }

      (OpenApiCodegen / generateApiTests).value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.API_TESTS, value.toString)
      }

      (OpenApiCodegen / withXml).value.foreach { value =>
        GlobalSettings.setProperty(CodegenConstants.WITH_XML, value.toString)
      }

      // now override with any specified parameters
      (OpenApiCodegen / verbose).value.foreach { value =>
        configurator.setVerbose(value)
      }
      (OpenApiCodegen / validateSpec).value.foreach { value =>
        configurator.setValidateSpec(value)
      }

      (OpenApiCodegen / skipOverwrite).value.foreach { value =>
        configurator.setSkipOverwrite(value)
      }

      if ((OpenApiCodegen / inputSpec).value.nonEmpty) {
        configurator.setInputSpec((OpenApiCodegen / inputSpec).value)
      }


      if ((OpenApiCodegen / generatorName).value.nonEmpty) {
        configurator.setGeneratorName((OpenApiCodegen / generatorName).value)
      }

      if ((OpenApiCodegen / outputDir).value.nonEmpty) {
        configurator.setOutputDir((OpenApiCodegen / outputDir).value)
      }

      if ((OpenApiCodegen / auth).value.nonEmpty) {
        configurator.setAuth((OpenApiCodegen / auth).value)
      }

      if ((OpenApiCodegen / templateDir).value.nonEmpty) {
        configurator.setTemplateDir((OpenApiCodegen / templateDir).value)
      }

      if ((OpenApiCodegen / packageName).value.nonEmpty) {
        configurator.setPackageName((OpenApiCodegen / packageName).value)
      }

      if ((OpenApiCodegen / apiPackage).value.nonEmpty) {
        configurator.setApiPackage((OpenApiCodegen / apiPackage).value)
      }

      if ((OpenApiCodegen / modelPackage).value.nonEmpty) {
        configurator.setModelPackage((OpenApiCodegen / modelPackage).value)
      }

      if ((OpenApiCodegen / modelNamePrefix).value.nonEmpty) {
        configurator.setModelNamePrefix((OpenApiCodegen / modelNamePrefix).value)
      }

      if ((OpenApiCodegen / modelNameSuffix).value.nonEmpty) {
        configurator.setModelNameSuffix((OpenApiCodegen / modelNameSuffix).value)
      }

      if ((OpenApiCodegen / invokerPackage).value.nonEmpty) {
        configurator.setInvokerPackage((OpenApiCodegen / invokerPackage).value)
      }

      if ((OpenApiCodegen / groupId).value.nonEmpty) {
        configurator.setGroupId((OpenApiCodegen / groupId).value)
      }

      if ((OpenApiCodegen / id).value.nonEmpty) {
        configurator.setArtifactId((OpenApiCodegen / id).value)
      }

      if ((version in openApiGenerate).value.nonEmpty) {
        configurator.setArtifactVersion((OpenApiCodegen / version).value)
      }

      if ((OpenApiCodegen / library).value.nonEmpty) {
        configurator.setLibrary((OpenApiCodegen / library).value)
      }

      if ((OpenApiCodegen / gitHost).value.nonEmpty) {
        configurator.setGitHost((OpenApiCodegen / gitHost).value)
      }

      if ((OpenApiCodegen / gitUserId).value.nonEmpty) {
        configurator.setGitUserId((OpenApiCodegen / gitUserId).value)
      }


      if ((OpenApiCodegen / gitRepoId).value.nonEmpty) {
        configurator.setGitRepoId((OpenApiCodegen / gitRepoId).value)
      }

      if ((OpenApiCodegen / releaseNote).value.nonEmpty) {
        configurator.setReleaseNote((OpenApiCodegen / releaseNote).value)
      }

      if ((OpenApiCodegen / httpUserAgent).value.nonEmpty) {
        configurator.setHttpUserAgent((OpenApiCodegen / httpUserAgent).value)
      }

      if ((OpenApiCodegen / ignoreFileOverride).value.nonEmpty) {
        configurator.setIgnoreFileOverride((OpenApiCodegen / ignoreFileOverride).value)
      }

      (OpenApiCodegen / removeOperationIdPrefix).value.foreach { value =>
        configurator.setRemoveOperationIdPrefix(value)
      }

      (OpenApiCodegen / logToStderr).value.foreach { value =>
        configurator.setLogToStderr(value)
      }

      (OpenApiCodegen / enablePostProcessFile).value.foreach { value =>
        configurator.setEnablePostProcessFile(value)
      }

      (OpenApiCodegen / skipValidateSpec).value.foreach { value =>
        configurator.setValidateSpec(!value)
      }

      (OpenApiCodegen / generateAliasAsModel).value.foreach { value =>
        configurator.setGenerateAliasAsModel(value)
      }

      if ((OpenApiCodegen / systemProperties).value.nonEmpty) {
        (OpenApiCodegen / systemProperties).value.foreach { entry =>
          configurator.addSystemProperty(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / instantiationTypes).value.nonEmpty) {
        (OpenApiCodegen / instantiationTypes).value.foreach { entry =>
          configurator.addInstantiationType(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / importMappings).value.nonEmpty) {
        (OpenApiCodegen / importMappings).value.foreach { entry =>
          configurator.addImportMapping(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / typeMappings).value.nonEmpty) {
        (OpenApiCodegen / typeMappings).value.foreach { entry =>
          configurator.addTypeMapping(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / additionalProperties).value.nonEmpty) {
        (OpenApiCodegen / additionalProperties).value.foreach { entry =>
          configurator.addAdditionalProperty(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / serverVariables).value.nonEmpty) {
        (OpenApiCodegen / serverVariables).value.foreach { entry =>
          configurator.addServerVariable(entry._1, entry._2)
        }
      }

      if ((OpenApiCodegen / languageSpecificPrimitives).value.nonEmpty) {
        (OpenApiCodegen / languageSpecificPrimitives).value.foreach { it =>
          configurator.addLanguageSpecificPrimitive(it)
        }
      }

      if ((OpenApiCodegen / reservedWordsMappings).value.nonEmpty) {
        (OpenApiCodegen / reservedWordsMappings).value.foreach { entry =>
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
