/*
 * Original code copied from https://github.com/mikybars/openapi-generator-mill-plugin
 * Original code published under the MIT License
 * Original Copyright Miguel Ibars
 */
package org.openapitools.generator.mill

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import mainargs.arg
import mill.T
import mill.api.{PathRef, Result, Task}
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator
import org.openapitools.codegen.config.GlobalSettings
import org.openapitools.codegen.validations.oas.{OpenApiEvaluator, RuleConfiguration}
import os.{Path, RelPath}
import upickle.ReadWriter as RW

import scala.jdk.CollectionConverters.*
import scala.jdk.javaapi.CollectionConverters

/**
 * Usage:
 * {{{
 *   object myModule extends JavaModule with OpenApiModule {
 *
 *      object openApiServer extends OpenApiConfig {
 *        def inputSpec: T[PathRef] = Task.Source(BuildCtx.workspaceRoot / "api" / "server-api.yaml")
 *        def apiPackage: T[String] = "com.acme.foo.boundary.web.api"
 *        def modelPackage: T[String] = "com.acme.foo.boundary.web.model"
 *        def generatorName: T[String] = "spring"
 *        def sourceFolder: T[String] = "src/main/java"
 *        def additionalProperties: T[Map[String, String]] = Map(
 *          "useSpringBoot3" -> "true",
 *          "dateLibrary" -> "java8",
 *          "interfaceOnly" -> "true",
 *          "performBeanValidation" -> "true",
 *          "useBeanValidation" -> "false",
 *          "skipDefaultInterface" -> "true",
 *          "useTags" -> "true",
 *        )
 *      }
 *
 *      object openApiClient extends OpenApiConfig {
 *        def inputSpec = T[PathRef] = Task.Source(BuildCtx.workspaceRoot / "api" / "some-client-api.yaml")
 *        def apiPackage: T[String] = "com.acme.foo.boundary.client.some.api"
 *        def modelPackage: T[String] = "com.acme.foo.boundary.client.some.model"
 *        def generatorName: T[String] = "java"
 *        def modelNameSuffix: T[String] = "Dto"
 *        def sourceFolder: T[String] = "src/main/java"
 *        def additionalProperties: T[Map[String, String]] = Map(
 *          "useTags" -> "true",
 *          "dateLibrary" -> "java8",
 *          "library" -> "webclient",
 *          "useJakartaEe" -> "true",
 *          "useOneOfInterfaces" -> "true",
 *          "useAbstractionForFiles" -> "true",
 *        )
 *      }
 *
 *      override def generatedSources: T[Seq[PathRef]] = Seq(
 *        PathRef(Task.dest),
 *        openApiServer.generate(),
 *        openApiClient.generate(),
 *      )
 *   }
 * }}}
 */
trait OpenApiModule extends mill.api.Module {

  trait OpenApiConfig extends mill.api.Module {
    
    case class ArtifactSettings(
                                 groupId: String, 
                                 artifactId: String, 
                                 artifactVersion: Option[String] = None
                               ) derives RW
    
    case class GitSettings(
                            host: String,
                            userId: String,
                            repoId: String
                          ) derives RW
    
    
    /** The Open API 2.0/3.x specification location. */
    def inputSpec: T[PathRef]

    /** The name of the generator which will handle codegen. */
    def generatorName: T[String]

    /** Package for generated api classes. */
    def apiPackage: T[String]

    /**
     * The additional folder passed to the generator. Usually this is done via the [[additionalProperties]] but since
     * some generators also use different defaults while others use none, this property needs to be set and will
     * override anything set in [[additionalProperties]].
     *
     * This is necessary so the plugin can construct the correct source folder structure.
     */
    def sourceFolder: T[String]

    /**
     * Suffix that will be appended to all api names. Default is the empty string.
     */
    def apiNameSuffix: T[String] = ""

    /**
     * Adds authorization headers when fetching the OpenAPI definitions remotely.
     * Pass in a URL-encoded string of name:header with a comma separating multiple values
     */
    def auth: T[Option[String]] = None

    /**
     * Sets custom User-Agent header value
     */
    def httpUserAgent: T[Option[String]] = None

    /** Package for generated model classes. */
    def modelPackage: T[String]

    /**
     * Prefix that will be prepended to all model names. Default is the empty string.
     */
    def modelNamePrefix: T[String] = ""

    /**
     * Suffix that will be appended to all model names. Default is the empty string.
     */
    def modelNameSuffix: T[String] = ""

    /** Sets additional properties that can be referenced by the mustache templates. */
    def additionalProperties: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Defines the user's target type.
     * {{{ "OffsetDateTime" -> "java.time.Instant" }}}
     *
     * @see [[https://openapi-generator.tech/docs/usage/#type-mappings-and-import-mappings]]
     * */
    def typeMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Informs the template of the type to be imported. Needed when type mappings are used.
     * {{{ "OffsetDateTime" -> "java.time.Instant" }}}
     * Since the [[typeMappings]] are used to change the default types, the import mappings are used to map the imports.
     *
     * @see [[https://openapi-generator.tech/docs/usage/#type-mappings-and-import-mappings]]
     * */
    def importMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between a given schema and the new one.
     */
    def schemaMappings: T[Map[String, String]] = Map.empty[String, String]

    /** Specify if the spec should be validated. Default is true. */
    def validateSpec: T[Boolean] = true

    /**
     * Specifies an override location for the .openapi-generator-ignore file.
     */
    def ignoreFileOverride: T[Option[Path]] = None

    /**
     * Specifies how a reserved name should be escaped to.
     */
    def reservedWordsMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Remove examples defined in the operation
     */
    def skipOperationExample: T[Boolean] = false

    /**
     * Defines which API-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * This option enables/disables generation of ALL api-related files.
     *
     * NOTE: Configuring any one of [[apiFilesConstrainedTo]], [[modelFilesConstrainedTo]], or [[supportingFilesConstrainedTo]] results
     * in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     * For more control over the generation of individual files, configure an ignored file and refer to it via [[ignoreFileOverride]].
     */
    def apiFilesConstrainedTo: T[Seq[String]] = Seq.empty[String]

    /**
     * Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [[apiFilesConstrainedTo]], [[modelFilesConstrainedTo]], or [[supportingFilesConstrainedTo]] results
     * in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     * For more control over the generation of individual files, configure an ignored file and refer to it via [[ignoreFileOverride]].
     */
    def modelFilesConstrainedTo: T[Seq[String]] = Seq.empty[String]

    /**
     * Defines which supporting files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * Supporting files are those related to `projects/frameworks` which may be modified
     * by consumers.
     *
     * NOTE: Configuring any one of [[apiFilesConstrainedTo]], [[modelFilesConstrainedTo]], or [[supportingFilesConstrainedTo]] results
     * in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     * For more control over the generation of individual files, configure an ignored file and refer to it via [[ignoreFileOverride]].
     */
    def supportingFilesConstrainedTo: T[Seq[String]] = Seq.empty[String]

    /**
     * Generate the APIs. Default is true.
     */
    def generateApis: T[Boolean] = true

    /**
     * Defines whether api-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignored file and
     * refer to it via [[ignoreFileOverride]].
     */
    def generateApiDocs: T[Boolean] = true

    /**
     * Defines whether api-related _test_ files should be generated.
     *
     * This option is currently disabled because Mill does not distinguish between normal- and test-sources.
     */
    // TODO figure out a clean way to support this
    final def generateApiTests: T[Boolean] = false

    /**
     * Generate the Models. Default is true.
     */
    def generateModels: T[Boolean] = true

    /**
     * Defines whether model-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignored file and
     * refer to it via [[ignoreFileOverride]].
     */
    def generateModelDocs: T[Boolean] = true

    /**
     * Defines whether model-related _test_ files should be generated.
     *
     * This option is currently disabled because Mill does not distinguish between normal- and test-sources.
     */
    // TODO figure out a clean way to support this
    def generateModelTests: T[Boolean] = false

    /**
     * Generate the supporting files. Default is true.
     */
    def generateSupportingFiles: T[Boolean] = true

    /**
     * Generate the models recursively if models should generate selectively (see [[modelFilesConstrainedTo]]) and all
     * dependent models are to generate.
     * Is enabled by default when [[modelFilesConstrainedTo]] is not empty for convenience.
     */
    def generateRecursiveDependentModels: T[Boolean] = modelFilesConstrainedTo().nonEmpty

    /**
     * Templating engine: "mustache" (default) or "handlebars" (beta)
     */
    def engine: T[Option[String]] = None

    /**
     * Specifies mappings between the inline scheme name and the new name
     */
    def inlineSchemaNameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies options for inline schemas
     */
    def inlineSchemaOptions: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between the property name and the new name
     */
    def nameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between the parameter name and the new name
     */
    def parameterNameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between the model name and the new name
     */
    def modelNameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between the enum name and the new name
     */
    def enumNameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings between the operation id name and the new name
     */
    def operationIdNameMappings: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Specifies mappings (rules) in OpenAPI normalizer
     */
    def openapiNormalizer: T[Map[String, String]] = Map.empty[String, String]

    /**
     * Root package for generated code.
     */
    def invokerPackage: T[Option[String]] = None

    /**
     * Artifact coordinates/packages used in generated build files.
     */
    def artifactSettings: T[Option[ArtifactSettings]] = None
    
    /**
     * Reference the library template (sub-template) of a generator.
     */
    def library: T[Option[String]] = None

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    def logToStderr: T[Boolean] = false

    /**
     * To enable the file post-processing hook. This enables executing an external post-processor (usually a linter program).
     * This only enables the post-processor. To define the post-processing command, define an environment variable such as
     * LANG_POST_PROCESS_FILE (e.g. GO_POST_PROCESS_FILE, SCALA_POST_PROCESS_FILE). Please open an issue if your target
     * generator does not support this functionality.
     */
    def enablePostProcessFile: T[Boolean] = false

    /**
     * Folder containing the template files.
     */
    def templateDirectory: T[Option[Path]] = None

    /**
     * To remove operationId prefix (e.g. user_getName => getName)
     */
    def removeOperationIdPrefix: T[Boolean] = false

    /**
     * To treat a document strictly against the spec. Default is true.
     */
    def strictSpec: T[Boolean] = true

    /**
     * Specifies additional language-specific primitive types in the format of type1,type2,type3,type3. For example, `String,boolean,Boolean,Double`.
     */
    def languageSpecificPrimitives: T[Set[String]] = Set.empty[String]

    /**
     * Git repository used in generated documentation. 
     */
    def gitSettings: T[Option[GitSettings]] = None
    
    /**
     * Sets specified global properties.
     */
    def globalProperties: T[Map[String, String]] = Map.empty[String, String]
    /**
     * To skip spec validation. When true, we will skip the default behavior of validating a spec before generation.
     */
    def skipValidateSpec: T[Boolean] = false

    /**
     * To generate alias (array, list, map) as model. When false, top-level objects defined as array, list, or map will result in those
     * definitions generated as top-level Array-of-items, List-of-items, Map-of-items definitions.
     * When true, A model representation either containing or extending the array,list,map (depending on specific generator implementation) will be generated.
     */
    def generateAliasAsModel: T[Boolean] = false

    /**
     * Defines whether the output dir should be cleaned up before generating the output.
     */
    def cleanupOutput: T[Boolean] = false

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    def skipOverwrite: T[Boolean] = false

    /**
     * Defines whether the generator should run in dry-run mode.
     */
    def dryRun: T[Boolean] = false


    /**
     * An additional Task, which can be run after the generation phase.
     *
     * For instance, in case you have a shared type which is defined in your importMappings,
     * the generator will still generate the type even though it is not used. The following
     * cleanup will remove all files which are are not used.
     * {{{
     *   override def cleanup: Task[Option[Path => Unit]] = Task.Anon {
     *     Some(
     *       (path: Path) => {
     *         val filenames = (for {
     *           (_, fqn) <- importMappings()
     *           if fqn.startsWith("my.relevant.package.")
     *         } yield fqn.split('.').last).toSeq
     *
     *         os.walk(path)
     *           .filter { p => os.isFile(p) && filenames.contains(p.baseName) }
     *           .foreach { p => os.remove(p) }
     *       })
     *   }
     * }}}
     * This will match all full-qualified-names against the provided package and delete the matches.
     * Note: this sample requires that custom type packages are completely separate.
     */
    def cleanup: Task[Option[Path => Unit]] = Task.Anon {
      None
    }

    /**
     * Runs the OpenAPI generator with the given configuration.
     */
    def generate: T[PathRef] = Task {
      val configurator = CodegenConfigurator()
      // don't call setAdditionalProperties with an immutable Scala Map, because the Setters after this one
      // might add to the attributes as well (which will then cause an Exception)
      additionalProperties().foreach((k, v) => configurator.addAdditionalProperty(k, v))

      configurator.setApiNameSuffix(apiNameSuffix())
        .setApiPackage(apiPackage())
        .setEnumNameMappings(enumNameMappings().asJava)
        .setGeneratorName(generatorName())
        .setGlobalProperties(globalProperties().asJava)
        .setInputSpec(inputSpec().path.toString())
        .setInlineSchemaNameMappings(inlineSchemaNameMappings().asJava)
        .setInlineSchemaOptions(inlineSchemaOptions().asJava)
        .setImportMappings(importMappings().asJava)
        .setLanguageSpecificPrimitives(languageSpecificPrimitives().asJava)
        .setModelNameMappings(modelNameMappings().asJava)
        .setModelNamePrefix(modelNamePrefix())
        .setModelNameSuffix(modelNameSuffix())
        .setModelPackage(modelPackage())
        .setNameMappings(nameMappings().asJava)
        .setOperationIdNameMappings(operationIdNameMappings().asJava)
        .setOpenapiNormalizer(openapiNormalizer().asJava)
        // should output-dir be configurable like in Gradle (don't think so)
        .setOutputDir(Task.dest.toString())
        .setParameterNameMappings(parameterNameMappings().asJava)
        .setReservedWordsMappings(reservedWordsMappings().asJava)
        .setSchemaMappings(schemaMappings().asJava)
        .setTypeMappings(typeMappings().asJava)
        .setValidateSpec(validateSpec())

      if(generateApis()){
        GlobalSettings.setProperty(CodegenConstants.APIS, apiFilesConstrainedTo().mkString(","))
      }
      if(generateModels()){
        GlobalSettings.setProperty(CodegenConstants.MODELS, modelFilesConstrainedTo().mkString(","))
      }
      GlobalSettings.setProperty(CodegenConstants.GENERATE_RECURSIVE_DEPENDENT_MODELS, generateRecursiveDependentModels().toString)
      if(generateSupportingFiles()){
        GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, supportingFilesConstrainedTo().mkString(","))
      }

      GlobalSettings.setProperty(CodegenConstants.API_DOCS, generateApiDocs().toString)
      GlobalSettings.setProperty(CodegenConstants.API_TESTS, generateApiTests().toString)
      GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, generateModelDocs().toString)
      GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, generateModelTests().toString)

      engine() match {
        case Some(s) if s.equalsIgnoreCase("handlebars") => configurator.setTemplatingEngineName("handlebars")
        case Some(s) => configurator.setTemplatingEngineName(s) // in case other engines are supported
        case None => () // use default
      }

      auth().filter(_.nonEmpty).foreach(authUrl => configurator.setAuth(authUrl))
      httpUserAgent().foreach(userAgent => configurator.setHttpUserAgent(userAgent))
      ignoreFileOverride().foreach(file => configurator.setIgnoreFileOverride(file.toNIO.toAbsolutePath.toString))
      invokerPackage().foreach(value => configurator.setInvokerPackage(value))
      artifactSettings().foreach(settings =>
        configurator.setGroupId(settings.groupId)
        configurator.setArtifactId(settings.artifactId)
        settings.artifactVersion.foreach(value => configurator.setArtifactVersion(value))
      )
      library().foreach(value => configurator.setLibrary(value))
      templateDirectory().foreach(file => configurator.setTemplateDir(file.toNIO.toAbsolutePath.toString))
      gitSettings().foreach(git =>
        configurator.setGitHost(git.host)
        configurator.setGitUserId(git.userId)
        configurator.setGitRepoId(git.repoId)
      )
      if (logToStderr()) {
        configurator.setLogToStderr(true)
      }
      if (enablePostProcessFile()) {
        configurator.setEnablePostProcessFile(true)
      }
      if (skipValidateSpec()) {
        configurator.setValidateSpec(false)
      }
      if (generateAliasAsModel()) {
        configurator.setGenerateAliasAsModel(true)
      }
      if (removeOperationIdPrefix()) {
        configurator.setRemoveOperationIdPrefix(true)
      }
      if (skipOperationExample()) {
        configurator.setSkipOperationExample(true)
      }
      if (strictSpec()) {
        configurator.setStrictSpecBehavior(true)
      }
      if (skipOverwrite()) {
        configurator.setSkipOverwrite(true)
      }

      if (cleanupOutput()) {
        os.remove.all(Task.dest)
        os.makeDir.all(Task.dest)
        Task.log.info(s"Cleaned up output directory ${Task.dest} before code generation (cleanupOutput set to true).")
      }
      val dryRunSetting = dryRun()

      // set source-folder as last to override potential duplicate
      configurator.addAdditionalProperty("sourceFolder", sourceFolder())

      DefaultGenerator(dryRunSetting).opts(configurator.toClientOptInput).generate()
      Task.log.info(s"Successfully generated code to ${Task.dest}")
      cleanup() match {
        case Some(f) => f(Task.dest)
        case None => // no-op
      }

      PathRef(Task.dest / RelPath(sourceFolder()))
    }

    /**
     * Validates currently configured [[inputSpec]]. This task outputs a list of validation issues and errors.
     *
     * @param recommend      prints warnings for recommended fixes. Default is true.
     * @param failOnWarnings fails the call when there are warnings. Default is false.
     * @return
     */
    def validate(
                  recommend: Boolean = true,
                  failOnWarnings: Boolean = false
                ): Task.Command[Unit] = Task.Command {
      given log: mill.api.Logger = Task.log
      runValidation(inputSpec().path, recommend, failOnWarnings)
    }
  }

  /**
   * Command which checks a passed OpenAPI definition.
   * This task outputs a list of validation issues and errors.
   *
   * @param spec           the path to the file to be validated.
   * @param recommend      prints warnings for recommended fixes. Default is true.
   * @param failOnWarnings fails the call when there are warnings. Default is false.
   */
  def validateOpenapiSpec(
                           @arg(positional = true)
                           spec: String,
                           recommend: Boolean = true,
                           failOnWarnings: Boolean = false
                         ): Task.Command[Unit] = Task.Command {
    given log: mill.api.Logger = Task.log
    runValidation(os.Path(spec), recommend, failOnWarnings)
  }

  private def runValidation(spec: Path, recommend: Boolean, failOnWarnings: Boolean)(using log: mill.api.Logger) = {
    log.info(s"Validating spec $spec")

    val options = ParseOptions()
    options.setResolve(true)

    val result = OpenAPIParser().readLocation(spec.toNIO.toAbsolutePath.toString, null, options)
    val messages = CollectionConverters.asScala(result.getMessages).toSet

    val ruleConfiguration = RuleConfiguration()
    ruleConfiguration.setEnableRecommendations(recommend)

    val evaluator = OpenApiEvaluator(ruleConfiguration)
    val validationResult = evaluator.validate(result.getOpenAPI)
    val warnings = CollectionConverters.asScala(validationResult.getWarnings)
    val errors = CollectionConverters.asScala(validationResult.getErrors)

    if (warnings.nonEmpty) {
      val sb = StringBuilder("Spec has issues or recommendations.\nIssues:\n")
      warnings.foreach(w => {
        sb.append(s"\t${w.getMessage}\n")
        log.debug(s"WARNING: ${w.getMessage}|${w.getDetails}")
      })
      log.info(sb.toString())
    }

    if (messages.nonEmpty || errors.nonEmpty) {
      val sb = new StringBuilder("Spec is invalid.\nIssues:\n")

      messages.foreach(m => {
        sb.append(s"\t$m\n")
        log.debug(s"ERROR: $m")
      })
      errors.foreach(e => {
        sb.append(s"\t$e\n")
        log.debug(s"ERROR: ${e.getMessage}|${e.getDetails}")
      })
      log.error(sb.toString())
      Result.Failure("Validation failed.")
    } else if (failOnWarnings && warnings.nonEmpty) {
      log.error("Warnings found in the spec and 'treatWarningsAsErrors' is enabled.\nFailing validation.\n")
      Result.Failure("Validation failed due to warnings (treatWarningsAsErrors = true).")
    } else {
      log.info("No error validations from swagger-parser or internal validations.")
      Result.Success("Spec is valid.")
    }
  }
}
