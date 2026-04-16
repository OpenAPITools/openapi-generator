/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.generator.gradle.plugin.tasks

import org.gradle.api.Action
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.FileSystemOperations
import org.gradle.api.file.ProjectLayout
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.logging.Logging
import org.gradle.api.provider.ListProperty
import org.gradle.api.provider.MapProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.*
import org.gradle.api.tasks.options.Option
import org.gradle.workers.WorkAction
import org.gradle.workers.WorkParameters
import org.gradle.workers.WorkerExecutor
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator
import org.openapitools.codegen.config.GlobalSettings
import org.openapitools.codegen.config.MergedSpecBuilder
import org.openapitools.generator.gradle.plugin.utils.isRemoteUri
import java.io.File
import javax.inject.Inject

// =========================================================================================
// 1. WORKER API PARAMETERS
// Defines the data that safely crosses the ClassLoader boundary.
// =========================================================================================
interface OpenApiWorkParameters : WorkParameters {
    val resolvedInputSpec: Property<String>
    val outputDir: DirectoryProperty
    val configFile: RegularFileProperty
    val verbose: Property<Boolean>
    val validateSpec: Property<Boolean>
    val generatorName: Property<String>
    val auth: Property<String>
    val templateDir: DirectoryProperty
    val templateResourcePath: Property<String>
    val packageName: Property<String>
    val apiPackage: Property<String>
    val modelPackage: Property<String>
    val modelNamePrefix: Property<String>
    val modelNameSuffix: Property<String>
    val apiNameSuffix: Property<String>
    val invokerPackage: Property<String>
    val groupId: Property<String>
    val id: Property<String>
    val version: Property<String>
    val library: Property<String>
    val gitHost: Property<String>
    val gitUserId: Property<String>
    val gitRepoId: Property<String>
    val releaseNote: Property<String>
    val httpUserAgent: Property<String>
    val ignoreFileOverride: RegularFileProperty
    val removeOperationIdPrefix: Property<Boolean>
    val skipOperationExample: Property<Boolean>
    val skipOverwrite: Property<Boolean>
    val logToStderr: Property<Boolean>
    val enablePostProcessFile: Property<Boolean>
    val skipValidateSpec: Property<Boolean>
    val generateAliasAsModel: Property<Boolean>
    val engine: Property<String>
    val dryRun: Property<Boolean>

    val globalProperties: MapProperty<String, String>
    val instantiationTypes: MapProperty<String, String>
    val importMappings: MapProperty<String, String>
    val schemaMappings: MapProperty<String, String>
    val inlineSchemaNameMappings: MapProperty<String, String>
    val inlineSchemaOptions: MapProperty<String, String>
    val nameMappings: MapProperty<String, String>
    val parameterNameMappings: MapProperty<String, String>
    val modelNameMappings: MapProperty<String, String>
    val enumNameMappings: MapProperty<String, String>
    val operationIdNameMappings: MapProperty<String, String>
    val openapiNormalizer: MapProperty<String, String>
    val typeMappings: MapProperty<String, String>
    val additionalProperties: MapProperty<String, Any>
    val serverVariables: MapProperty<String, String>
    val reservedWordsMappings: MapProperty<String, String>
    val configOptions: MapProperty<String, String>

    val languageSpecificPrimitives: ListProperty<String>
    val openapiGeneratorIgnoreList: ListProperty<String>

    val supportingFilesConstrainedTo: ListProperty<String>
    val modelFilesConstrainedTo: ListProperty<String>
    val apiFilesConstrainedTo: ListProperty<String>
    val generateModelTests: Property<Boolean>
    val generateModelDocumentation: Property<Boolean>
    val generateApiTests: Property<Boolean>
    val generateApiDocumentation: Property<Boolean>
}


// =========================================================================================
// 2. WORKER API ACTION
// Executes the actual code generation in an isolated ClassLoader to protect GlobalSettings.
// =========================================================================================
abstract class OpenApiWorkAction : WorkAction<OpenApiWorkParameters> {

    private val logger = Logging.getLogger(OpenApiWorkAction::class.java)

    override fun execute() {
        val params = parameters

        val configurator = if (params.configFile.isPresent) {
            CodegenConfigurator.fromFile(params.configFile.get().asFile.absolutePath)
        } else {
            CodegenConfigurator()
        }

        try {
            // Apply Global Settings
            if (params.supportingFilesConstrainedTo.orNull?.isNotEmpty() == true) {
                GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, params.supportingFilesConstrainedTo.get().joinToString(","))
            } else {
                GlobalSettings.clearProperty(CodegenConstants.SUPPORTING_FILES)
            }

            if (params.modelFilesConstrainedTo.orNull?.isNotEmpty() == true) {
                GlobalSettings.setProperty(CodegenConstants.MODELS, params.modelFilesConstrainedTo.get().joinToString(","))
            } else {
                GlobalSettings.clearProperty(CodegenConstants.MODELS)
            }

            if (params.apiFilesConstrainedTo.orNull?.isNotEmpty() == true) {
                GlobalSettings.setProperty(CodegenConstants.APIS, params.apiFilesConstrainedTo.get().joinToString(","))
            } else {
                GlobalSettings.clearProperty(CodegenConstants.APIS)
            }

            params.generateApiDocumentation.orNull?.let { GlobalSettings.setProperty(CodegenConstants.API_DOCS, it.toString()) }
            params.generateModelDocumentation.orNull?.let { GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, it.toString()) }
            params.generateModelTests.orNull?.let { GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, it.toString()) }
            params.generateApiTests.orNull?.let { GlobalSettings.setProperty(CodegenConstants.API_TESTS, it.toString()) }

            // Apply Configurator Settings
            params.resolvedInputSpec.orNull?.let { configurator.setInputSpec(it) }
            params.outputDir.orNull?.let { configurator.setOutputDir(it.asFile.absolutePath) }
            params.verbose.orNull?.let { configurator.setVerbose(it) }
            params.validateSpec.orNull?.let { configurator.setValidateSpec(it) }
            params.skipOverwrite.orNull?.let { configurator.setSkipOverwrite(it) }
            params.generatorName.orNull?.let { configurator.setGeneratorName(it) }
            params.auth.orNull?.let { configurator.setAuth(it) }

            params.templateDir.orNull?.let { configurator.setTemplateDir(it.asFile.absolutePath) }
            params.templateResourcePath.orNull?.let {
                if (params.templateDir.isPresent) logger.warn("Both templateDir and templateResourcePath were configured. templateResourcePath overwrites templateDir.")
                configurator.setTemplateDir(it)
            }

            params.packageName.orNull?.let { configurator.setPackageName(it) }
            params.apiPackage.orNull?.let { configurator.setApiPackage(it) }
            params.modelPackage.orNull?.let { configurator.setModelPackage(it) }
            params.modelNamePrefix.orNull?.let { configurator.setModelNamePrefix(it) }
            params.modelNameSuffix.orNull?.let { configurator.setModelNameSuffix(it) }
            params.apiNameSuffix.orNull?.let { configurator.setApiNameSuffix(it) }
            params.invokerPackage.orNull?.let { configurator.setInvokerPackage(it) }
            params.groupId.orNull?.let { configurator.setGroupId(it) }
            params.id.orNull?.let { configurator.setArtifactId(it) }
            params.version.orNull?.let { configurator.setArtifactVersion(it) }
            params.library.orNull?.let { configurator.setLibrary(it) }
            params.gitHost.orNull?.let { configurator.setGitHost(it) }
            params.gitUserId.orNull?.let { configurator.setGitUserId(it) }
            params.gitRepoId.orNull?.let { configurator.setGitRepoId(it) }
            params.releaseNote.orNull?.let { configurator.setReleaseNote(it) }
            params.httpUserAgent.orNull?.let { configurator.setHttpUserAgent(it) }
            params.ignoreFileOverride.orNull?.let { configurator.setIgnoreFileOverride(it.asFile.absolutePath) }
            params.removeOperationIdPrefix.orNull?.let { configurator.setRemoveOperationIdPrefix(it) }
            params.skipOperationExample.orNull?.let { configurator.setSkipOperationExample(it) }
            params.logToStderr.orNull?.let { configurator.setLogToStderr(it) }
            params.enablePostProcessFile.orNull?.let { configurator.setEnablePostProcessFile(it) }
            params.skipValidateSpec.orNull?.let { configurator.setValidateSpec(!it) }
            params.generateAliasAsModel.orNull?.let { configurator.setGenerateAliasAsModel(it) }

            params.engine.orNull?.let {
                if ("handlebars".equals(it, ignoreCase = true)) configurator.setTemplatingEngineName("handlebars")
                else configurator.setTemplatingEngineName(it)
            }

            // Maps and Lists
            params.globalProperties.orNull?.forEach { (k, v) -> configurator.addGlobalProperty(k, v) }
            params.instantiationTypes.orNull?.forEach { (k, v) -> configurator.addInstantiationType(k, v) }
            params.importMappings.orNull?.forEach { (k, v) -> configurator.addImportMapping(k, v) }
            params.schemaMappings.orNull?.forEach { (k, v) -> configurator.addSchemaMapping(k, v) }
            params.inlineSchemaNameMappings.orNull?.forEach { (k, v) -> configurator.addInlineSchemaNameMapping(k, v) }
            params.inlineSchemaOptions.orNull?.forEach { (k, v) -> configurator.addInlineSchemaOption(k, v) }
            params.nameMappings.orNull?.forEach { (k, v) -> configurator.addNameMapping(k, v) }
            params.parameterNameMappings.orNull?.forEach { (k, v) -> configurator.addParameterNameMapping(k, v) }
            params.modelNameMappings.orNull?.forEach { (k, v) -> configurator.addModelNameMapping(k, v) }
            params.enumNameMappings.orNull?.forEach { (k, v) -> configurator.addEnumNameMapping(k, v) }
            params.operationIdNameMappings.orNull?.forEach { (k, v) -> configurator.addOperationIdNameMapping(k, v) }
            params.openapiNormalizer.orNull?.forEach { (k, v) -> configurator.addOpenapiNormalizer(k, v) }
            params.typeMappings.orNull?.forEach { (k, v) -> configurator.addTypeMapping(k, v) }
            params.additionalProperties.orNull?.forEach { (k, v) -> configurator.addAdditionalProperty(k, v) }
            params.serverVariables.orNull?.forEach { (k, v) -> configurator.addServerVariable(k, v) }
            params.reservedWordsMappings.orNull?.forEach { (k, v) -> configurator.addAdditionalReservedWordMapping(k, v) }

            params.languageSpecificPrimitives.orNull?.forEach { configurator.addLanguageSpecificPrimitive(it) }
            params.openapiGeneratorIgnoreList.orNull?.forEach { configurator.addOpenapiGeneratorIgnoreList(it) }

            val clientOptInput = configurator.toClientOptInput()
            val codegenConfig = clientOptInput.config

            params.configOptions.orNull?.let { userOptions ->
                codegenConfig.cliOptions().forEach {
                    if (userOptions.containsKey(it.opt)) {
                        clientOptInput.config.additionalProperties()[it.opt] = userOptions[it.opt]
                    }
                }
            }

            // Run Generator
            val isDryRun = params.dryRun.getOrElse(false)
            DefaultGenerator(isDryRun).opts(clientOptInput).generate()

            params.outputDir.orNull?.let { dir ->
                logger.lifecycle("Successfully generated code to ${dir.asFile.absolutePath}")
            }

        } catch (e: Exception) {
            // Gradle's Worker API hides nested exception messages by default.
            // We append the original error message to the top-level GradleException
            // so it prints clearly in the console without needing --stacktrace.
            val errorMessage = e.message ?: e.javaClass.simpleName

            // Optional: You can also log it explicitly to the error channel
            logger.error("OpenAPI code generation failed: $errorMessage", e)

            throw GradleException("OpenAPI code generation failed: $errorMessage", e)
        } finally {
            // Clean up static state in this isolated ClassLoader
            GlobalSettings.reset()
        }
    }
}

/**
 * A task which generates the desired code.
 *
 * Example (CLI):
 *
 * ./gradlew -q openApiGenerate --input=/path/to/file
 *
 * @author Jim Schubert
 */

// =========================================================================================
// 3. GRADLE TASK
// Handles Gradle inputs/outputs, up-to-date checks, and submits work to the Worker API.
// =========================================================================================
@CacheableTask
abstract class GenerateTask : DefaultTask() {

    @get:Inject
    abstract val workerExecutor: WorkerExecutor

    @get:Inject
    abstract val fs: FileSystemOperations

    @get:Inject
    abstract val layout: ProjectLayout

    /**
     * The verbosity of generation
     */
    @get:Optional
    @get:Input
    abstract val verbose: Property<Boolean>

    /**
     * Whether an input specification should be validated upon generation.
     */
    @get:Optional
    @get:Input
    abstract val validateSpec: Property<Boolean>

    /**
     * The name of the generator which will handle codegen. (see "openApiGenerators" task)
     */
    @get:Optional
    @get:Input
    abstract val generatorName: Property<String>

    /**
     * This is the configuration for reference paths where schemas for openapi generation are stored
     * The directory which contains the additional schema files
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.ABSOLUTE)
    abstract val schemaLocation: DirectoryProperty

    /**
     * The output target directory into which code will be generated.
     */
    @get:Optional
    @get:OutputDirectory
    abstract val outputDir: DirectoryProperty

    @Suppress("unused")
    @Option(option = "input", description = "The input specification (local path or URL/URI).")
    fun setInput(value: String) {
        if (value.isNotEmpty()) {
            if (value.isRemoteUri()) {
                remoteInputSpec.set(value)
            } else {
                inputSpec.set(layout.projectDirectory.file(value))
            }
        }
    }

    /**
     * The Open API 2.0/3.x specification location.
     *
     * Be default, Gradle will treat the openApiGenerate task as up-to-date based only on this file, regardless of
     * changes to any $ref referenced files. Use the `inputSpecRootDirectory` property to have Gradle track changes to
     * an entire directory of spec files.
     */
    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpec: RegularFileProperty

    /**
     * Local root folder with spec files.
     *
     * By default, a merged spec file will be generated based on the contents of the directory. To disable this, set the
     * `inputSpecRootDirectorySkipMerge` property.
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpecRootDirectory: DirectoryProperty

    /**
     * Skip bundling all spec files into a merged spec file, if true.
     */
    @get:Input
    @get:Optional
    abstract val inputSpecRootDirectorySkipMerge: Property<Boolean>

    /**
     * Name of the file that will contain all merged specs
     */
    @get:Input
    @get:Optional
    abstract val mergedFileName: Property<String>

    /**
     * The remote Open API 2.0/3.x specification URL location.
     */
    @get:Input
    @get:Optional
    abstract val remoteInputSpec: Property<String>

    /**
     * The template directory holding a custom template.
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val templateDir: DirectoryProperty

    /**
     * Resource path containing template files.
     */
    @get:Optional
    @get:Input
    abstract val templateResourcePath: Property<String>

    /**
     * Adds authorization headers when fetching the OpenAPI definitions remotely.
     * Pass in a URL-encoded string of name:header with a comma separating multiple values
     */
    @get:Optional
    @get:Input
    abstract val auth: Property<String>

    /**
     * Sets specified global properties.
     */
    @get:Optional
    @get:Input
    abstract val globalProperties: MapProperty<String, String>

    /**
     * Path to json configuration file.
     * File content should be in a json format { "optionKey":"optionValue", "optionKey1":"optionValue1"...}
     * Supported options can be different for each language. Run config-help -g {generator name} command for language specific config options.
     */
    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val configFile: RegularFileProperty

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    @get:Optional
    @get:Input
    abstract val skipOverwrite: Property<Boolean>

    /**
     * Package for generated classes (where supported)
     */
    @get:Optional
    @get:Input
    abstract val packageName: Property<String>

    /**
     * Package for generated api classes
     */
    @get:Optional
    @get:Input
    abstract val apiPackage: Property<String>

    /**
     * Package for generated models
     */
    @get:Optional
    @get:Input
    abstract val modelPackage: Property<String>

    /**
     * Prefix that will be prepended to all model names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    abstract val modelNamePrefix: Property<String>

    /**
     * Suffix that will be appended to all model names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    abstract val modelNameSuffix: Property<String>

    /**
     * Suffix that will be appended to all api names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    abstract val apiNameSuffix: Property<String>

    /**
     * Sets instantiation type mappings.
     */
    @get:Optional
    @get:Input
    abstract val instantiationTypes: MapProperty<String, String>

    /**
     * Sets mappings between OpenAPI spec types and generated code types.
     */
    @get:Optional
    @get:Input
    abstract val typeMappings: MapProperty<String, String>

    /**
     * Sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value.
     * You can also have multiple occurrences of this option.
     */
    @get:Optional
    @get:Input
    abstract val additionalProperties: MapProperty<String, Any>

    /**
     * Sets server variable for server URL template substitution, in the format of name=value,name=value.
     * You can also have multiple occurrences of this option.
     */
    @get:Optional
    @get:Input
    abstract val serverVariables: MapProperty<String, String>

    /**
     * Specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double.
     */
    @get:Optional
    @get:Input
    abstract val languageSpecificPrimitives: ListProperty<String>

    /**
     * Specifies .openapi-generator-ignore list in the form of relative/path/to/file1,relative/path/to/file2. For example: README.md,pom.xml.
     */
    @get:Optional
    @get:Input
    abstract val openapiGeneratorIgnoreList: ListProperty<String>

    /**
     * Specifies mappings between a given class and the import that should be used for that class.
     */
    @get:Optional
    @get:Input
    abstract val importMappings: MapProperty<String, String>

    /**
     * Specifies mappings between a given schema and the new one.
     */
    @get:Optional
    @get:Input
    abstract val schemaMappings: MapProperty<String, String>

    /**
     * Specifies mappings between the inline scheme name and the new name
     */
    @get:Optional
    @get:Input
    abstract val inlineSchemaNameMappings: MapProperty<String, String>

    /**
     * Specifies options for inline schemas
     */
    @get:Optional
    @get:Input
    abstract val inlineSchemaOptions: MapProperty<String, String>

    /**
     * Specifies mappings between the property name and the new name
     */
    @get:Optional
    @get:Input
    abstract val nameMappings: MapProperty<String, String>

    /**
     * Specifies mappings between the parameter name and the new name
     */
    @get:Optional
    @get:Input
    abstract val parameterNameMappings: MapProperty<String, String>

    /**
     * Specifies mappings between the model name and the new name
     */
    @get:Optional
    @get:Input
    abstract val modelNameMappings: MapProperty<String, String>

    /**
     * Specifies mappings between the enum name and the new name
     */
    @get:Optional
    @get:Input
    abstract val enumNameMappings: MapProperty<String, String>

    /**
     * Specifies mappings between the operation id name and the new name
     */
    @get:Optional
    @get:Input
    abstract val operationIdNameMappings: MapProperty<String, String>

    /**
     * Specifies mappings (rules) in OpenAPI normalizer
     */
    @get:Optional
    @get:Input
    abstract val openapiNormalizer: MapProperty<String, String>

    /**
     * Root package for generated code.
     */
    @get:Optional
    @get:Input
    abstract val invokerPackage: Property<String>

    /**
     * GroupId in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    abstract val groupId: Property<String>

    /**
     * ArtifactId in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    abstract val id: Property<String>

    /**
     * Artifact version in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    abstract val version: Property<String>

    /**
     * Reference the library template (sub-template) of a generator.
     */
    @get:Optional
    @get:Input
    abstract val library: Property<String>

    /**
     * Git host, e.g. gitlab.com.
     */
    @get:Optional
    @get:Input
    abstract val gitHost: Property<String>

    /**
     * Git user ID, e.g. openapitools.
     */
    @get:Optional
    @get:Input
    abstract val gitUserId: Property<String>

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    @get:Optional
    @get:Input
    abstract val gitRepoId: Property<String>

    /**
     * Release note, default to 'Minor update'.
     */
    @get:Optional
    @get:Input
    abstract val releaseNote: Property<String>

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}/{language}'
     */
    @get:Optional
    @get:Input
    abstract val httpUserAgent: Property<String>

    /**
     * Specifies how a reserved name should be escaped to.
     */
    @get:Optional
    @get:Input
    abstract val reservedWordsMappings: MapProperty<String, String>

    /**
     * Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation.
     */
    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val ignoreFileOverride: RegularFileProperty

    /**
     * Remove prefix of operationId, e.g. config_getId => getId
     */
    @get:Optional
    @get:Input
    abstract val removeOperationIdPrefix: Property<Boolean>

    /**
     * Remove examples defined in the operation
     */
    @get:Optional
    @get:Input
    abstract val skipOperationExample: Property<Boolean>

    /**
     * Defines which API-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * This option enables/disables generation of ALL api-related files.
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val apiFilesConstrainedTo: ListProperty<String>

    /**
     * Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val modelFilesConstrainedTo: ListProperty<String>

    /**
     * Defines which supporting files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * Supporting files are those related to `projects/frameworks` which may be modified
     * by consumers.
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val supportingFilesConstrainedTo: ListProperty<String>

    /**
     * Defines whether model-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val generateModelTests: Property<Boolean>

    /**
     * Defines whether model-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val generateModelDocumentation: Property<Boolean>

    /**
     * Defines whether api-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val generateApiTests: Property<Boolean>

    /**
     * Defines whether api-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    abstract val generateApiDocumentation: Property<Boolean>

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    @get:Optional
    @get:Input
    abstract val logToStderr: Property<Boolean>

    /**
     * To enable the file post-processing hook. This enables executing an external post-processor (usually a linter program).
     * This only enables the post-processor. To define the post-processing command, define an environment variable such as
     * LANG_POST_PROCESS_FILE (e.g. GO_POST_PROCESS_FILE, SCALA_POST_PROCESS_FILE). Please open an issue if your target
     * generator does not support this functionality.
     */
    @get:Optional
    @get:Input
    abstract val enablePostProcessFile: Property<Boolean>

    /**
     * To skip spec validation. When true, we will skip the default behavior of validating a spec before generation.
     */
    @get:Optional
    @get:Input
    abstract val skipValidateSpec: Property<Boolean>

    /**
     * To generate alias (array, list, map) as model. When false, top-level objects defined as array, list, or map will result in those
     * definitions generated as top-level Array-of-items, List-of-items, Map-of-items definitions.
     * When true, A model representation either containing or extending the array,list,map (depending on specific generator implementation) will be generated.
     */
    @get:Optional
    @get:Input
    abstract val generateAliasAsModel: Property<Boolean>

    /**
     * A dynamic map of options specific to a generator.
     */
    @get:Optional
    @get:Input
    abstract val configOptions: MapProperty<String, String>

    /**
     * Templating engine: "mustache" (default) or "handlebars" (beta)
     */
    @get:Optional
    @get:Input
    abstract val engine: Property<String>

    /**
     * Defines whether the output dir should be cleaned up before generating the output.
     *
     */
    @get:Optional
    @get:Input
    abstract val cleanupOutput: Property<Boolean>

    /**
     * Defines whether the generator should run in dry-run mode.
     */
    @get:Optional
    @get:Input
    abstract val dryRun: Property<Boolean>

    init {
        inputSpecRootDirectorySkipMerge.convention(false)
        mergedFileName.convention("merged")
    }

    @Suppress("unused")
    @TaskAction
    fun doWork() {
        var finalResolvedInputSpec = ""

        if (inputSpec.isPresent && remoteInputSpec.isPresent) {
            logger.warn("Both inputSpec and remoteInputSpec are specified. The remoteInputSpec takes priority.")
        }

        inputSpec.orNull?.let { finalResolvedInputSpec = it.asFile.absolutePath }

        remoteInputSpec.orNull?.takeIf { it.isNotEmpty() }?.let {
            finalResolvedInputSpec = it
            logger.warn("Using remoteInputSpec may result in stale build caches if the remote content changes.")
        }

        inputSpecRootDirectory.orNull?.let { inputDir ->
            if (!inputSpecRootDirectorySkipMerge.get()) {
                finalResolvedInputSpec = MergedSpecBuilder(
                    inputDir.asFile.absolutePath,
                    mergedFileName.get()
                ).buildMergedSpec()
                logger.info("Merge input spec used: {}", finalResolvedInputSpec)
            }
        }

        cleanupOutput.orNull?.let { cleanup ->
            if (cleanup && outputDir.isPresent) {
                fs.delete { delete(outputDir) }
                logger.lifecycle("Cleaned up output directory ${outputDir.get().asFile.path} before code generation.")
            }
        }

// Submit generation logic to the isolated Worker API Queue
        val workQueue = workerExecutor.classLoaderIsolation()

        workQueue.submit(OpenApiWorkAction::class.java, object : Action<OpenApiWorkParameters> {
            override fun execute(parameters: OpenApiWorkParameters) {
                parameters.resolvedInputSpec.set(finalResolvedInputSpec)
                parameters.outputDir.set(outputDir)
                parameters.configFile.set(configFile)
                parameters.verbose.set(verbose)
                parameters.validateSpec.set(validateSpec)
                parameters.generatorName.set(generatorName)
                parameters.auth.set(auth)
                parameters.templateDir.set(templateDir)
                parameters.templateResourcePath.set(templateResourcePath)
                parameters.packageName.set(packageName)
                parameters.apiPackage.set(apiPackage)
                parameters.modelPackage.set(modelPackage)
                parameters.modelNamePrefix.set(modelNamePrefix)
                parameters.modelNameSuffix.set(modelNameSuffix)
                parameters.apiNameSuffix.set(apiNameSuffix)
                parameters.invokerPackage.set(invokerPackage)
                parameters.groupId.set(groupId)
                parameters.id.set(id)
                parameters.version.set(version)
                parameters.library.set(library)
                parameters.gitHost.set(gitHost)
                parameters.gitUserId.set(gitUserId)
                parameters.gitRepoId.set(gitRepoId)
                parameters.releaseNote.set(releaseNote)
                parameters.httpUserAgent.set(httpUserAgent)
                parameters.ignoreFileOverride.set(ignoreFileOverride)
                parameters.removeOperationIdPrefix.set(removeOperationIdPrefix)
                parameters.skipOperationExample.set(skipOperationExample)
                parameters.skipOverwrite.set(skipOverwrite)
                parameters.logToStderr.set(logToStderr)
                parameters.enablePostProcessFile.set(enablePostProcessFile)
                parameters.skipValidateSpec.set(skipValidateSpec)
                parameters.generateAliasAsModel.set(generateAliasAsModel)
                parameters.engine.set(engine)
                parameters.dryRun.set(dryRun)

                parameters.globalProperties.set(globalProperties)
                parameters.instantiationTypes.set(instantiationTypes)
                parameters.importMappings.set(importMappings)
                parameters.schemaMappings.set(schemaMappings)
                parameters.inlineSchemaNameMappings.set(inlineSchemaNameMappings)
                parameters.inlineSchemaOptions.set(inlineSchemaOptions)
                parameters.nameMappings.set(nameMappings)
                parameters.parameterNameMappings.set(parameterNameMappings)
                parameters.modelNameMappings.set(modelNameMappings)
                parameters.enumNameMappings.set(enumNameMappings)
                parameters.operationIdNameMappings.set(operationIdNameMappings)
                parameters.openapiNormalizer.set(openapiNormalizer)
                parameters.typeMappings.set(typeMappings)
                parameters.additionalProperties.set(additionalProperties)
                parameters.serverVariables.set(serverVariables)
                parameters.reservedWordsMappings.set(reservedWordsMappings)
                parameters.configOptions.set(configOptions)

                parameters.languageSpecificPrimitives.set(languageSpecificPrimitives)
                parameters.openapiGeneratorIgnoreList.set(openapiGeneratorIgnoreList)
                parameters.supportingFilesConstrainedTo.set(supportingFilesConstrainedTo)
                parameters.modelFilesConstrainedTo.set(modelFilesConstrainedTo)
                parameters.apiFilesConstrainedTo.set(apiFilesConstrainedTo)
                parameters.generateModelTests.set(generateModelTests)
                parameters.generateModelDocumentation.set(generateModelDocumentation)
                parameters.generateApiTests.set(generateApiTests)
                parameters.generateApiDocumentation.set(generateApiDocumentation)
            }
        })
    }

    // ========================================================================
    // Kotlin DSL extension functions for property setters
    // These allow Kotlin DSL users to call .set(String) on file/directory properties
    // when configuring tasks directly (e.g., tasks.named<GenerateTask>("openApiGenerate") { ... })
    // ========================================================================

    /**
     * Extension function to allow setting file properties with a String path in Kotlin DSL.
     * Example: inputSpec.set("$rootDir/api.yaml")
     */
    fun RegularFileProperty.set(path: String) {
        when (this) {
            inputSpec -> {
                if (path.isRemoteUri()) {
                    remoteInputSpec.set(path)
                } else {
                    this.set(layout.projectDirectory.file(path))
                }
            }
            configFile, ignoreFileOverride -> {
                this.set(layout.projectDirectory.file(path))
            }
            else -> {
                // Fallback for any other RegularFileProperty
                this.set(layout.projectDirectory.file(path))
            }
        }
    }

    /**
     * Extension function to allow setting directory properties with a String path in Kotlin DSL.
     * Example: outputDir.set("$buildDir/generated")
     */
    fun DirectoryProperty.set(path: String) {
        // All directory properties use the same conversion logic
        this.set(layout.projectDirectory.dir(path))
    }

    // ========================================================================
    // Groovy DSL bridge methods
    // These methods allow Groovy DSL users to set properties using String paths.
    // Groovy's property syntax allows calling these as:
    //   - Method style: setInputSpecAsString("$rootDir/api.yaml")
    //   - Property style: inputSpecAsString = "$rootDir/api.yaml"
    // ========================================================================

    /**
     * Groovy-compatible setter for inputSpec property.
     * Accepts a String and automatically routes to remote or local file based on URI detection.
     * Clears the opposite property to prevent stale values from taking precedence.
     */
    fun setInputSpecAsString(path: String) {
        if (path.isRemoteUri()) {
            remoteInputSpec.set(path)
            inputSpec.set(null as File?)  // Clear local file to prevent conflicts
        } else {
            inputSpec.set(layout.projectDirectory.file(path))
            remoteInputSpec.set(null as String?)  // Clear remote URL to prevent conflicts
        }
    }

    /**
     * Groovy-compatible setter for configFile property.
     */
    fun setConfigFileAsString(path: String) {
        configFile.set(layout.projectDirectory.file(path))
    }

    /**
     * Groovy-compatible setter for ignoreFileOverride property.
     */
    fun setIgnoreFileOverrideAsString(path: String) {
        ignoreFileOverride.set(layout.projectDirectory.file(path))
    }

    /**
     * Groovy-compatible setter for templateDir property.
     */
    fun setTemplateDirAsString(path: String) {
        templateDir.set(layout.projectDirectory.dir(path))
    }

    /**
     * Groovy-compatible setter for outputDir property.
     */
    fun setOutputDirAsString(path: String) {
        outputDir.set(layout.projectDirectory.dir(path))
    }

    /**
     * Groovy-compatible setter for inputSpecRootDirectory property.
     */
    fun setInputSpecRootDirectoryAsString(path: String) {
        inputSpecRootDirectory.set(layout.projectDirectory.dir(path))
    }

    /**
     * Groovy-compatible setter for schemaLocation property.
     */
    fun setSchemaLocationAsString(path: String) {
        schemaLocation.set(layout.projectDirectory.dir(path))
    }
}