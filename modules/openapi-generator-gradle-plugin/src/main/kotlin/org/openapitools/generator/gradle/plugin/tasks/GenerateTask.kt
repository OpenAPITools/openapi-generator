/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.generator.gradle.plugin.tasks

import org.gradle.api.Action
import javax.inject.Inject
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.FileSystemOperations
import org.gradle.api.file.ProjectLayout
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.ListProperty
import org.gradle.api.provider.MapProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputDirectory
import org.gradle.api.tasks.InputFile
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.PathSensitive
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.options.Option
import org.gradle.workers.WorkAction
import org.gradle.workers.WorkParameters
import org.gradle.workers.WorkerExecutor
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator
import org.openapitools.codegen.config.GlobalSettings
import org.openapitools.codegen.config.MergedSpecBuilder
import org.gradle.api.logging.Logging
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

    @get:Optional
    @get:Input
    abstract val verbose: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val validateSpec: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val generatorName: Property<String>

    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.ABSOLUTE)
    abstract val schemaLocation: DirectoryProperty

    @get:Optional
    @get:OutputDirectory
    abstract val outputDir: DirectoryProperty

    @Suppress("unused")
    @set:Option(option = "input", description = "The input specification.")
    @get:Internal
    var input: String? = null
        set(value) {
            if (value != null) {
                inputSpec.set(layout.projectDirectory.file(value))
            }
        }

    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpec: RegularFileProperty

    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpecRootDirectory: DirectoryProperty

    @get:Input
    @get:Optional
    abstract val inputSpecRootDirectorySkipMerge: Property<Boolean>

    @get:Input
    @get:Optional
    abstract val mergedFileName: Property<String>

    @get:Input
    @get:Optional
    abstract val remoteInputSpec: Property<String>

    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val templateDir: DirectoryProperty

    @get:Optional
    @get:Input
    abstract val templateResourcePath: Property<String>

    @get:Optional
    @get:Input
    abstract val auth: Property<String>

    @get:Optional
    @get:Input
    abstract val globalProperties: MapProperty<String, String>

    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val configFile: RegularFileProperty

    @get:Optional
    @get:Input
    abstract val skipOverwrite: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val packageName: Property<String>

    @get:Optional
    @get:Input
    abstract val apiPackage: Property<String>

    @get:Optional
    @get:Input
    abstract val modelPackage: Property<String>

    @get:Optional
    @get:Input
    abstract val modelNamePrefix: Property<String>

    @get:Optional
    @get:Input
    abstract val modelNameSuffix: Property<String>

    @get:Optional
    @get:Input
    abstract val apiNameSuffix: Property<String>

    @get:Optional
    @get:Input
    abstract val instantiationTypes: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val typeMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val additionalProperties: MapProperty<String, Any>

    @get:Optional
    @get:Input
    abstract val serverVariables: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val languageSpecificPrimitives: ListProperty<String>

    @get:Optional
    @get:Input
    abstract val openapiGeneratorIgnoreList: ListProperty<String>

    @get:Optional
    @get:Input
    abstract val importMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val schemaMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val inlineSchemaNameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val inlineSchemaOptions: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val nameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val parameterNameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val modelNameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val enumNameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val operationIdNameMappings: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val openapiNormalizer: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val invokerPackage: Property<String>

    @get:Optional
    @get:Input
    abstract val groupId: Property<String>

    @get:Optional
    @get:Input
    abstract val id: Property<String>

    @get:Optional
    @get:Input
    abstract val version: Property<String>

    @get:Optional
    @get:Input
    abstract val library: Property<String>

    @get:Optional
    @get:Input
    abstract val gitHost: Property<String>

    @get:Optional
    @get:Input
    abstract val gitUserId: Property<String>

    @get:Optional
    @get:Input
    abstract val gitRepoId: Property<String>

    @get:Optional
    @get:Input
    abstract val releaseNote: Property<String>

    @get:Optional
    @get:Input
    abstract val httpUserAgent: Property<String>

    @get:Optional
    @get:Input
    abstract val reservedWordsMappings: MapProperty<String, String>

    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val ignoreFileOverride: RegularFileProperty

    @get:Optional
    @get:Input
    abstract val removeOperationIdPrefix: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val skipOperationExample: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val apiFilesConstrainedTo: ListProperty<String>

    @get:Optional
    @get:Input
    abstract val modelFilesConstrainedTo: ListProperty<String>

    @get:Optional
    @get:Input
    abstract val supportingFilesConstrainedTo: ListProperty<String>

    @get:Optional
    @get:Input
    abstract val generateModelTests: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val generateModelDocumentation: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val generateApiTests: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val generateApiDocumentation: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val logToStderr: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val enablePostProcessFile: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val skipValidateSpec: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val generateAliasAsModel: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val configOptions: MapProperty<String, String>

    @get:Optional
    @get:Input
    abstract val engine: Property<String>

    @get:Optional
    @get:Input
    abstract val cleanupOutput: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val dryRun: Property<Boolean>

    init {
        inputSpecRootDirectorySkipMerge.convention(false)
        mergedFileName.convention("merged")
    }

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
}