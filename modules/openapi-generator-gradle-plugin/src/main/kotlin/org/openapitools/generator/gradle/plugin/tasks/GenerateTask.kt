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

import javax.inject.Inject
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.Project
import org.gradle.api.file.ConfigurableFileCollection
import org.gradle.api.file.FileSystemOperations
import org.gradle.api.model.ObjectFactory
import org.gradle.api.provider.Property
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Classpath
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
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.listProperty
import org.gradle.kotlin.dsl.mapProperty
import org.gradle.kotlin.dsl.property
import org.gradle.util.GradleVersion
import org.gradle.workers.WorkerExecutor

/**
 * A task which generates the desired code.
 *
 * Example (CLI):
 *
 * ./gradlew -q openApiGenerate --input=/path/to/file
 *
 * @author Jim Schubert
 */
@CacheableTask
open class GenerateTask @Inject constructor(
    private val objectFactory: ObjectFactory,
    private val workerExecutor: WorkerExecutor
) : DefaultTask() {

    /**
     * Extra classpath entries for the code generation worker.
     * Add custom generator jars via the `openApiGeneratorClasspath` configuration.
     * Parent-first classloader delegation means these entries supplement,
     * not override, the plugin's own classpath.
     */
    @get:Optional
    @get:Classpath
    val generatorClasspath: ConfigurableFileCollection = project.objects.fileCollection()

    /**
     * The verbosity of generation
     */
    @get:Optional
    @get:Input
    val verbose = project.objects.property<Boolean>()

    /**
     * Whether an input specification should be validated upon generation.
     */
    @get:Optional
    @get:Input
    val validateSpec = project.objects.property<Boolean>()

    /**
     * The name of the generator which will handle codegen. (see "openApiGenerators" task)
     */
    @get:Optional
    @get:Input
    val generatorName = project.objects.property<String>()

    /**
     * This is the configuration for reference paths where schemas for openapi generation are stored
     * The directory which contains the additional schema files
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.ABSOLUTE)
    val schemaLocation = project.objects.property<String>()

    /**
     * The output target directory into which code will be generated.
     */
    @get:Optional
    @get:OutputDirectory
    val outputDir = project.objects.property<String>()

    @Suppress("unused")
    @set:Option(option = "input", description = "The input specification.")
    @get:Internal
    var input: String? = null
        set(value) {
            inputSpec.set(value)
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
    val inputSpec = project.objects.property<String>()

    /**
     * Local root folder with spec files.
     *
     * By default, a merged spec file will be generated based on the contents of the directory. To disable this, set the
     * `inputSpecRootDirectorySkipMerge` property.
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    val inputSpecRootDirectory = project.objects.property<String>();

    /**
     * Skip bundling all spec files into a merged spec file, if true.
     */
    @get:Input
    @get:Optional
    val inputSpecRootDirectorySkipMerge = project.objects.property<Boolean>()

    /**
     * Name of the file that will contain all merged specs
     */
    @get:Input
    @get:Optional
    val mergedFileName = project.objects.property<String>();

    /**
     * The remote Open API 2.0/3.x specification URL location.
     */
    @get:Input
    @get:Optional
    val remoteInputSpec = project.objects.property<String>()

    /**
     * The template directory holding a custom template.
     */
    @get:Optional
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    val templateDir = project.objects.property<String>()

    /**
     * Resource path containing template files.
     */
    @get:Optional
    @get:Input
    val templateResourcePath = project.objects.property<String>()

    /**
     * Adds authorization headers when fetching the OpenAPI definitions remotely.
     * Pass in a URL-encoded string of name:header with a comma separating multiple values
     */
    @get:Optional
    @get:Input
    val auth = project.objects.property<String>()

    /**
     * Sets specified global properties.
     */
    @get:Optional
    @get:Input
    val globalProperties = project.objects.mapProperty<String, String>()

    /**
     * Path to json configuration file.
     * File content should be in a json format { "optionKey":"optionValue", "optionKey1":"optionValue1"...}
     * Supported options can be different for each language. Run config-help -g {generator name} command for language specific config options.
     */
    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    val configFile = project.objects.property<String>()

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    @get:Optional
    @get:Input
    val skipOverwrite = project.objects.property<Boolean>()

    /**
     * Package for generated classes (where supported)
     */
    @get:Optional
    @get:Input
    val packageName = project.objects.property<String>()

    /**
     * Package for generated api classes
     */
    @get:Optional
    @get:Input
    val apiPackage = project.objects.property<String>()

    /**
     * Package for generated models
     */
    @get:Optional
    @get:Input
    val modelPackage = project.objects.property<String>()

    /**
     * Prefix that will be prepended to all model names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    val modelNamePrefix = project.objects.property<String>()

    /**
     * Suffix that will be appended to all model names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    val modelNameSuffix = project.objects.property<String>()

    /**
     * Suffix that will be appended to all api names. Default is the empty string.
     */
    @get:Optional
    @get:Input
    val apiNameSuffix = project.objects.property<String>()

    /**
     * Sets instantiation type mappings.
     */
    @get:Optional
    @get:Input
    val instantiationTypes = project.objects.mapProperty<String, String>()

    /**
     * Sets mappings between OpenAPI spec types and generated code types.
     */
    @get:Optional
    @get:Input
    val typeMappings = project.objects.mapProperty<String, String>()

    /**
     * Sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value.
     * You can also have multiple occurrences of this option.
     */
    @get:Optional
    @get:Input
    val additionalProperties = project.objects.mapProperty<String, Any>()

    /**
     * Sets server variable for server URL template substitution, in the format of name=value,name=value.
     * You can also have multiple occurrences of this option.
     */
    @get:Optional
    @get:Input
    val serverVariables = project.objects.mapProperty<String, String>()

    /**
     * Specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double.
     */
    @get:Optional
    @get:Input
    val languageSpecificPrimitives = project.objects.listProperty<String>()

    /**
     * Specifies .openapi-generator-ignore list in the form of relative/path/to/file1,relative/path/to/file2. For example: README.md,pom.xml.
     */
    @get:Optional
    @get:Input
    val openapiGeneratorIgnoreList = project.objects.listProperty<String>()

    /**
     * Specifies mappings between a given class and the import that should be used for that class.
     */
    @get:Optional
    @get:Input
    val importMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between a given schema and the new one.
     */
    @get:Optional
    @get:Input
    val schemaMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the inline scheme name and the new name
     */
    @get:Optional
    @get:Input
    val inlineSchemaNameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies options for inline schemas
     */
    @get:Optional
    @get:Input
    val inlineSchemaOptions = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the property name and the new name
     */
    @get:Optional
    @get:Input
    val nameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the parameter name and the new name
     */
    @get:Optional
    @get:Input
    val parameterNameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the model name and the new name
     */
    @get:Optional
    @get:Input
    val modelNameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the enum name and the new name
     */
    @get:Optional
    @get:Input
    val enumNameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings between the operation id name and the new name
     */
    @get:Optional
    @get:Input
    val operationIdNameMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies mappings (rules) in OpenAPI normalizer
     */
    @get:Optional
    @get:Input
    val openapiNormalizer = project.objects.mapProperty<String, String>()

    /**
     * Root package for generated code.
     */
    @get:Optional
    @get:Input
    val invokerPackage = project.objects.property<String>()

    /**
     * GroupId in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    val groupId = project.objects.property<String>()

    /**
     * ArtifactId in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    val id = project.objects.property<String>()

    /**
     * Artifact version in generated pom.xml/build.gradle.kts or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Optional
    @get:Input
    val version = project.objects.property<String>()

    /**
     * Reference the library template (sub-template) of a generator.
     */
    @get:Optional
    @get:Input
    val library = project.objects.property<String>()

    /**
     * Git host, e.g. gitlab.com.
     */
    @get:Optional
    @get:Input
    val gitHost = project.objects.property<String>()

    /**
     * Git user ID, e.g. openapitools.
     */
    @get:Optional
    @get:Input
    val gitUserId = project.objects.property<String>()

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    @get:Optional
    @get:Input
    val gitRepoId = project.objects.property<String>()

    /**
     * Release note, default to 'Minor update'.
     */
    @get:Optional
    @get:Input
    val releaseNote = project.objects.property<String>()

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}/{language}'
     */
    @get:Optional
    @get:Input
    val httpUserAgent = project.objects.property<String>()

    /**
     * Specifies how a reserved name should be escaped to.
     */
    @get:Optional
    @get:Input
    val reservedWordsMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation.
     */
    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    val ignoreFileOverride = project.objects.property<String>()

    /**
     * Remove prefix of operationId, e.g. config_getId => getId
     */
    @get:Optional
    @get:Input
    val removeOperationIdPrefix = project.objects.property<Boolean>()

    /**
     * Remove examples defined in the operation
     */
    @get:Optional
    @get:Input
    val skipOperationExample = project.objects.property<Boolean>()

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
    val apiFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Optional
    @get:Input
    val modelFilesConstrainedTo = project.objects.listProperty<String>()

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
    val supportingFilesConstrainedTo = project.objects.listProperty<String>()

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
    val generateModelTests = project.objects.property<Boolean>()

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
    val generateModelDocumentation = project.objects.property<Boolean>()

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
    val generateApiTests = project.objects.property<Boolean>()

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
    val generateApiDocumentation = project.objects.property<Boolean>()

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    @get:Optional
    @get:Input
    val logToStderr = project.objects.property<Boolean>()

    /**
     * To enable the file post-processing hook. This enables executing an external post-processor (usually a linter program).
     * This only enables the post-processor. To define the post-processing command, define an environment variable such as
     * LANG_POST_PROCESS_FILE (e.g. GO_POST_PROCESS_FILE, SCALA_POST_PROCESS_FILE). Please open an issue if your target
     * generator does not support this functionality.
     */
    @get:Optional
    @get:Input
    val enablePostProcessFile = project.objects.property<Boolean>()

    /**
     * To skip spec validation. When true, we will skip the default behavior of validating a spec before generation.
     */
    @get:Optional
    @get:Input
    val skipValidateSpec = project.objects.property<Boolean>()

    /**
     * To generate alias (array, list, map) as model. When false, top-level objects defined as array, list, or map will result in those
     * definitions generated as top-level Array-of-items, List-of-items, Map-of-items definitions.
     * When true, A model representation either containing or extending the array,list,map (depending on specific generator implementation) will be generated.
     */
    @get:Optional
    @get:Input
    val generateAliasAsModel = project.objects.property<Boolean>()

    /**
     * A dynamic map of options specific to a generator.
     */
    @get:Optional
    @get:Input
    val configOptions = project.objects.mapProperty<String, String>()

    /**
     * Templating engine: "mustache" (default) or "handlebars" (beta)
     */
    @get:Optional
    @get:Input
    val engine = project.objects.property<String>()

    /**
     * Defines whether the output dir should be cleaned up before generating the output.
     *
     */
    @get:Optional
    @get:Input
    val cleanupOutput = project.objects.property<Boolean>()

    /**
     * Defines whether the generator should run in dry-run mode.
     */
    @get:Optional
    @get:Input
    val dryRun = project.objects.property<Boolean>()

    private fun <T> Property<T>.ifNotEmpty(block: Property<T>.(T) -> Unit) {
        if (isPresent) {
            when (val value = get()) {
                is String -> if (value.isNotEmpty()) block(value)
                else -> block(value)
            }
        }
    }

    private fun createFileSystemManager(): FileSystemManager {
        return if(GradleVersion.current() >= GradleVersion.version("6.0")) {
            objectFactory.newInstance(FileSystemManagerDefault::class.java)
        } else {
            objectFactory.newInstance(FileSystemManagerLegacy::class.java, project)
        }
    }

    @Suppress("unused")
    @TaskAction
    fun doWork() {
        cleanupOutput.ifNotEmpty { cleanup ->
            if (cleanup) {
                createFileSystemManager().delete(outputDir)
                val out = services.get(StyledTextOutputFactory::class.java).create("openapi")
                out.withStyle(StyledTextOutput.Style.Success)
                out.println("Cleaned up output directory ${outputDir.get()} before code generation (cleanupOutput set to true).")
            }
        }

        // classLoaderIsolation prevents Jackson version conflicts with other
        // plugins (issue #18753). The isolated classloader inherits the plugin's
        // own classpath (openapi-generator and its dependencies). Extra entries
        // from openApiGeneratorClasspath, if configured, supply custom generators.
        val taskRef = this
        val extraClasspath = taskRef.generatorClasspath.files
        if (extraClasspath.isNotEmpty()) {
            logger.info("OpenAPI Generator: adding {} extra classpath entries from openApiGeneratorClasspath", extraClasspath.size)
        }

        val workQueue = workerExecutor.classLoaderIsolation {
            if (extraClasspath.isNotEmpty()) {
                classpath.from(extraClasspath)
            }
        }

        workQueue.submit(GenerateWorkAction::class.java) {
            inputSpec.set(taskRef.inputSpec)
            inputSpecRootDirectory.set(taskRef.inputSpecRootDirectory)
            inputSpecRootDirectorySkipMerge.set(taskRef.inputSpecRootDirectorySkipMerge)
            mergedFileName.set(taskRef.mergedFileName)
            remoteInputSpec.set(taskRef.remoteInputSpec)
            verbose.set(taskRef.verbose)
            validateSpec.set(taskRef.validateSpec)
            generatorName.set(taskRef.generatorName)
            outputDir.set(taskRef.outputDir)
            templateDir.set(taskRef.templateDir)
            templateResourcePath.set(taskRef.templateResourcePath)
            auth.set(taskRef.auth)
            globalProperties.set(taskRef.globalProperties)
            configFile.set(taskRef.configFile)
            skipOverwrite.set(taskRef.skipOverwrite)
            packageName.set(taskRef.packageName)
            apiPackage.set(taskRef.apiPackage)
            modelPackage.set(taskRef.modelPackage)
            modelNamePrefix.set(taskRef.modelNamePrefix)
            modelNameSuffix.set(taskRef.modelNameSuffix)
            apiNameSuffix.set(taskRef.apiNameSuffix)
            instantiationTypes.set(taskRef.instantiationTypes)
            typeMappings.set(taskRef.typeMappings)
            additionalProperties.set(taskRef.additionalProperties)
            serverVariables.set(taskRef.serverVariables)
            languageSpecificPrimitives.set(taskRef.languageSpecificPrimitives)
            openapiGeneratorIgnoreList.set(taskRef.openapiGeneratorIgnoreList)
            importMappings.set(taskRef.importMappings)
            schemaMappings.set(taskRef.schemaMappings)
            inlineSchemaNameMappings.set(taskRef.inlineSchemaNameMappings)
            inlineSchemaOptions.set(taskRef.inlineSchemaOptions)
            nameMappings.set(taskRef.nameMappings)
            parameterNameMappings.set(taskRef.parameterNameMappings)
            modelNameMappings.set(taskRef.modelNameMappings)
            enumNameMappings.set(taskRef.enumNameMappings)
            operationIdNameMappings.set(taskRef.operationIdNameMappings)
            openapiNormalizer.set(taskRef.openapiNormalizer)
            invokerPackage.set(taskRef.invokerPackage)
            groupId.set(taskRef.groupId)
            id.set(taskRef.id)
            version.set(taskRef.version)
            library.set(taskRef.library)
            gitHost.set(taskRef.gitHost)
            gitUserId.set(taskRef.gitUserId)
            gitRepoId.set(taskRef.gitRepoId)
            releaseNote.set(taskRef.releaseNote)
            httpUserAgent.set(taskRef.httpUserAgent)
            reservedWordsMappings.set(taskRef.reservedWordsMappings)
            ignoreFileOverride.set(taskRef.ignoreFileOverride)
            removeOperationIdPrefix.set(taskRef.removeOperationIdPrefix)
            skipOperationExample.set(taskRef.skipOperationExample)
            apiFilesConstrainedTo.set(taskRef.apiFilesConstrainedTo)
            modelFilesConstrainedTo.set(taskRef.modelFilesConstrainedTo)
            supportingFilesConstrainedTo.set(taskRef.supportingFilesConstrainedTo)
            generateModelTests.set(taskRef.generateModelTests)
            generateModelDocumentation.set(taskRef.generateModelDocumentation)
            generateApiTests.set(taskRef.generateApiTests)
            generateApiDocumentation.set(taskRef.generateApiDocumentation)
            configOptions.set(taskRef.configOptions)
            logToStderr.set(taskRef.logToStderr)
            enablePostProcessFile.set(taskRef.enablePostProcessFile)
            skipValidateSpec.set(taskRef.skipValidateSpec)
            generateAliasAsModel.set(taskRef.generateAliasAsModel)
            engine.set(taskRef.engine)
            dryRun.set(taskRef.dryRun)
        }

        try {
            workQueue.await()
        } catch (e: Exception) {
            throw GradleException("Code generation failed. See the worker output above for details.", e)
        }

        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")
        out.withStyle(StyledTextOutput.Style.Success)
        out.println("Successfully generated code to ${outputDir.get()}")
    }
}

internal interface FileSystemManager {

    fun delete(outputDir: Property<String>)

}

internal open class FileSystemManagerLegacy @Inject constructor(private val project: Project): FileSystemManager {

    override fun delete(outputDir: Property<String>) {
        project.delete(outputDir)
    }
}

internal open class FileSystemManagerDefault @Inject constructor(private val fs: FileSystemOperations) : FileSystemManager {

    override fun delete(outputDir: Property<String>) {
        fs.delete { delete(outputDir) }
    }
}
