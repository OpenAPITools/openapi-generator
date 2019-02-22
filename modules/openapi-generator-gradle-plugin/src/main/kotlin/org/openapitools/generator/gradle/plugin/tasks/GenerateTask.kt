/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.generator.gradle.plugin.tasks

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.provider.Property
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.listProperty
import org.gradle.kotlin.dsl.property
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator
import org.openapitools.codegen.config.GeneratorProperties


/**
 * A task which generates the desired code.
 *
 * Example (CLI):
 *
 * ./gradlew -q openApiGenerate
 *
 * @author Jim Schubert
 */
open class GenerateTask : DefaultTask() {

    /**
     * The verbosity of generation
     */
    @get:Internal
    val verbose = project.objects.property<Boolean>()

    /**
     * Whether or not an input specification should be validated upon generation.
     */
    @get:Internal
    val validateSpec = project.objects.property<Boolean>()

    /**
     * The name of the generator which will handle codegen. (see "openApiGenerators" task)
     */
    @get:Internal
    val generatorName = project.objects.property<String>()

    /**
     * The output target directory into which code will be generated.
     */
    @get:Internal
    val outputDir = project.objects.property<String>()

    /**
     * The Open API 2.0/3.x specification location.
     */
    @get:Internal
    val inputSpec = project.objects.property<String>()

    /**
     * The template directory holding a custom template.
     */
    @get:Internal
    val templateDir = project.objects.property<String?>()

    /**
     * Adds authorization headers when fetching the OpenAPI definitions remotely.
     * Pass in a URL-encoded string of name:header with a comma separating multiple values
     */
    @get:Internal
    val auth = project.objects.property<String>()

    /**
     * Sets specified system properties.
     */
    @get:Internal
    val systemProperties = project.objects.property<Map<String, String>>()

    /**
     * Path to json configuration file.
     * File content should be in a json format { "optionKey":"optionValue", "optionKey1":"optionValue1"...}
     * Supported options can be different for each language. Run config-help -g {generator name} command for language specific config options.
     */
    @get:Internal
    val configFile = project.objects.property<String>()

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    @get:Internal
    val skipOverwrite = project.objects.property<Boolean?>()

    /**
     * Package for generated api classes
     */
    @get:Internal
    val apiPackage = project.objects.property<String>()

    /**
     * Package for generated models
     */
    @get:Internal
    val modelPackage = project.objects.property<String>()

    /**
     * Prefix that will be prepended to all model names. Default is the empty string.
     */
    @get:Internal
    val modelNamePrefix = project.objects.property<String>()

    /**
     * Suffix that will be appended to all model names. Default is the empty string.
     */
    @get:Internal
    val modelNameSuffix = project.objects.property<String>()

    /**
     * Sets instantiation type mappings.
     */
    @get:Internal
    val instantiationTypes = project.objects.property<Map<String, String>>()

    /**
     * Sets mappings between OpenAPI spec types and generated code types.
     */
    @get:Internal
    val typeMappings = project.objects.property<Map<String, String>>()

    /**
     * Sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value.
     * You can also have multiple occurrences of this option.
     */
    @get:Internal
    val additionalProperties = project.objects.property<Map<String, String>>()

    /**
     * Specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double.
     */
    @get:Internal
    val languageSpecificPrimitives = project.objects.listProperty<String>()

    /**
     * Specifies mappings between a given class and the import that should be used for that class.
     */
    @get:Internal
    val importMappings = project.objects.property<Map<String, String>>()

    /**
     * Root package for generated code.
     */
    @get:Internal
    val invokerPackage = project.objects.property<String>()

    /**
     * GroupId in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Internal
    val groupId = project.objects.property<String>()

    /**
     * ArtifactId in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Internal
    val id = project.objects.property<String>()

    /**
     * Artifact version in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    @get:Internal
    val version = project.objects.property<String>()

    /**
     * Reference the library template (sub-template) of a generator.
     */
    @get:Internal
    val library = project.objects.property<String?>()

    /**
     * Git user ID, e.g. openapitools.
     */
    @get:Internal
    val gitUserId = project.objects.property<String?>()

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    @get:Internal
    val gitRepoId = project.objects.property<String?>()

    /**
     * Release note, default to 'Minor update'.
     */
    @get:Internal
    val releaseNote = project.objects.property<String?>()

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}}/{language}'
     */
    @get:Internal
    val httpUserAgent = project.objects.property<String?>()

    /**
     * Specifies how a reserved name should be escaped to.
     */
    @get:Internal
    val reservedWordsMappings = project.objects.property<Map<String, String>>()

    /**
     * Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation.
     */
    @get:Internal
    val ignoreFileOverride = project.objects.property<String?>()

    /**
     * Remove prefix of operationId, e.g. config_getId => getId
     */
    @get:Internal
    val removeOperationIdPrefix = project.objects.property<Boolean?>()

    /**
     * Defines which API-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * This option enables/disables generation of ALL api-related files.
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val apiFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val modelFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines which supporting files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * Supporting files are those related to projects/frameworks which may be modified
     * by consumers.
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val supportingFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines whether or not model-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val generateModelTests = project.objects.property<Boolean>()

    /**
     * Defines whether or not model-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val generateModelDocumentation = project.objects.property<Boolean>()

    /**
     * Defines whether or not api-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val generateApiTests = project.objects.property<Boolean>()

    /**
     * Defines whether or not api-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    @get:Internal
    val generateApiDocumentation = project.objects.property<Boolean>()

    /**
     * A special-case setting which configures some generators with XML support. In some cases,
     * this forces json OR xml, so the default here is false.
     */
    @get:Internal
    val withXml = project.objects.property<Boolean>()


    /**
     * To write all log messages (not just errors) to STDOUT
     */
    @get:Internal
    val logToStderr = project.objects.property<Boolean>()

    /**
     * To enable the file post-processing hook. This enables executing an external post-processor (usually a linter program).
     * This only enables the post-processor. To define the post-processing command, define an environment variable such as
     * LANG_POST_PROCESS_FILE (e.g. GO_POST_PROCESS_FILE, SCALA_POST_PROCESS_FILE). Please open an issue if your target
     * generator does not support this functionality.
     */
    @get:Internal
    val enablePostProcessFile = project.objects.property<Boolean>()

    /**
     * To skip spec validation. When true, we will skip the default behavior of validating a spec before generation.
     */
    @get:Internal
    val skipValidateSpec = project.objects.property<Boolean>()

    /**
     * To generate alias (array, list, map) as model. When false, top-level objects defined as array, list, or map will result in those
     * definitions generated as top-level Array-of-items, List-of-items, Map-of-items definitions.
     * When true, A model representation either containing or extending the array,list,map (depending on specific generator implementation) will be generated.
     */
    @get:Internal
    val generateAliasAsModel = project.objects.property<Boolean>()

    /**
     * A dynamic map of options specific to a generator.
     */
    @get:Internal
    val configOptions = project.objects.property<Map<String, String>>()

    private fun <T : Any?> Property<T>.ifNotEmpty(block: Property<T>.(T) -> Unit) {
        if (isPresent) {
            val item: T? = get()
            if (item != null) {
                when (get()) {
                    is String -> if ((get() as String).isNotEmpty()) {
                        block(get())
                    }
                    is String? -> if (true == (get() as String?)?.isNotEmpty()) {
                        block(get())
                    }
                    else -> block(get())
                }
            }
        }
    }

    @Suppress("unused")
    @TaskAction
    fun doWork() {
        val configurator: CodegenConfigurator = if (configFile.isPresent) {
            CodegenConfigurator.fromFile(configFile.get())
        } else CodegenConfigurator()

        try {
            if (systemProperties.isPresent) {
                systemProperties.get().forEach { (key, value) ->
                    configurator.addSystemProperty(key, value)
                }
            }

            if (supportingFilesConstrainedTo.isPresent && supportingFilesConstrainedTo.get().isNotEmpty()) {
                GeneratorProperties.setProperty(CodegenConstants.SUPPORTING_FILES, supportingFilesConstrainedTo.get().joinToString(","))
            } else {
                GeneratorProperties.clearProperty(CodegenConstants.SUPPORTING_FILES)
            }

            if (modelFilesConstrainedTo.isPresent && modelFilesConstrainedTo.get().isNotEmpty()) {
                GeneratorProperties.setProperty(CodegenConstants.MODELS, modelFilesConstrainedTo.get().joinToString(","))
            } else {
                GeneratorProperties.clearProperty(CodegenConstants.MODELS)
            }

            if (apiFilesConstrainedTo.isPresent && apiFilesConstrainedTo.get().isNotEmpty()) {
                GeneratorProperties.setProperty(CodegenConstants.APIS, apiFilesConstrainedTo.get().joinToString(","))
            } else {
                GeneratorProperties.clearProperty(CodegenConstants.APIS)
            }

            if (generateApiDocumentation.isPresent) {
                GeneratorProperties.setProperty(CodegenConstants.API_DOCS, generateApiDocumentation.get().toString())
            }

            if (generateModelDocumentation.isPresent) {
                GeneratorProperties.setProperty(CodegenConstants.MODEL_DOCS, generateModelDocumentation.get().toString())
            }

            if (generateModelTests.isPresent) {
                GeneratorProperties.setProperty(CodegenConstants.MODEL_TESTS, generateModelTests.get().toString())
            }

            if (generateApiTests.isPresent) {
                GeneratorProperties.setProperty(CodegenConstants.API_TESTS, generateApiTests.get().toString())
            }

            if (withXml.isPresent) {
                GeneratorProperties.setProperty(CodegenConstants.WITH_XML, withXml.get().toString())
            }

            // now override with any specified parameters
            verbose.ifNotEmpty { value ->
                configurator.isVerbose = value
            }

            validateSpec.ifNotEmpty { value ->
                configurator.isValidateSpec = value
            }

            skipOverwrite.ifNotEmpty { value ->
                configurator.isSkipOverwrite = value ?: false
            }

            inputSpec.ifNotEmpty { value ->
                configurator.inputSpec = value
            }

            generatorName.ifNotEmpty { value ->
                configurator.generatorName = value
            }

            outputDir.ifNotEmpty { value ->
                configurator.outputDir = value
            }

            auth.ifNotEmpty { value ->
                configurator.auth = value
            }

            templateDir.ifNotEmpty { value ->
                configurator.templateDir = value
            }

            apiPackage.ifNotEmpty { value ->
                configurator.apiPackage = value
            }

            modelPackage.ifNotEmpty { value ->
                configurator.modelPackage = value
            }

            modelNamePrefix.ifNotEmpty { value ->
                configurator.modelNamePrefix = value
            }

            modelNameSuffix.ifNotEmpty { value ->
                configurator.modelNameSuffix = value
            }

            invokerPackage.ifNotEmpty { value ->
                configurator.invokerPackage = value
            }

            groupId.ifNotEmpty { value ->
                configurator.groupId = value
            }

            id.ifNotEmpty { value ->
                configurator.artifactId = value
            }

            version.ifNotEmpty { value ->
                configurator.artifactVersion = value
            }

            library.ifNotEmpty { value ->
                configurator.library = value
            }

            gitUserId.ifNotEmpty { value ->
                configurator.gitUserId = value
            }

            gitRepoId.ifNotEmpty { value ->
                configurator.gitRepoId = value
            }

            releaseNote.ifNotEmpty { value ->
                configurator.releaseNote = value
            }

            httpUserAgent.ifNotEmpty { value ->
                configurator.httpUserAgent = value
            }

            ignoreFileOverride.ifNotEmpty { value ->
                configurator.ignoreFileOverride = value
            }

            removeOperationIdPrefix.ifNotEmpty { value ->
                configurator.removeOperationIdPrefix = value!!
            }

            logToStderr.ifNotEmpty { value ->
                configurator.logToStderr = value
            }

            enablePostProcessFile.ifNotEmpty { value ->
                configurator.enablePostProcessFile = value
            }

            skipValidateSpec.ifNotEmpty { value ->
                configurator.setValidateSpec(value)
            }

            generateAliasAsModel.ifNotEmpty { value ->
                configurator.setGenerateAliasAsModel(value)
            }

            if (systemProperties.isPresent) {
                systemProperties.get().forEach { entry ->
                    configurator.addSystemProperty(entry.key, entry.value)
                }
            }

            if (instantiationTypes.isPresent) {
                instantiationTypes.get().forEach { entry ->
                    configurator.addInstantiationType(entry.key, entry.value)
                }
            }

            if (importMappings.isPresent) {
                importMappings.get().forEach { entry ->
                    configurator.addImportMapping(entry.key, entry.value)
                }
            }

            if (typeMappings.isPresent) {
                typeMappings.get().forEach { entry ->
                    configurator.addTypeMapping(entry.key, entry.value)
                }
            }

            if (additionalProperties.isPresent) {
                additionalProperties.get().forEach { entry ->
                    configurator.addAdditionalProperty(entry.key, entry.value)
                }
            }

            if (languageSpecificPrimitives.isPresent) {
                languageSpecificPrimitives.get().forEach {
                    configurator.addLanguageSpecificPrimitive(it)
                }
            }

            if (reservedWordsMappings.isPresent) {
                reservedWordsMappings.get().forEach { entry ->
                    configurator.addAdditionalReservedWordMapping(entry.key, entry.value)
                }
            }

            val clientOptInput = configurator.toClientOptInput()
            val codgenConfig = clientOptInput.config

            if (configOptions.isPresent) {
                val userSpecifiedConfigOptions = configOptions.get()
                codgenConfig.cliOptions().forEach {
                    if (userSpecifiedConfigOptions.containsKey(it.opt)) {
                        clientOptInput.config.additionalProperties()[it.opt] = userSpecifiedConfigOptions[it.opt]
                    }
                }
            }

            try {
                val out = services.get(StyledTextOutputFactory::class.java).create("openapi")
                out.withStyle(StyledTextOutput.Style.Success)

                DefaultGenerator().opts(clientOptInput).generate()

                out.println("Successfully generated code to ${configurator.outputDir}")
            } catch (e: RuntimeException) {
                throw GradleException("Code generation failed.", e)
            }
        } finally {
            GeneratorProperties.reset()
        }
    }
}
