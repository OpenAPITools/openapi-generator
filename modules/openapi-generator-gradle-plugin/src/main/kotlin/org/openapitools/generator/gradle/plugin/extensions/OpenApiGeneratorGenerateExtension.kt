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

package org.openapitools.generator.gradle.plugin.extensions

import org.gradle.api.Project
import org.gradle.kotlin.dsl.listProperty
import org.gradle.kotlin.dsl.mapProperty
import org.gradle.kotlin.dsl.property

/**
 * Gradle project level extension object definition for the generate task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorGenerateExtension(project: Project) {

    /**
     * The verbosity of generation
     */
    val verbose = project.objects.property<Boolean>()

    /**
     * Whether or not an input specification should be validated upon generation.
     */
    val validateSpec = project.objects.property<Boolean>()

    /**
     * The name of the generator which will handle codegen. (see "openApiGenerators" task)
     */
    val generatorName = project.objects.property<String>()

    /**
     * The output target directory into which code will be generated.
     */
    val outputDir = project.objects.property<String>()

    /**
     * The Open API 2.0/3.x specification location.
     */
    val inputSpec = project.objects.property<String>()

    /**
     * The template directory holding a custom template.
     */
    val templateDir = project.objects.property<String?>()

    /**
     * Adds authorization headers when fetching the OpenAPI definitions remotely.
     * Pass in a URL-encoded string of name:header with a comma separating multiple values
     */
    val auth = project.objects.property<String>()

    /**
     * Sets specified global properties.
     */
    val globalProperties = project.objects.mapProperty<String, String>()

    /**
     * Path to json configuration file.
     * File content should be in a json format { "optionKey":"optionValue", "optionKey1":"optionValue1"...}
     * Supported options can be different for each language. Run config-help -g {generator name} command for language specific config options.
     */
    val configFile = project.objects.property<String>()

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    val skipOverwrite = project.objects.property<Boolean?>()

    /**
     * Package for generated classes (where supported)
     */
    val packageName = project.objects.property<String>()

    /**
     * Package for generated api classes
     */
    val apiPackage = project.objects.property<String>()

    /**
     * Package for generated models
     */
    val modelPackage = project.objects.property<String>()

    /**
     * Prefix that will be prepended to all model names. Default is the empty string.
     */
    val modelNamePrefix = project.objects.property<String>()

    /**
     * Suffix that will be appended to all model names. Default is the empty string.
     */
    val modelNameSuffix = project.objects.property<String>()

    /**
     * Sets instantiation type mappings.
     */
    val instantiationTypes = project.objects.mapProperty<String, String>()

    /**
     * Sets mappings between OpenAPI spec types and generated code types.
     */
    val typeMappings = project.objects.mapProperty<String, String>()

    /**
     * Sets additional properties that can be referenced by the mustache templates.
     */
    val additionalProperties = project.objects.mapProperty<String, Any>()

    /**
     * Sets server variable for server URL template substitution, in the format of name=value,name=value.
     */
    val serverVariables = project.objects.mapProperty<String, String>()

    /**
     * Specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double.
     */
    val languageSpecificPrimitives = project.objects.listProperty<String>()

    /**
     * Specifies mappings between a given class and the import that should be used for that class.
     */
    val importMappings = project.objects.mapProperty<String, String>()

    /**
     * Root package for generated code.
     */
    val invokerPackage = project.objects.property<String>()

    /**
     * GroupId in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    val groupId = project.objects.property<String>()

    /**
     * ArtifactId in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    val id = project.objects.property<String>()

    /**
     * Artifact version in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators.
     */
    val version = project.objects.property<String>()

    /**
     * Reference the library template (sub-template) of a generator.
     */
    val library = project.objects.property<String?>()

    /**
     * Git host, e.g. gitlab.com.
     */
    val gitHost = project.objects.property<String?>()

    /**
     * Git user ID, e.g. openapitools.
     */
    val gitUserId = project.objects.property<String?>()

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    val gitRepoId = project.objects.property<String?>()

    /**
     * Release note, default to 'Minor update'.
     */
    val releaseNote = project.objects.property<String?>()

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}/{language}'
     */
    val httpUserAgent = project.objects.property<String?>()

    /**
     * Specifies how a reserved name should be escaped to. Otherwise, the default _<name> is used.
     */
    val reservedWordsMappings = project.objects.mapProperty<String, String>()

    /**
     * Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation.
     */
    val ignoreFileOverride = project.objects.property<String?>()

    /**
     * Remove prefix of operationId, e.g. config_getId => getId
     */
    val removeOperationIdPrefix = project.objects.property<Boolean?>()

    /**
     * Skip examples defined in the operation
     */
    val skipOperationExample = project.objects.property<Boolean?>()

    /**
     * Defines which API-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
    val apiFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all).
     *
     * NOTE: Configuring any one of [apiFilesConstrainedTo], [modelFilesConstrainedTo], or [supportingFilesConstrainedTo] results
     *   in others being disabled. That is, OpenAPI Generator considers any one of these to define a subset of generation.
     *   For more control over generation of individual files, configure an ignore file and refer to it via [ignoreFileOverride].
     */
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
    val supportingFilesConstrainedTo = project.objects.listProperty<String>()

    /**
     * Defines whether or not model-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    val generateModelTests = project.objects.property<Boolean>()

    /**
     * Defines whether or not model-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL model-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    val generateModelDocumentation = project.objects.property<Boolean>()

    /**
     * Defines whether or not api-related _test_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _test_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    val generateApiTests = project.objects.property<Boolean>()

    /**
     * Defines whether or not api-related _documentation_ files should be generated.
     *
     * This option enables/disables generation of ALL api-related _documentation_ files.
     *
     * For more control over generation of individual files, configure an ignore file and
     * refer to it via [ignoreFileOverride].
     */
    val generateApiDocumentation = project.objects.property<Boolean>()

    /**
     * A special-case setting which configures some generators with XML support. In some cases,
     * this forces json OR xml, so the default here is false.
     */
    val withXml = project.objects.property<Boolean>()

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    val logToStderr = project.objects.property<Boolean>()

    /**
     * To enable the file post-processing hook. This enables executing an external post-processor (usually a linter program).
     * This only enables the post-processor. To define the post-processing command, define an environment variable such as
     * LANG_POST_PROCESS_FILE (e.g. GO_POST_PROCESS_FILE, SCALA_POST_PROCESS_FILE). Please open an issue if your target
     * generator does not support this functionality.
     */
    val enablePostProcessFile = project.objects.property<Boolean>()

    /**
     * To skip spec validation. When true, we will skip the default behavior of validating a spec before generation.
     */
    val skipValidateSpec = project.objects.property<Boolean>()

    /**
     * To generate alias (array, list, map) as model. When false, top-level objects defined as array, list, or map will result in those
     * definitions generated as top-level Array-of-items, List-of-items, Map-of-items definitions.
     * When true, A model representation either containing or extending the array,list,map (depending on specific generator implementation) will be generated.
     */
    val generateAliasAsModel = project.objects.property<Boolean>()

    /**
     * A map of options specific to a generator.
     */
    val configOptions = project.objects.mapProperty<String, String>()

    /**
     * Templating engine: "mustache" (default) or "handlebars" (beta)
     */
    val engine = project.objects.property<String?>()

    init {
        applyDefaults()
    }

    @Suppress("MemberVisibilityCanBePrivate")
    fun applyDefaults() {
        releaseNote.set("Minor update")
        modelNamePrefix.set("")
        modelNameSuffix.set("")
        generateModelTests.set(true)
        generateModelDocumentation.set(true)
        generateApiTests.set(true)
        generateApiDocumentation.set(true)
        withXml.set(false)
        configOptions.set(mapOf())
        validateSpec.set(true)
        logToStderr.set(false)
        enablePostProcessFile.set(false)
        skipValidateSpec.set(false)
        generateAliasAsModel.set(false)
    }
}
