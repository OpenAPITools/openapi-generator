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

import org.gradle.api.provider.Property
import org.gradle.workers.WorkAction
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator
import org.openapitools.codegen.config.GlobalSettings
import org.openapitools.codegen.config.MergedSpecBuilder
import org.slf4j.LoggerFactory

/**
 * Executes OpenAPI code generation as a Gradle [WorkAction].
 *
 * [GenerateTask] submits this action via
 * [org.gradle.workers.WorkerExecutor.classLoaderIsolation] to prevent
 * Jackson version conflicts with other Gradle plugins (see issue #18753).
 * The isolated classloader inherits the plugin's own classpath, including
 * openapi-generator and its correct Jackson version.
 */
abstract class GenerateWorkAction : WorkAction<GenerateWorkParameters> {

    private val logger = LoggerFactory.getLogger(GenerateWorkAction::class.java)

    override fun execute() {
        var resolvedInputSpec = ""

        parameters.inputSpec.ifNotEmpty { value ->
            resolvedInputSpec = value
        }

        parameters.remoteInputSpec.ifNotEmpty { value ->
            resolvedInputSpec = value
        }

        parameters.inputSpecRootDirectory.ifNotEmpty { inputSpecRootDirectoryValue ->
            val skipMerge = parameters.inputSpecRootDirectorySkipMerge.getOrElse(false)
            if (!skipMerge) {
                resolvedInputSpec = MergedSpecBuilder(
                    inputSpecRootDirectoryValue,
                    parameters.mergedFileName.getOrElse("merged")
                ).buildMergedSpec()
                logger.info("Merge input spec would be used - {}", resolvedInputSpec)
            }
        }

        val configurator: CodegenConfigurator = if (parameters.configFile.isPresent) {
            CodegenConfigurator.fromFile(parameters.configFile.get())
        } else {
            CodegenConfigurator()
        }

        try {
            if (parameters.globalProperties.isPresent) {
                parameters.globalProperties.get().forEach { (key, value) ->
                    configurator.addGlobalProperty(key, value)
                }
            }

            if (parameters.supportingFilesConstrainedTo.isPresent && parameters.supportingFilesConstrainedTo.get().isNotEmpty()) {
                GlobalSettings.setProperty(
                    CodegenConstants.SUPPORTING_FILES,
                    parameters.supportingFilesConstrainedTo.get().joinToString(",")
                )
            } else {
                GlobalSettings.clearProperty(CodegenConstants.SUPPORTING_FILES)
            }

            if (parameters.modelFilesConstrainedTo.isPresent && parameters.modelFilesConstrainedTo.get().isNotEmpty()) {
                GlobalSettings.setProperty(CodegenConstants.MODELS, parameters.modelFilesConstrainedTo.get().joinToString(","))
            } else {
                GlobalSettings.clearProperty(CodegenConstants.MODELS)
            }

            if (parameters.apiFilesConstrainedTo.isPresent && parameters.apiFilesConstrainedTo.get().isNotEmpty()) {
                GlobalSettings.setProperty(CodegenConstants.APIS, parameters.apiFilesConstrainedTo.get().joinToString(","))
            } else {
                GlobalSettings.clearProperty(CodegenConstants.APIS)
            }

            if (parameters.generateApiDocumentation.isPresent) {
                GlobalSettings.setProperty(CodegenConstants.API_DOCS, parameters.generateApiDocumentation.get().toString())
            }

            if (parameters.generateModelDocumentation.isPresent) {
                GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, parameters.generateModelDocumentation.get().toString())
            }

            if (parameters.generateModelTests.isPresent) {
                GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, parameters.generateModelTests.get().toString())
            }

            if (parameters.generateApiTests.isPresent) {
                GlobalSettings.setProperty(CodegenConstants.API_TESTS, parameters.generateApiTests.get().toString())
            }

            if (parameters.inputSpec.isPresent && parameters.remoteInputSpec.isPresent) {
                logger.warn("Both inputSpec and remoteInputSpec is specified. The remoteInputSpec will take priority over inputSpec.")
            }

            configurator.setInputSpec(resolvedInputSpec)

            parameters.verbose.ifNotEmpty { value -> configurator.setVerbose(value) }
            parameters.validateSpec.ifNotEmpty { value -> configurator.setValidateSpec(value) }
            parameters.skipOverwrite.ifNotEmpty { value -> configurator.setSkipOverwrite(value) }
            parameters.generatorName.ifNotEmpty { value -> configurator.setGeneratorName(value) }
            parameters.outputDir.ifNotEmpty { value -> configurator.setOutputDir(value) }
            parameters.auth.ifNotEmpty { value -> configurator.setAuth(value) }

            parameters.templateDir.ifNotEmpty { value -> configurator.setTemplateDir(value) }
            parameters.templateResourcePath.ifNotEmpty { value ->
                parameters.templateDir.ifNotEmpty {
                    logger.warn("Both templateDir and templateResourcePath were configured. templateResourcePath overwrites templateDir.")
                }
                configurator.setTemplateDir(value)
            }

            parameters.packageName.ifNotEmpty { value -> configurator.setPackageName(value) }
            parameters.apiPackage.ifNotEmpty { value -> configurator.setApiPackage(value) }
            parameters.modelPackage.ifNotEmpty { value -> configurator.setModelPackage(value) }
            parameters.modelNamePrefix.ifNotEmpty { value -> configurator.setModelNamePrefix(value) }
            parameters.modelNameSuffix.ifNotEmpty { value -> configurator.setModelNameSuffix(value) }
            parameters.apiNameSuffix.ifNotEmpty { value -> configurator.setApiNameSuffix(value) }
            parameters.invokerPackage.ifNotEmpty { value -> configurator.setInvokerPackage(value) }
            parameters.groupId.ifNotEmpty { value -> configurator.setGroupId(value) }
            parameters.id.ifNotEmpty { value -> configurator.setArtifactId(value) }
            parameters.version.ifNotEmpty { value -> configurator.setArtifactVersion(value) }
            parameters.library.ifNotEmpty { value -> configurator.setLibrary(value) }
            parameters.gitHost.ifNotEmpty { value -> configurator.setGitHost(value) }
            parameters.gitUserId.ifNotEmpty { value -> configurator.setGitUserId(value) }
            parameters.gitRepoId.ifNotEmpty { value -> configurator.setGitRepoId(value) }
            parameters.releaseNote.ifNotEmpty { value -> configurator.setReleaseNote(value) }
            parameters.httpUserAgent.ifNotEmpty { value -> configurator.setHttpUserAgent(value) }
            parameters.ignoreFileOverride.ifNotEmpty { value -> configurator.setIgnoreFileOverride(value) }
            parameters.removeOperationIdPrefix.ifNotEmpty { value -> configurator.setRemoveOperationIdPrefix(value) }
            parameters.skipOperationExample.ifNotEmpty { value -> configurator.setSkipOperationExample(value) }
            parameters.logToStderr.ifNotEmpty { value -> configurator.setLogToStderr(value) }
            parameters.enablePostProcessFile.ifNotEmpty { value -> configurator.setEnablePostProcessFile(value) }

            parameters.skipValidateSpec.ifNotEmpty { value -> configurator.setValidateSpec(!value) }
            parameters.generateAliasAsModel.ifNotEmpty { value -> configurator.setGenerateAliasAsModel(value) }

            parameters.engine.ifNotEmpty { value ->
                if ("handlebars".equals(value, ignoreCase = true)) {
                    configurator.setTemplatingEngineName("handlebars")
                } else {
                    configurator.setTemplatingEngineName(value)
                }
            }

            if (parameters.instantiationTypes.isPresent) {
                parameters.instantiationTypes.get().forEach { entry ->
                    configurator.addInstantiationType(entry.key, entry.value)
                }
            }

            if (parameters.importMappings.isPresent) {
                parameters.importMappings.get().forEach { entry ->
                    configurator.addImportMapping(entry.key, entry.value)
                }
            }

            if (parameters.schemaMappings.isPresent) {
                parameters.schemaMappings.get().forEach { entry ->
                    configurator.addSchemaMapping(entry.key, entry.value)
                }
            }

            if (parameters.inlineSchemaNameMappings.isPresent) {
                parameters.inlineSchemaNameMappings.get().forEach { entry ->
                    configurator.addInlineSchemaNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.inlineSchemaOptions.isPresent) {
                parameters.inlineSchemaOptions.get().forEach { entry ->
                    configurator.addInlineSchemaOption(entry.key, entry.value)
                }
            }

            if (parameters.nameMappings.isPresent) {
                parameters.nameMappings.get().forEach { entry ->
                    configurator.addNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.parameterNameMappings.isPresent) {
                parameters.parameterNameMappings.get().forEach { entry ->
                    configurator.addParameterNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.modelNameMappings.isPresent) {
                parameters.modelNameMappings.get().forEach { entry ->
                    configurator.addModelNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.enumNameMappings.isPresent) {
                parameters.enumNameMappings.get().forEach { entry ->
                    configurator.addEnumNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.operationIdNameMappings.isPresent) {
                parameters.operationIdNameMappings.get().forEach { entry ->
                    configurator.addOperationIdNameMapping(entry.key, entry.value)
                }
            }

            if (parameters.openapiNormalizer.isPresent) {
                parameters.openapiNormalizer.get().forEach { entry ->
                    configurator.addOpenapiNormalizer(entry.key, entry.value)
                }
            }

            if (parameters.typeMappings.isPresent) {
                parameters.typeMappings.get().forEach { entry ->
                    configurator.addTypeMapping(entry.key, entry.value)
                }
            }

            if (parameters.additionalProperties.isPresent) {
                parameters.additionalProperties.get().forEach { entry ->
                    configurator.addAdditionalProperty(entry.key, entry.value)
                }
            }

            if (parameters.serverVariables.isPresent) {
                parameters.serverVariables.get().forEach { entry ->
                    configurator.addServerVariable(entry.key, entry.value)
                }
            }

            if (parameters.languageSpecificPrimitives.isPresent) {
                parameters.languageSpecificPrimitives.get().forEach {
                    configurator.addLanguageSpecificPrimitive(it)
                }
            }

            if (parameters.openapiGeneratorIgnoreList.isPresent) {
                parameters.openapiGeneratorIgnoreList.get().forEach {
                    configurator.addOpenapiGeneratorIgnoreList(it)
                }
            }

            if (parameters.reservedWordsMappings.isPresent) {
                parameters.reservedWordsMappings.get().forEach { entry ->
                    configurator.addAdditionalReservedWordMapping(entry.key, entry.value)
                }
            }

            val dryRunSetting = parameters.dryRun.getOrElse(false)

            val clientOptInput = configurator.toClientOptInput()

            if (parameters.configOptions.isPresent) {
                val userSpecifiedConfigOptions = parameters.configOptions.get()
                clientOptInput.config.cliOptions().forEach {
                    if (userSpecifiedConfigOptions.containsKey(it.opt)) {
                        clientOptInput.config.additionalProperties()[it.opt] = userSpecifiedConfigOptions[it.opt]
                    }
                }
            }

            try {
                DefaultGenerator(dryRunSetting).opts(clientOptInput).generate()
            } catch (e: RuntimeException) {
                throw RuntimeException("Code generation failed.", e)
            }
        } finally {
            GlobalSettings.reset()
        }
    }

    /**
     * Mirrors the `ifNotEmpty` helper from [GenerateTask] for [WorkParameters] properties.
     */
    private fun <T> Property<T>.ifNotEmpty(block: Property<T>.(T) -> Unit) {
        if (isPresent) {
            when (val value = get()) {
                is String -> if (value.isNotEmpty()) block(value)
                else -> block(value)
            }
        }
    }
}
