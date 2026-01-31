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

package org.openapitools.generator.gradle.plugin

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorGenerateExtension
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorGeneratorsExtension
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorMetaExtension
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorValidateExtension
import org.openapitools.generator.gradle.plugin.tasks.GenerateTask
import org.openapitools.generator.gradle.plugin.tasks.GeneratorsTask
import org.openapitools.generator.gradle.plugin.tasks.MetaTask
import org.openapitools.generator.gradle.plugin.tasks.ValidateTask
import kotlin.jvm.java

/**
 * A plugin providing common Open API Generator use cases.
 *
 * @author Jim Schubert
 */
@Suppress("unused")
class OpenApiGeneratorPlugin : Plugin<Project> {
    override fun apply(project: Project) {
        project.run {
            val meta = extensions.create(
                "openApiMeta",
                OpenApiGeneratorMetaExtension::class.java,
                project
            )

            val validate = extensions.create(
                "openApiValidate",
                OpenApiGeneratorValidateExtension::class.java,
                project
            )

            val generate = extensions.create(
                "openApiGenerate",
                OpenApiGeneratorGenerateExtension::class.java,
                project
            )

            val generators = extensions.create(
                "openApiGenerators",
                OpenApiGeneratorGeneratorsExtension::class.java,
                project
            )

            generate.outputDir.convention(project.layout.buildDirectory.map { it.asFile.resolve("generate-resources/main").absolutePath })

            tasks.apply {
                register("openApiGenerators", GeneratorsTask::class.java).configure { task ->
                    task.group = pluginGroup
                    task.description = "Lists generators available via Open API Generators."

                    task.include.set(generators.include)
                }

                register("openApiMeta", MetaTask::class.java).configure { task ->
                    task.group = pluginGroup
                    task.description = "Generates a new generator to be consumed via Open API Generator."

                    task.generatorName.set(meta.generatorName)
                    task.packageName.set(meta.packageName)
                    task.outputFolder.set(meta.outputFolder)
                }

                register("openApiValidate", ValidateTask::class.java).configure { task ->
                    task.group = pluginGroup
                    task.description = "Validates an Open API 2.0 or 3.x specification document."

                    task.inputSpec.set(validate.inputSpec)
                    task.recommend.set(validate.recommend)
                    task.treatWarningsAsErrors.set(validate.treatWarningsAsErrors)
                }

                register("openApiGenerate", GenerateTask::class.java).configure { task ->
                    task.group = pluginGroup
                    task.description =
                        "Generate code via Open API Tools Generator for Open API 2.0 or 3.x specification documents."

                    task.verbose.set(generate.verbose)
                    task.validateSpec.set(generate.validateSpec)
                    task.generatorName.set(generate.generatorName)
                    task.outputDir.set(generate.outputDir)
                    task.inputSpec.set(generate.inputSpec)
                    task.inputSpecRootDirectory.set(generate.inputSpecRootDirectory)
                    task.inputSpecRootDirectorySkipMerge.set(generate.inputSpecRootDirectorySkipMerge)
                    task.remoteInputSpec.set(generate.remoteInputSpec)
                    task.templateDir.set(generate.templateDir)
                    task.templateResourcePath.set(generate.templateResourcePath)
                    task.auth.set(generate.auth)
                    task.globalProperties.set(generate.globalProperties)
                    task.configFile.set(generate.configFile)
                    task.skipOverwrite.set(generate.skipOverwrite)
                    task.packageName.set(generate.packageName)
                    task.apiPackage.set(generate.apiPackage)
                    task.modelPackage.set(generate.modelPackage)
                    task.modelNamePrefix.set(generate.modelNamePrefix)
                    task.modelNameSuffix.set(generate.modelNameSuffix)
                    task.apiNameSuffix.set(generate.apiNameSuffix)
                    task.instantiationTypes.set(generate.instantiationTypes)
                    task.typeMappings.set(generate.typeMappings)
                    task.additionalProperties.set(generate.additionalProperties)
                    task.serverVariables.set(generate.serverVariables)
                    task.languageSpecificPrimitives.set(generate.languageSpecificPrimitives)
                    task.openapiGeneratorIgnoreList.set(generate.openapiGeneratorIgnoreList)
                    task.importMappings.set(generate.importMappings)
                    task.schemaMappings.set(generate.schemaMappings)
                    task.inlineSchemaNameMappings.set(generate.inlineSchemaNameMappings)
                    task.inlineSchemaOptions.set(generate.inlineSchemaOptions)
                    task.nameMappings.set(generate.nameMappings)
                    task.modelNameMappings.set(generate.modelNameMappings)
                    task.parameterNameMappings.set(generate.parameterNameMappings)
                    task.openapiNormalizer.set(generate.openapiNormalizer)
                    task.invokerPackage.set(generate.invokerPackage)
                    task.groupId.set(generate.groupId)
                    task.id.set(generate.id)
                    task.version.set(generate.version)
                    task.library.set(generate.library)
                    task.gitHost.set(generate.gitHost)
                    task.gitUserId.set(generate.gitUserId)
                    task.gitRepoId.set(generate.gitRepoId)
                    task.releaseNote.set(generate.releaseNote)
                    task.httpUserAgent.set(generate.httpUserAgent)
                    task.reservedWordsMappings.set(generate.reservedWordsMappings)
                    task.ignoreFileOverride.set(generate.ignoreFileOverride)
                    task.removeOperationIdPrefix.set(generate.removeOperationIdPrefix)
                    task.skipOperationExample.set(generate.skipOperationExample)
                    task.apiFilesConstrainedTo.set(generate.apiFilesConstrainedTo)
                    task.modelFilesConstrainedTo.set(generate.modelFilesConstrainedTo)
                    task.supportingFilesConstrainedTo.set(generate.supportingFilesConstrainedTo)
                    task.generateModelTests.set(generate.generateModelTests)
                    task.generateModelDocumentation.set(generate.generateModelDocumentation)
                    task.generateApiTests.set(generate.generateApiTests)
                    task.generateApiDocumentation.set(generate.generateApiDocumentation)
                    task.configOptions.set(generate.configOptions)
                    task.logToStderr.set(generate.logToStderr)
                    task.enablePostProcessFile.set(generate.enablePostProcessFile)
                    task.skipValidateSpec.set(generate.skipValidateSpec)
                    task.generateAliasAsModel.set(generate.generateAliasAsModel)
                    task.engine.set(generate.engine)
                    task.cleanupOutput.set(generate.cleanupOutput)
                    task.dryRun.set(generate.dryRun)
                }
            }
        }
    }

    companion object {
        const val pluginGroup = "OpenAPI Tools"
    }
}



