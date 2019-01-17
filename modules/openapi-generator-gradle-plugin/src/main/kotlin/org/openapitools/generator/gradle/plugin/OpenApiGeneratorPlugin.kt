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

package org.openapitools.generator.gradle.plugin

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorGenerateExtension
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorMetaExtension
import org.openapitools.generator.gradle.plugin.extensions.OpenApiGeneratorValidateExtension
import org.openapitools.generator.gradle.plugin.tasks.GenerateTask
import org.openapitools.generator.gradle.plugin.tasks.GeneratorsTask
import org.openapitools.generator.gradle.plugin.tasks.MetaTask
import org.openapitools.generator.gradle.plugin.tasks.ValidateTask

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

            generate.outputDir.set("$buildDir/generate-resources/main")

            tasks.apply {
                create("openApiGenerators", GeneratorsTask::class.java) {
                    group = pluginGroup
                    description = "Lists generators available via Open API Generators."
                }

                create("openApiMeta", MetaTask::class.java) {
                    group = pluginGroup
                    description = "Generates a new generator to be consumed via Open API Generator."

                    generatorName.set(meta.generatorName)
                    packageName.set(meta.packageName)
                    outputFolder.set(meta.outputFolder)
                }

                create("openApiValidate", ValidateTask::class.java) {
                    group = pluginGroup
                    description = "Validates an Open API 2.0 or 3.x specification document."

                    inputSpec.set(validate.inputSpec)
                }

                create("openApiGenerate", GenerateTask::class.java) {
                    group = pluginGroup
                    description = "Generate code via Open API Tools Generator for Open API 2.0 or 3.x specification documents."

                    verbose.set(generate.verbose)
                    validateSpec.set(generate.validateSpec)
                    generatorName.set(generate.generatorName)
                    outputDir.set(generate.outputDir)
                    inputSpec.set(generate.inputSpec)
                    templateDir.set(generate.templateDir)
                    auth.set(generate.auth)
                    systemProperties.set(generate.systemProperties)
                    configFile.set(generate.configFile)
                    skipOverwrite.set(generate.skipOverwrite)
                    apiPackage.set(generate.apiPackage)
                    modelPackage.set(generate.modelPackage)
                    modelNamePrefix.set(generate.modelNamePrefix)
                    modelNameSuffix.set(generate.modelNameSuffix)
                    instantiationTypes.set(generate.instantiationTypes)
                    typeMappings.set(generate.typeMappings)
                    additionalProperties.set(generate.additionalProperties)
                    languageSpecificPrimitives.set(generate.languageSpecificPrimitives)
                    importMappings.set(generate.importMappings)
                    invokerPackage.set(generate.invokerPackage)
                    groupId.set(generate.groupId)
                    id.set(generate.id)
                    version.set(generate.version)
                    library.set(generate.library)
                    gitUserId.set(generate.gitUserId)
                    gitRepoId.set(generate.gitRepoId)
                    releaseNote.set(generate.releaseNote)
                    httpUserAgent.set(generate.httpUserAgent)
                    reservedWordsMappings.set(generate.reservedWordsMappings)
                    ignoreFileOverride.set(generate.ignoreFileOverride)
                    removeOperationIdPrefix.set(generate.removeOperationIdPrefix)
                    apiFilesConstrainedTo.set(generate.apiFilesConstrainedTo)
                    modelFilesConstrainedTo.set(generate.modelFilesConstrainedTo)
                    supportingFilesConstrainedTo.set(generate.supportingFilesConstrainedTo)
                    generateModelTests.set(generate.generateModelTests)
                    generateModelDocumentation.set(generate.generateModelDocumentation)
                    generateApiTests.set(generate.generateApiTests)
                    generateApiDocumentation.set(generate.generateApiDocumentation)
                    withXml.set(generate.withXml)
                    configOptions.set(generate.configOptions)
                    logToStderr.set(generate.logToStderr)
                    enablePostProcessFile.set(generate.enablePostProcessFile)
                    skipValidateSpec.set(generate.skipValidateSpec)
                    generateAliasAsModel.set(generate.generateAliasAsModel)
                }
            }
        }
    }

    companion object {
        const val pluginGroup = "OpenAPI Tools"
    }
}

