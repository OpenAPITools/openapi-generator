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

import org.gradle.api.provider.ListProperty
import org.gradle.api.provider.MapProperty
import org.gradle.api.provider.Property
import org.gradle.workers.WorkParameters

/**
 * Parameters for [GenerateWorkAction].
 *
 * Each property mirrors a field on [GenerateTask]. The task wires values
 * before submitting work to the worker.
 */
interface GenerateWorkParameters : WorkParameters {
    // Input spec (resolved by the task before submission)
    val inputSpec: Property<String>
    val inputSpecRootDirectory: Property<String>
    val inputSpecRootDirectorySkipMerge: Property<Boolean>
    val mergedFileName: Property<String>
    val remoteInputSpec: Property<String>

    // Generator settings
    val verbose: Property<Boolean>
    val validateSpec: Property<Boolean>
    val generatorName: Property<String>
    val outputDir: Property<String>
    val templateDir: Property<String>
    val templateResourcePath: Property<String>
    val auth: Property<String>
    val configFile: Property<String>
    val skipOverwrite: Property<Boolean>
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
    val ignoreFileOverride: Property<String>
    val removeOperationIdPrefix: Property<Boolean>
    val skipOperationExample: Property<Boolean>
    val logToStderr: Property<Boolean>
    val enablePostProcessFile: Property<Boolean>
    val skipValidateSpec: Property<Boolean>
    val generateAliasAsModel: Property<Boolean>
    val engine: Property<String>
    val dryRun: Property<Boolean>

    // Generation scope
    val generateModelTests: Property<Boolean>
    val generateModelDocumentation: Property<Boolean>
    val generateApiTests: Property<Boolean>
    val generateApiDocumentation: Property<Boolean>
    val apiFilesConstrainedTo: ListProperty<String>
    val modelFilesConstrainedTo: ListProperty<String>
    val supportingFilesConstrainedTo: ListProperty<String>
    val openapiGeneratorIgnoreList: ListProperty<String>
    val languageSpecificPrimitives: ListProperty<String>

    // Maps
    val globalProperties: MapProperty<String, String>
    val instantiationTypes: MapProperty<String, String>
    val typeMappings: MapProperty<String, String>
    val additionalProperties: MapProperty<String, Any>
    val serverVariables: MapProperty<String, String>
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
    val reservedWordsMappings: MapProperty<String, String>
    val configOptions: MapProperty<String, String>
}
