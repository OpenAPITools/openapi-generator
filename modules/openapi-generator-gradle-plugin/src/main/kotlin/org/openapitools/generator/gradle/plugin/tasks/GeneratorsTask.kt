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

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.listProperty
import org.openapitools.codegen.CodegenConfigLoader
import org.openapitools.codegen.CodegenType
import org.openapitools.codegen.meta.GeneratorMetadata
import org.openapitools.codegen.meta.Stability

/**
 * A task which lists out the generators available in OpenAPI Generator
 *
 * Example (CLI):
 *
 * ./gradlew -q openApiGenerators
 *
 * @author Jim Schubert
 */
open class GeneratorsTask : DefaultTask() {
    /**
     * A list of stability indexes to include (value: all,beta,stable,experimental,deprecated). Excludes deprecated by default.
     */
    @get:Internal
    val include = project.objects.listProperty<String>()

    @Suppress("unused")
    @TaskAction
    fun doWork() {
        val generators = CodegenConfigLoader.getAll()

        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")

        StringBuilder().apply {
            val types = CodegenType.values()

            val stabilities = if (include.isPresent) {
                when {
                    include.get().contains("all") -> Stability.values().toList()
                    else -> include.get().map { Stability.forDescription(it) }
                }
            } else {
                Stability.values().filterNot { it == Stability.DEPRECATED }
            }

            append("The following generators are available:")

            append(System.lineSeparator())
            append(System.lineSeparator())

            for (type in types) {
                append(type.name).append(" generators:")
                append(System.lineSeparator())

                generators.filter { it.tag == type }
                        .sortedBy { it.name }
                        .forEach { generator ->

                            val meta: GeneratorMetadata? = generator.generatorMetadata
                            val include = stabilities.contains(meta?.stability)
                            if (include) {
                                append("    - ")
                                append(generator.name)

                                meta?.stability?.let {
                                    if (it != Stability.STABLE) {
                                        append(" (${it.value()})")
                                    }
                                }

                                append(System.lineSeparator())
                            }
                        }

                append(System.lineSeparator())
                append(System.lineSeparator())
            }

            out.withStyle(StyledTextOutput.Style.Success)
            out.formatln("%s%n", toString())
        }
    }
}