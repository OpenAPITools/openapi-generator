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
import org.gradle.api.tasks.TaskAction
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.openapitools.codegen.CodegenConfigLoader
import org.openapitools.codegen.CodegenType

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
    @Suppress("unused")
    @TaskAction
    fun doWork() {
        val generators = CodegenConfigLoader.getAll()

        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")

        StringBuilder().apply {
            val types = CodegenType.values()

            append("The following generators are available:")

            append(System.lineSeparator())
            append(System.lineSeparator())

            for (type in types) {
                append(type.name).append(" generators:")
                append(System.lineSeparator())

                generators.filter { it.tag == type }
                        .sortedBy { it.name }
                        .forEach({ generator ->
                            append("    - ")
                            append(generator.name)
                            append(System.lineSeparator())
                        })

                append(System.lineSeparator())
                append(System.lineSeparator())
            }

            out.withStyle(StyledTextOutput.Style.Success)
            out.formatln("%s%n", toString())
        }
    }
}