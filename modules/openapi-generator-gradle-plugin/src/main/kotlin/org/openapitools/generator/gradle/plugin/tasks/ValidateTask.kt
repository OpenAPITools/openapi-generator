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

import io.swagger.parser.OpenAPIParser
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.options.Option
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.property

/**
 * A generator which validates an Open API spec. This task outputs a list of validation issues and errors.
 *
 * Example:
 * cli:
 *
 *   ./gradlew openApiValidate --input=/path/to/file
 *
 * build.gradle:
 *
 *   openApiMeta {
 *      inputSpec = "path/to/spec.yaml"
 *   }
 *
 * @author Jim Schubert
 */
open class ValidateTask : DefaultTask() {
    @get:Internal
    var inputSpec = project.objects.property<String>()

    @Suppress("unused")
    @get:Internal
    @set:Option(option = "input", description = "The input specification.")
    var input: String? = null
        set(value) {
            inputSpec.set(value)
        }

    @Suppress("unused")
    @TaskAction
    fun doWork() {
        val spec = inputSpec.get()
        logger.quiet("Validating spec $spec")
        val result = OpenAPIParser().readLocation(spec, null, null)
        val messages = result.messages.toSet()
        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")

        if (messages.isNotEmpty()) {

            out.withStyle(StyledTextOutput.Style.Error)
            out.println("\nSpec is invalid.\nIssues:\n")

            messages.forEach {
                out.withStyle(StyledTextOutput.Style.Error)
                out.println("\t$it\n")
            }

            throw GradleException("Validation failed.")
        } else {
            out.withStyle(StyledTextOutput.Style.Success)
            out.println("Spec is valid.")
        }
    }
}