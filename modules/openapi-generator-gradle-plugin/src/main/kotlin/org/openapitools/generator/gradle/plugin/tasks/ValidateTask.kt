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

@file:Suppress("UnstableApiUsage")

package org.openapitools.generator.gradle.plugin.tasks

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.logging.Logging
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputFile
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.PathSensitive
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.options.Option
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.property
import org.openapitools.codegen.validations.oas.OpenApiEvaluator
import org.openapitools.codegen.validations.oas.RuleConfiguration

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
    @get:InputFile
    @PathSensitive(PathSensitivity.RELATIVE)
    val inputSpec = project.objects.property<String>()

    @Optional
    @Input
    val recommend = project.objects.property<Boolean?>()

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
        val logger = Logging.getLogger(javaClass)

        val spec = inputSpec.get()
        val recommendations = recommend.get()

        logger.quiet("Validating spec $spec")

        val options = ParseOptions()
        options.isResolve = true

        val result = OpenAPIParser().readLocation(spec, null, options)
        val messages = result.messages.toSet()
        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")

        val ruleConfiguration = RuleConfiguration()
        ruleConfiguration.isEnableRecommendations = recommendations

        val evaluator = OpenApiEvaluator(ruleConfiguration)
        val validationResult = evaluator.validate(result.openAPI)

        if (validationResult.warnings.isNotEmpty()) {
            out.withStyle(StyledTextOutput.Style.Info)
            out.println("\nSpec has issues or recommendations.\nIssues:\n")

            validationResult.warnings.forEach {
                out.withStyle(StyledTextOutput.Style.Info)
                out.println("\t${it.message}\n")
                logger.debug("WARNING: ${it.message}|${it.details}")
            }
        }

        if (messages.isNotEmpty() || validationResult.errors.isNotEmpty()) {

            out.withStyle(StyledTextOutput.Style.Error)
            out.println("\nSpec is invalid.\nIssues:\n")

            messages.forEach {
                out.withStyle(StyledTextOutput.Style.Error)
                out.println("\t$it\n")
                logger.debug("ERROR: $it")
            }

            validationResult.errors.forEach {
                out.withStyle(StyledTextOutput.Style.Error)
                out.println("\t${it.message}\n")
                logger.debug("ERROR: ${it.message}|${it.details}")
            }

            throw GradleException("Validation failed.")
        } else {
            out.withStyle(StyledTextOutput.Style.Success)
            logger.debug("No error validations from swagger-parser or internal validations.")
            out.println("Spec is valid.")
        }
    }
}
