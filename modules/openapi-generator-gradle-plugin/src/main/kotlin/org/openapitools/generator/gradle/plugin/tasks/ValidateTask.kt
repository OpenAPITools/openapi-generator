/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.generator.gradle.plugin.tasks

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.ProjectLayout
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.*
import org.gradle.api.tasks.options.Option
import org.openapitools.codegen.validations.oas.OpenApiEvaluator
import org.openapitools.codegen.validations.oas.RuleConfiguration
import javax.inject.Inject
import kotlin.text.get
import kotlin.text.set

/**
 * A generator which validates an Open API spec. This task outputs a list of validation issues and errors.
 *
 * Example:
 * cli:
 *
 * ./gradlew openApiValidate --input=/path/to/file
 *
 * build.gradle.kts:
 *
 * openApiValidate {
 * inputSpec.set(layout.projectDirectory.file("path/to/spec.yaml"))
 * }
 *
 * @author Jim Schubert
 */
@CacheableTask
abstract class ValidateTask : DefaultTask() {

    @get:Inject
    abstract val layout: ProjectLayout

    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpec: RegularFileProperty

    @get:Optional
    @get:Input
    abstract val recommend: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val treatWarningsAsErrors: Property<Boolean>

    init {
        recommend.convention(true)
        treatWarningsAsErrors.convention(false)
    }

    @get:Internal
    @set:Option(option = "input", description = "The input specification.")
    var input: String? = null
        set(value) {
            if (value != null) {
                inputSpec.set(layout.projectDirectory.file(value))
            }
        }

    @TaskAction
    fun doWork() {
        // Evaluate inputs
        val specFile = inputSpec.get().asFile
        val specPath = specFile.absolutePath
        val recommendations = recommend.get()
        val failOnWarnings = treatWarningsAsErrors.get()

        logger.lifecycle("Validating spec $specPath")

        val options = ParseOptions()
        options.isResolve = true

        val result = OpenAPIParser().readLocation(specPath, null, options)
        val messages = result.messages.toSet()

        val ruleConfiguration = RuleConfiguration()
        ruleConfiguration.isEnableRecommendations = recommendations

        val evaluator = OpenApiEvaluator(ruleConfiguration)
        val validationResult = evaluator.validate(result.openAPI)

        val hasErrors = messages.isNotEmpty() || validationResult.errors.isNotEmpty()
        val hasWarnings = validationResult.warnings.isNotEmpty()

        if (hasWarnings) {
            logger.warn("\nSpec has issues or recommendations.\nIssues:\n")
            validationResult.warnings.forEach {
                logger.warn("\t${it.message}")
                logger.debug("WARNING: ${it.message}|${it.details}")
            }
            logger.warn("") // spacing line
        }

        if (hasErrors) {
            logger.error("\nSpec is invalid.\nIssues:\n")

            messages.forEach {
                logger.error("\t$it")
                logger.debug("ERROR: $it")
            }

            validationResult.errors.forEach {
                logger.error("\t${it.message}")
                logger.debug("ERROR: ${it.message}|${it.details}")
            }
            logger.error("") // spacing line

            throw GradleException("Validation failed. Spec is invalid.")
        }

        if (failOnWarnings && hasWarnings) {
            logger.error("\nWarnings found in the spec and 'treatWarningsAsErrors' is enabled.\nFailing validation.\n")
            throw GradleException("Validation failed due to warnings (treatWarningsAsErrors = true).")
        }

        logger.debug("No error validations from swagger-parser or internal validations.")
        logger.lifecycle("Spec is valid.")
    }
}