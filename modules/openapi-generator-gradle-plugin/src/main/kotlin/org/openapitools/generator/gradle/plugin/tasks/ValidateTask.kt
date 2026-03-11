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
import org.openapitools.generator.gradle.plugin.utils.isRemoteUri
import java.io.File
import javax.inject.Inject

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

    @get:Optional
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val inputSpec: RegularFileProperty

    @get:Optional
    @get:Input
    abstract val remoteInputSpec: Property<String>

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

    @Suppress("unused")
    @Option(option = "input", description = "The input specification (local path or URL).")
    fun setInput(value: String) {
        if (value.isNotEmpty()) {
            if (value.isRemoteUri()) {
                remoteInputSpec.set(value)
            } else {
                inputSpec.set(layout.projectDirectory.file(value))
            }
        }
    }

    @TaskAction
    fun doWork() {
        // Evaluate inputs - prefer remote if provided, fallback to local file
        val specLocation = remoteInputSpec.orNull ?: inputSpec.orNull?.asFile?.absolutePath

        if (specLocation == null) {
            throw GradleException("You must configure either inputSpec or provide a valid remote input via --input")
        }

        val recommendations = recommend.get()
        val failOnWarnings = treatWarningsAsErrors.get()

        logger.lifecycle("Validating spec $specLocation")

        val options = ParseOptions()
        options.isResolve = true

        // Pass specLocation instead of specPath
        val result = OpenAPIParser().readLocation(specLocation, null, options)
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

    // ========================================================================
    // Kotlin DSL extension function for property setter
    // Allows Kotlin DSL users to call .set(String) on the inputSpec property
    // when configuring tasks directly (e.g., tasks.named<ValidateTask>("openApiValidate") { ... })
    // ========================================================================

    /**
     * Extension function to allow setting inputSpec with a String path in Kotlin DSL.
     * Example: inputSpec.set("$rootDir/api.yaml")
     */
    fun RegularFileProperty.set(path: String) {
        when (this) {
            inputSpec -> {
                if (path.isRemoteUri()) {
                    remoteInputSpec.set(path)
                } else {
                    this.set(layout.projectDirectory.file(path))
                }
            }
            else -> {
                // Fallback for any other RegularFileProperty
                this.set(layout.projectDirectory.file(path))
            }
        }
    }

    // ========================================================================
    // Groovy DSL bridge methods
    // These methods allow Groovy DSL users to set properties using String paths.
    // Groovy's property syntax allows calling these as:
    //   - Method style: setInputSpecAsString("$rootDir/api.yaml")
    //   - Property style: inputSpecAsString = "$rootDir/api.yaml"
    // ========================================================================

    /**
     * Groovy-compatible setter for inputSpec property.
     * Accepts a String and automatically routes to remote or local file based on URI detection.
     * Clears the opposite property to prevent stale values from taking precedence.
     */
    fun setInputSpecAsString(path: String) {
        if (path.isRemoteUri()) {
            remoteInputSpec.set(path)
            inputSpec.set(null as File?)  // Clear local file to prevent conflicts
        } else {
            inputSpec.set(layout.projectDirectory.file(path))
            remoteInputSpec.set(null as String?)  // Clear remote URL to prevent conflicts
        }
    }
}