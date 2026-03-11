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

package org.openapitools.generator.gradle.plugin.extensions

import org.gradle.api.Project
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.Property
import org.gradle.kotlin.dsl.property
import org.openapitools.generator.gradle.plugin.utils.isRemoteUri
import java.io.File

/**
 * Gradle project level extension object definition for the generators task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorValidateExtension(private val project: Project) {
    /**
     * The input specification to validate. Supports all formats supported by the Parser.
     */
    val inputSpec: RegularFileProperty = project.objects.fileProperty()

    /**
     * The remote input specification to validate. Supports URLs/URIs.
     */
    val remoteInputSpec: Property<String> = project.objects.property()

    /**
     * Whether to offer recommendations related to the validated specification document.
     */
    val recommend: Property<Boolean> = project.objects.property<Boolean>().convention(true)

    /**
     * Whether to treat warnings as errors and fail the task.
     */
    val treatWarningsAsErrors: Property<Boolean> = project.objects.property<Boolean>().convention(false)

    // ========================================================================
    // Backwards-compatibility bridge setter for Groovy DSL
    // This allows Groovy users to use assignment syntax: inputSpec = "path"
    // For Kotlin DSL, use the extension function below instead.
    // ========================================================================

    /** Backwards-compatibility bridge for inputSpec */
    fun setInputSpec(path: String) {
        if (path.isRemoteUri()) {
            remoteInputSpec.set(path)
            inputSpec.set(null as File?)  // Clear local file to prevent conflicts
        } else {
            inputSpec.set(project.layout.projectDirectory.file(path))
            remoteInputSpec.set(null as String?)  // Clear remote URL to prevent conflicts
        }
    }

    // ========================================================================
    // Kotlin DSL extension function for property setter
    // Allows Kotlin DSL users to call .set(String) on the inputSpec property
    // ========================================================================

    /**
     * Extension function to allow setting inputSpec with a String path in Kotlin DSL.
     * Example: inputSpec.set("$rootDir/api.yaml")
     */
    fun RegularFileProperty.set(path: String) {
        if (this === inputSpec) {
            setInputSpec(path)
        } else {
            // Fallback for any other RegularFileProperty
            this.set(project.layout.projectDirectory.file(path))
        }
    }
}