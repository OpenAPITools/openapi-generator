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

package org.openapitools.generator.gradle.plugin.extensions

import org.gradle.api.Project
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.provider.Property
import org.gradle.kotlin.dsl.property

/**
 * Gradle project level extension object definition for the meta-generator task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorMetaExtension(private val project: Project) {
    /**
     * The human-readable generator name of the newly created template generator.
     */
    val generatorName: Property<String> = project.objects.property()

    /**
     * The packageName generatorName to put the main class into (defaults to org.openapitools.codegen)
     */
    val packageName: Property<String> = project.objects.property()

    /**
     * Where to write the generated files (current dir by default).
     */
    val outputFolder: DirectoryProperty = project.objects.directoryProperty()

    init {
        generatorName.convention("default")
        packageName.convention("org.openapitools.codegen")

        // Use the native layout project directory instead of an empty string
        outputFolder.convention(project.layout.projectDirectory)
    }

    // ========================================================================
    // Backwards-compatibility bridge setter for Groovy DSL
    // This allows Groovy users to use assignment syntax: outputFolder = "path"
    // For Kotlin DSL, use the extension function below instead.
    // ========================================================================

    /** Backwards-compatibility bridge for outputFolder */
    fun setOutputFolder(path: String) {
        outputFolder.set(project.layout.projectDirectory.dir(path))
    }

    // ========================================================================
    // Kotlin DSL extension function for property setter
    // Allows Kotlin DSL users to call .set(String) on the outputFolder property
    // ========================================================================

    /**
     * Extension function to allow setting outputFolder with a String path in Kotlin DSL.
     * Example: outputFolder.set("$buildDir/generated")
     */
    fun DirectoryProperty.set(path: String) {
        if (this === outputFolder) {
            setOutputFolder(path)
        } else {
            // Fallback for any other DirectoryProperty
            this.set(project.layout.projectDirectory.dir(path))
        }
    }
}