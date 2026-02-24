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
        // Use .convention() to allow users to cleanly override these defaults
        generatorName.convention("default")
        packageName.convention("org.openapitools.codegen")

        // Use the native layout project directory instead of an empty string
        outputFolder.convention(project.layout.projectDirectory)
    }

    // ========================================================================
    // Backwards-compatibility bridge setter for Groovy/Kotlin DSL
    // Allows users to continue assigning paths as standard strings.
    // ========================================================================

    /** Backwards-compatibility bridge for outputFolder */
    fun setOutputFolder(path: String) {
        outputFolder.set(project.layout.projectDirectory.dir(path))
    }
}