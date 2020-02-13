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
import org.gradle.kotlin.dsl.property

/**
 * Gradle project level extension object definition for the meta-generator task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorMetaExtension(project: Project) {
    /**
     * The human-readable generator name of the newly created template generator.
     */
    val generatorName = project.objects.property<String>()

    /**
     * The packageName generatorName to put the main class into (defaults to org.openapitools.codegen)
     */
    val packageName = project.objects.property<String>()

    /**
     * Where to write the generated files (current dir by default).
     */
    val outputFolder = project.objects.property<String>()

    init {
        generatorName.set("default")
        packageName.set("org.openapitools.codegen")
        outputFolder.set("")
    }
}