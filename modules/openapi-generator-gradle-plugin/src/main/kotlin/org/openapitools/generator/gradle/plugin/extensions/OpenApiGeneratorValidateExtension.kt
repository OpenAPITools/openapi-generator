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
 * Gradle project level extension object definition for the generators task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorValidateExtension(project: Project) {
    /**
     * The input specification to validate. Supports all formats supported by the Parser.
     */
    val inputSpec = project.objects.property<String>()

    /**
     * Whether or not to offer recommendations related to the validated specification document.
     */
    val recommend = project.objects.property<Boolean?>()

    init {
        applyDefaults()
    }

    @Suppress("MemberVisibilityCanBePrivate")
    fun applyDefaults(){
        recommend.set(true)
    }
}