/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import org.gradle.api.tasks.Internal
import org.gradle.kotlin.dsl.listProperty
import org.openapitools.codegen.meta.Stability

/**
 * Gradle project level extension object definition for the generators task
 *
 * @author Jim Schubert
 */
open class OpenApiGeneratorGeneratorsExtension(project: Project) {
    /**
     * A list of stability indexes to include (value: all,beta,stable,experimental,deprecated). Excludes deprecated by default.
     */
    val include = project.objects.listProperty<String>()

    init {
        applyDefaults()
    }

    @Suppress("MemberVisibilityCanBePrivate")
    fun applyDefaults(){
        include.set(Stability.values().map { s -> s.value() }.filterNot { it == Stability.DEPRECATED.value() })
    }
}
