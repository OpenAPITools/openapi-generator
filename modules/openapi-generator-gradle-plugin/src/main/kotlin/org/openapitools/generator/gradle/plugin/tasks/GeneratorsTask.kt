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

import org.gradle.api.DefaultTask
import org.gradle.api.provider.ListProperty
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import org.gradle.work.DisableCachingByDefault
import org.openapitools.codegen.CodegenConfigLoader
import org.openapitools.codegen.CodegenType
import org.openapitools.codegen.meta.GeneratorMetadata
import org.openapitools.codegen.meta.Stability

/**
 * A task which lists out the generators available in OpenAPI Generator
 *
 * Example (CLI):
 *
 * ./gradlew -q openApiGenerators
 *
 * @author Jim Schubert
 */
@DisableCachingByDefault(because = "not worth caching")
abstract class GeneratorsTask : DefaultTask() {

    /**
     * A list of stability indexes to include (value: all,beta,stable,experimental,deprecated). Excludes deprecated by default.
     */
    @get:Internal
    abstract val include: ListProperty<String>

    @TaskAction
    fun doWork() {
        val generators = CodegenConfigLoader.getAll()

        // Safely extract the includes, falling back to the default if empty or not present
        val stabilities = include.orNull?.takeIf { it.isNotEmpty() }?.let { includes ->
            if (includes.contains("all")) {
                Stability.values().toList()
            } else {
                includes.map { Stability.forDescription(it) }
            }
        } ?: Stability.values().filterNot { it == Stability.DEPRECATED }

        val sb = StringBuilder()
        sb.append("The following generators are available:\n\n")

        for (type in CodegenType.values()) {
            sb.append(type.name).append(" generators:\n")

            generators.filter { it.tag == type }
                .sortedBy { it.name }
                .forEach { generator ->
                    val meta: GeneratorMetadata? = generator.generatorMetadata
                    val shouldInclude = stabilities.contains(meta?.stability)

                    if (shouldInclude) {
                        sb.append("    - ")
                        sb.append(generator.name)

                        meta?.stability?.let { stability ->
                            if (stability != Stability.STABLE) {
                                sb.append(" (${stability.value()})")
                            }
                        }

                        sb.append("\n")
                    }
                }

            sb.append("\n\n")
        }

        // Use Gradle's standard lifecycle logger instead of internal StyledTextOutputFactory
        logger.lifecycle(sb.toString())
    }
}