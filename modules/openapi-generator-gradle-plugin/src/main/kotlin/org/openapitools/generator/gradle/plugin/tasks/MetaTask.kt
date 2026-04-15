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

import com.samskivert.mustache.Mustache
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.ProjectLayout
import org.gradle.api.provider.Property
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction
import org.openapitools.codegen.CodegenConfig
import org.openapitools.codegen.CodegenConstants
import org.openapitools.codegen.SupportingFile
import org.openapitools.codegen.TemplateManager
import org.openapitools.codegen.templating.CommonTemplateContentLocator
import org.openapitools.codegen.templating.MustacheEngineAdapter
import org.openapitools.codegen.templating.TemplateManagerOptions
import java.io.File
import java.io.IOException
import java.nio.charset.Charset
import javax.inject.Inject

/**
 * A task which generates a new generator (meta). Useful for redistributable generator packages.
 *
 * @author Jim Schubert
 */
@CacheableTask
abstract class MetaTask : DefaultTask() {

    @get:Inject
    abstract val layout: ProjectLayout

    @get:Input
    abstract val generatorName: Property<String>

    @get:Input
    abstract val packageName: Property<String>

    @get:OutputDirectory
    abstract val outputFolder: DirectoryProperty

    @TaskAction
    fun doWork() {
        val packageToPath = packageName.get().replace(".", File.separator)
        val dir = outputFolder.get().asFile
        val klass = "${generatorName.get().titleCasedTextOnly()}Generator"

        val templateResourceDir = generatorName.get().hyphenatedTextOnly()

        logger.debug("package: {}", packageName.get())
        logger.debug("dir: {}", dir.absolutePath)
        logger.debug("generator class: {}", klass)

        val supportingFiles = listOf(
            SupportingFile("pom.mustache", "", "pom.xml"),
            SupportingFile("generatorClass.mustache", dir("src", "main", "java", packageToPath), "$klass.java"),
            SupportingFile("README.mustache", "", "README.md"),
            SupportingFile("api.template", dir("src", "main", "resources", templateResourceDir), "api.mustache"),
            SupportingFile("model.template", dir("src", "main", "resources", templateResourceDir), "model.mustache"),
            SupportingFile("myFile.template", dir("src", "main", "resources", templateResourceDir), "myFile.mustache"),
            SupportingFile(
                "services.mustache",
                dir("src", "main", "resources", "META-INF", "services"),
                CodegenConfig::class.java.canonicalName
            )
        )

        val currentVersion = CodegenConstants::class.java.`package`.implementationVersion ?: "unknown"

        val data = mapOf(
            "generatorPackage" to packageToPath,
            "generatorClass" to klass,
            "name" to templateResourceDir,
            "fullyQualifiedGeneratorClass" to "${packageName.get()}.$klass",
            "openapiGeneratorVersion" to currentVersion
        )

        supportingFiles.forEach {
            try {
                val destinationFolder = File(dir, it.folder)
                destinationFolder.mkdirs()
                val outputFile = File(destinationFolder, it.destinationFilename)

                val templateProcessor = TemplateManager(
                    TemplateManagerOptions(false, false),
                    MustacheEngineAdapter(),
                    arrayOf(CommonTemplateContentLocator("codegen"))
                )

                val template = templateProcessor.getFullTemplateContents(it.templateFile)
                var formatted = template

                val loader = Mustache.TemplateLoader { name ->
                    templateProcessor.getTemplateReader("$name.mustache")
                }

                if (it.templateFile.endsWith(".mustache")) {
                    formatted = Mustache.compiler()
                        .withLoader(loader)
                        .defaultValue("")
                        .compile(template).execute(data)
                }

                outputFile.writeText(formatted, Charset.forName("UTF8"))

                logger.lifecycle("Wrote file to ${outputFile.absolutePath}")

            } catch (e: IOException) {
                logger.error("Failed to generate file: ${e.message}", e)
                throw GradleException("Can't generate project", e)
            }
        }

        logger.lifecycle("Created generator $klass")
    }

    private fun String.titleCasedTextOnly(): String =
        split(Regex("[^a-zA-Z0-9]")).joinToString(separator = "") { it.capitalize() }

    private fun String.hyphenatedTextOnly(): String =
        split(Regex("[^a-zA-Z0-9]")).joinToString(separator = "-") { it.toLowerCase() }

    private fun dir(vararg parts: String): String = parts.joinToString(separator = File.separator)

    // ========================================================================
    // Kotlin DSL extension function for property setter
    // Allows Kotlin DSL users to call .set(String) on the outputFolder property
    // when configuring tasks directly (e.g., tasks.named<MetaTask>("openApiMeta") { ... })
    // ========================================================================

    /**
     * Extension function to allow setting outputFolder with a String path in Kotlin DSL.
     * Example: outputFolder.set("$buildDir/generated")
     */
    fun DirectoryProperty.set(path: String) {
        // All directory properties use the same conversion logic
        this.set(layout.projectDirectory.dir(path))
    }

    // ========================================================================
    // Groovy DSL bridge methods
    // These methods allow Groovy DSL users to set properties using String paths.
    // Groovy's property syntax allows calling these as:
    //   - Method style: setOutputFolderAsString("$buildDir/generated")
    //   - Property style: outputFolderAsString = "$buildDir/generated"
    // ========================================================================

    /**
     * Groovy-compatible setter for outputFolder property.
     */
    fun setOutputFolderAsString(path: String) {
        outputFolder.set(layout.projectDirectory.dir(path))
    }
}