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

package org.openapitools.generator.gradle.plugin.tasks

import com.samskivert.mustache.Mustache
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction
import org.gradle.internal.logging.text.StyledTextOutput
import org.gradle.internal.logging.text.StyledTextOutputFactory
import org.gradle.kotlin.dsl.property
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

/**
 * A task which generates a new generator (meta). Useful for redistributable generator packages.
 *
 * @author Jim Schubert
 */
@CacheableTask
open class MetaTask : DefaultTask() {

    @get:Input
    val generatorName = project.objects.property<String>()

    @get:Input
    val packageName = project.objects.property<String>()

    @get:OutputDirectory
    val outputFolder = project.objects.property<String>()

    @Suppress("unused")
    @TaskAction
    fun doWork() {

        val packageToPath = packageName.get().replace(".", File.separator)
        val dir = File(outputFolder.get())
        val klass = "${generatorName.get().titleCasedTextOnly()}Generator"

        val templateResourceDir = generatorName.get().hyphenatedTextOnly()

        val out = services.get(StyledTextOutputFactory::class.java).create("openapi")

        out.withStyle(StyledTextOutput.Style.Info)

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
                SupportingFile("services.mustache", dir("src", "main", "resources", "META-INF", "services"), CodegenConfig::class.java.canonicalName))

        val currentVersion = CodegenConstants::class.java.`package`.implementationVersion

        val data = mapOf("generatorPackage" to packageToPath,
                "generatorClass" to klass,
                "name" to templateResourceDir,
                "fullyQualifiedGeneratorClass" to "${packageName.get()}.$klass",
                "openapiGeneratorVersion" to currentVersion)

        supportingFiles.map {
            try {
                val destinationFolder = File(File(dir.absolutePath), it.folder)
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

                out.formatln("Wrote file to %s", outputFile.absolutePath)

                // TODO: register outputs
                // return outputFile
            } catch (e: IOException) {
                logger.error(e.message)
                throw GradleException("Can't generate project", e)
            }
        }
        out.withStyle(StyledTextOutput.Style.Success)
        out.formatln("Created generator %s", klass)
    }

    private fun String.titleCasedTextOnly(): String =
            this.split(Regex("[^a-zA-Z0-9]")).joinToString(separator = "", transform = String::capitalize)

    private fun String.hyphenatedTextOnly(): String =
            this.split(Regex("[^a-zA-Z0-9]")).joinToString(separator = "-", transform = String::toLowerCase)

    private fun dir(vararg parts: String): String =
            parts.joinToString(separator = File.separator)
}
