package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import java.nio.file.Files.createTempDirectory
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class MetaTaskDslTest : TestBase() {
    override var temp: File = createTempDirectory(javaClass.simpleName).toFile()

    @DataProvider(name = "property_format_provider")
    private fun propertyFormatProvider(): Array<Array<String>> = arrayOf(
        arrayOf("STRING"),
        arrayOf("FILE")
    )

    @Test(dataProvider = "property_format_provider")
    fun `openApiMeta should generate desired project contents`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val outputFolderSetting = when (propertyFormat) {
            PropertyFormat.STRING -> """outputFolder = "${'$'}buildDir/meta""""
            PropertyFormat.FILE -> """outputFolder.set(file("${'$'}buildDir/meta"))"""
        }
        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
            |
            | openApiMeta {
            |     generatorName = "Sample"
            |     packageName = "org.openapitools.example"
            |     $outputFolderSetting
            | }
        """.trimMargin())

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiMeta", "--stacktrace")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Wrote file to"), "User-friendly write notice is missing.")

        // To avoid any OS-specific output causing issues with our stdout comparisons, only compare on expected filenames.
        listOf(
                "SampleGenerator.java",
                "README.md",
                "api.mustache",
                "model.mustache",
                "myFile.mustache",
                "org.openapitools.codegen.CodegenConfig",
                "pom.xml"
        ).map {
            assertTrue(result.output.contains(it), "Expected $it to be listed in gradle stdout.")
        }

        assertEquals(
                TaskOutcome.SUCCESS,
                result.task(":openApiMeta")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiMeta")?.outcome}"
        )
    }

    @Test
    fun `openApiMeta should support Kotlin DSL set String syntax`() {
        // Build script using Kotlin DSL with .set(String) syntax
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            openApiMeta {
                generatorName.set("KotlinTest")
                packageName.set("org.openapitools.example.kotlin")
                outputFolder.set("${'$'}buildDir/meta-kotlin")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the meta task
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiMeta", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(result.output.contains("Wrote file to"), "User-friendly write notice is missing.")
        assertTrue(result.output.contains("KotlinTestGenerator.java"), "Expected generator class file")
        assertEquals(
            TaskOutcome.SUCCESS,
            result.task(":openApiMeta")?.outcome,
            "Expected a successful run"
        )

        // Verify the generator was created in the right location
        val generatorFile = File(temp, "build/meta-kotlin/src/main/java/org/openapitools/example/kotlin/KotlinTestGenerator.java")
        assertTrue(generatorFile.exists(), "Generator file should have been created")
    }

    @Test
    fun `openApiMeta should support task-level configuration with Kotlin DSL set String syntax`() {
        // Build script using Kotlin DSL configuring task directly
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            tasks.named<org.openapitools.generator.gradle.plugin.tasks.MetaTask>("openApiMeta") {
                generatorName.set("DirectTask")
                packageName.set("org.openapitools.example.direct")
                outputFolder.set("${'$'}buildDir/meta-direct")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the meta task
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiMeta", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(result.output.contains("Wrote file to"), "User-friendly write notice is missing.")
        assertTrue(result.output.contains("DirectTaskGenerator.java"), "Expected generator class file")
        assertEquals(
            TaskOutcome.SUCCESS,
            result.task(":openApiMeta")?.outcome,
            "Expected a successful run with task-level configuration"
        )

        // Verify the generator was created in the right location
        val generatorFile = File(temp, "build/meta-direct/src/main/java/org/openapitools/example/direct/DirectTaskGenerator.java")
        assertTrue(generatorFile.exists(), "Generator file should have been created with task-level configuration")
    }
}