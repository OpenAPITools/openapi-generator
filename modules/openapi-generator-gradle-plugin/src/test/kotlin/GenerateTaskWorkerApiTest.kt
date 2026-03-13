package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GenerateTaskWorkerApiTest : TestBase() {
    override var temp: File = File(System.getProperty("java.io.tmpdir"), javaClass.simpleName)

    /**
     * Extra dependencies in `openApiGeneratorClasspath` reach the worker
     * and appear in the log as extra classpath entries.
     */
    @Test
    fun `openApiGenerate should pass extra classpath entries to worker when openApiGeneratorClasspath is configured`() {
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")!!
        )
        val buildContents = """
            plugins {
                id 'org.openapi.generator'
            }
            repositories {
                mavenLocal()
                mavenCentral()
            }
            dependencies {
                openApiGeneratorClasspath("org.openapitools:openapi-generator:${openApiGeneratorVersion()}")
            }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
            }
        """.trimIndent()
        withProject(buildContents, projectFiles)

        // Act
        val result = build {
            withArguments("openApiGenerate", "--stacktrace", "--info")
        }

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Generation should succeed when openApiGeneratorClasspath is explicitly configured"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected SUCCESS but got ${result.task(":openApiGenerate")?.outcome}"
        )
        assertTrue(
            File(temp, "build/kotlin/src/main/kotlin").exists(),
            "Generated sources should exist"
        )
        assertTrue(
            result.output.contains("extra classpath entries"),
            "Should log extra classpath entries when openApiGeneratorClasspath is configured"
        )
    }

    /**
     * Generation succeeds through the WorkAction path without explicit
     * classpath configuration, exercising the full property wiring from
     * GenerateTask through GenerateWorkParameters to GenerateWorkAction.
     */
    @Test
    fun `openApiGenerate should succeed through WorkAction without extra classpath configuration`() {
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")!!
        )
        val buildContents = """
            plugins {
                id 'org.openapi.generator'
            }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
            }
        """.trimIndent()
        withProject(buildContents, projectFiles)

        // Act
        val result = build {
            withArguments("openApiGenerate", "--stacktrace")
        }

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Generation should succeed with automatic classpath isolation"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected SUCCESS but got ${result.task(":openApiGenerate")?.outcome}"
        )
        assertTrue(
            File(temp, "build/kotlin/src/main/kotlin").exists(),
            "Generated sources should exist"
        )
    }

    private fun openApiGeneratorVersion(): String =
        System.getProperty("openApiGeneratorVersion", "7.21.0-SNAPSHOT")
}
