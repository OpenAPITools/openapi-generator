package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import java.nio.file.Files.createTempDirectory
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Test class for Groovy DSL bridge methods (setPropertyNameAsString pattern).
 * These tests verify that Groovy users can use both method-style and property-style
 * syntax to set file/directory properties using String paths.
 *
 * Also tests the fix for stale remoteInputSpec values.
 */
class GroovyBridgeMethodsTest : TestBase() {
    override var temp: File = createTempDirectory(javaClass.simpleName).toFile()

    @Test
    fun `Custom GenerateTask should accept all AsString bridge methods`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        
        tasks.register('customGenerate', org.openapitools.generator.gradle.plugin.tasks.GenerateTask) {
            generatorName = "kotlin"
            setInputSpecAsString("spec.yaml")
            setOutputDirAsString("build/custom-kotlin")
            setTemplateDirAsString("templates")
            apiPackage = "org.openapitools.custom.api"
            invokerPackage = "org.openapitools.custom.invoker"
            modelPackage = "org.openapitools.custom.model"
        }
        """.trimIndent()

        withProjectFiles(buildContents, includeTemplates = true)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("customGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation in custom task using AsString methods"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":customGenerate")?.outcome,
            "Expected a successful run with custom task using AsString methods"
        )
    }

    @Test
    fun `Custom GenerateTask should accept all AsString bridge methods in groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        
        tasks.register('customGenerate', org.openapitools.generator.gradle.plugin.tasks.GenerateTask) {
            generatorName = "kotlin"
            inputSpecAsString = "spec.yaml"
            outputDirAsString = "build/custom-kotlin"
            templateDirAsString = "templates"
            apiPackage = "org.openapitools.custom.api"
            invokerPackage = "org.openapitools.custom.invoker"
            modelPackage = "org.openapitools.custom.model"
        }
        """.trimIndent()

        withProjectFiles(buildContents, includeTemplates = true)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("customGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation in custom task using AsString methods"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":customGenerate")?.outcome,
            "Expected a successful run with custom task using AsString methods"
        )
    }

    @Test
    fun `setInputSpecAsString should clear stale remoteInputSpec when setting local file`() {
        // This test verifies the P2 bug fix: setting a local file after a remote URL
        // should clear the remote URL to prevent it from taking precedence
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        
        tasks.register('testGenerate', org.openapitools.generator.gradle.plugin.tasks.GenerateTask) {
            generatorName = "kotlin"
            // First set a remote URL (simulating previous configuration)
            setInputSpecAsString("https://example.com/api.yaml")
            // Then override with a local file - this should clear the remote URL
            setInputSpecAsString("spec.yaml")
            setOutputDirAsString("build/test-kotlin")
            apiPackage = "org.openapitools.test.api"
            invokerPackage = "org.openapitools.test.invoker"
            modelPackage = "org.openapitools.test.model"
            skipValidateSpec = true
        }
        """.trimIndent()

        withProjectFiles(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("testGenerate")
            .withPluginClasspath()
            .build()

        // Assert - should use local file, not remote URL
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation using local file (not remote URL)"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":testGenerate")?.outcome,
            "Expected a successful run with local file overriding remote URL"
        )
    }

    // Helper method to create project files
    private fun withProjectFiles(buildContents: String, includeConfig: Boolean = false, includeTemplates: Boolean = false) {
        File(temp, "build.gradle").writeText(buildContents)

        // Create spec file
        val specContent = javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        val specFile = File(temp, "spec.yaml")
        specContent?.copyTo(specFile.outputStream())

        // Create config file if needed
        if (includeConfig) {
            val configFile = File(temp, "config.json")
            configFile.writeText("{}")
        }

        // Create templates directory if needed
        if (includeTemplates) {
            val templatesDir = File(temp, "templates")
            templatesDir.mkdirs()
        }
    }
}

