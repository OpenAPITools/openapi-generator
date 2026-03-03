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
 */
class GroovyBridgeMethodsTest : TestBase() {
    override var temp: File = createTempDirectory(javaClass.simpleName).toFile()

    @Test
    fun `GenerateTask should accept inputSpecAsString property style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpecAsString = "spec.yaml"
            outputDirAsString = "build/kotlin"
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
        }
        """.trimIndent()

        withProjectFiles(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation using inputSpecAsString property style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run with inputSpecAsString property"
        )
    }

    @Test
    fun `GenerateTask should accept setInputSpecAsString method style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            setInputSpecAsString("spec.yaml")
            setOutputDirAsString("build/kotlin")
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
        }
        """.trimIndent()

        withProjectFiles(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation using setInputSpecAsString method style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run with setInputSpecAsString method"
        )
    }

    @Test
    fun `GenerateTask should accept all file and directory AsString properties`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpecAsString = "spec.yaml"
            outputDirAsString = "build/kotlin"
            templateDirAsString = "templates"
            configFileAsString = "config.json"
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
        }
        """.trimIndent()

        withProjectFiles(buildContents, includeConfig = true, includeTemplates = true)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation using all AsString properties"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run with all AsString properties"
        )
    }

    @Test
    fun `GenerateTask should handle remote inputSpec with inputSpecAsString`() {
        // Arrange
        val specUrl = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/b6b8c0db872fb4a418ae496e89c7e656e14be165/modules/openapi-generator-gradle-plugin/src/test/resources/specs/petstore-v3.0.yaml"
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpecAsString = "$specUrl"
            outputDirAsString = "build/kotlin"
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
        }
        """.trimIndent()

        File(temp, "build.gradle").writeText(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "Expected successful generation using remote URL with inputSpecAsString"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run with remote inputSpecAsString"
        )
    }

    @Test
    fun `ValidateTask should accept inputSpecAsString property style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiValidate {
            inputSpecAsString = "spec.yaml"
        }
        """.trimIndent()

        withProjectFiles(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiValidate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Spec is valid"),
            "Expected successful validation using inputSpecAsString property style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiValidate")?.outcome,
            "Expected a successful validation with inputSpecAsString property"
        )
    }

    @Test
    fun `ValidateTask should accept setInputSpecAsString method style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiValidate {
            setInputSpecAsString("spec.yaml")
        }
        """.trimIndent()

        withProjectFiles(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiValidate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Spec is valid"),
            "Expected successful validation using setInputSpecAsString method style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiValidate")?.outcome,
            "Expected a successful validation with setInputSpecAsString method"
        )
    }

    @Test
    fun `MetaTask should accept outputFolderAsString property style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiMeta {
            generatorName = "TestGenerator"
            packageName = "org.openapitools.example"
            outputFolderAsString = "build/meta"
        }
        """.trimIndent()

        File(temp, "build.gradle").writeText(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiMeta")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Created generator"),
            "Expected successful meta generation using outputFolderAsString property style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiMeta")?.outcome,
            "Expected a successful run with outputFolderAsString property"
        )
    }

    @Test
    fun `MetaTask should accept setOutputFolderAsString method style in Groovy DSL`() {
        // Arrange
        val buildContents = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiMeta {
            generatorName = "TestGenerator"
            packageName = "org.openapitools.example"
            setOutputFolderAsString("build/meta")
        }
        """.trimIndent()

        File(temp, "build.gradle").writeText(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiMeta")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Created generator"),
            "Expected successful meta generation using setOutputFolderAsString method style"
        )
        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiMeta")?.outcome,
            "Expected a successful run with setOutputFolderAsString method"
        )
    }

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

