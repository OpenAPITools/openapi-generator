package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.BeforeMethod
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GenerateTaskConfigurationCacheTest : TestBase() {

    private lateinit var projectDirCC: File

    @BeforeMethod
    override fun before() {
        initialize()
        projectDirCC = temp.resolve("projectDirCC").apply { mkdir() }
    }

    @DataProvider(name = "gradle_version_provider")
    private fun gradleVersionProviderWithConfigurationCache(): Array<Array<String>> = arrayOf(
        arrayOf("8.14.4", "STRING"),
        arrayOf("8.14.4", "FILE")
    )

    // inputSpec tests

    private fun inputSpecExtensionContents(format: PropertyFormat) = """
        generatorName = "kotlin"
        inputSpec = ${"spec.yaml".toPropertyReference(format)}
        cleanupOutput.set(true)
        """.trimIndent()

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiGenerate should reuse configuration cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        withProject(inputSpecExtensionContents(propertyFormat))

        // Act
        val result1 = build {
            withProjectDir(projectDirCC)
            withArguments("--configuration-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        val expectedRelativeFilePathSet = projectDirCC.toRelativeFilePathSet()

        val result2 = build {
            withProjectDir(projectDirCC)
            withArguments("--configuration-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertTrue(result1.output.contains("Configuration cache entry stored."))
        assertEquals(TaskOutcome.SUCCESS, result2.task(":openApiGenerate")?.outcome)
        assertTrue(result2.output.contains("Configuration cache entry reused."))
        assertEquals(expectedRelativeFilePathSet, projectDirCC.toRelativeFilePathSet())
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiGenerate should handle up-to-date with configuration cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        withProject(inputSpecExtensionContents(propertyFormat))

        // Act - First run: Should be SUCCESS and store the configuration cache
        val result1 = build {
            withProjectDir(projectDirCC)
            withArguments("--configuration-cache", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert first run
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertTrue(result1.output.contains("Configuration cache entry stored."))

        // Act - Second run: Should be UP-TO-DATE and reuse the cache
        val result2 = build {
            withProjectDir(projectDirCC)
            withArguments("--configuration-cache", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert second run
        assertEquals(TaskOutcome.UP_TO_DATE, result2.task(":openApiGenerate")?.outcome)
        assertTrue(result2.output.contains("Configuration cache entry reused."))

        // Act - Third run: Modify spec file and task should re-execute
        val specFile = projectDirCC.resolve("spec.yaml")
        specFile.appendText("\n# Trigger change")

        val result3 = build {
            withProjectDir(projectDirCC)
            withArguments("--configuration-cache", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert third run
        assertEquals(TaskOutcome.SUCCESS, result3.task(":openApiGenerate")?.outcome)
        assertTrue(result3.output.contains("Configuration cache entry reused."))
    }

    private fun getJavaVersion(): Int {
        val version = System.getProperty("java.version")
        val parts = version.split('.')
        if (parts.first() == "1") return parts.getOrElse(1) { "0" }.toInt()
        return parts.first().toInt()
    }

    // Helper methods & test fixtures

    private fun File.toRelativeFilePathSet() =
        resolve("build").walk().map { it.toRelativeString(resolve("build")) }.toSet()

    private fun withProject(extensionContents: String) {
        val settingsContents = """
            rootProject.name = "openapi-generator"
            """.trimIndent()
        val buildContents = """
            plugins {
              id 'base'
              id 'org.openapi.generator'
            }
            openApiGenerate {
                $extensionContents
            }
            """.trimIndent()
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")!!
        )
        withProject(
            projectDir = projectDirCC,
            settingsContents = settingsContents,
            buildContents = buildContents,
            projectFiles = projectFiles
        )
    }
}
