package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.SkipException
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
    private fun gradleVersionProviderWithConfigurationCache(): Array<Array<String>> = arrayOf(arrayOf("8.1.1"), arrayOf("7.6"))

    @DataProvider(name = "gradle_version_provider_without_cc")
    private fun gradleVersionProviderWithoutConfigurationCache(): Array<Array<String>> = arrayOf(arrayOf("5.6.1"))

    // inputSpec tests

    private val inputSpecExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        cleanupOutput.set(true)
        """.trimIndent()

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiGenerate should reuse configuration cache`(gradleVersion: String) {
        // Arrange
        withProject(inputSpecExtensionContents)

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

    private fun getJavaVersion(): Int {
        val version = System.getProperty("java.version")
        val parts = version.split('.')
        if (parts.first() == "1") return parts.getOrElse(1) { "0" }.toInt()
        return parts.first().toInt()
    }

    @Test(dataProvider = "gradle_version_provider_without_cc")
    fun `openApiGenerate should work with Gradle legacy versions`(gradleVersion: String) {
        if(getJavaVersion() > 12) {
            // https://docs.gradle.org/current/userguide/compatibility.html
            throw SkipException("Skipping test as Gradle ${gradleVersion} is not compatible with Java ${getJavaVersion()}")
        }
        // Arrange
        withProject(inputSpecExtensionContents)

        // Act
        val result1 = build {
            withProjectDir(projectDirCC)
            withArguments("clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        val expectedRelativeFilePathSet = projectDirCC.toRelativeFilePathSet()

        val result2 = build {
            withProjectDir(projectDirCC)
            withArguments("clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.SUCCESS, result2.task(":openApiGenerate")?.outcome)
        assertEquals(expectedRelativeFilePathSet, projectDirCC.toRelativeFilePathSet())
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
