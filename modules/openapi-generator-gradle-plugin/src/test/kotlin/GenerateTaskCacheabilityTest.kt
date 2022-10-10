package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.BeforeMethod
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals

// todo should we consider testing with multiple different Gradle versions?
class GenerateTaskCacheabilityTest : TestBase() {

    private lateinit var buildCacheDir: File
    private lateinit var projectDir1: File
    private lateinit var projectDir2: File

    @BeforeMethod
    override fun before() {
        initialize()
        buildCacheDir = temp.resolve("buildCacheDir").apply { mkdir() }
        projectDir1 = temp.resolve("projectDir1").apply { mkdir() }
        projectDir2 = temp.resolve("projectDir2").apply { mkdir() }
    }

    // templateDir tests

    private val templateDirExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        templateDir = file("templateDir").absolutePath
        """.trimIndent()

    @Test
    fun `templateDir - same directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve("templateDir").mkdir()
        runCacheabilityTestUsingSameDirectory(templateDirExtensionContents)
    }

    @Test
    fun `templateDir - different directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve("templateDir").mkdir()
        runCacheabilityTestUsingDifferentDirectories(templateDirExtensionContents)
    }

    // configFile tests

    private val configFileExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        configFile = file("configFile").absolutePath
        """.trimIndent()

    @Test
    fun `configFile - same directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve("configFile").apply {
            createNewFile()
            writeText("""{"foo":"bar"}""")
        }
        runCacheabilityTestUsingSameDirectory(configFileExtensionContents)
    }

    @Test
    fun `configFile - different directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve("configFile").apply {
            createNewFile()
            writeText("""{"foo":"bar"}""")
        }
        runCacheabilityTestUsingDifferentDirectories(configFileExtensionContents)
    }

    // ignoreFileOverride tests

    private val ignoreFileOverrideExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        ignoreFileOverride = file(".openapi-generator-ignore").absolutePath
        """.trimIndent()

    @Test
    fun `ignoreFileOverride - same directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve(".openapi-generator-ignore").createNewFile()
        runCacheabilityTestUsingSameDirectory(ignoreFileOverrideExtensionContents)
    }

    @Test
    fun `ignoreFileOverride - different directory - openApiGenerate task output should come from cache`() {
        projectDir1.resolve(".openapi-generator-ignore").createNewFile()
        runCacheabilityTestUsingDifferentDirectories(ignoreFileOverrideExtensionContents)
    }

    // Helper methods & test fixtures

    private fun runCacheabilityTestUsingSameDirectory(extensionContents: String) {
        // Arrange
        withProject(extensionContents)

        // Act
        val result1 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "openApiGenerate")
        }

        val result2 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "clean", "openApiGenerate")
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.FROM_CACHE, result2.task(":openApiGenerate")?.outcome)
    }

    private fun runCacheabilityTestUsingDifferentDirectories(extensionContents: String) {
        // Arrange
        withProject(extensionContents)
        projectDir1.copyRecursively(projectDir2)

        // Act
        val result1 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "openApiGenerate")
        }

        val result2 = build {
            withProjectDir(projectDir2)
            withArguments("--build-cache", "openApiGenerate")
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.FROM_CACHE, result2.task(":openApiGenerate")?.outcome)
    }

    private fun withProject(extensionContents: String) {
        val settingsContents = """
            buildCache {
                local {
                    directory = file("$buildCacheDir")
                }
            }
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
            projectDir = projectDir1,
            settingsContents = settingsContents,
            buildContents = buildContents,
            projectFiles = projectFiles
        )
    }
}
