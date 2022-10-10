package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.BeforeMethod
import org.testng.annotations.DataProvider
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
        buildCacheDir = temp.resolve("buildCacheDir")
        buildCacheDir.mkdir()
        projectDir1 = temp.resolve("projectDir1")
        projectDir1.mkdir()
        projectDir2 = temp.resolve("projectDir2")
        projectDir2.mkdir()
    }

    @Test(dataProvider = "extension_contents")
    fun `openApiGenerate task output should come from cache after clean`(extensionContents: String) {
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

    @Test(dataProvider = "extension_contents")
    fun `openApiGenerate task output should come from cache when running from different directory`(extensionContents: String) {
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

    private companion object {
        private const val buildDir = "\$buildDir"
    }

    @DataProvider(name = "extension_contents")
    fun extensionContents() = arrayOf(
        `with minimal configuration`(),
        `with ignoreFileOverride`()
    )

    private fun `with minimal configuration`() = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        outputDir = file("$buildDir/output").absolutePath
        """.trimIndent().arrayOf()

    private fun `with ignoreFileOverride`() = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        outputDir = file("$buildDir/output").absolutePath
        ignoreFileOverride = file("$buildDir/.openapi-generator-ignore").absolutePath
        """.trimIndent().arrayOf()

    private fun String.arrayOf() = arrayOf(this)

    private fun withProject(extensionContents: String) {
        withProject(
            projectDir = projectDir1,
            settingsContents = """
                buildCache {
                    local {
                        directory = file("$buildCacheDir")
                    }
                }
                rootProject.name = "openapi-generator"
            """.trimIndent(),
            buildContents = """
                plugins {
                  id 'base'
                  id 'org.openapi.generator'
                }
                openApiGenerate {
                    $extensionContents
                }
            """.trimIndent(),
            projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")!!
            )
        )
    }
}
