package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.BeforeMethod
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals

class GenerateTaskFromCacheTest : TestBase() {

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

    @DataProvider(name = "gradle_version_provider")
    private fun gradleVersionProvider(): Array<Array<String>> = arrayOf(arrayOf("8.1.1"), arrayOf("7.6"))

    // inputSpec tests

    private val inputSpecExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        """.trimIndent()

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - same directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        runCacheabilityTestUsingSameDirectory(gradleVersion, inputSpecExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - different directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, inputSpecExtensionContents)
    }

    // templateDir tests

    private val templateDirExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        templateDir = file("templateDir").absolutePath
        """.trimIndent()

    private fun initializeTemplateDirTest() {
        projectDir1.resolve("templateDir").mkdir()
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - same directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeTemplateDirTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, templateDirExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - different directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeTemplateDirTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, templateDirExtensionContents)
    }

    // configFile tests

    private val configFileExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        configFile = file("configFile").absolutePath
        """.trimIndent()

    private fun initializeConfigFileTest() {
        val configFile = projectDir1.resolve("configFile")
        configFile.createNewFile()
        configFile.writeText("""{"foo":"bar"}""")
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - same directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeConfigFileTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, configFileExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - different directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeConfigFileTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, configFileExtensionContents)
    }

    // ignoreFileOverride tests

    private val ignoreFileOverrideExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        ignoreFileOverride = file(".openapi-generator-ignore").absolutePath
        """.trimIndent()

    private fun initializeIgnoreFileTest() {
        projectDir1.resolve(".openapi-generator-ignore").createNewFile()
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - same directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeIgnoreFileTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, ignoreFileOverrideExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - different directory - openApiGenerate task output should come from cache`(gradleVersion: String) {
        initializeIgnoreFileTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, ignoreFileOverrideExtensionContents)
    }

    // Helper methods & test fixtures

    private fun runCacheabilityTestUsingSameDirectory(gradleVersion: String, extensionContents: String) {
        // Arrange
        withProject(extensionContents)

        // Act
        val result1 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        val expectedRelativeFilePathSet = projectDir1.toRelativeFilePathSet()

        val result2 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.FROM_CACHE, result2.task(":openApiGenerate")?.outcome)
        assertEquals(expectedRelativeFilePathSet, projectDir1.toRelativeFilePathSet())
    }

    private fun runCacheabilityTestUsingDifferentDirectories(gradleVersion: String, extensionContents: String) {
        // Arrange
        withProject(extensionContents)
        projectDir1.copyRecursively(projectDir2)

        // Act
        val result1 = build {
            withProjectDir(projectDir1)
            withArguments("--build-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        val result2 = build {
            withProjectDir(projectDir2)
            withArguments("--build-cache", "clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.FROM_CACHE, result2.task(":openApiGenerate")?.outcome)
        assertEquals(projectDir1.toRelativeFilePathSet(), projectDir2.toRelativeFilePathSet())
    }

    private fun File.toRelativeFilePathSet() =
        resolve("build").walk().map { it.toRelativeString(resolve("build")) }.toSet()

    private fun withProject(extensionContents: String) {
        val settingsContents = """
            buildCache {
                local {
                    directory = file("${buildCacheDir.toURI()}")
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
