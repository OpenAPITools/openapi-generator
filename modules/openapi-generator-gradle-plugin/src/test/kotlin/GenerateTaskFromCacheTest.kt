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
    private fun gradleVersionProvider(): Array<Array<String>> = arrayOf(
        arrayOf("8.14.4", "STRING"),
        arrayOf("8.14.4", "FILE")
    )

    // inputSpec tests

    private fun inputSpecExtensionContents(format: PropertyFormat) = """
        generatorName = "kotlin"
        inputSpec = ${"spec.yaml".toPropertyReference(format)}
        """.trimIndent()

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - same directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        runCacheabilityTestUsingSameDirectory(gradleVersion, inputSpecExtensionContents(propertyFormat))
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - different directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, inputSpecExtensionContents(propertyFormat))
    }

    // templateDir tests

    private fun templateDirExtensionContents(format: PropertyFormat) = """
        generatorName = "kotlin"
        inputSpec = ${"spec.yaml".toPropertyReference(format)}
        templateDir = ${"templateDir".toPropertyReference(format)}
        """.trimIndent()

    private fun initializeTemplateDirTest() {
        projectDir1.resolve("templateDir").mkdir()
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - same directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeTemplateDirTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, templateDirExtensionContents(propertyFormat))
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - different directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeTemplateDirTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, templateDirExtensionContents(propertyFormat))
    }

    // configFile tests

    private fun configFileExtensionContents(format: PropertyFormat) = """
        generatorName = "kotlin"
        inputSpec = ${"spec.yaml".toPropertyReference(format)}
        configFile = ${"configFile".toPropertyReference(format)}
        """.trimIndent()

    private fun initializeConfigFileTest() {
        val configFile = projectDir1.resolve("configFile")
        configFile.createNewFile()
        configFile.writeText("""{"foo":"bar"}""")
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - same directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeConfigFileTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, configFileExtensionContents(propertyFormat))
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - different directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeConfigFileTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, configFileExtensionContents(propertyFormat))
    }

    // ignoreFileOverride tests

    private fun ignoreFileOverrideExtensionContents(format: PropertyFormat) = """
        generatorName = "kotlin"
        inputSpec = ${"spec.yaml".toPropertyReference(format)}
        ignoreFileOverride = ${".openapi-generator-ignore".toPropertyReference(format)}
        """.trimIndent()

    private fun initializeIgnoreFileTest() {
        projectDir1.resolve(".openapi-generator-ignore").createNewFile()
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - same directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeIgnoreFileTest()
        runCacheabilityTestUsingSameDirectory(gradleVersion, ignoreFileOverrideExtensionContents(propertyFormat))
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - different directory - openApiGenerate task output should come from cache`(gradleVersion: String, format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        initializeIgnoreFileTest()
        runCacheabilityTestUsingDifferentDirectories(gradleVersion, ignoreFileOverrideExtensionContents(propertyFormat))
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
