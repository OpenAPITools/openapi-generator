package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals

class GenerateTaskUpToDateTest : TestBase() {

    @DataProvider(name = "gradle_version_provider")
    private fun gradleVersionProvider(): Array<Array<String>> = arrayOf(arrayOf("8.1.1"), arrayOf("7.6"))

    // inputSpec tests

    private val inputSpecExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        """.trimIndent()

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - no file changes - should be up-to-date`(gradleVersion: String) {
        runShouldBeUpToDateTest(gradleVersion, inputSpecExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `inputSpec - has file changes - should execute`(gradleVersion: String) {
        runShouldExecuteTest(gradleVersion, inputSpecExtensionContents) {
            val inputSpec = File(temp, "spec.yaml")
            val newContents = inputSpec.readText().replace("version: 1.0.0", "version: 1.0.1")
            inputSpec.writeText(newContents)
        }
    }

    // templateDir tests

    private val templateDirExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        templateDir = file("templateDir").absolutePath
        """.trimIndent()

    private fun initializeTemplateDirTest(): File {
        val templateDir = temp.resolve("templateDir")
        templateDir.mkdir()
        return templateDir.resolve("templateFile").apply { writeText("contents") }
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - no file changes - should be up-to-date`(gradleVersion: String) {
        initializeTemplateDirTest()
        runShouldBeUpToDateTest(gradleVersion, templateDirExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `templateDir - has file changes - should execute`(gradleVersion: String) {
        val templateFile = initializeTemplateDirTest()
        runShouldExecuteTest(gradleVersion, templateDirExtensionContents) {
            templateFile.writeText("new contents")
        }
    }

    // configFile tests

    private val configFileExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        configFile = file("configFile").absolutePath
        """.trimIndent()

    private fun initializeConfigFileTest(): File {
        return temp.resolve("configFile").apply { writeText("""{"foo":"bar"}""") }
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - no file changes - should be up-to-date`(gradleVersion: String) {
        initializeConfigFileTest()
        runShouldBeUpToDateTest(gradleVersion, configFileExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `configFile - has file changes - should execute`(gradleVersion: String) {
        val configFile = initializeConfigFileTest()
        runShouldExecuteTest(gradleVersion, configFileExtensionContents) {
            configFile.writeText("""{"foo":"baz"}""")
        }
    }

    // ignoreFileOverride tests

    private val ignoreFileOverrideExtensionContents = """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        ignoreFileOverride = file(".openapi-generator-ignore").absolutePath
        """.trimIndent()

    private fun initializeIgnoreFileTest(): File {
        return temp.resolve(".openapi-generator-ignore").apply { writeText(".some_file_to_ignore") }
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - no file changes - should be up-to-date`(gradleVersion: String) {
        initializeIgnoreFileTest()
        runShouldBeUpToDateTest(gradleVersion, ignoreFileOverrideExtensionContents)
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `ignoreFileOverride - has file changes - should execute`(gradleVersion: String) {
        val ignoreFileOverride = initializeIgnoreFileTest()
        runShouldExecuteTest(gradleVersion, ignoreFileOverrideExtensionContents) {
            ignoreFileOverride.writeText(".new_file_to_ignore")
        }
    }

    // Helper methods & test fixtures

    private fun runShouldBeUpToDateTest(gradleVersion: String, extensionContents: String) {
        // Arrange
        withProject(extensionContents)

        // Act
        val result1 = build {
            withArguments("clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        val result2 = build {
            withArguments("openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.UP_TO_DATE, result2.task(":openApiGenerate")?.outcome)
    }

    private fun runShouldExecuteTest(gradleVersion: String, extensionContents: String, action: () -> Unit) {
        // Arrange
        withProject(extensionContents)

        // Act
        val result1 = build {
            withArguments("clean", "openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        action()

        val result2 = build {
            withArguments("openApiGenerate")
            withGradleVersion(gradleVersion)
        }

        // Assert
        assertEquals(TaskOutcome.SUCCESS, result1.task(":openApiGenerate")?.outcome)
        assertEquals(TaskOutcome.SUCCESS, result2.task(":openApiGenerate")?.outcome)
    }

    private fun withProject(extensionContents: String) {
        val buildContents = """
            plugins {
              id 'base'
              id 'org.openapi.generator'
            }
            openApiGenerate {
                $extensionContents
            }
            """.trimIndent()
        File(temp, "build.gradle").writeText(buildContents)
        File(javaClass.classLoader.getResource("specs/petstore-v3.0.yaml")!!.toURI())
            .copyTo(File(temp, "spec.yaml"))
    }
}
