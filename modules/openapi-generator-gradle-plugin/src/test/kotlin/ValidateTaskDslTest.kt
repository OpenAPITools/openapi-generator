package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome.FAILED
import org.gradle.testkit.runner.TaskOutcome.SUCCESS
import org.gradle.util.GradleVersion
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ValidateTaskDslTest : TestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    @DataProvider(name = "gradle_version_provider")
    fun gradleVersionProvider(): Array<Array<String?>> = arrayOf(
        arrayOf(null), // uses the version of Gradle used to build the plugin itself
        arrayOf("5.6.4"),
        arrayOf("6.9"),
        arrayOf("7.0"))

    private fun getGradleRunner(gradleVersion: String?): GradleRunner {
        val gradleRunner = GradleRunner.create()
        return if (gradleVersion.isNullOrBlank()) {
            //Use the current version of Gradle
            gradleRunner
        } else {
            gradleRunner.withGradleVersion(gradleVersion)
        }
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiValidate should fail on non-file spec`(gradleVersion: String?) {
        // Arrange
        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
            |
            | openApiValidate {
            |   inputSpec = "some_location"
            | }
        """.trimMargin())

        // Act
        val result = getGradleRunner(gradleVersion)
                .withProjectDir(temp)
                .withArguments("openApiValidate")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        val gradleActualVersion = gradleVersion ?: GradleVersion.current().version
        val gradleVersionParts = gradleActualVersion.split(".")
        val isBeforeGradle7 = (gradleVersionParts.isEmpty() || gradleVersionParts[0].toInt() < 7)
        val expectedMessage = if (isBeforeGradle7) {
            "some_location' specified for property 'inputSpec' does not exist"
        } else {
            "An input file was expected to be present but it doesn't exist."
        }
        assertTrue(result.output.contains(expectedMessage), "Unexpected/no message presented to the user for a spec pointing to an invalid URI.")
        assertEquals(FAILED, result.task(":openApiValidate")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiValidate should succeed on valid spec`(gradleVersion: String?) {
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )

        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
            |
            | openApiValidate {
            |   inputSpec = file("spec.yaml").absolutePath
            | }
        """.trimMargin(), projectFiles)

        // Act
        val result = getGradleRunner(gradleVersion)
                .withProjectDir(temp)
                .withArguments("openApiValidate")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Spec is valid."), "Unexpected/no message presented to the user for a valid spec.")
        assertEquals(SUCCESS, result.task(":openApiValidate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test(dataProvider = "gradle_version_provider")
    fun `openApiValidate should fail on invalid spec`(gradleVersion: String?) {
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid.yaml")
        )
        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
            |
            | openApiValidate {
            |   inputSpec = file('spec.yaml').absolutePath
            | }
        """.trimMargin(), projectFiles)

        // Act
        val result = getGradleRunner(gradleVersion)
                .withProjectDir(temp)
                .withArguments("openApiValidate")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        assertTrue(result.output.contains("Spec is invalid."), "Unexpected/no message presented to the user for an invalid spec.")
        assertEquals(FAILED, result.task(":openApiValidate")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiValidate")?.outcome}")
    }

}
