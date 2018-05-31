package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome.FAILED
import org.gradle.testkit.runner.TaskOutcome.SUCCESS
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ValidateTaskDslTest : TestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    @Test
    fun `openApiValidate should fail on non-file spec`() {
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
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiValidate")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        assertTrue(result.output.contains("unable to read location `some_location`"), "Unexpected/no message presented to the user for a spec pointing to an invalid URI.")
        assertEquals(FAILED, result.task(":openApiValidate")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test
    fun `openApiValidate should succeed on valid spec`() {
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
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiValidate")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Spec is valid."), "Unexpected/no message presented to the user for a valid spec.")
        assertEquals(SUCCESS, result.task(":openApiValidate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test
    fun `openApiValidate should fail on invalid spec`() {
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
        val result = GradleRunner.create()
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