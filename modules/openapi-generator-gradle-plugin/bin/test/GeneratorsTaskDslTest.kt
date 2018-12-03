package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GeneratorsTaskDslTest : TestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    @Test
    fun `openApiGenerators should list generators available to the user`() {
        // Arrange
        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
        """.trimMargin())

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerators")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("The following generators are available:"), "User friendly generator notice is missing.")
        assertTrue(result.output.contains("CLIENT generators:"), "Expected client generator header is missing.")
        assertTrue(result.output.contains("android"), "Spot-checking listed client generators is missing a client generator.")
        assertTrue(result.output.contains("SERVER generators:"), "Expected server generator header is missing.")
        assertTrue(result.output.contains("kotlin-server"), "Spot-checking listed server generators is missing a server generator.")
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerators")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiGenerators")?.outcome}")
    }
}