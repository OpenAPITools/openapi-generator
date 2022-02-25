package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class MetaTaskDslTest : TestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    @Test
    fun `openApiMeta should generate desired project contents`() {
        // Arrange
        val buildDirReplacement = "\$buildDir/meta"
        withProject("""
            | plugins {
            |   id 'org.openapi.generator'
            | }
            |
            | openApiMeta {
            |     generatorName = "Sample"
            |     packageName = "org.openapitools.example"
            |     outputFolder = "$buildDirReplacement".toString()
            | }
        """.trimMargin())

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiMeta", "--stacktrace")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Wrote file to"), "User friendly write notice is missing.")

        // To avoid any OS-specific output causing issues with our stdout comparisons, only compare on expected filenames.
        listOf(
                "SampleGenerator.java",
                "README.md",
                "api.mustache",
                "model.mustache",
                "myFile.mustache",
                "org.openapitools.codegen.CodegenConfig",
                "pom.xml"
        ).map {
            assertTrue(result.output.contains(it), "Expected $it to be listed in gradle stdout.")
        }

        assertEquals(
                TaskOutcome.SUCCESS,
                result.task(":openApiMeta")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiMeta")?.outcome}"
        )
    }
}