package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GenerateTaskDslTest : TestBase()  {
    override var temp: File = createTempDir(javaClass.simpleName)

    private val defaultBuildGradle = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = file("spec.yaml").absolutePath
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent()

    @Test
    fun `openApiGenerate should create an expected file structure from DSL config`() {
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle, projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Successfully generated code to"), "User friendly generate notice is missing.")

        listOf(
            "build/kotlin/.openapi-generator-ignore",
            "build/kotlin/docs/PetsApi.md",
            "build/kotlin/docs/Error.md",
            "build/kotlin/docs/Pet.md",
            "build/kotlin/README.md",
            "build/kotlin/build.gradle",
            "build/kotlin/.openapi-generator/VERSION",
            "build/kotlin/settings.gradle",
            "build/kotlin/src/main/kotlin/org/openapitools/example/model/Pet.kt",
            "build/kotlin/src/main/kotlin/org/openapitools/example/model/Error.kt",
            "build/kotlin/src/main/kotlin/org/openapitools/example/api/PetsApi.kt",
            "build/kotlin/src/main/kotlin/org/openapitools/client/infrastructure/ApiClient.kt"
        ).map {
            val f = File(temp, it)
            assertTrue(f.exists() && f.isFile, "An expected file was not generated when invoking the generation.")
        }

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}")
    }
}