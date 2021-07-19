package org.openapitools.generator.gradle.plugin

import com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo
import com.github.tomakehurst.wiremock.client.WireMock.aResponse
import com.github.tomakehurst.wiremock.client.WireMock.stubFor
import com.github.tomakehurst.wiremock.client.WireMock.get
import com.github.tomakehurst.wiremock.WireMockServer
import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.AfterClass
import org.testng.annotations.BeforeClass
import org.testng.annotations.Test
import java.io.File
import java.io.InputStreamReader
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GenerateTaskDslWithRemoteSpecTest : TestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    private val wireMockServer: WireMockServer = WireMockServer()
    private val validSpecContent: String = loadSpecContent("specs/petstore-v3.0.yaml")
    private val invalidSpecContent: String = loadSpecContent("specs/petstore-v3.0-invalid.yaml")
    private val defaultBuildGradle = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = "http://127.0.0.1:8080/spec.yaml"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent()

    @BeforeClass
    fun startWireMockServer() {
        wireMockServer.start()
    }

    @AfterClass
    fun stopWireMockServer() {
        wireMockServer.stop()
    }

    @Test
    fun `openApiGenerate should create an expected file structure from DSL config`() {
        // Arrange
        stubValidSpec()
        withProject(defaultBuildGradle)

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

    @Test
    fun `openApiGenerate should used up-to-date instead of regenerate`() {
        // Arrange
        stubValidSpec()
        withProject(defaultBuildGradle)

        // Act
        val resultFirstRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate", "--info")
                .withPluginClasspath()
                .build()
        val resultSecondRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate", "--info")
                .withPluginClasspath()
                .build()

        // Assert
        assertFalse(resultFirstRun.output.contains("Task :openApiGenerate UP-TO-DATE"), "First run should not be up-to-date")
        assertTrue(resultSecondRun.output.contains("Task :openApiGenerate UP-TO-DATE"), "Task of second run should be up-to-date")
    }

    @Test
    fun `openApiGenerate should use cache instead of regenerate`() {
        // Arrange
        stubValidSpec()
        withProject(defaultBuildGradle)

        // Act
        val resultFirstRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate", "--build-cache", "--info")
                .withPluginClasspath()
                .build()

        // delete the build directory from the last run
        File(temp, "build/kotlin").deleteRecursively()

        // re-run
        val resultSecondRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate", "--build-cache", "--info")
                .withPluginClasspath()
                .build()

        // re-run without deletes
        val resultThirdRun = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--build-cache", "--info")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(resultFirstRun.output.contains("No history is available."), "First run should not be up-to-date")
        assertFalse(resultSecondRun.output.contains("No history is available."), "Task of second run should be from cache")
        assertTrue(resultSecondRun.output.contains("has been removed."), "Task of second run should detect cache changes for untracked files")
        assertTrue(resultThirdRun.output.contains("Skipping task ':openApiGenerate' as it is up-to-date."), "Task of third run should not require rebuild")
    }

    @Test
    fun `openApiValidate should fail on invalid spec`() {
        // Arrange
        stubInvalidSpec()
        withProject(defaultBuildGradle)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        assertTrue(result.output.contains("issues with the specification"), "Unexpected/no message presented to the user for an invalid spec.")
        assertEquals(TaskOutcome.FAILED, result.task(":openApiGenerate")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test
    fun `openApiValidate should ok skip spec validation`() {
        // Arrange
        stubInvalidSpec()
        withProject("""
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = "http://127.0.0.1:8080/spec.yaml"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            skipValidateSpec = true
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent())

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("validation has been explicitly disabled"), "Unexpected/no message presented to the user for an invalid spec.")
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}")
    }

    @Test
    fun `openapiGenerate should attempt to set handlebars when specified as engine`() {
        // Arrange
        stubValidSpec()
        withProject("""
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = "http://127.0.0.1:8080/spec.yaml"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            engine = "handlebars"
        }
    """.trimIndent())

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate", "--stacktrace")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        // rather than write out full handlebars generator templates, we'll just test that the configurator has set handlebars as the engine.
        assertTrue(result.output.contains("HandlebarsException"), "Stack should expose an exception for missing templates.")
        assertTrue(result.output.contains("handlebars"), "Build should have attempted to use handlebars.")
        assertEquals(TaskOutcome.FAILED, result.task(":openApiGenerate")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiGenerate")?.outcome}")
    }

    private fun stubValidSpec() {
        stubFor(get(urlEqualTo("/spec.yaml"))
            .willReturn(aResponse()
                .withStatus(200)
                .withBody(validSpecContent))
        )
    }

    private fun stubInvalidSpec() {
        stubFor(get(urlEqualTo("/spec.yaml"))
            .willReturn(aResponse()
                .withStatus(200)
                .withBody(invalidSpecContent))
        )
    }
    
    private fun loadSpecContent(path: String): String {
        return InputStreamReader(
            javaClass.classLoader.getResourceAsStream(path)
        ).readText()
    }
}