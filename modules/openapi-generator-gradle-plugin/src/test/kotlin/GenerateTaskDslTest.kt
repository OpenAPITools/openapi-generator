package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import java.nio.file.Files.createTempDirectory
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GenerateTaskDslTest : TestBase() {
    override var temp: File = createTempDirectory(javaClass.simpleName).toFile()

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
    fun `openApiGenerate should create an expected file structure from URL config`() {
        val specUrl = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/b6b8c0db872fb4a418ae496e89c7e656e14be165/modules/openapi-generator-gradle-plugin/src/test/resources/specs/petstore-v3.0.yaml"
        // Arrange
        val buildContents = """
         plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            remoteInputSpec = "$specUrl"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
        """.trimIndent()
        File(temp, "build.gradle").writeText(buildContents)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "User friendly generate notice is missing."
        )

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
            assertTrue(f.exists() && f.isFile, "An expected file was not generated when invoking the generation: $f")
        }

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}"
        )
    }

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
            assertTrue(f.exists() && f.isFile, "An expected file was not generated when invoking the generation: $f")
        }

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}")
    }

    @Test
    fun `openApiGenerate should not cleanup outputDir by default`() {
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle, projectFiles)

        val oldFile = File(temp, "build/kotlin/should-not-be-removed")
        oldFile.mkdirs()
        oldFile.createNewFile()

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "User friendly generate notice is missing."
        )

        assertTrue(oldFile.exists(), "Old files should NOT have been removed")

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}"
        )
    }

    @Test
    fun `openApiGenerate should cleanup outputDir`() {
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(
            """
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
            cleanupOutput = true
        }
    """.trimIndent(),
            projectFiles
        )

        val oldFile = File(temp, "build/kotlin/should-be-removed")
        oldFile.mkdirs()
        oldFile.createNewFile()

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Successfully generated code to"),
            "User friendly generate notice is missing."
        )

        assertFalse(oldFile.exists(), "Old files should have been removed")

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}"
        )
    }

    @Test
    fun `should apply prefix & suffix config parameters`() {
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject("""
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "java"
            inputSpec = file("spec.yaml").absolutePath
            outputDir = file("build/java").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            modelNamePrefix = "ModelPref"
            modelNameSuffix = "Suff"
            apiNameSuffix = "ApiClassSuffix"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent(), projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments("openApiGenerate")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("Successfully generated code to"), "User friendly generate notice is missing.")

        listOf(
                "build/java/src/main/java/org/openapitools/example/model/ModelPrefPetSuff.java",
                "build/java/src/main/java/org/openapitools/example/model/ModelPrefErrorSuff.java",
                "build/java/src/main/java/org/openapitools/example/api/PetsApiClassSuffix.java"
        ).map {
            val f = File(temp, it)
            assertTrue(f.exists() && f.isFile, "An expected file was not generated when invoking the generation. - $f")
        }

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
                "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}")
    }

    @Test
    fun `openApiGenerate should used up-to-date instead of regenerate`() {
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle, projectFiles)

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
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle, projectFiles)

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
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid.yaml")
        )

        withProject(defaultBuildGradle, projectFiles)

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
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid.yaml")
        )

        withProject("""
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
            skipValidateSpec = true
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent(), projectFiles)

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
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )

        withProject("""
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
            engine = "handlebars"
        }
    """.trimIndent(), projectFiles)

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

    @Test
    fun `openapiGenerate should attempt to set my-custom-engine (or any other) when specified as engine`() {
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )

        withProject("""
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
            engine = "my-custom-engine"
        }
    """.trimIndent(), projectFiles)

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .buildAndFail()

        // Assert
        // as the custom generator doesn't exist, we'll just test that the configurator has set my-custom-engine as the engine.
        assertTrue(result.output.contains("my-custom-engine"), "Build should have attempted to use my-custom-engine.")
        assertEquals(TaskOutcome.FAILED, result.task(":openApiGenerate")?.outcome,
            "Expected a failed run, but found ${result.task(":openApiGenerate")?.outcome}")
    }
}
