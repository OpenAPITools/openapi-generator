package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GenerateTaskDslTest : WiremockTestBase() {
    override var temp: File = createTempDir(javaClass.simpleName)

    private val buildGradleLocalSpecExt = """
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

    private val buildGradleRemoteSpecExt = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            remoteSpec = "http://127.0.0.1:${port}/spec.yaml"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent()

    private val buildGradleLocalSpecTask = """
        plugins {
          id 'org.openapi.generator'
        }
        
        tasks.register('myTask', org.openapitools.generator.gradle.plugin.tasks.GenerateTask) {
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

    private val buildGradleRemoteSpecTask = """
        plugins {
          id 'org.openapi.generator'
        }
        
        tasks.register('myTask', org.openapitools.generator.gradle.plugin.tasks.GenerateTask) {
            generatorName = "kotlin"
            remoteSpec = "http://127.0.0.1:${port}/spec.yaml"
            outputDir = file("build/kotlin").absolutePath
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
    """.trimIndent()

    @DataProvider(name = "buildGradle")
    fun buildScripts(): MutableIterator<Array<String>> {
        val testData: ArrayList<Array<String>> = arrayListOf()
        testData.add(arrayOf(buildGradleLocalSpecExt, "openApiGenerate"))
        testData.add(arrayOf(buildGradleLocalSpecTask, "myTask"))
        testData.add(arrayOf(buildGradleRemoteSpecExt, "openApiGenerate"))
        testData.add(arrayOf(buildGradleRemoteSpecTask, "myTask"))
        return testData.iterator()
    }

    @Test(dataProvider = "buildGradle")
    fun `openApiGenerate should create an expected file structure from DSL config`(buildScript: String,
                                                                                   taskName: String) {
        mockSpecRemoteRequest()
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(buildScript, projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName)
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

        assertEquals(TaskOutcome.SUCCESS, result.task(":${taskName}")?.outcome,
            "Expected a successful run, but found ${result.task(":${taskName}")?.outcome}")
    }

    @Test(dataProvider = "buildGradle")
    fun `openApiGenerate should used up-to-date instead of regenerate`(buildScript: String,
                                                                       taskName: String) {
        mockSpecRemoteRequest()
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(buildScript, projectFiles)

        // Act
        val resultFirstRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName, "--info")
                .withPluginClasspath()
                .build()
        val resultSecondRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName, "--info")
                .withPluginClasspath()
                .build()

        // Assert
        assertFalse(resultFirstRun.output.contains("Task :${taskName} UP-TO-DATE"), "First run should not be up-to-date")
        assertTrue(resultSecondRun.output.contains("Task :${taskName} UP-TO-DATE"), "Task of second run should be up-to-date")
    }

    @Test(dataProvider = "buildGradle")
    fun `openApiGenerate should use cache instead of regenerate`(buildScript: String,
                                                                 taskName: String) {
        mockSpecRemoteRequest()
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(buildScript, projectFiles)

        // Act
        val resultFirstRun = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName, "--build-cache", "--info")
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
                .withArguments(taskName, "--build-cache", "--info")
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(resultFirstRun.output.contains("No history is available."), "First run should not be up-to-date")
        assertFalse(resultSecondRun.output.contains("No history is available."), "Task of second run should be from cache")
        assertTrue(resultSecondRun.output.contains("has been removed."), "Task of second run should detect cache changes for untracked files")
        assertTrue(resultThirdRun.output.contains("Skipping task ':${taskName}' as it is up-to-date."), "Task of third run should not require rebuild")
    }

    @Test(dataProvider = "buildGradle")
    fun `openApiValidate should fail on invalid spec`(buildScript: String,
                                                      taskName: String) {
        mockSpecRemoteRequest("specs/petstore-v3.0-invalid.yaml")
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid.yaml")
        )

        withProject(buildScript, projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName)
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        assertTrue(result.output.contains("issues with the specification"), "Unexpected/no message presented to the user for an invalid spec.")
        assertEquals(TaskOutcome.FAILED, result.task(":${taskName}")?.outcome,
                "Expected a failed run, but found ${result.task(":openApiValidate")?.outcome}")
    }

    @Test(dataProvider = "buildGradle")
    fun `openApiValidate should ok skip spec validation`(buildScript: String,
                                                         taskName: String) {
        mockSpecRemoteRequest("specs/petstore-v3.0-invalid.yaml")

        val buildScriptWithHandlebars = buildScript.replace("""generatorName = "kotlin"""", """
            generatorName = "kotlin"
                skipValidateSpec = true
        """.trimIndent())
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid.yaml")
        )

        withProject(buildScriptWithHandlebars, projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName)
                .withPluginClasspath()
                .build()

        // Assert
        assertTrue(result.output.contains("validation has been explicitly disabled"), "Unexpected/no message presented to the user for an invalid spec.")
        assertEquals(TaskOutcome.SUCCESS, result.task(":${taskName}")?.outcome,
            "Expected a successful run, but found ${result.task(":${taskName}")?.outcome}")
    }

    @Test(dataProvider = "buildGradle")
    fun `openapiGenerate should attempt to set handlebars when specified as engine`(buildScript: String,
                                                                                    taskName: String) {
        mockSpecRemoteRequest()

        val buildScriptWithHandlebars = buildScript.replace("""generatorName = "kotlin"""", """
            generatorName = "kotlin"
                engine = "handlebars"
        """.trimIndent())
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )

        withProject(buildScriptWithHandlebars, projectFiles)

        // Act
        val result = GradleRunner.create()
                .withProjectDir(temp)
                .withArguments(taskName, "--stacktrace")
                .withPluginClasspath()
                .buildAndFail()

        // Assert
        // rather than write out full handlebars generator templates, we'll just test that the configurator has set handlebars as the engine.
        assertTrue(result.output.contains("HandlebarsException"), "Stack should expose an exception for missing templates.")
        assertTrue(result.output.contains("handlebars"), "Build should have attempted to use handlebars.")
        assertEquals(TaskOutcome.FAILED, result.task(":${taskName}")?.outcome,
            "Expected a failed run, but found ${result.task(":${taskName}")?.outcome}")
    }
}
