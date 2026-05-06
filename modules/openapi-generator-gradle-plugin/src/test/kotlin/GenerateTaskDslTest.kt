package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import java.io.File
import java.nio.file.Files.createDirectory
import java.nio.file.Files.createTempDirectory
import java.nio.file.Paths
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class GenerateTaskDslTest : TestBase() {
    override var temp: File = createTempDirectory(javaClass.simpleName).toFile()

    @DataProvider(name = "property_format_provider")
    private fun propertyFormatProvider(): Array<Array<String>> = arrayOf(
        arrayOf("STRING"),
        arrayOf("FILE")
    )

    private fun defaultBuildGradle(format: PropertyFormat) = """
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = ${"spec.yaml".toPropertyReference(format)}
            outputDir = ${"build/kotlin".toPropertyReference(format)}
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
        val urlParams ="?meaningless=params&amp;so=it&amp;results=in&amp;illegal=filenames&amp;on=windows"
        // Arrange
        val buildContents = """
         plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            remoteInputSpec = "$specUrl$urlParams"
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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should create an expected file structure from root directory config`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml"),
            "spec-2.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.1.yaml")
        )

        // Arrange
        val buildContents = """
         plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpecRootDirectory = ${"specs".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
        }
        """.trimIndent()
        val tempContractDirectory: File = createDirectory(Paths.get("${temp.path}/specs")).toFile()

        File(temp, "build.gradle").writeText(buildContents)
        projectFiles.forEach { entry ->
            val target = File(tempContractDirectory, entry.key)
            entry.value?.copyTo(target.outputStream())
        }

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should create an expected file structure from DSL config`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle(propertyFormat), projectFiles)

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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should not cleanup outputDir by default`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
            "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle(propertyFormat), projectFiles)

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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should cleanup outputDir`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
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

    @Test(dataProvider = "property_format_provider")
    fun `should apply prefix & suffix config parameters`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/java".toPropertyReference(propertyFormat)}
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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should used up-to-date instead of regenerate`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle(propertyFormat), projectFiles)

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

    @Test(dataProvider = "property_format_provider")
    fun `openApiGenerate should use cache instead of regenerate`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")
        )
        withProject(defaultBuildGradle(propertyFormat), projectFiles)

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

    @Test(dataProvider = "property_format_provider")
    fun `openApiValidate should fail on invalid spec`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid-due-to-missing-info-attribute.yaml")
        )

        withProject(defaultBuildGradle(propertyFormat), projectFiles)

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

    @Test(dataProvider = "property_format_provider")
    fun `openApiValidate should ok skip spec validation`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
        // Arrange
        val projectFiles = mapOf(
                "spec.yaml" to javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0-invalid-due-to-missing-info-attribute.yaml")
        )

        withProject("""
        plugins {
          id 'org.openapi.generator'
        }
        openApiGenerate {
            generatorName = "kotlin"
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
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

    @Test(dataProvider = "property_format_provider")
    fun `openapiGenerate should attempt to set handlebars when specified as engine`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
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

    @Test(dataProvider = "property_format_provider")
    fun `openapiGenerate should attempt to set my-custom-engine (or any other) when specified as engine`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
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

    @Test(dataProvider = "property_format_provider")
    fun `openapiGenerate should set dryRun flag`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
            dryRun = true
        }
    """.trimIndent(),
            projectFiles
        )

        // Act
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .build()

        // Assert
        assertTrue(
            result.output.contains("Dry Run Results:"),
            "Dry run results message is missing."
        )
    }

    @Test(dataProvider = "property_format_provider")
    fun `openapiGenerate should set openapiGeneratorIgnoreList option`(format: String) {
        val propertyFormat = PropertyFormat.valueOf(format)
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
            inputSpec = ${"spec.yaml".toPropertyReference(propertyFormat)}
            outputDir = ${"build/kotlin".toPropertyReference(propertyFormat)}
            apiPackage = "org.openapitools.example.api"
            invokerPackage = "org.openapitools.example.invoker"
            modelPackage = "org.openapitools.example.model"
            configOptions = [
                    dateLibrary: "java8"
            ]
            openapiGeneratorIgnoreList = ["README.md"]
        }
    """.trimIndent(),
            projectFiles
        )

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

        assertTrue(
            "README.md" !in File(temp, "build/kotlin/").list(),
            "README.md should not be generated when it is in the openapiGeneratorIgnoreList."
        )

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Expected a successful run, but found ${result.task(":openApiGenerate")?.outcome}"
        )
    }

    @DataProvider(name = "gradle_version_provider")
    private fun gradleVersionProvider(): Array<Array<String>> = arrayOf(
        arrayOf("8.14.4"),
        arrayOf("8.5"),
    )

    @Test(dataProvider = "gradle_version_provider")
    fun `test implicit task wiring from producer task to generator`(gradleVersion: String) {
        // Build script with a producer task that creates a spec file at execution time
        val buildContents = """
            plugins {
                id 'org.openapi.generator'
            }

            // A task that creates a spec file at execution time
            abstract class SpecProducerTask extends DefaultTask {
                @OutputFile
                abstract RegularFileProperty getOutputFile()

                @TaskAction
                void create() {
                    getOutputFile().get().asFile.text = '''
openapi: 3.0.0
info:
  title: Produced API
  version: 1.0.0
paths:
  /pets:
    get:
      responses:
        '200':
          description: Success
'''.stripIndent()
                }
            }

            // Register the producer
            def producer = tasks.register("produceSpec", SpecProducerTask) {
                outputFile = layout.buildDirectory.file("dynamic-spec.yaml")
            }

            // Configure the generator with implicit wiring
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec.set(producer.flatMap { it.outputFile })
                outputDir = layout.buildDirectory.dir("generated")
                apiPackage = "org.openapitools.example.api"
                modelPackage = "org.openapitools.example.model"
            }
        """.trimIndent()

        File(temp, "build.gradle").writeText(buildContents)

        // Run the generator
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate")
            .withPluginClasspath()
            .withGradleVersion(gradleVersion)
            .build()

        // Verify: The producer task MUST have run because the generator needed its output
        val producerTask = result.task(":produceSpec")
        val generatorTask = result.task(":openApiGenerate")

        assertNotNull(producerTask, "Producer task should have been part of the graph")
        assertEquals(TaskOutcome.SUCCESS, producerTask.outcome, "Producer task should have succeeded")
        assertNotNull(generatorTask, "Generator task should have been part of the graph")
        assertEquals(TaskOutcome.SUCCESS, generatorTask.outcome, "Generator task should have succeeded")

        // Check that the generator actually produced something
        val versionFile = File(temp, "build/generated/.openapi-generator/VERSION")
        assertTrue(versionFile.exists(), "Generator should have run and produced output")
    }

    @Test
    fun `openApiGenerate should support Kotlin DSL set String syntax`() {
        // Create a spec file
        val specContent = """
openapi: 3.0.0
info:
  title: Test API
  version: 1.0.0
paths:
  /test:
    get:
      responses:
        '200':
          description: Success
        """.trimIndent()
        File(temp, "spec.yaml").writeText(specContent)

        // Build script using Kotlin DSL with .set(String) syntax
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            openApiGenerate {
                generatorName.set("kotlin")
                inputSpec.set("${'$'}projectDir/spec.yaml")
                outputDir.set("${'$'}buildDir/generated-kotlin")
                apiPackage.set("org.openapitools.example.api")
                modelPackage.set("org.openapitools.example.model")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the generator
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Verify the task succeeded
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)

        // Verify output was generated
        val versionFile = File(temp, "build/generated-kotlin/.openapi-generator/VERSION")
        assertTrue(versionFile.exists(), "Generator should have produced output")
    }

    @Test
    fun `openApiGenerate should support Kotlin DSL set String syntax for all file and directory properties`() {
        // Create a spec file and config file
        val specContent = """
openapi: 3.0.0
info:
  title: Test API
  version: 1.0.0
paths:
  /test:
    get:
      responses:
        '200':
          description: Success
        """.trimIndent()
        File(temp, "spec.yaml").writeText(specContent)

        val configContent = """
{
  "dateLibrary": "java8"
}
        """.trimIndent()
        File(temp, "config.json").writeText(configContent)

        // Build script using Kotlin DSL with .set(String) syntax for multiple properties
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            openApiGenerate {
                generatorName.set("kotlin")
                inputSpec.set("${'$'}projectDir/spec.yaml")
                outputDir.set("${'$'}buildDir/generated")
                configFile.set("${'$'}projectDir/config.json")
                apiPackage.set("org.openapitools.example.api")
                modelPackage.set("org.openapitools.example.model")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the generator
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Verify the task succeeded
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)

        // Verify output was generated
        val versionFile = File(temp, "build/generated/.openapi-generator/VERSION")
        assertTrue(versionFile.exists(), "Generator should have produced output")
    }

    @Test
    fun `openApiGenerate should support remote input spec with Kotlin DSL set String syntax`() {
        // Build script using remote URL with .set(String) syntax
        val specUrl = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/b6b8c0db872fb4a418ae496e89c7e656e14be165/modules/openapi-generator-gradle-plugin/src/test/resources/specs/petstore-v3.0.yaml"
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            openApiGenerate {
                generatorName.set("kotlin")
                inputSpec.set("$specUrl")
                outputDir.set("${'$'}buildDir/generated-remote")
                apiPackage.set("org.openapitools.example.api")
                modelPackage.set("org.openapitools.example.model")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the generator
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Verify the task succeeded
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)

        // Verify output was generated
        val versionFile = File(temp, "build/generated-remote/.openapi-generator/VERSION")
        assertTrue(versionFile.exists(), "Generator should have produced output from remote spec")
    }

    @Test
    fun `openApiGenerate should support task-level configuration with Kotlin DSL set String syntax`() {
        // Create a spec file
        val specContent = """
openapi: 3.0.0
info:
  title: Test API
  version: 1.0.0
paths:
  /test:
    get:
      responses:
        '200':
          description: Success
        """.trimIndent()
        File(temp, "spec.yaml").writeText(specContent)

        // Build script using Kotlin DSL configuring task directly
        val buildContents = """
            plugins {
                id("org.openapi.generator")
            }
            
            tasks.named<org.openapitools.generator.gradle.plugin.tasks.GenerateTask>("openApiGenerate") {
                generatorName.set("kotlin")
                inputSpec.set("${'$'}projectDir/spec.yaml")
                outputDir.set("${'$'}buildDir/task-configured")
                apiPackage.set("org.openapitools.example.api")
                modelPackage.set("org.openapitools.example.model")
            }
        """.trimIndent()

        File(temp, "build.gradle.kts").writeText(buildContents)

        // Run the generator
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .build()

        // Verify the task succeeded
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)

        // Verify output was generated
        val versionFile = File(temp, "build/task-configured/.openapi-generator/VERSION")
        assertTrue(versionFile.exists(), "Generator should have produced output with task-level configuration")
    }
}
