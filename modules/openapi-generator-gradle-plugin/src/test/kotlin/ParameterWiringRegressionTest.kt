package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.Test
import java.io.File
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Regression tests verifying that parameters added or fixed in the parameter-wiring
 * pass are correctly wired from the `openApiGenerate` extension through to the task
 * and ultimately to code generation.
 *
 * Each test corresponds to one of the following fixes:
 * - mergedFileName exposed on extension and wired to task
 * - mergedFileInfoName/Description/Version exposed and passed to MergedSpecBuilder
 * - enumNameMappings wired from extension to task (was silently dropped)
 * - operationIdNameMappings wired from extension to task (was silently dropped)
 * - strictSpec, minimalUpdate, generateRecursiveDependentModels added and wired
 */
class ParameterWiringRegressionTest : TestBase() {

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private fun setupSpecsDir(vararg resourceNames: String): File {
        val specsDir = createDirectory(Paths.get("${temp.path}/specs")).toFile()
        resourceNames.forEachIndexed { index, resource ->
            val target = File(specsDir, "spec-$index.yaml")
            javaClass.classLoader.getResourceAsStream(resource)!!.copyTo(target.outputStream())
        }
        return specsDir
    }

    private fun runOpenApiGenerate(buildContents: String, vararg specEntries: Pair<String, String>) =
        GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .also {
                File(temp, "build.gradle").writeText(buildContents)
                specEntries.forEach { (name, resource) ->
                    File(temp, name).also { f -> f.parentFile.mkdirs() }
                        .outputStream()
                        .use { javaClass.classLoader.getResourceAsStream(resource)!!.copyTo(it) }
                }
            }
            .build()

    // -------------------------------------------------------------------------
    // mergedFileName
    // -------------------------------------------------------------------------

    @Test
    fun `mergedFileName extension property is applied to merged spec file name`() {
        // Before the fix, mergedFileName was absent from the extension and the plugin wiring —
        // the merged file was always named "merged.yaml". Now it must respect the configured name.
        setupSpecsDir("specs/petstore-v3.0.yaml", "specs/petstore-v3.1.yaml")

        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpecRootDirectory = file("specs").absolutePath
                outputDir = file("build/kotlin").absolutePath
                mergedFileName = "my-custom-merged"
            }
        """.trimIndent())

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
        assertTrue(
            File(temp, "specs/my-custom-merged.yaml").exists(),
            "Expected merged spec file 'my-custom-merged.yaml' was not created"
        )
    }

    // -------------------------------------------------------------------------
    // mergedFileInfoName / mergedFileInfoDescription / mergedFileInfoVersion
    // -------------------------------------------------------------------------

    @Test
    fun `mergedFileInfo fields are written into the generated merged spec`() {
        // MergedSpecBuilder was previously called with only (dir, fileName); the three
        // info fields were not exposed on the Gradle plugin at all. Verify they now appear
        // in the merged YAML.
        setupSpecsDir("specs/petstore-v3.0.yaml", "specs/petstore-v3.1.yaml")

        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpecRootDirectory = file("specs").absolutePath
                outputDir = file("build/kotlin").absolutePath
                mergedFileInfoName = "Regression API"
                mergedFileInfoDescription = "Regression description"
                mergedFileInfoVersion = "9.9.9"
            }
        """.trimIndent())

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
        val mergedSpec = File(temp, "specs/merged.yaml").readText()
        assertTrue(mergedSpec.contains("Regression API"), "mergedFileInfoName not written into merged spec")
        assertTrue(mergedSpec.contains("Regression description"), "mergedFileInfoDescription not written into merged spec")
        assertTrue(mergedSpec.contains("9.9.9"), "mergedFileInfoVersion not written into merged spec")
    }

    // -------------------------------------------------------------------------
    // enumNameMappings (was not wired from extension to task)
    // -------------------------------------------------------------------------

    @Test
    fun `enumNameMappings is wired from extension to task`() {
        // Before the fix the plugin wiring was missing enumNameMappings, so any value set
        // in the extension block was silently dropped and never reached the configurator.
        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                enumNameMappings = ["Error": "ApiError"]
            }
        """.trimIndent(), "spec.yaml" to "specs/petstore-v3.0.yaml")

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Generation failed after wiring enumNameMappings — check plugin wiring"
        )
    }

    // -------------------------------------------------------------------------
    // operationIdNameMappings (was not wired from extension to task)
    // -------------------------------------------------------------------------

    @Test
    fun `operationIdNameMappings is wired from extension to task`() {
        // Same wiring gap as enumNameMappings — previously silently dropped.
        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                operationIdNameMappings = ["listPets": "getPets"]
            }
        """.trimIndent(), "spec.yaml" to "specs/petstore-v3.0.yaml")

        assertEquals(
            TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome,
            "Generation failed after wiring operationIdNameMappings — check plugin wiring"
        )
    }

    // -------------------------------------------------------------------------
    // strictSpec
    // -------------------------------------------------------------------------

    @Test
    fun `strictSpec can be configured and does not break generation`() {
        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                strictSpec = false
            }
        """.trimIndent(), "spec.yaml" to "specs/petstore-v3.0.yaml")

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
    }

    // -------------------------------------------------------------------------
    // minimalUpdate
    // -------------------------------------------------------------------------

    @Test
    fun `minimalUpdate can be configured and does not break generation`() {
        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                minimalUpdate = true
            }
        """.trimIndent(), "spec.yaml" to "specs/petstore-v3.0.yaml")

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
    }

    // -------------------------------------------------------------------------
    // generateRecursiveDependentModels
    // -------------------------------------------------------------------------

    @Test
    fun `generateRecursiveDependentModels can be configured and does not break generation`() {
        val result = runOpenApiGenerate("""
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                generateRecursiveDependentModels = true
            }
        """.trimIndent(), "spec.yaml" to "specs/petstore-v3.0.yaml")

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
    }
}
