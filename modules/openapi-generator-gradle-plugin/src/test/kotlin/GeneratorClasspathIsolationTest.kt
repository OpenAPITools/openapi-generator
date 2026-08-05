package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.annotations.AfterMethod
import org.testng.annotations.Test
import java.io.File
import java.io.FileOutputStream
import java.nio.file.Files
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import javax.tools.ToolProvider
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Functional tests verifying that a custom `NORMALIZER_CLASS` (openapiNormalizer rule) is
 * resolvable by the code generation worker when it is supplied via either:
 * - a dependency on the `openApiGeneratorExtra` configuration created by the plugin, or
 * - the `generatorClasspath` property exposed on the `openApiGenerate` extension,
 *
 * under both `workerIsolation = "process"` and `workerIsolation = "classloader"`.
 *
 * Regression coverage for: a custom NORMALIZER_CLASS not on the plugin's own runtime classpath
 * previously failed to load (most reliably reproducible under "process" isolation, since a
 * forked worker JVM only has the plugin's own classpath) because there was no supported way to
 * forward a user classpath to the worker in either isolation mode.
 */
class GeneratorClasspathIsolationTest : TestBase() {

    companion object {
        private const val NORMALIZER_CLASS_NAME = "com.example.fixture.NoOpNormalizer"
    }

    private val fixtureRoots = mutableListOf<File>()

    @AfterMethod
    fun cleanUpFixtureRoots() {
        fixtureRoots.forEach { it.deleteRecursively() }
        fixtureRoots.clear()
    }

    /**
     * Compiles a trivial `OpenAPINormalizer` subclass and packages it into a jar file that is
     * *not* on the Gradle plugin's own runtime/test classpath, simulating a user-supplied
     * normalizer artifact.
     */
    private fun buildNormalizerFixtureJar(): File {
        val fixtureRoot = Files.createTempDirectory("normalizer-fixture").toFile()
        fixtureRoots.add(fixtureRoot)
        val sourceDir = File(fixtureRoot, "src").apply { mkdirs() }
        val classesDir = File(fixtureRoot, "classes").apply { mkdirs() }

        val packageDir = File(sourceDir, "com/example/fixture").apply { mkdirs() }
        val sourceFile = File(packageDir, "NoOpNormalizer.java")
        sourceFile.writeText(
            """
            package com.example.fixture;

            import io.swagger.v3.oas.models.OpenAPI;
            import java.util.Map;

            public class NoOpNormalizer extends org.openapitools.codegen.OpenAPINormalizer {
                public NoOpNormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
                    super(openAPI, inputRules);
                }
            }
            """.trimIndent()
        )

        val compiler = ToolProvider.getSystemJavaCompiler()
        val classpath = System.getProperty("java.class.path")
        val result = compiler.run(
            null, null, null,
            "-d", classesDir.absolutePath,
            "-cp", classpath,
            sourceFile.absolutePath
        )
        assertEquals(0, result, "Failed to compile NORMALIZER_CLASS test fixture")

        val jarFile = File(fixtureRoot, "normalizer-fixture.jar")
        JarOutputStream(FileOutputStream(jarFile)).use { jar ->
            classesDir.walkTopDown().filter { it.isFile }.forEach { classFile ->
                val entryName = classFile.relativeTo(classesDir).path.replace(File.separatorChar, '/')
                jar.putNextEntry(JarEntry(entryName))
                jar.write(classFile.readBytes())
                jar.closeEntry()
            }
        }
        return jarFile
    }

    private fun runOpenApiGenerateExpectingSuccess(buildContents: String): org.gradle.testkit.runner.BuildResult =
        GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .also { File(temp, "build.gradle").writeText(buildContents) }
            .build()

    private fun copySpec(): File {
        val spec = File(temp, "spec.yaml")
        javaClass.classLoader.getResourceAsStream("specs/petstore-v3.0.yaml")!!.copyTo(spec.outputStream())
        return spec
    }

    // -------------------------------------------------------------------------
    // Negative control: without any extra classpath, a custom NORMALIZER_CLASS not on the
    // plugin's classpath must fail with a clear error - guards against a false-positive test.
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS without generatorClasspath fails with a clear error`() {
        copySpec()

        // Note: DefaultGenerator logs (but does not fail the build on) NORMALIZER_CLASS load
        // failures - this is pre-existing behavior unrelated to this fix. Assert on the log
        // output instead of the task outcome.
        val result = runOpenApiGenerateExpectingSuccess(
            """
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME"]
            }
            """.trimIndent()
        )

        assertTrue(
            result.output.contains("ClassNotFoundException"),
            "Expected a ClassNotFoundException to be reported for the unresolvable NORMALIZER_CLASS, got:\n${result.output}"
        )
        assertTrue(
            result.output.contains(NORMALIZER_CLASS_NAME),
            "Expected the clear error message to reference the unresolvable class name, got:\n${result.output}"
        )
        // Additional, more stable marker: our custom normalizer wrapping message should also be present,
        // independent of the exact stack trace formatting DefaultGenerator happens to log.
        assertTrue(
            result.output.contains("Failed to load custom NORMALIZER_CLASS"),
            "Expected the wrapped classpath-guidance message to be logged, got:\n${result.output}"
        )
    }

    private fun assertNormalizerLoadedSuccessfully(result: org.gradle.testkit.runner.BuildResult) {
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
        // Guard against a false-positive SUCCESS: DefaultGenerator only logs (but does not fail
        // the build on) a NORMALIZER_CLASS load failure, so a regression that drops the forwarded
        // classpath would otherwise leave these tests passing. Assert the failure markers are absent.
        assertTrue(
            !result.output.contains("Failed to load custom NORMALIZER_CLASS"),
            "Did not expect a NORMALIZER_CLASS load failure to be logged, got:\n${result.output}"
        )
        assertTrue(
            !result.output.contains("ClassNotFoundException"),
            "Did not expect a ClassNotFoundException to be logged, got:\n${result.output}"
        )
    }

    // -------------------------------------------------------------------------
    // openApiGeneratorExtra configuration - process isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via openApiGeneratorExtra configuration under process isolation`() {
        copySpec()
        val jar = buildNormalizerFixtureJar()

        val result = runOpenApiGenerateExpectingSuccess(
            """
            plugins { id 'org.openapi.generator' }
            dependencies {
                openApiGeneratorExtra(files("${jar.absolutePath.replace("\\", "\\\\")}"))
            }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME"]
                workerIsolation = "process"
            }
            """.trimIndent()
        )

        assertNormalizerLoadedSuccessfully(result)
    }

    // -------------------------------------------------------------------------
    // openApiGeneratorExtra configuration - classloader isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via openApiGeneratorExtra configuration under classloader isolation`() {
        copySpec()
        val jar = buildNormalizerFixtureJar()

        val result = runOpenApiGenerateExpectingSuccess(
            """
            plugins { id 'org.openapi.generator' }
            dependencies {
                openApiGeneratorExtra(files("${jar.absolutePath.replace("\\", "\\\\")}"))
            }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME"]
                workerIsolation = "classloader"
            }
            """.trimIndent()
        )

        assertNormalizerLoadedSuccessfully(result)
    }

    // -------------------------------------------------------------------------
    // generatorClasspath extension property - low-level escape hatch, process isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via generatorClasspath property under process isolation`() {
        copySpec()
        val jar = buildNormalizerFixtureJar()

        val result = runOpenApiGenerateExpectingSuccess(
            """
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME"]
                workerIsolation = "process"
                generatorClasspath.from(files("${jar.absolutePath.replace("\\", "\\\\")}"))
            }
            """.trimIndent()
        )

        assertNormalizerLoadedSuccessfully(result)
    }

    // -------------------------------------------------------------------------
    // generatorClasspath extension property - low-level escape hatch, classloader isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via generatorClasspath property under classloader isolation`() {
        copySpec()
        val jar = buildNormalizerFixtureJar()

        val result = runOpenApiGenerateExpectingSuccess(
            """
            plugins { id 'org.openapi.generator' }
            openApiGenerate {
                generatorName = "kotlin"
                inputSpec = file("spec.yaml").absolutePath
                outputDir = file("build/kotlin").absolutePath
                openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME"]
                workerIsolation = "classloader"
                generatorClasspath.from(files("${jar.absolutePath.replace("\\", "\\\\")}"))
            }
            """.trimIndent()
        )

        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
    }
}
