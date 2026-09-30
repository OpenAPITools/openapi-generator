package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.testng.SkipException
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
        private const val GENERATOR_CLASS_NAME = "com.example.fixture.MarkerCodegen"
    }

    private val fixtureRoots = mutableListOf<File>()

    @AfterMethod
    fun cleanUpFixtureRoots() {
        fixtureRoots.forEach { it.deleteRecursively() }
        fixtureRoots.clear()
    }

    private fun buildFixtureJar(
        fixtureName: String,
        sourceFileName: String,
        source: String,
        compilationFailureMessage: String
    ): File {
        val fixtureRoot = Files.createTempDirectory("$fixtureName-fixture").toFile()
        fixtureRoots.add(fixtureRoot)
        val sourceDir = File(fixtureRoot, "src").apply { mkdirs() }
        val classesDir = File(fixtureRoot, "classes").apply { mkdirs() }
        val sourceFile = File(sourceDir, "com/example/fixture/$sourceFileName").apply {
            parentFile.mkdirs()
            writeText(source)
        }

        val compiler = ToolProvider.getSystemJavaCompiler()
            ?: throw SkipException("No system Java compiler available (test requires a JDK, not a JRE)")
        val result = compiler.run(
            null, null, null,
            "-d", classesDir.absolutePath,
            "-cp", System.getProperty("java.class.path"),
            sourceFile.absolutePath
        )
        assertEquals(0, result, compilationFailureMessage)

        val jarFile = File(fixtureRoot, "$fixtureName-fixture.jar")
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

    /**
     * Compiles a trivial `OpenAPINormalizer` subclass and packages it into a jar file that is
     * *not* on the Gradle plugin's own runtime/test classpath, simulating a user-supplied
     * normalizer artifact.
     */
    private fun buildNormalizerFixtureJar(): File {
        return buildFixtureJar(
            "normalizer",
            "NoOpNormalizer.java",
            """
            package com.example.fixture;

            import io.swagger.v3.oas.models.OpenAPI;
            import java.io.IOException;
            import java.nio.file.Files;
            import java.nio.file.Paths;
            import java.util.Map;

            public class NoOpNormalizer extends org.openapitools.codegen.OpenAPINormalizer {
                private final Map<String, String> inputRules;

                public NoOpNormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
                    super(openAPI, inputRules);
                    this.inputRules = inputRules;
                }

                @Override
                public void normalize() {
                    String markerFile = inputRules.get("MARKER_FILE");
                    if (markerFile != null) {
                        try {
                            Files.writeString(Paths.get(markerFile), "NORMALIZER_RAN");
                        } catch (IOException e) {
                            throw new RuntimeException("Failed to write normalizer marker file", e);
                        }
                    }
                    super.normalize();
                }
            }
            """.trimIndent(),
            "Failed to compile NORMALIZER_CLASS test fixture"
        )
    }

    private fun buildGeneratorFixtureJar(): File {
        return buildFixtureJar(
            "generator",
            "MarkerCodegen.java",
            """
            package com.example.fixture;

            import java.io.IOException;
            import java.nio.file.Files;
            import java.nio.file.Paths;
            import org.openapitools.codegen.DefaultCodegen;

            public class MarkerCodegen extends DefaultCodegen {
                @Override
                public String getName() {
                    return "marker-codegen";
                }

                @Override
                public void processOpts() {
                    String markerFile = (String) additionalProperties().get("markerFile");
                    if (markerFile != null) {
                        try {
                            Files.writeString(Paths.get(markerFile), "GENERATOR_RAN");
                        } catch (IOException e) {
                            throw new RuntimeException("Failed to write generator marker file", e);
                        }
                    }
                    super.processOpts();
                }
            }
            """.trimIndent(),
            "Failed to compile custom generator test fixture"
        )
    }

    private data class Fixture(val jar: File, val marker: File)

    private fun normalizerFixture(): Fixture {
        writeSpec()
        return Fixture(buildNormalizerFixtureJar(), File(temp, "normalizer-ran.marker"))
    }

    private fun generatorFixture(): Fixture {
        writeSpec()
        return Fixture(buildGeneratorFixtureJar(), File(temp, "generator-ran.marker"))
    }

    private fun groovyPath(file: File) = file.absolutePath.replace("\\", "\\\\")

    private fun buildScript(taskConfiguration: String, extraClasspath: File? = null) = buildString {
        appendLine("plugins { id 'org.openapi.generator' }")
        extraClasspath?.let {
            appendLine()
            appendLine("dependencies {")
            appendLine("    openApiGeneratorExtra(files(\"${groovyPath(it)}\"))")
            appendLine("}")
        }
        appendLine("openApiGenerate {")
        appendLine(taskConfiguration.prependIndent("    "))
        appendLine("}")
    }

    private fun withWorkerConfiguration(
        taskConfiguration: String,
        workerIsolation: String?,
        generatorClasspath: File?
    ) = listOfNotNull(
        taskConfiguration,
        workerIsolation?.let { "workerIsolation = \"$it\"" },
        generatorClasspath?.let { """generatorClasspath.from(files("${groovyPath(it)}"))""" }
    ).joinToString("\n")

    private fun normalizerConfiguration(
        marker: File,
        workerIsolation: String? = null,
        generatorClasspath: File? = null
    ) = withWorkerConfiguration(
        """
        generatorName = "kotlin"
        inputSpec = file("spec.yaml").absolutePath
        outputDir = file("build/kotlin").absolutePath
        openapiNormalizer = ["NORMALIZER_CLASS": "$NORMALIZER_CLASS_NAME", "MARKER_FILE": "${groovyPath(marker)}"]
        """.trimIndent(),
        workerIsolation,
        generatorClasspath
    )

    private fun generatorConfiguration(
        marker: File,
        workerIsolation: String? = null,
        generatorClasspath: File? = null
    ) = withWorkerConfiguration(
        """
        generatorName = "$GENERATOR_CLASS_NAME"
        inputSpec = file("spec.yaml").absolutePath
        outputDir = file("build/generator").absolutePath
        additionalProperties = [markerFile: "${groovyPath(marker)}"]
        """.trimIndent(),
        workerIsolation,
        generatorClasspath
    )

    private fun createRunner(buildContents: String): GradleRunner =
        GradleRunner.create()
            .withProjectDir(temp)
            .withArguments("openApiGenerate", "--stacktrace")
            .withPluginClasspath()
            .also { File(temp, "build.gradle").writeText(buildContents) }

    private fun runOpenApiGenerateExpectingSuccess(taskConfiguration: String, extraClasspath: File? = null) =
        createRunner(buildScript(taskConfiguration, extraClasspath)).build()

    private fun runOpenApiGenerateExpectingFailure(taskConfiguration: String) =
        createRunner(buildScript(taskConfiguration)).buildAndFail()

    private fun writeSpec(): File {
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
        writeSpec()
        val marker = File(temp, "normalizer-ran.marker")

        // Note: DefaultGenerator logs (but does not fail the build on) NORMALIZER_CLASS load
        // failures - this is pre-existing behavior unrelated to this fix. Assert on the log
        // output instead of the task outcome.
        val result = runOpenApiGenerateExpectingSuccess(normalizerConfiguration(marker))

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
        // Direct proof the normalizer never ran (in addition to the log-based checks above).
        assertTrue(
            !marker.exists(),
            "Did not expect the normalizer marker file to be created, since NORMALIZER_CLASS could not be loaded"
        )
    }

    private fun assertNormalizerLoadedSuccessfully(result: org.gradle.testkit.runner.BuildResult, marker: File) {
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
        // Direct proof the custom normalizer's normalize() actually executed, rather than relying
        // solely on the absence of failure markers above.
        assertTrue(
            marker.exists(),
            "Expected the custom normalizer to have written its marker file, proving it actually ran"
        )
        assertEquals("NORMALIZER_RAN", marker.readText())
    }

    @Test
    fun `custom generator without generatorClasspath fails with a clear error`() {
        writeSpec()
        val marker = File(temp, "generator-ran.marker")

        val result = runOpenApiGenerateExpectingFailure(generatorConfiguration(marker))

        assertTrue(result.output.contains(GENERATOR_CLASS_NAME))
        assertTrue(result.output.contains("classpath"))
        assertTrue(!marker.exists())
    }

    private fun assertGeneratorLoadedSuccessfully(result: org.gradle.testkit.runner.BuildResult, marker: File) {
        assertEquals(TaskOutcome.SUCCESS, result.task(":openApiGenerate")?.outcome)
        assertTrue(marker.exists(), "Expected the custom generator to write its marker file")
        assertEquals("GENERATOR_RAN", marker.readText())
    }

    @Test
    fun `custom generator loads via openApiGeneratorExtra configuration under process isolation`() {
        val fixture = generatorFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            generatorConfiguration(fixture.marker, "process"),
            fixture.jar
        )

        assertGeneratorLoadedSuccessfully(result, fixture.marker)
    }

    @Test
    fun `custom generator loads via openApiGeneratorExtra configuration under classloader isolation`() {
        val fixture = generatorFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            generatorConfiguration(fixture.marker, "classloader"),
            fixture.jar
        )

        assertGeneratorLoadedSuccessfully(result, fixture.marker)
    }

    @Test
    fun `custom generator loads via generatorClasspath property under process isolation`() {
        val fixture = generatorFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            generatorConfiguration(fixture.marker, "process", fixture.jar)
        )

        assertGeneratorLoadedSuccessfully(result, fixture.marker)
    }

    @Test
    fun `custom generator loads via generatorClasspath property under classloader isolation`() {
        val fixture = generatorFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            generatorConfiguration(fixture.marker, "classloader", fixture.jar)
        )

        assertGeneratorLoadedSuccessfully(result, fixture.marker)
    }

    // -------------------------------------------------------------------------
    // openApiGeneratorExtra configuration - process isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via openApiGeneratorExtra configuration under process isolation`() {
        val fixture = normalizerFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            normalizerConfiguration(fixture.marker, "process"),
            fixture.jar
        )

        assertNormalizerLoadedSuccessfully(result, fixture.marker)
    }

    // -------------------------------------------------------------------------
    // openApiGeneratorExtra configuration - classloader isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via openApiGeneratorExtra configuration under classloader isolation`() {
        val fixture = normalizerFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            normalizerConfiguration(fixture.marker, "classloader"),
            fixture.jar
        )

        assertNormalizerLoadedSuccessfully(result, fixture.marker)
    }

    // -------------------------------------------------------------------------
    // generatorClasspath extension property - low-level escape hatch, process isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via generatorClasspath property under process isolation`() {
        val fixture = normalizerFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            normalizerConfiguration(fixture.marker, "process", fixture.jar)
        )

        assertNormalizerLoadedSuccessfully(result, fixture.marker)
    }

    // -------------------------------------------------------------------------
    // generatorClasspath extension property - low-level escape hatch, classloader isolation
    // -------------------------------------------------------------------------

    @Test
    fun `custom NORMALIZER_CLASS loads via generatorClasspath property under classloader isolation`() {
        val fixture = normalizerFixture()

        val result = runOpenApiGenerateExpectingSuccess(
            normalizerConfiguration(fixture.marker, "classloader", fixture.jar)
        )

        assertNormalizerLoadedSuccessfully(result, fixture.marker)
    }
}
