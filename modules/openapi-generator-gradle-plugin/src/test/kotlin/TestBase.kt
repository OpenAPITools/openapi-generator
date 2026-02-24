package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.testng.annotations.AfterMethod
import org.testng.annotations.BeforeMethod
import java.io.File
import java.io.InputStream
import java.nio.file.Files.createTempDirectory

abstract class TestBase {
    protected open lateinit var temp: File

    @BeforeMethod
    protected open fun before() {
        initialize()
    }

    protected fun initialize() {
        temp = createTempDirectory(javaClass.simpleName).toFile()
        temp.deleteOnExit()
    }

    @AfterMethod
    protected fun after() {
        temp.deleteRecursively()
    }

    protected fun withProject(
        buildContents: String,
        projectFiles: Map<String, InputStream> = mapOf(),
        projectDir: File? = temp,
        settingsContents: String? = null
    ) {
        File(projectDir, "build.gradle").writeText(buildContents)
        if (!settingsContents.isNullOrEmpty()) {
            File(projectDir, "settings.gradle").writeText(settingsContents)
        }

        projectFiles.forEach { entry ->
            val target = File(projectDir, entry.key)
            entry.value.copyTo(target.outputStream())
        }
    }

    protected fun build(configure: GradleRunner.() -> Unit = {}) =
        GradleRunner.create()
            .withProjectDir(temp)
            .withPluginClasspath()
            .forwardOutput()
            .apply(configure)
            .build()!!
}

/**
 * Defines how file/directory properties are referenced in Gradle build scripts.
 * - STRING: Uses .absolutePath (e.g., file("spec.yaml").absolutePath)
 * - FILE: Uses direct file() reference (e.g., file("spec.yaml"))
 *
 * Note: File format tests only run with the newest Gradle version (8.7) for performance.
 * All properties in a single test use the same format (no mixing).
 */
enum class PropertyFormat {
    STRING,
    FILE
}

/**
 * Converts a file path to a Gradle property reference based on format.
 * @param format The property format to use
 * @return Formatted property value (either file("path").absolutePath or file("path"))
 */
fun String.toPropertyReference(format: PropertyFormat): String {
    return when (format) {
        PropertyFormat.STRING -> """file("$this").absolutePath"""
        PropertyFormat.FILE -> """file("$this")"""
    }
}