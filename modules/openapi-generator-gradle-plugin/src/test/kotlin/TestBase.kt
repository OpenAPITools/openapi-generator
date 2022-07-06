package org.openapitools.generator.gradle.plugin

import org.testng.annotations.AfterMethod
import org.testng.annotations.BeforeMethod
import java.io.File
import java.io.InputStream
import java.nio.file.Files.createTempDirectory

abstract class TestBase {
    protected open lateinit var temp: File

    @BeforeMethod
    protected fun before() {
        temp = createTempDirectory(javaClass.simpleName).toFile()
        temp.deleteOnExit()
    }

    @AfterMethod
    protected fun after() {
        temp.deleteRecursively()
    }

    protected fun withProject(buildContents: String, projectFiles: Map<String, InputStream> = mapOf()) {
        File(temp, "build.gradle").writeText(buildContents)

        projectFiles.forEach { entry ->
            val target = File(temp, entry.key)
            entry.value.copyTo(target.outputStream())
        }
    }
}