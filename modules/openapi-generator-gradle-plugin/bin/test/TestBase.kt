package org.openapitools.generator.gradle.plugin

import org.testng.annotations.AfterMethod
import org.testng.annotations.BeforeMethod
import java.io.File
import java.io.InputStream

abstract class TestBase {
    protected open lateinit var temp: File

    @BeforeMethod
    protected fun before() {
        temp = createTempDir(javaClass.simpleName)
        temp.deleteOnExit()
    }

    @AfterMethod
    protected fun after(){
        temp.deleteRecursively()
    }

    protected fun withProject(
            buildContents: String,
            projectFiles: Map<String, InputStream> = mapOf()
    ) {
        val buildFile = File(temp,"build.gradle")
        buildFile.writeText(buildContents)

        projectFiles.forEach { entry ->
            val target = File(temp, entry.key)
            entry.value.copyTo(target.outputStream())
        }
    }
}