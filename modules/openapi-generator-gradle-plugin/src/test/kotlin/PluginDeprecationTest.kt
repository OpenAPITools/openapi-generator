package org.openapitools.generator.gradle.plugin

import org.gradle.testkit.runner.GradleRunner
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import kotlin.test.assertFalse

class PluginDeprecationTest : TestBase() {

    @DataProvider(name = "gradle_version_provider")
    fun gradleVersionProvider(): Array<Array<String?>> = arrayOf(
        arrayOf(null), // uses the version of Gradle used to build the plugin itself
        arrayOf("9.8.0")
    )

    @Test(dataProvider = "gradle_version_provider")
    fun `applying the plugin should emit no deprecation warnings`(gradleVersion: String?) {
        withProject(
            """
            | plugins {
            |   id 'org.openapi.generator'
            | }
            """.trimMargin()
        )

        // --warning-mode=fail turns any deprecation into a build failure, so build() throws on one.
        val result = GradleRunner.create()
            .withProjectDir(temp)
            .withPluginClasspath()
            .apply { if (gradleVersion != null) withGradleVersion(gradleVersion) }
            .withArguments("help", "--warning-mode=fail")
            .forwardOutput()
            .build()

        assertFalse(result.output.contains("Deprecated Gradle features were used"), result.output)
    }
}
