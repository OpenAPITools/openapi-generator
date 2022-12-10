import org.apache.tools.ant.DirectoryScanner

plugins {
    id("com.gradle.enterprise") version "3.12"
    id("com.gradle.common-custom-user-data-gradle-plugin") version "1.8.2"
}

rootProject.name = "openapi-generator-project"

includeBuild("build-logic")

include("openapi-generator")
include("openapi-generator-core")
include("openapi-generator-gradle-plugin")

rootProject.children.forEach { project ->
    project.projectDir = file("modules/${project.name}")
}

// Gradle applies some default excludes patterns that must be removed for compatibility with Maven
// See: https://docs.gradle.org/current/userguide/working_with_files.html#sec:file_trees
DirectoryScanner.removeDefaultExclude("**/.gitignore")

gradleEnterprise {
    buildScan {
        val acceptTOSProp = providers.gradleProperty("${rootProject.name}.acceptGradleTOS")
        if (acceptTOSProp.map { it.toBoolean() }.getOrElse(false)) {
            termsOfServiceUrl = "https://gradle.com/terms-of-service"
            termsOfServiceAgree = "yes"
            publishAlways()
            obfuscation {
                hostname { null }
                ipAddresses { null }
                username { null }
            }
        }
    }
}
