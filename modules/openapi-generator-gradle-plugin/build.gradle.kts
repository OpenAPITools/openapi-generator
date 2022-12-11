plugins {
    `kotlin-dsl`
    id("com.gradle.plugin-publish") version "1.0.0"
    id("java-gradle-plugin")
    id("org.openapitools.kotlin")
}

description = """
This plugin supports common functionality found in Open API Generator CLI as a Gradle plugin.

This gives you the ability to generate client SDKs, documentation, new generators, and to validate Open API 2.0 and 3.x
specifications as part of your build. Other tasks are available as command line tasks.
"""

dependencies {
    implementation("com.samskivert:jmustache:1.14")
    implementation("io.swagger.parser.v3:swagger-parser:2.1.1")
    implementation(project(":openapi-generator"))
    implementation(project(":openapi-generator-core"))
}

@Suppress("UnstableApiUsage")
gradlePlugin {
    website.set("https://openapi-generator.tech/")
    vcsUrl.set("https://github.com/OpenAPITools/openapi-generator")
    plugins {
        register("openApiGenerator") {
            id = "org.openapi.generator"
            description = "OpenAPI Generator allows generation of API client libraries (SDK generation), server stubs, documentation and configuration automatically given an OpenAPI Spec (v2, v3)."
            displayName = "OpenAPI Generator Gradle Plugin"
            implementationClass = "org.openapitools.generator.gradle.plugin.OpenApiGeneratorPlugin"
            tags.addAll("openapi-3.0", "openapi-2.0", "openapi", "swagger", "codegen", "sdk")
        }
    }
}

// Signing requires three keys to be defined: signing.keyId, signing.password, and signing.secretKeyRingFile.
// These can be passed to the Gradle command:
//     ./gradlew -Psigning.keyId=yourid
// or stored as key=value pairs in ~/.gradle/gradle.properties
// You can also apply them in CI via environment variables. See Gradle's docs for details.
signing {
    val isReleaseVersion: Boolean by extra
    setRequired { isReleaseVersion && gradle.taskGraph.hasTask("publishPluginMavenPublicationToSonatypeRepository") }
    sign(publishing.publications)
}
