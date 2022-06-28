plugins {
    kotlin("jvm") version "1.5.31"
    id("com.gradle.plugin-publish") version "1.0.0-rc-3"
    `java-gradle-plugin`
    `kotlin-dsl`
    `maven-publish`
    signing
}

// Shared OpenAPI Generator version be passed via command line arg as -PopenApiGeneratorVersion=VERSION
val openApiGeneratorVersion: String by project

dependencies {
    implementation(gradleApi())
    // TODO: Compile and use plugin locally?
    implementation("org.openapitools:openapi-generator:$openApiGeneratorVersion")

    testImplementation(kotlin("test"))
    testImplementation("org.testng:testng:7.6.0")
}

tasks {
    javadoc {
        (options as StandardJavadocDocletOptions).addBooleanOption("html5", true)
    }

    val javadocJar by registering(Jar::class) {
        dependsOn(javadoc)
        from(javadoc)
        archiveClassifier.set("javadoc")
    }

    val sourcesJar by registering(Jar::class) {
        from(sourceSets.main.get().allSource)
        archiveClassifier.set("sources")
    }

    artifacts {
        archives(jar)
        archives(javadocJar)
        archives(sourcesJar)
    }

    test {
        useTestNG()
        testLogging.showStandardStreams = false
        failFast = true
        addTestOutputListener { descriptor, _ ->
            logger.lifecycle("Running test: $descriptor")
        }
        addTestOutputListener { descriptor, event ->
            // SLF4J may complain about multiple bindings depending on how this is run.
            // This is just a warning, but can make test output less readable. So we ignore it specifically.
            if (!event.message.contains("SLF4J:")) {
                logger.lifecycle("Test: $descriptor produced standard out/err: ${event.message}")
            }
        }
    }
}

group = "org.openapitools"
version = openApiGeneratorVersion
description = """
This plugin supports common functionality found in Open API Generator CLI as a gradle plugin.

This gives you the ability to generate client SDKs, documentation, new generators, and to validate Open API 2.0 and 3.x
specifications as part of your build. Other tasks are available as command line tasks.
"""

publishing {
    publications {
        register<MavenPublication>("default") {
            from(components.getByName("java"))
            pom {
                name.set("OpenAPI-Generator Contributors")
                description.set(project.description)
                url.set("https://openapi-generator.tech")
                organization {
                    name.set("org.openapitools")
                    url.set("https://github.com/OpenAPITools")
                }
                licenses {
                    license {
                        name.set("The Apache Software License, Version 2.0")
                        url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
                        distribution.set("repo")
                    }
                }
                developers {
                    developer {
                        id.set("openapitools")
                        name.set("OpenAPI-Generator Contributors")
                        email.set("team@openapitools.org")
                    }
                }
                scm {
                    url.set("https://github.com/OpenAPITools/openapi-generator")
                    connection.set("scm:git:git://github.com/OpenAPITools/openapi-generator.git")
                    developerConnection.set("scm:git:ssh://git@github.com:OpenAPITools/openapi-generator.git")
                }
                issueManagement {
                    system.set("GitHub")
                    url.set("https://github.com/OpenAPITools/openapi-generator/issues")
                }
            }
        }
    }

    repositories {
        maven {
            name = "sonatype"
            credentials {
                val ossrhUsername: String by project
                val ossrhPassword: String by project
                username = ossrhUsername
                password = ossrhPassword
            }
            url = uri("https://oss.sonatype.org/service/local/staging/deploy/maven2")
        }
    }
}

pluginBundle {
    website = "https://openapi-generator.tech/"
    vcsUrl = "https://github.com/OpenAPITools/openapi-generator"
    description =
        "OpenAPI Generator allows generation of API client libraries (SDK generation), server stubs, documentation and configuration automatically given an OpenAPI Spec (v2, v3)"
    tags = listOf("openapi-3.0", "openapi-2.0", "openapi", "swagger", "codegen", "sdk")
}

gradlePlugin {
    plugins {
        create("openApiGenerator") {
            id = "org.openapi.generator"
            displayName = "OpenAPI Generator Gradle Plugin"
            version = openApiGeneratorVersion
            group = "org.openapitools"
            implementationClass = "org.openapitools.generator.gradle.plugin.OpenApiGeneratorPlugin"
        }
    }
}

//// signing will require three keys to be defined: signing.keyId, signing.password, and signing.secretKeyRingFile.
//// These can be passed to the gradle command:
////     ./gradlew -Psigning.keyId=yourid
//// or stored as key=value pairs in ~/.gradle/gradle.properties
//// You can also apply them in CI via environment variables. See Gradle's docs for details.
signing {
    setRequired {
        !openApiGeneratorVersion.endsWith("SNAPSHOT") && gradle.taskGraph.hasTask("publishPluginMavenPublicationToNexusRepository")
    }
    sign(publishing.publications)
}