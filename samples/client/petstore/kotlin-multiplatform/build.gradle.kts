import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTarget

plugins {
    kotlin("multiplatform") version "1.5.31" // kotlin_version
    kotlin("plugin.serialization") version "1.5.31" // kotlin_version
}

group = "org.openapitools"
version = "1.0.0"

val kotlin_version = "1.5.31"
val coroutines_version = "1.5.2"
val serialization_version = "1.3.0"
val ktor_version = "1.6.3"

repositories {
    mavenCentral()
}

kotlin {
    jvm()
    ios { binaries { framework { freeCompilerArgs += "-Xobjc-generics" } } }
    js {
        browser()
        nodejs()
    }

    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:$coroutines_version")
                implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:$serialization_version")
                api("io.ktor:ktor-client-core:$ktor_version")
                api("io.ktor:ktor-client-json:$ktor_version")
                api("io.ktor:ktor-client-serialization:$ktor_version")
            }
        }

        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
                implementation("io.ktor:ktor-client-mock:$ktor_version")
            }
        }

        val jvmMain by getting {
            dependencies {
                implementation(kotlin("stdlib-jdk7"))
                implementation("io.ktor:ktor-client-cio-jvm:$ktor_version")
            }
        }

        val jvmTest by getting {
            dependencies {
                implementation(kotlin("test-junit"))
            }
        }

        val iosMain by getting {
            dependencies {
                api("io.ktor:ktor-client-ios:$ktor_version")
            }
        }

        val iosTest by getting

        val jsMain by getting {
            dependencies {
                api("io.ktor:ktor-client-js:$ktor_version")
            }
        }

        val jsTest by getting

        all {
            languageSettings.apply {
                useExperimentalAnnotation("kotlin.Experimental")
            }
        }
    }
}

tasks {
    register("iosTest") {
        val device = project.findProperty("device")?.toString() ?: "iPhone 8"
        dependsOn("linkDebugTestIosX64")
        group = JavaBasePlugin.VERIFICATION_GROUP
        description = "Execute unit tests on ${device} simulator"
        doLast {
            val binary = kotlin.targets.getByName<KotlinNativeTarget>("iosX64").binaries.getTest("DEBUG")
            exec {
                commandLine("xcrun", "simctl", "spawn", device, binary.outputFile)
            }
        }
    }
}
