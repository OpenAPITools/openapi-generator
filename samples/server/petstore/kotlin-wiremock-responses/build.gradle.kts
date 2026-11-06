import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

group = "org.openapitools"
version = "1.0.0"

tasks.wrapper {
    gradleVersion = "8.7"
}

plugins {
    alias(libs.plugins.kotlin.jvm)
}

tasks.withType<KotlinCompile>().configureEach {
    compilerOptions {
        jvmTarget.set(JvmTarget.JVM_11)
    }
}

dependencies {
    implementation(libs.wiremock)
    implementation(libs.jackson.databind)
}