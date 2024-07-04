import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "org.openapitools"
version = "1.0.0"

tasks.wrapper {
    gradleVersion = "8.7"
}

plugins {
    alias(libs.plugins.kotlin.jvm)
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "11"
}

dependencies {
    implementation(libs.wiremock)
    implementation(libs.jackson.databind)
}