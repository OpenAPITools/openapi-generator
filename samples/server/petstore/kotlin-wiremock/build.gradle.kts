group = "org.openapitools"
version = "1.0.0"

tasks.wrapper {
    gradleVersion = "8.7"
}

plugins {
    alias(libs.plugins.kotlin.jvm)
}

dependencies {
    implementation(libs.wiremock)
    implementation(libs.jackson.databind)
}