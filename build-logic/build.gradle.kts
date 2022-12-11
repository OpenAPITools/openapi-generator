plugins {
    `kotlin-dsl`
}

repositories {
    gradlePluginPortal()
}

dependencies {
    // This plugin version must be aligned with the version embedded in Gradle
    // See: https://docs.gradle.org/current/userguide/compatibility.html#kotlin
    implementation("org.jetbrains.kotlin.jvm:org.jetbrains.kotlin.jvm.gradle.plugin:$embeddedKotlinVersion")
    implementation("io.github.gradle-nexus:publish-plugin:1.1.0")
}
