
val kotlin_version: String by project
val logback_version: String by project

group = "org.openapitools"
version = "1.0.0"

plugins {
    kotlin("jvm") version "2.0.20"
    application
    kotlin("plugin.serialization") version "2.0.20"
}

application {
    mainClass.set("io.ktor.server.netty.EngineMain")

    val isDevelopment: Boolean = project.ext.has("development")
    applicationDefaultJvmArgs = listOf("-Dio.ktor.development=$isDevelopment")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(platform("io.ktor:ktor-bom:3.0.2"))
    implementation("ch.qos.logback:logback-classic:$logback_version")
    implementation("com.typesafe:config:1.4.1")
    implementation("io.ktor:ktor-server-auth")
    implementation("io.ktor:ktor-client-apache")
    implementation("io.ktor:ktor-server-auto-head-response")
    implementation("io.ktor:ktor-server-default-headers")
    implementation("io.ktor:ktor-server-content-negotiation")
    implementation("io.ktor:ktor-serialization-kotlinx-json")
    implementation("io.ktor:ktor-server-resources")
    implementation("io.ktor:ktor-server-hsts")
    implementation("io.ktor:ktor-server-compression")
    implementation("io.dropwizard.metrics:metrics-core:4.1.18")
    implementation("io.ktor:ktor-server-metrics")
    implementation("io.ktor:ktor-server-netty")

    testImplementation("org.jetbrains.kotlin:kotlin-test-junit:$kotlin_version")
}
