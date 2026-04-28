import org.jetbrains.kotlin.gradle.dsl.JvmTarget

group = "org.openapitools"
version = "1.0.0"
java.sourceCompatibility = JavaVersion.VERSION_17

repositories {
    mavenCentral()
}

kotlin {
    compilerOptions {
        jvmTarget.set(JvmTarget.JVM_17)
    }
}

plugins {
    val kotlinVersion = "2.2.0"
    id("org.jetbrains.kotlin.jvm") version kotlinVersion
    id("org.jetbrains.kotlin.plugin.jpa") version kotlinVersion
    id("org.jetbrains.kotlin.plugin.spring") version kotlinVersion
    id("org.springframework.boot") version "4.0.1"
    id("io.spring.dependency-management") version "1.1.7"
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
        implementation("org.springframework.boot:spring-boot-starter-webmvc")

    implementation("com.google.code.findbugs:jsr305:3.0.2")
    val jackson3Version = "3.1.0"
    implementation("tools.jackson.dataformat:jackson-dataformat-yaml:$jackson3Version")
    implementation("tools.jackson.dataformat:jackson-dataformat-xml:$jackson3Version")
    implementation("tools.jackson.module:jackson-module-kotlin:$jackson3Version")
    implementation("org.springframework.data:spring-data-commons")
    implementation("org.springframework.boot:spring-boot-starter-validation")
    implementation("jakarta.validation:jakarta.validation-api")
    implementation("jakarta.annotation:jakarta.annotation-api:3.0.0")

    testImplementation("org.jetbrains.kotlin:kotlin-test-junit5")
    testImplementation("org.springframework.boot:spring-boot-starter-test") {
        exclude(module = "junit")
    }
}