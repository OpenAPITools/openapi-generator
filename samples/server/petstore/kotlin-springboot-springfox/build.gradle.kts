import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath("org.springframework.boot:spring-boot-gradle-plugin:2.6.7")
    }
}

group = "org.openapitools"
version = "1.0.0"

repositories {
    mavenCentral()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

plugins {
    val kotlinVersion = "1.6.21"
    id("org.jetbrains.kotlin.jvm") version kotlinVersion
    id("org.jetbrains.kotlin.plugin.jpa") version kotlinVersion
    id("org.jetbrains.kotlin.plugin.spring") version kotlinVersion
    id("org.springframework.boot") version "2.6.7"
    id("io.spring.dependency-management") version "1.0.11.RELEASE"
}

dependencies {
    compile("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    compile("org.jetbrains.kotlin:kotlin-reflect")
    compile("org.springframework.boot:spring-boot-starter-web")
    compile("io.springfox:springfox-swagger2:2.9.2")
    compile("org.webjars:swagger-ui:4.10.3")
    compile("org.webjars:webjars-locator-core")

    compile("com.google.code.findbugs:jsr305:3.0.2")
    compile("com.fasterxml.jackson.dataformat:jackson-dataformat-yaml")
    compile("com.fasterxml.jackson.dataformat:jackson-dataformat-xml")
    compile("com.fasterxml.jackson.datatype:jackson-datatype-jsr310")
    compile("com.fasterxml.jackson.module:jackson-module-kotlin")
    compile("jakarta.validation:jakarta.validation-api")
    compile("jakarta.annotation:jakarta.annotation-api:2.1.0")

    testCompile("org.jetbrains.kotlin:kotlin-test-junit5")
    testCompile("org.springframework.boot:spring-boot-starter-test") {
        exclude(module = "junit")
    }
}
