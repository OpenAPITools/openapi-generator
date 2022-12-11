import org.gradle.api.tasks.testing.logging.TestLogEvent.FAILED
import org.gradle.api.tasks.testing.logging.TestLogEvent.SKIPPED

plugins {
    id("java")
    id("maven-publish")
    id("org.openapitools.base")
    id("org.openapitools.publishing")
}

java {
    withSourcesJar()
    withJavadocJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(8))
    }
}

repositories {
    mavenCentral()
    maven {
        name = "Sonatype Snapshots"
        url = uri("https://oss.sonatype.org/content/repositories/snapshots/")
    }
}

dependencies {
    testImplementation("org.testng:testng:7.5")
}

tasks.withType<Test>().configureEach {
    useTestNG()

    testLogging {
        showStandardStreams = false
        events = setOf(SKIPPED, FAILED)
    }

    // Disabled to ensure test execution is equivalent to Maven
    // See: https://docs.gradle.org/current/userguide/java_testing.html#sec:test_detection
    isScanForTestClasses = false
}
