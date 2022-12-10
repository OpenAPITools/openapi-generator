import org.gradle.api.tasks.testing.logging.TestLogEvent.FAILED
import org.gradle.api.tasks.testing.logging.TestLogEvent.SKIPPED

plugins {
    java
    `maven-publish`
    id("org.openapitools.base")
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

tasks.withType<JavaCompile>().configureEach {
    options.encoding = "UTF-8"
}

tasks.withType<Javadoc>().configureEach {
    if (java.toolchain.languageVersion.map { it.canCompileOrRun(9) }.getOrElse(false)) {
        (options as StandardJavadocDocletOptions).addBooleanOption("html5", true)
    }
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
