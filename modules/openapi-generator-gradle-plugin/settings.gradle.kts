pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}

dependencyResolutionManagement {
    repositories {
        mavenCentral()
        maven { setUrl("https://oss.sonatype.org/content/repositories/releases/") }
        maven { setUrl("https://oss.sonatype.org/content/repositories/snapshots/") }
    }
}

rootProject.name = "openapi-generator-gradle-plugin"
