plugins {
    kotlin("multiplatform") version "1.4.21"
    kotlin("plugin.serialization") version "1.4.21"
    id("com.android.application")
    id("kotlin-android-extensions")
}

group = "org.openapitools"
version = "1.0.0"

val ktorVersion = "1.4.1"

repositories {
    jcenter()
}

kotlin {
    explicitApi()

    /*
     * To find out how to configure the targets, please follow the link:
     * https://kotlinlang.org/docs/reference/building-mpp-with-gradle.html#setting-up-targets
     */
    jvm {
        val main by compilations.getting {
            kotlinOptions {
                // Setup the Kotlin compiler options for the 'main' compilation:
                jvmTarget = "1.8"
            }
        }
    }
    android()
    js(BOTH) {
        browser()
    }

    sourceSets {
        val commonMain by getting {
            kotlin.srcDir("src/common/main")

            dependencies {
                implementation(kotlin("stdlib-common"))
                api("io.ktor:ktor-client-core:$ktorVersion")
                api("io.ktor:ktor-client-json:$ktorVersion")
                api("io.ktor:ktor-client-serialization:$ktorVersion")
            }
        }

        val commonTest by getting {
            kotlin.srcDir("src/common/test")

            dependencies {
                implementation(kotlin("test-common"))
                implementation(kotlin("test-annotations-common"))
                implementation("io.ktor:ktor-client-mock:$ktorVersion")
            }
        }

        val jvmMain by getting {
            kotlin.srcDir("src/jvm/main")

            dependencies {
                implementation(kotlin("stdlib-jdk8"))
                api("io.ktor:ktor-client-core-jvm:$ktorVersion")
            }
        }

        val jvmTest by getting {
            kotlin.srcDir("src/jvm/test")

            dependencies {
                implementation(kotlin("test"))
                implementation(kotlin("test-junit5"))
                implementation("org.junit.jupiter:junit-jupiter:5.6.2")
                implementation("io.ktor:ktor-client-apache:$ktorVersion")
                implementation("io.ktor:ktor-client-mock-jvm:$ktorVersion")
            }
        }

        val androidMain by getting {
            kotlin.srcDir("src/android/main")

            dependencies {
                implementation(kotlin("stdlib-jdk8"))
                api("io.ktor:ktor-client-okhttp:$ktorVersion")
            }
        }

        val androidTest by getting {
            kotlin.srcDir("src/android/test")

            dependencies {
                implementation("io.ktor:ktor-client-mock-jvm:$ktorVersion")
            }
        }

        val jsMain by getting {
            kotlin.srcDir("src/js/main")

            dependencies {
                implementation(kotlin("stdlib-js"))
                api("io.ktor:ktor-client-js:$ktorVersion")
            }
        }

        val jsTest by getting {
            kotlin.srcDir("src/js/test")

            dependencies {
                implementation("io.ktor:ktor-client-mock-js:$ktorVersion")
            }
        }

    }
}

android {
    compileSdkVersion(29)
    defaultConfig {
        minSdkVersion(24)
        targetSdkVersion(29)
        versionCode = 1
        versionName = "1.0.0"
    }
}

tasks {
    named<Test>("jvmTest") {
        useJUnitPlatform()
    }
}
