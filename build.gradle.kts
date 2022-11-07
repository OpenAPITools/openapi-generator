import io.github.gradlenexus.publishplugin.CloseNexusStagingRepository
import io.github.gradlenexus.publishplugin.ReleaseNexusStagingRepository

plugins {
    id("org.openapitools.base")
    id("io.github.gradle-nexus.publish-plugin") version "1.1.0"
}

nexusPublishing {
    repositories {
        sonatype {
            username.set(providers.gradleProperty("ossrhUsername"))
            password.set(providers.gradleProperty("ossrhPassword"))

            // To retrieve: ./gradlew -Psigning.keyId="$SIGNING_KEY" -Psigning.password="$SIGNING_PASSPHRASE" -Psigning.secretKeyRingFile="$SIGNING_SECRET" getStagingProfile
            stagingProfileId.set("456297f829bbbe")
        }
    }
}

tasks.withType<CloseNexusStagingRepository>().configureEach {
    onlyIf { nexusPublishing.useStaging.get() }
}

tasks.withType<ReleaseNexusStagingRepository>().configureEach {
    onlyIf { nexusPublishing.useStaging.get() }
}