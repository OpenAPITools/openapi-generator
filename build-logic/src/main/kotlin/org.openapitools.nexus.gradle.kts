import io.github.gradlenexus.publishplugin.CloseNexusStagingRepository
import io.github.gradlenexus.publishplugin.ReleaseNexusStagingRepository

plugins {
    id("io.github.gradle-nexus.publish-plugin")
    id("org.openapitools.base")
}

nexusPublishing {
    repositories {
        sonatype {
            stagingProfileId.set("456297f829bbbe")
            username.set(providers.gradleProperty("ossrhUsername"))
            password.set(providers.gradleProperty("ossrhPassword"))
        }
    }
}

tasks.withType<CloseNexusStagingRepository>().configureEach {
    onlyIf { nexusPublishing.useStaging.get() }
}

tasks.withType<ReleaseNexusStagingRepository>().configureEach {
    onlyIf { nexusPublishing.useStaging.get() }
}
