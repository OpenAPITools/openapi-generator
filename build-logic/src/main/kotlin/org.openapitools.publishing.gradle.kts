plugins {
    id("maven-publish")
    id("org.openapitools.base")
    id("signing")
}

tasks.withType<AbstractPublishToMaven>().configureEach {
    dependsOn(LifecycleBasePlugin.CHECK_TASK_NAME)
}

mavenPom {
    name.set("OpenAPI-Generator Contributors")
    description.set(project.description)
    url.set("https://openapi-generator.tech")

    organization {
        name.set("org.openapitools")
        url.set("https://github.com/OpenAPITools")
    }

    licenses {
        license {
            name.set("The Apache Software License, Version 2.0")
            url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
            distribution.set("repo")
        }
    }

    developers {
        developer {
            id.set("openapitools")
            name.set("OpenAPI-Generator Contributors")
            email.set("team@openapitools.org")
        }
    }

    scm {
        url.set("https://github.com/OpenAPITools/openapi-generator")
        connection.set("scm:git:git://github.com/OpenAPITools/openapi-generator.git")
        developerConnection.set("scm:git:ssh://git@github.com:OpenAPITools/openapi-generator.git")
    }

    issueManagement {
        system.set("GitHub")
        url.set("https://github.com/OpenAPITools/openapi-generator/issues")
    }
}

signing {
    val isReleaseVersion: Boolean by extra
    setRequired { isReleaseVersion && containsSonatypePublishTask() }
    sign(publishing.publications)
}

fun Project.mavenPom(action: Action<MavenPom>) {
    // afterEvaluate is necessary because java-gradle-plugin
    // creates its publications in an afterEvaluate callback
    afterEvaluate {
        val publishing = project.extensions.getByType<PublishingExtension>()
        publishing.publications.withType<MavenPublication>().configureEach { pom(action) }
    }
}

fun Project.containsSonatypePublishTask(): Boolean {
    return gradle.taskGraph.allTasks.any { task ->
        task.path.startsWith(project.path) && task.name.endsWith("ToSonatypeRepository")
    }
}
