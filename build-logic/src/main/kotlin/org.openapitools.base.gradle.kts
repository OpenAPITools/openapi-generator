plugins {
    base
}

if (!hasProperty("openApiGeneratorVersion")) {
    throw GradleException("Property 'openApiGeneratorVersion' must be defined in the root gradle.properties file.")
}

val openApiGeneratorVersion: String by properties

group = "org.openapitools"
version = openApiGeneratorVersion

extra["isReleaseVersion"] = openApiGeneratorVersion.endsWith("SNAPSHOT")
