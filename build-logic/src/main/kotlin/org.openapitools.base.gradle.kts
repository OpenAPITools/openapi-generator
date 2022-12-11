plugins {
    base
}

val openApiGeneratorVersion: String by properties

group = "org.openapitools"
version = openApiGeneratorVersion

val isReleaseVersion by extra(openApiGeneratorVersion.endsWith("SNAPSHOT"))
