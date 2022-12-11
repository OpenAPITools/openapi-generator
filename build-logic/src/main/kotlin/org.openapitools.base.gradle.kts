plugins {
    id("base")
}

val openApiGeneratorVersion: String by properties
val isReleaseVersion by extra(openApiGeneratorVersion.endsWith("SNAPSHOT"))

group = "org.openapitools"
version = openApiGeneratorVersion
