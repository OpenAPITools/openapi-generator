# Local Spec Sample

This example assumes you have Gradle 5.6.4+ installed. No gradle wrapper is provided in samples.

First, publish the openapi-generator-gradle-plugin locally via `./gradlew assemble publishToMavenLocal` in the module directory.

Then, run the following tasks in this example directory.

```bash
gradle openApiGenerate              # expected outcome: BUILD SCCESSFUL
gradle openApiMeta                  # expected outcome: BUILD SCCESSFUL
gradle openApiValidate              # expected outcome: BUILD FAILED 
gradle buildGoSdk                   # expected outcome: BUILD SCCESSFUL
gradle buildDotnetSdk               # expected outcome: BUILD SCCESSFUL
gradle generateGoWithInvalidSpec    # expected outcome: BUILD FAILED 
```

The samples can be tested against other versions of the plugin using the `openApiGeneratorVersion` property. For example:

```bash
gradle -PopenApiGeneratorVersion=5.1.0 openApiValidate
```
