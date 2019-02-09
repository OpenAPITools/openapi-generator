# Local Spec Sample

This example assumes you have Gradle 4.7+ installed. No gradle wrapper is provided in samples.

First, publish the openapi-generator-gradle-plugin locally via `./gradlew assemble install` in the module directory.

Then, run the following tasks in this example directory.

```bash
gradle openApiGenerate
gradle openApiMeta
gradle openApiValidate
gradle buildGoSdk
gradle generateGoWithInvalidSpec
```

The samples can be tested against other versions of the plugin using the `openApiGeneratorVersion` property. For example:

```bash
gradle -PopenApiGeneratorVersion=3.3.4 openApiValidate
```
