# Local Spec Sample

This example assumes you have Gradle 4.7+ installed. No gradle wrapper is provided in samples.

First, publish the openapi-generator-gradle-plugin locally via `sh gradlew build publishToMavenLocal` in the module directory.

Then, run the following tasks in this example directory.

```bash
gradle openApiGenerate
gradle openApiMeta
gradle openApiValidate
gradle buildGoSdk
gradle generateGoWithInvalidSpec
```
