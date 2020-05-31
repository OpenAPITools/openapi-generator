---
id: globals
title: Global Properties
---

## Available Global Properties

| Property | Description | Acceptable value |
| -------- | ------------| ---------------- |
| debugOpenAPI | Dumps JSON formatted and fully parsed OpenAPI document during generation | none |
| debugModels | Dumps JSON formatted template-bound model information during generation | none |
| debugOperations | Dumps JSON formatted template-bound operation information during generation | none |
| debugSupportingFiles | Dumps JSON formatted Supporting File information during generation | none |
| verbose | Defines the verbosity | `true` or `false` |
| generateAliasAsModel | Defines whether primitive types defined at the model/schema level will be wrapped in a model | `true` or `false` |
| org.openapitools.codegen.utils.oncelogger.enabled | Enable/disable the "OnceLogger" which reduces noise for select repeated logs | `true` or `false` |
| supportingFiles | Allows the user to define which supporting files will be generated. Prefer using the more robust `.openapi-generator-ignore`. | no value, or a comma-separated string of file names |
| models | Allows the user to define which models will be generated. Prefer using the more robust `.openapi-generator-ignore`. | no value, or a comma-separated string of model names |
| apis | Allows the user to define which apis will be generated. Prefer using the more robust `.openapi-generator-ignore`. | no value, or a comma-separated string of api names |
| apiDocs | Allows the user to define if api docs will be generated. Prefer using the more robust `.openapi-generator-ignore`. | `true` or `false` |
| modelDocs | Allows the user to define if model docs will be generated. Prefer using the more robust `.openapi-generator-ignore`. | `true` or `false` |
| apiTests | Allows the user to define if api tests will be generated. Prefer using the more robust `.openapi-generator-ignore`. | `true` or `false` |
| modelTests | Allows the user to define if model tests will be generated. Prefer using the more robust `.openapi-generator-ignore`. | `true` or `false` |
| withXml | Allows the user to control support of XML generated constructs, where supported | none |


## Note on Global Property declaration

There are _two ways_ to provide selective generation properties or "global properties". First, these can be passed as Java System Properties. Second, these can be passed via the global property tooling option (`--global-property` in CLI and `globalProperty` in Maven and Gradle configurations). This differentiation is new in version 5.0 with the removal of the `-D` CLI option and the renaming of `systemProperties`. If you're upgrading to OpenAPI Generator 5.0+

While the examples seen in [Customization](./customization.md) use the Java System Property syntax, keep in mind that the following are equivalent:

```sh
java -Dmodels {jar} generate {opts}
```

and

```sh
java {jar} generate {opts} --global-property=models
```

Why the two differing ways to provide the same properties? We previously accepted a `-D` tooling option which resembled Java System Property declaration. In older versions of OpenAPI Generator, the option modified the SystemProperties collection directly and was truly a "system property". This option changed during the 4.x release in an effort to make OpenAPI Generator thread-safe and isolate its configuration via thread locals. We no longer mutate System Properties. In the 4.x release and earlier, specifying the tooling `-D` option with system properties intended for other tools like swagger-parser rather than passing them as true Java System Properties would lead to unexpected behavior for the user; if our tool set the system property _after_ invoking certain code, it would seem to the user like Java System Properties weren't working! 