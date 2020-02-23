# sbt-openapi-generator

A Sbt plugin to support the OpenAPI generator project

# Usage

Add to your `project/plugins.sbt`:

```sbt
addSbtPlugin("org.openapitools" % "sbt-openapi-generator" % "4.3.0")
```

# Configuration

Configuration based on project module is recommended way to separate specifications by modules.

You must define one of the settings `inputSpec` or `configFile` to able run plugin to generate.

Settings will be picked up from `configFile` first if defined and then will be overwritten with module specified settings

With the next example module `generated` will be defined as:

```sbt
lazy val generated = project.in(file("generated"))
  .settings(
    inConfig(OpenApiCodegen) {
      Seq(
        inputSpec := "openapi.yaml",
        configFile := "config.yaml"
        // outputPath := (baseDirectory in ThisBuild).value + "/generated"
      )
    }
  )
```

There is a helpers to have boolean settings more readable. Instead of `Some(true)` you can do next:
```sbt
        validateSpec := SettingDisabled,
        generateModelTests := SettingEnabled,
```
# Execution 

To print all available languages use 
```shell script
sbt openApiGenerators
```

To run generation process
```shell script
sbt openApiGenerate
```
or per defined module
```shell script
sbt generated/openApiGenerate
```

# Settings


| Setting  | Type   | Description              | 
|----------|--------|--------------------------|
| language | `String` | Generated language      |
| inputSpec| `String` | The Open API 2.0/3.x specification location |
| outputDir| `String` | The output target directory into which code will be generated |
| configFile| `String` | Path to json configuration file |
| additionalProperties | `Map[String, String]` | Sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value. You can also have multiple occurrences of this option |
| systemProperties | `Map[String, String]` |Sets specified system properties |
| verbose | `Option[Boolean]` | The verbosity of generation |
| validateSpec | `Option[Boolean]` | Whether or not an input specification should be validated upon generation |
| generatorName | `String` | The name of the generator which will handle codegen. (see \"openApiGenerators\" task) |
| templateDir | `String` | The template directory holding a custom template |
| auth | `String` | Adds authorization headers when fetching the OpenAPI definitions remotely. Pass in a URL-encoded string of name:header with a comma separating multiple values |
| skipOverwrite | `Option[Boolean]` | Specifies if the existing files should be overwritten during the generation |
| packageName | `String` | Package for generated classes (where supported) | 
| apiPackage | `String` | Package for generated api classes | 
| modelPackage | `String` | Package for generated models | 
| modelNamePrefix | `String` | Prefix that will be prepended to all model names | 
| modelNameSuffix | `String` | Suffix that will be appended to all model names |
| instantiationTypes | `Map[String, String]` | Sets instantiation type mappings |
| typeMappings | `Map[String, String]` | Sets mappings between OpenAPI spec types and generated code types |
| serverVariables | `Map[String, String]` | Sets server variable for server URL template substitution, in the format of name=value,name=value. You can also have multiple occurrences of this option |
| languageSpecificPrimitives | `List[String]` | Specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double |
| importMappings | `Map[String, String]` | Specifies mappings between a given class and the import that should be used for that class |
| invokerPackage | `String` | Root package for generated code | 
| groupId | `String` | groupId in generated pom.xml/build.sbt | 
| id | `String` | artifactId in generated pom.xml/build.sbt. This also becomes part of the generated library's filename |
| library | `String` | library template (sub-template) | 
| gitHost | `String` |Git host, e.g. gitlab.com | 
| gitUserId | `String` | Git user ID, e.g. openapitools | 
| gitRepoId | `String` | Git repo ID, e.g. openapi-generator | 
| releaseNote | `String` | Release note, default to 'Minor update' | 
| httpUserAgent | `String` | HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}}/{language}' |
| reservedWordsMappings | `Map[String, String]` | ]("Specifies how a reserved name should be escaped to |
| ignoreFileOverride | `String` | Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation. |
| removeOperationIdPrefix | `Option[Boolean]` | Remove prefix of operationId, e.g. config_getId => getId |
| apiFilesConstrainedTo | `List[String]` | Defines which API-related files should be generated. This allows you to create a subset of generated files (or none at all) |
| modelFilesConstrainedTo | `List[String]` | Defines which model-related files should be generated. This allows you to create a subset of generated files (or none at all) |
| supportingFilesConstrainedTo | `List[String]` | Defines which supporting files should be generated. This allows you to create a subset of generated files (or none at all | 
| generateModelTests | `Option[Boolean]` | Specifies that model tests are to be generated | 
| generateModelDocumentation | `Option[Boolean]` | Defines whether or not model-related _documentation_ files should be generated |
| generateApiTests | `Option[Boolean]` | Specifies that api tests are to be generated |
| generateApiDocumentation | `Option[Boolean]` | Defines whether or not api-related _documentation_ files should be generated | 
| withXml | `Option[Boolean]` | A special-case setting which configures some generators with XML support. In some cases, this forces json OR xml, so the default here is false | 
| logToStderr | `Option[Boolean]` | To write all log messages (not just errors) to STDOUT | 
| enablePostProcessFile | `Option[Boolean]` | Enable post-processing file using environment variables | \
| skipValidateSpec | `Option[Boolean]` | To skip spec validation. When true, we will skip the default behavior of validating a spec before generation |
| generateAliasAsModel | `Option[Boolean]` | Generate model implementation for aliases to map and array schemas |

# Examples

Please see [an sbt-test configuration](src/sbt-test) for using the plugin. 
Do not run those examples directly, please copy them to separate place first.

# Contribution and Tests

Write plugin integration tests under [src/sbt-test](src/sbt-test)

Execute next to run tests:

```shell script
sbt scripted
```

More information how to write and execute tests [is here](https://www.scala-sbt.org/1.x/docs/Testing-sbt-plugins.html)