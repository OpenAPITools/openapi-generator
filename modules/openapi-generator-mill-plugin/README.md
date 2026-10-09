openapi-generator-mill-plugin
============================

A [Mill](https://mill-build.org) library to support the OpenAPI generator project. 

Usage
============================

1. Add the plugin to your `build.mill` as `mvnDeps` in the header section
2. import `org.openapitools.generator.mill.OpenApiModule`
3. add `OpenApiModule` to the module definition
4. configure 1-n `OpenApiConfig` as sub-modules

```scala
//| mill-version: 1.0.6
//| mvnDeps:
//|   - org.openapitools:openapi-generator-mill-plugin:7.19.0 # 1.

import mill.*

import org.openapitools.generator.mill.OpenApiModule // 2.

object `package` extends JavaModule with MavenModule with OpenApiModule { // 3.

  override def mvnDeps = Seq(
    mvn"jakarta.platform:jakarta.jakartaee-api:11.0.0",
    mvn"com.fasterxml.jackson.core:jackson-databind:2.20.0",
  )

  object openapi extends OpenApiConfig { // 4.
    def inputSpec: T[PathRef] = Task.Source(BuildCtx.workspaceRoot / "api" / "petstore.yaml")
    def apiPackage: T[String] = "com.acme.foo.boundary.web.api"
    def modelPackage: T[String] = "com.acme.foo.boundary.web.model"
    def generatorName: T[String] = "jaxrs-spec"
    def sourceFolder: T[String] = "src/main/java"

    def additionalProperties: T[Map[String, String]] = Map(
      "dateLibrary" -> "java8",
      "useJakartaEe" -> "true",
      "useSwaggerAnnotations" -> "false",
      "interfaceOnly" -> "true",
      "useTags" -> "true",
    )
  }
}
```

Followed by:

```bash
mill openapi.generate
```

Usually you want to include the generation to the `compile` phase and have the sources in your source-tree which can
be achieved by adding the generation task to the `generatedSources`.

```scala 
override def generatedSources: T[Seq[PathRef]] = Seq(
  PathRef(Task.dest),
  openapi.generate(),
)
```

Followed by:

```bash
mill __.compile
```

This works because `generatedSources` expects a list of `PathRef`s which constitute all folders that contain additional
(generated) sources and the `generate` task from each `OpenApiConfig` returns a `PathRef` to the folder where it put the sources.


### General Configuration parameters for OpenApiConfig

| Option                             | Description                                                                                                                                                                                                                                                                                                                                                                                      |
|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `verbose`                          | verbose mode (`false` by default)                                                                                                                                                                                                                                                                                                                                                                |
| `inputSpec`                        | OpenAPI Spec file path                                                                                                                                                                                                                                                                                                                                                                           |
| `inputSpecRootDirectory`           | Local root folder with spec file(s)                                                                                                                                                                                                                                                                                                                                                              |
| `mergedFileName`                   | Name of the file that will contain all merged specs                                                                                                                                                                                                                                                                                                                                              |
| `generatorName`                    | target generator name                                                                                                                                                                                                                                                                                                                                                                            |
| `cleanupOutput`                    | Defines whether the output directory should be cleaned up before generating the output (`false` by default).                                                                                                                                                                                                                                                                                     |
| `cleanup`                          | Defines a task which contains an `Option[Path => Unit]` which is called after the generation completed. Useful for instance to delete generated Types which are already replaced by import/type-mappings.                                                                                                                                                                                        |
| `gitSettings`                      | sets Git information of the project (with `host`, `userId` and `repoId`)                                                                                                                                                                                                                                                                                                                         |
| `templateDirectory`                | directory with mustache templates                                                                                                                                                                                                                                                                                                                                                                |
| `engine`                           | The name of templating engine to use, "mustache" (default) or "handlebars" (beta)                                                                                                                                                                                                                                                                                                                |
| `auth`                             | adds authorization headers when fetching the OpenAPI definitions remotely. Pass in a URL-encoded string of `name:header` with a comma separating multiple values                                                                                                                                                                                                                                 |
| `skipOverwrite`                    | Specifies if the existing files should be overwritten during the generation. (`false` by default)                                                                                                                                                                                                                                                                                                |
| `apiPackage`                       | the package to use for generated api objects/classes                                                                                                                                                                                                                                                                                                                                             |
| `modelPackage`                     | the package to use for generated model objects/classes                                                                                                                                                                                                                                                                                                                                           |
| `invokerPackage`                   | the package to use for the generated invoker objects                                                                                                                                                                                                                                                                                                                                             |
| `packageName`                      | the default package name to use for the generated objects                                                                                                                                                                                                                                                                                                                                        |
| `artifactSettings`                 | sets project information in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators                                                                                                                                                                                                                                                      |
| `library`                          | library template (sub-template)                                                                                                                                                                                                                                                                                                                                                                  |
| `modelNamePrefix`                  | Sets the prefix for model classes and enums                                                                                                                                                                                                                                                                                                                                                      |
| `modelNameSuffix`                  | Sets the suffix for model classes and enums                                                                                                                                                                                                                                                                                                                                                      |
| `apiNameSuffix`                    | Sets the suffix for api classes                                                                                                                                                                                                                                                                                                                                                                  |
| `ignoreFileOverride`               | specifies the full path to a `.openapi-generator-ignore` used for pattern based overrides of generated outputs                                                                                                                                                                                                                                                                                   |
| `httpUserAgent`                    | Sets custom User-Agent header value                                                                                                                                                                                                                                                                                                                                                              |
| `removeOperationIdPrefix`          | remove operationId prefix (e.g. user_getName => getName)                                                                                                                                                                                                                                                                                                                                         |
| `skipOperationExample`             | skip examples defined in the operation                                                                                                                                                                                                                                                                                                                                                           |
| `logToStderr`                      | write all log messages (not just errors) to STDERR                                                                                                                                                                                                                                                                                                                                               |
| `enablePostProcessFile`            | post-processing hook                                                                                                                                                                                                                                                                                                                                                                             |
| `skipValidateSpec`                 | Whether or not to skip validating the input spec prior to generation. By default, invalid specifications will result in an error.                                                                                                                                                                                                                                                                |
| `strictSpec`                       | Whether or not to treat an input document strictly against the spec. 'MUST' and 'SHALL' wording in OpenAPI spec is strictly adhered to. e.g. when false, no fixes will be applied to documents which pass validation but don't follow the spec.                                                                                                                                                  |
| `openapiNormalizer`                | specifies the rules to be enabled in OpenAPI normalizer in the form of RULE_1=true,RULE_2=original.                                                                                                                                                                                                                                                                                              |
| `generateAliasAsModel`             | generate alias (array, map) as model                                                                                                                                                                                                                                                                                                                                                             |
| `configOptions`                    | N/A                                                                                                                                                                                                                                                                                                                                                                                              | a **map** of generator-specific parameters. To show a full list of generator-specified parameters (options), please use `configHelp` (explained below)
| `importMappings`                   | specifies mappings between a given class and the import that should be used for that class in the format of type=import,type=import. You can also have multiple occurrences of this option                                                                                                                                                                                                       |
| `typeMappings`                     | sets mappings between OpenAPI spec types and generated code types in the format of OpenAPIType=generatedType,OpenAPIType=generatedType. For example: `array=List,map=Map,string=String`. You can also have multiple occurrences of this option. To map a specified format, use type+format, e.g. string+password=EncryptedString will map `type: string, format: password` to `EncryptedString`. |
| `schemaMappings`                   | specifies mappings between the schema and the new name in the format of schema_a=Cat,schema_b=Bird. https://openapi-generator.tech/docs/customization/#schema-mapping                                                                                                                                                                                                                            |
| `nameMappings`                     | specifies mappings between the property name and the new name in the format of property_a=firstProperty,property_b=secondProperty. https://openapi-generator.tech/docs/customization/#name-mapping                                                                                                                                                                                               |
| `modelNameMappings`                | specifies mappings between the model name and the new name in the format of model_a=FirstModel,model_b=SecondModel. https://openapi-generator.tech/docs/customization/#name-mapping                                                                                                                                                                                                              |
| `parameterNameMappings`            | specifies mappings between the parameter name and the new name in the format of param_a=first_parameter,param_b=second_parameter. https://openapi-generator.tech/docs/customization/#name-mapping                                                                                                                                                                                                |
| `inlineSchemaNameMappings`         | specifies mappings between the inline schema name and the new name in the format of inline_object_2=Cat,inline_object_5=Bird.                                                                                                                                                                                                                                                                    |
| `inlineSchemaOptions`              | specifies the options used when naming inline schema in inline model resolver                                                                                                                                                                                                                                                                                                                    |
| `languageSpecificPrimitives`       | specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: `String,boolean,Boolean,Double`. You can also have multiple occurrences of this option                                                                                                                                                                                             |
| `additionalProperties`             | sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value. You can also have multiple occurrences of this option                                                                                                                                                                                                                        |
| `reservedWordsMappings`            | specifies how a reserved name should be escaped to. Otherwise, the default `_<name>` is used. For example `id=identifier`. You can also have multiple occurrences of this option                                                                                                                                                                                                                 |
| `generateApis`                     | generate the apis (`true` by default). To generate only a subset, define via `apiFilesConstrainedTo`.                                                                                                                                                                                                                                                                                            |
| `apiFilesConstrainedTo`            | A comma separated list of apis to generate.  All apis is the default.                                                                                                                                                                                                                                                                                                                            |
| `generateModels`                   | generate the models (`true` by default). To generate only a subset, define via `modelFilesConstrainedTo`.                                                                                                                                                                                                                                                                                        |
| `modelFilesConstrainedTo`          | A comma separated list of models to generate.  All models is the default.                                                                                                                                                                                                                                                                                                                        |
| `generateRecursiveDependentModels` | Enables dependent Models to be generated when `modelFilesConstrainedTo` is used. Default depends on `modelFilesConstrainedTo` (true when nonEmpty)                                                                                                                                                                                                                                               |
| `generateSupportingFiles`          | generate the supporting files (`true` by default). To generate only a subset, define via `supportingFilesConstrainedTo`.                                                                                                                                                                                                                                                                         |
| `supportingFilesConstrainedTo`     | A list of supporting files to generate.  When not defined, all files will be generated.                                                                                                                                                                                                                                                                                                          |
| `generateModelTests`               | generate the model tests (currently disabled)                                                                                                                                                                                                                                                                                                                                                    |
| `generateModelDocumentation`       | generate the model documentation (`true` by default)                                                                                                                                                                                                                                                                                                                                             |
| `generateApiTests`                 | generate the api tests (currently disabled)                                                                                                                                                                                                                                                                                                                                                      |
| `generateApiDocumentation`         | generate the api documentation (`true` by default)                                                                                                                                                                                                                                                                                                                                               |
| `dryRun`                           | Defines whether the generator should run in dry-run mode. In dry-run mode no files are written and a summary about file states is output ( `false` by default).                                                                                                                                                                                                                                  |

### Type and import mappings

To override the mappings between OpenAPI spec types and the types used in the generated code, set `typeMappings`.

```scala
def typeMappings: T[Map[String, String]] = Map(
  "time" -> "LocalTime"
)
```

For types that are not already included in the generator configuration, you may need to add a corresponding `importMapping` too.

```scala
def typeMappings: T[Map[String, String]] = Map(
  "binary" -> "StreamingOutput",
  "file" -> "StreamingOutput"
)
def importMappings: T[Map[String, String]] = Map(
  "StreamingOutput" -> "javax.ws.rs.core.StreamingOutput",
)
```

### Validate Command

You can validate any OpenAPI spec file by calling `validateOpenapiSpec` on the `OpenApiModule`.

```bash
mill validateOpenapiSpec $(pwd)/api/petstore-v3.0-invalid.yaml
```

This command has two additional parameters:
* `--failOnWarnings true` enable failing the check already on warnings
* `--recommend false`  

You can also validate your `OpenApiConfig` object by calling `validate` on it.

```bash
mill yourConfigObject.validate
```

