## Migration guide: from Swagger Codegen to OpenAPI Generator

OpenAPI Generator is a fork of `swagger-codegen` between version `2.3.1` and `2.4.0`.
This community-driven version called "OpenAPI Generator" provides similar functionalities and can be used as drop-in replacement.
This guide explains the major differences in order to help you with the migration.

### New docker images

The docker images are available on DockerHub: https://hub.docker.com/u/openapitools/

**CLI for OpenAPI Generator**

Image to run OpenAPI Generator in the command line (see [OpenAPI Generator CLI Docker Image](../README.md#openapi-generator-cli-docker-image))

Old: `swaggerapi/swagger-codegen-cli`

New: `openapitools/openapi-generator-cli`

**OpenAPI Generator as web service**

Image to run OpenAPI Generator as a web service (see [OpenAPI Generator Online Docker Image](../README.md#openapi-generator-online-docker-image))

Old: `swaggerapi/swagger-generator`

New: `openapitools/openapi-generator-online`


### New maven coordinates

You can find our released artefact on maven central:

**Core:**

Old:

```
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen</artifactId>
</dependency>
```

New:

```
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator</artifactId>
</dependency>
```

**Cli:**

```
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-cli</artifactId>
</dependency>
```

New:

```
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-cli</artifactId>
</dependency>
```

**Maven plugin:**

```
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
</dependency>
```

New:

```
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
</dependency>
```

### Changes in Maven Plugin

OpenAPI Generator 3.0.0 has introduced `<generatorName>` and deprecated `<language>`, because this refers to generator names which embed more than just "language".

If both options are present, you'll be presented with an error. If only `<language>` is provided, you'll be presented instructions for updating to the new config.


### New generators names

When you run OpenAPI Generator, you need to select a target generator (`-g` option in the cli).
All languages of `swagger-codegen` have been migrated to `openapi-generator`, but some names were changed, in order to be more consistent.

| name in `swagger-codegen` | name in `openapi-generator`  |
|--|--|
| `akka-scala` | `scala-akka` |
| `scala` | `scala-httpclient` |
| `jaxrs` | `jaxrs-jersey` |
| `qt5cpp` | `cpp-qt5` |
| `cpprest` | `cpp-restsdk` |
| `tizen` | `cpp-tizen` |
| `sinatra` | `ruby-sinatra` |
| `swift` | `swift2-deprecated` |
| `lumen` | `php-lumen` |
| `slim` | `php-slim` |
| `ze-ph` | `php-ze-ph` |
| `nancyfx` | `csharp-nancyfx` |

We provide a temporary mapping in code for these old values. You'll receive a warning with instructions to migrate to the new names.

### New parameters name

Some parameters were renamed.
Often you need to replace "Swagger", with "OpenAPI".
Some examples:

| name in `swagger-codegen` | name in `openapi-generator`  |
|--|--|
| `debugSwagger` | `debugOpenAPI` |
| `GenerateSwaggerMetadata` | `GenerateOpenAPIMetadata` |
| `swagger.codegen.undertow.apipackage` | `openapi.codegen.undertow.apipackage` |
| `swagger.codegen.undertow.modelpackage` | `openapi.codegen.undertow.modelpackage` |


### Renamed Mustache Template Variables 

The template variable `{{datatype}}` was renamed to `{{dataType}}` for consistency reason.
Corresponding java code: `CodegenProperty.datatype` is renamed to `CodegenProperty.dataType`.

(If you're **not** using customized templates with the `-t` option, you can ignore the mustache variable renaming above.)

### Ignore file

`.swagger-codegen-ignore` is replaced by `.openapi-generator-ignore`.
The syntax inside the file stays the same.

You don't need to rename the file manually, OpenAPI Generator will do it when your run it against an existing an existing output directory.
(When there is no `.openapi-generator-ignore` in a folder, if a `.swagger-codegen-ignore` file is present it will be considered and renamed to `.openapi-generator-ignore`).


### Metadata folder

The metatata folder (to store the `VERSION` file for example) is now called `.openapi-generator/` instead of `.swagger-codegen/`.



### New default values for the generated code

If you use a generator without specifying each parameter, you might see some differences in the generated code.
As example the default package name used in the generated code has changed. 
You need to have a look at the specific value, depending of your target language, but often `Swagger` îs replaced by `OpenAPITools` and `io.swagger` is replaced by `org.openapitools`.
Concretely if you did not specify anything when you are generating java code, a file `org/openapitools/api/PetApi.java`  might be generated instead of `io/swagger/api/PetApi.java`.

If this is a problem for you, you need to explicitly set the the parameter value in order to match with the `swagger-codgen` default value (`apiPackage` == `io.swagger` in the previous example with the java generator).


### New fully qualified name for the classes 

If you have extended some generators in your project, and you are looking for a specific class, replace the `io.swagger.codegen` package (old name) with `org.openapitools.codegen` package (new name).

Example: `org.openapitools.codegen.DefaultGenerator`


[Back to OpenAPI-Generator's README page](../README.md)
