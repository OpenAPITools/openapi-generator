---
id: swagger-codegen-migration
title: Migrating from Swagger Codegen
---

OpenAPI Generator is a fork of `swagger-codegen` between version `2.3.1` and `2.4.0`. For the reasons behind the fork, please refer to the [Q&A](https://github.com/OpenAPITools/openapi-generator/blob/master/docs/qna.md).
This community-driven version called "OpenAPI Generator" provides similar functionalities and can be used as drop-in replacement.
This guide explains the major differences in order to help you with the migration.


**Table of contents**

  - [New docker images](#new-docker-images)
  - [New maven coordinates](#new-maven-coordinates)
  - [Changes in Maven Plugin](#changes-in-maven-plugin)
  - [New generators names](#new-generators-names)
  - [New parameters name](#new-parameters-name)
  - [Renamed Mustache Template Variables](#renamed-mustache-template-variables)
  - [Ignore file](#ignore-file)
  - [metadata-folder](#metadata-folder)
  - [New default values for the generated code](#new-default-values-for-the-generated-code)
  - [New fully qualified name for the classes](#new-fully-qualified-name-for-the-classes)
  - [Body parameter name](#body-parameter-name)
  - [Default basePath](#default-basepath)
  - [Nullable](#nullable)

## New docker images

The docker images are available on DockerHub: https://hub.docker.com/u/openapitools/

**CLI for OpenAPI Generator**

Image to run OpenAPI Generator in the command line (see [OpenAPI Generator CLI Docker Image](https://github.com/OpenAPITools/openapi-generator/blob/master/README.md#openapi-generator-cli-docker-image))

Old: `swaggerapi/swagger-codegen-cli`

New: `openapitools/openapi-generator-cli`

**OpenAPI Generator as web service**

Image to run OpenAPI Generator as a web service (see [OpenAPI Generator Online Docker Image](https://github.com/OpenAPITools/openapi-generator/blob/master/README.md#openapi-generator-online-docker-image))

Old: `swaggerapi/swagger-generator`

New: `openapitools/openapi-generator-online`


## New maven coordinates

You can find our released artefact on maven central:

**Core:**

Old:

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen</artifactId>
</dependency>
```

New:

```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator</artifactId>
</dependency>
```

**Cli:**

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-cli</artifactId>
</dependency>
```

New:

```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-cli</artifactId>
</dependency>
```

**Maven plugin:**

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
</dependency>
```

New:

```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
</dependency>
```

## Changes in Maven Plugin

OpenAPI Generator 3.0.0 has introduced `<generatorName>` and deprecated `<language>`, because this refers to generator names which embed more than just "language".

If both options are present, you'll be presented with an error. If only `<language>` is provided, you'll be presented instructions for updating to the new config.


## New generators names

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
| `ze-ph` | `php-mezzio-ph` |

We provide a temporary mapping in code for these old values. You'll receive a warning with instructions to migrate to the new names.

## New parameters name

Some parameters were renamed.
Often you need to replace "Swagger", with "OpenAPI".
Some examples:

| name in `swagger-codegen` | name in `openapi-generator`  |
|--|--|
| `debugSwagger` | `debugOpenAPI` |
| `GenerateSwaggerMetadata` | `GenerateOpenAPIMetadata` |
| `swagger.codegen.undertow.apipackage` | `openapi.codegen.undertow.apipackage` |
| `swagger.codegen.undertow.modelpackage` | `openapi.codegen.undertow.modelpackage` |


## Renamed Mustache Template Variables 

The template variable `{{datatype}}` was renamed to `{{dataType}}` for consistency reason.
Corresponding java code: `CodegenProperty.datatype` is renamed to `CodegenProperty.dataType`.

(If you're **not** using customized templates with the `-t` option, you can ignore the mustache variable renaming above.)

## Ignore file

`.swagger-codegen-ignore` is replaced by `.openapi-generator-ignore`.
The syntax inside the file stays the same.

You don't need to rename the file manually, OpenAPI Generator will do it when your run it against an existing output directory.
(When there is no `.openapi-generator-ignore` in a folder, if a `.swagger-codegen-ignore` file is present it will be considered and renamed to `.openapi-generator-ignore`).


## Metadata folder

The metadata folder (to store the `VERSION` file for example) is now called `.openapi-generator/` instead of `.swagger-codegen/`.



## New default values for the generated code

If you use a generator without specifying each parameter, you might see some differences in the generated code.
As example the default package name used in the generated code has changed. 
You need to have a look at the specific value, depending of your target language, but often `Swagger` îs replaced by `OpenAPITools` and `io.swagger` is replaced by `org.openapitools`.
Concretely if you did not specify anything when you are generating java code, a file `org/openapitools/api/PetApi.java`  might be generated instead of `io/swagger/api/PetApi.java`.

If this is a problem for you, you need to explicitly set the the parameter value in order to match with the `swagger-codgen` default value (`apiPackage` == `io.swagger` in the previous example with the java generator).


## New fully qualified name for the classes 

If you have extended some generators in your project, and you are looking for a specific class, replace the `io.swagger.codegen` package (old name) with `org.openapitools.codegen` package (new name).

Example: `org.openapitools.codegen.DefaultGenerator`

## Body parameter name

:bangbang: Since 4.0.0-beta, the body parameter name in OAS v2 is automatically preserved in the vendor extension `x-codegen-request-body-name`

In OpenAPI spec v3, there's no body parameter, which is replaced by [Request Body Object](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#request-body-object). The parameter name for Request Body is named automatically based on the model name (e.g. User). To control how the "Request Body" parameter is named, please add the vendor extension `x-codegen-request-body-name` to the operation:

OpenAPI Spec v3:
```yaml
paths:
  /pet:
    post:
      tags:
        - pet
      summary: Add a new pet to the store
      description: ''
      operationId: addPet
      x-codegen-request-body-name: new_body_name
      responses:
        '405':
          description: Invalid input
      security:
        - petstore_auth:
            - 'write:pets'
            - 'read:pets'
      requestBody:
        $ref: '#/components/requestBodies/Pet'
```

OpenAPI Spec v2:
```yaml
paths:
  /pet:
    post:
      tags:
        - pet
      summary: Add a new pet to the store
      description: ''
      operationId: addPet
      x-codegen-request-body-name: new_body_name
      consumes:
        - application/json
        - application/xml
      produces:
        - application/xml
        - application/json
      parameters:
        - in: body
          name: body
          description: Pet object that needs to be added to the store
          required: true
          schema:
            $ref: '#/definitions/Pet'
      responses:
        '405':
          description: Invalid input
      security:
        - petstore_auth:
            - 'write:pets'
            - 'read:pets'
```
If your API client is using named parameters in the function call (e.g. Perl required & optional parameters, Ruby optional parameters), you will need to add `x-codegen-request-body-name` to the spec to restore the original body parameter name.

## Default basePath

The default `basePath` has been changed from `https://localhost` to `http://localhost` (http without s)

## Nullable

OpenAPI spec v3 has better support for `nullable`. If you're still using OpenAPI/Swagger spec v2, please use `x-nullable: true` instead.
