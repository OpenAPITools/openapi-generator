---
id: templating
title: Using Templates
---

It's easy to work with templates for codegen!

The generator workflow has [transforming logic](https://github.com/openapitools/openapi-generator/tree/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/languages) as well as templates for each generation of code.

Each generator will create a data structure from the OpenAPI document; OpenAPI 2.0 and OpenAPI 3.x documents are normalized into the same API model within the generator. This model is then applied to the templates.  While generators do not need to perform transformations, it's often necessary in order to add more advanced support for your language or framework. You may need to refer to the generator implementation to understand some of the logic while creating or customizing templates (see [FinchServerCodegen.java](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/FinchServerCodegen.java) for an advanced example).

The transform logic needs to implement [CodegenConfig.java](https://github.com/openapitools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/CodegenConfig.java) and is most easily done by extending [DefaultCodegen.java](https://github.com/openapitools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/DefaultCodegen.java).  Take a look at the various implementations as a guideline while the instructions get more complete.

## Modifying Templates

> OpenAPI Generator applies user-defined templates via options:  
> * CLI: `-t/--template` CLI options
> * Maven Plugin: `templateDirectory`
> * Gradle Plugin: `templateDir` 

Built-in templates are written in Mustache and processed by [jmustache](https://github.com/samskivert/jmustache). We plan to eventually support Handlebars and user-defined template engines via plugins.

OpenAPI Generator supports user-defined templates. This approach is often the easiest when creating a custom template. Our generators implement a combination of language and framework features, and it's fully possible to use an existing generator to implement a custom template for a different framework. Suppose you have internal utilities which you'd like to incorporate into generated code (e.g. logging, monitoring, fault-handling)... this is easy to add via custom templates.

> **Note:** You cannot use this approach to create new templates, only override existing ones. If you'd like to create a new generator to contribute back to the project, see `new.sh` in the repository root. If you'd like to create a private generator for more templating control, see the [customization](./customization.md) docs.

### Custom Logic

For this example, let's modify a Java client to use AOP via [jcabi/jcabi-aspects](https://github.com/jcabi/jcabi-aspects). We'll log API method execution at the `INFO` level. The jcabi-aspects project could also be used to implement method retries on failures; this would be a great exercise to further play around with templating. 

The Java generator supports a `library` option. This option works by defining base templates, then applying library-specific template overrides. This allows for template reuse for libraries sharing the same programming language. Templates defined as a library need only modify or extend the templates concerning the library, and generation falls back to the root templates (the "defaults") when not extended by the library. Generators which support the `library` option will only support the libraries known by the generator at compile time, and will throw a runtime error if you try to provide a custom library name.

To get started, we will need to copy our target generator's directory in full. The directory will be located under `modules/opeanpi-generator/src/main/resources/{generator}`. In general, the generator directory matches the generator name (what you would pass to the `generator` option), but this is not a requirement-- if you are having a hard time finding the template directory, look at the `embeddedTemplateDir` option in your target generator's implementation.

If you've already cloned openapi-generator, find and copy the `modules/opeanpi-generator/src/main/resources/Java` directory. If you have the [Refined GitHub](https://github.com/sindresorhus/refined-github) Chrome or Firefox Extension, you can navigate to this directory on GitHub and click the "Download" button. Or, to pull the directory from latest master:

```bash
mkdir -p ~/.openapi-generator/templates/ && cd $_
curl -L https://api.github.com/repos/OpenAPITools/openapi-generator/tarball | tar xz
mv `ls`/modules/openapi-generator/src/main/resources/Java ./Java
\rm -rf OpenAPITools-openapi-generator-*
cd Java
```

**Optional**: Before modifying your templates, you may want to `git init && git add . && git commit -am 'initial'` so you can easily revert to the base templates.

At this point, you have _every_ Java library's template locally. Let's delete all libraries except the `resteasy` library we'll be extending:

```bash
ls -d libraries/* | grep -v resteasy | xargs rm -rf
```

Execute `tree` in this Java directory and inspect the mustache files and directory structure. You'll notice there are quite a few templates in the directory root, but extending this root to support resteasy only requires modifying a handful of files:

```bash
tree libraries/resteasy/
libraries/resteasy/
├── ApiClient.mustache
├── JSON.mustache
├── api.mustache
├── build.gradle.mustache
├── build.sbt.mustache
└── pom.mustache

0 directories, 6 files
```

> NOTE: Some generators may be sensitive to _which_ files exist. If you're concerned with redundant files like `pom.mustache` and `build.sbt.mustache`, you can try deleting them. If the generator you're customizing fails at runtime, just `touch` these files to create an empty file.


First, let's add our new dependency to `libraries/resteasy/build.gradle.mustache`:

```diff
diff --git a/libraries/resteasy/build.gradle.mustache b/libraries/resteasy/build.gradle.mustache
index 3b40702..a6d12e0 100644
--- a/libraries/resteasy/build.gradle.mustache
+++ b/libraries/resteasy/build.gradle.mustache
@@ -134,6 +134,7 @@ ext {
 }
 
 dependencies {
+    compile "com.jcabi:jcabi-aspects:0.22.6"
     compile "io.swagger:swagger-annotations:$swagger_annotations_version"
     compile "org.jboss.resteasy:resteasy-client:$resteasy_version"
     compile "org.jboss.resteasy:resteasy-multipart-provider:$resteasy_version"

```

Then, we'll add the necessary import to `api.mustache`. This file is the template which becomes the API invoking class (e.g. `PetApi` or `StoreApi`).

```diff
diff --git a/libraries/resteasy/api.mustache b/libraries/resteasy/api.mustache
index a4d0f9f..49b17c7 100644
--- a/libraries/resteasy/api.mustache
+++ b/libraries/resteasy/api.mustache
@@ -1,5 +1,6 @@
 package {{package}};
 
+import com.jcabi.aspects.Loggable;
 import {{invokerPackage}}.ApiException;
 import {{invokerPackage}}.ApiClient;
 import {{invokerPackage}}.Configuration;

```

Next, we'll find the code which generates API methods. You'll see `{{#operations}}{{#operation}}` which is a mustache "loop" which executes the template logic if the model applied to the template has an `operations` array, and a non-null `operation` instance in that array. You can pass `-DdebugOpenAPI` when generating via CLI to inspect the full object model.

Further down in `api.mustache`, find implementation of the method call, and add the `@Loggable` annotation. This template is easy because it has a single method implementation.

```diff
diff --git a/libraries/resteasy/api.mustache b/libraries/resteasy/api.mustache
index 49b17c7..16ee191 100644
--- a/libraries/resteasy/api.mustache
+++ b/libraries/resteasy/api.mustache
@@ -57,6 +57,7 @@ public class {{classname}} {
   {{#isDeprecated}}
   @Deprecated
   {{/isDeprecated}}
+  @Loggable(Loggable.INFO)
   public {{#returnType}}{{{returnType}}} {{/returnType}}{{^returnType}}void {{/returnType}}{{operationId}}({{#allParams}}{{{dataType}}} {{paramName}}{{#hasMore}}, {{/hasMore}}{{/allParams}}) throws ApiException {
     Object {{localVariablePrefix}}localVarPostBody = {{#bodyParam}}{{paramName}}{{/bodyParam}}{{^bodyParam}}new Object(){{/bodyParam}};
     {{#allParams}}{{#required}}

```

Finally, because our new dependency relies on AspectJ and code weaving, let's modify the `build.gradle.mustache` again to set this up.

```diff
diff --git a/build.gradle.mustache b/build.gradle.mustache
index 04a9d55..7a93c50 100644
--- a/build.gradle.mustache
+++ b/build.gradle.mustache
@@ -1,5 +1,6 @@
 apply plugin: 'idea'
 apply plugin: 'eclipse'
+apply plugin: 'aspectj'
 
 group = '{{groupId}}'
 version = '{{artifactVersion}}'
@@ -12,6 +13,7 @@ buildscript {
     dependencies {
         classpath 'com.android.tools.build:gradle:2.3.+'
         classpath 'com.github.dcendents:android-maven-gradle-plugin:1.5'
+        classpath "net.uberfoo.gradle:gradle-aspectj:2.2"
     }
 }
 
@@ -140,9 +142,18 @@ ext {
     jersey_version = "1.19.4"
     jodatime_version = "2.9.9"
     junit_version = "4.12"
+    aspectjVersion = '1.9.0'
 }
 
+sourceCompatibility = '1.8'
+targetCompatibility = '1.8'
+
 dependencies {
+    compile "com.jcabi:jcabi-aspects:0.22.6"
+    aspectpath "com.jcabi:jcabi-aspects:0.22.6"
+    // usually, client code leaves logging implementation to the consumer code
+    compile "org.apache.logging.log4j:log4j-slf4j-impl:2.8.2"
+    compile "org.apache.logging.log4j:log4j-core:2.8.2"
     compile "io.swagger:swagger-annotations:$swagger_annotations_version"
     compile "com.sun.jersey:jersey-client:$jersey_version"
     compile "com.sun.jersey.contribs:jersey-multipart:$jersey_version"

```

> NOTE: This example includes log4j-slf4j-impl to demonstrate that our new code is working. Generally you'll want to leave logging implementations up to your consumers.

And because the java client generates with an outdated Gradle 2.6, let's update the gradle version in the default template (`Java/gradle-wrapper.properties.mustache`):

```diff
diff --git a/gradle-wrapper.properties.mustache b/gradle-wrapper.properties.mustache
index b7a3647..3d9d088 100644
--- a/gradle-wrapper.properties.mustache
+++ b/gradle-wrapper.properties.mustache
@@ -3,4 +3,4 @@ distributionBase=GRADLE_USER_HOME
 distributionPath=wrapper/dists
 zipStoreBase=GRADLE_USER_HOME
 zipStorePath=wrapper/dists
-distributionUrl=https\://services.gradle.org/distributions/gradle-2.6-bin.zip
+distributionUrl=https\://services.gradle.org/distributions/gradle-4.8-bin.zip

```

Now we're ready to generate the client with our simple changes. When we pass the template directory option to our toolset, we _must_ pass the generator's root directory and _not_ the library-only directory.

```bash
openapi-generator generate -g java --library resteasy \
    -t ~/.openapi-generator/templates/Java \
    -o ~/.openapi-generator/example \
    -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml
```

Make sure your custom template compiles:

```bash
cd ~/.openapi-generator/example
gradle assemble
# or, regenerate the wrapper 
gradle wrapper --gradle-version 4.8 --distribution-type all
./gradlew assemble
```

You should see a log message showing our added dependency being downloaded:

```text
…
Download https://jcenter.bintray.com/com/jcabi/jcabi-aspects/0.22.6/jcabi-aspects-0.22.6.pom
…
```

And for the sake of verifying our AOP modifications work, let's create a `src/main/resources/log4j2.properties` file in our new client code:

```properties
status = error
dest = err
name = PropertiesConfig

property.filename = target/rolling/rollingtest.log

filter.threshold.type = ThresholdFilter
filter.threshold.level = debug

appender.console.type = Console
appender.console.name = STDOUT
appender.console.layout.type = PatternLayout
appender.console.layout.pattern = %m%n
appender.console.filter.threshold.type = ThresholdFilter
appender.console.filter.threshold.level = error

appender.rolling.type = RollingFile
appender.rolling.name = RollingFile
appender.rolling.fileName = ${filename}
appender.rolling.filePattern = target/rolling2/test1-%d{MM-dd-yy-HH-mm-ss}-%i.log.gz
appender.rolling.layout.type = PatternLayout
appender.rolling.layout.pattern = %d %p %C{1.} [%t] %m%n
appender.rolling.policies.type = Policies
appender.rolling.policies.time.type = TimeBasedTriggeringPolicy
appender.rolling.policies.time.interval = 2
appender.rolling.policies.time.modulate = true
appender.rolling.policies.size.type = SizeBasedTriggeringPolicy
appender.rolling.policies.size.size=100MB
appender.rolling.strategy.type = DefaultRolloverStrategy
appender.rolling.strategy.max = 5

logger.rolling.name = org.openapitools.client.api.PetApi
logger.rolling.level = debug
logger.rolling.additivity = false
logger.rolling.appenderRef.rolling.ref = RollingFile

rootLogger.level = info
rootLogger.appenderRef.stdout.ref = STDOUT
```

Execute `./gradlew build` and then `cat target/rolling/rollingtest.log`. You should see messages logged for every call in PetApi with a stubbed unit test.

Congratulations! You've now modified one of the built-in templates to meet your client code's needs.

Adding/modifying template logic simply requires a little bit of [mustache](https://mustache.github.io/), for which you can use existing templates as a guide.

## Structures

Aside from transforming an API document, the implementing class gets to decide how to apply the data structure to templates. We can decide which data structure to apply to which template files. You have the following structures at your disposal.

Examples for the following structures will be presented using the following spec document:

```yaml
  swagger: "2.0"
  info: 
    version: "1.0.0"
    title: "Swagger Petstore"
    description: "A sample API that uses a petstore as an example to demonstrate features in the swagger-2.0 specification"
    termsOfService: "http://swagger.io/terms/"
    contact: 
      name: "Swagger API Team"
    license: 
      name: "MIT"
  host: "petstore.swagger.io"
  basePath: "/api"
  schemes: 
    - "http"
  consumes: 
    - "application/json"
  produces: 
    - "application/json"
  paths: 
    /pets: 
      get: 
        description: "Returns all pets from the system that the user has access to"
        produces: 
          - "application/json"
        responses: 
          "200":
            description: "A list of pets."
            schema: 
              type: "array"
              items: 
                $ref: "#/definitions/Pet"
  definitions: 
    Pet: 
      type: "object"
      required: 
        - "id"
        - "name"
      properties: 
        id: 
          type: "integer"
          format: "int64"
        name: 
          type: "string"
        tag: 
          type: "string"

```

### Operations

> Inspect operation structures passed to templates with system property `-DdebugOpenAPI`
> 
> Example:
> 
> ```bash
> openapi-generator generate -g go \
>     -o out \
>     -i petstore-minimal.yaml \
>     -DdebugOpenAPI
> ```
>

There is a data structure which represents all the operations that are defined in the OpenAPI specification.  A single API file is created for each `OperationGroup`, which is essentially a grouping of different operations.  See the `addOperationToGroup` in `DefaultCodegen.java` for details on this operation.

You can have many files created for each `OperationGroup` by processing multiple templates and assigning a different file naming pattern to them.  So for a single file per operation:

```java
// process the `api.mustache` template and output a single file with suffix `.java`:
apiTemplateFiles.put("api.mustache", ".java");
```

For C-like languages which also require header files, you may create two files per operation.

```objc
// create a header and implementation for each operation group:
apiTemplateFiles.put("api-header.mustache", ".h");
apiTemplateFiles.put("api-body.mustache", ".m");
```
Here, an Operation with tag `Pet` will generate two files: `SWGPetApi.h` and `SWGPetApi.m`. The `SWG` prefix and `Api` suffix are options specific to the Objective-C geneator.

### Models

> Inspect models passed to templates with system property `-DdebugModels`
> 
> Execute:
> 
> ```bash
> openapi-generator generate -g go \
>     -o out \
>     -i petstore-minimal.yaml \
>     -DdebugModels
> ```
>

Each model identified inside the generator will be passed into the `Models` data structure and will generate a new model file (or files) for each model.

A `Pet` model with three properties will provide a _lot_ of information about the type and properties. The output from `-DdebugModels` is presented in truncated format here.

```json
[ {
  "importPath" : "openapi.Pet",
  "model" : {
    "name" : "Pet",
    "classname" : "Pet",
    "classVarName" : "Pet",
    "modelJson" : "{\n  \"required\" : [ \"id\", \"name\" ],\n  \"type\" : \"object\",\n  \"properties\" : {\n    \"id\" : {\n      \"type\" : \"integer\",\n      \"format\" : \"int64\"\n    },\n    \"name\" : {\n      \"type\" : \"string\"\n    },\n    \"tag\" : {\n      \"type\" : \"string\"\n    }\n  }\n}",
    "dataType" : "map[string]interface{}",
    "classFilename" : "model_pet",
    "isAlias" : false,
    "isString" : false,
    "isInteger" : false,
    "vars" : [ {
      "baseName" : "id",
      "getter" : "getId",
      "setter" : "setId",
      "dataType" : "int64",
      "datatypeWithEnum" : "int64",
      "dataFormat" : "int64",
      "name" : "Id",
      "defaultValueWithParam" : " = data.id;",
      "baseType" : "int64",
      "example" : "null",
      "jsonSchema" : "{\n  \"type\" : \"integer\",\n  \"format\" : \"int64\"\n}",
      "exclusiveMinimum" : false,
      "exclusiveMaximum" : false,
      "hasMore" : true,
      "required" : true,
      "secondaryParam" : false,
      "hasMoreNonReadOnly" : true,
      "isPrimitiveType" : true,
      "isModel" : false,
      "isContainer" : false,
      "isNotContainer" : true,
      "isString" : false,
      "isNumeric" : true,
      "isInteger" : false,
      "isLong" : true,
      "isNumber" : false,
      "isFloat" : false,
      "isDouble" : false,
      "isByteArray" : false,
      "isBinary" : false,
      "isFile" : false,
      "isBoolean" : false,
      "isDate" : false,
      "isDateTime" : false,
      "isUuid" : false,
      "isEmail" : false,
      "isFreeFormObject" : false,
      "isListContainer" : false,
      "isMapContainer" : false,
      "isEnum" : false,
      "isReadOnly" : false,
      "isWriteOnly" : false,
      "isNullable" : false,
      "vendorExtensions" : { },
      "hasValidation" : false,
      "isInherited" : false,
      "nameInCamelCase" : "Id",
      "nameInSnakeCase" : "ID",
      "isXmlAttribute" : false,
      "isXmlWrapped" : false,
      "datatype" : "int64",
      "iexclusiveMaximum" : false
    }, {
      "baseName" : "name",
      "getter" : "getName",
      "setter" : "setName",
      "dataType" : "string",
      "datatypeWithEnum" : "string",
      "name" : "Name",
      "defaultValueWithParam" : " = data.name;",
      "baseType" : "string",
      "example" : "null",
      "jsonSchema" : "{\n  \"type\" : \"string\"\n}",
      "exclusiveMinimum" : false,
      "exclusiveMaximum" : false,
      "hasMore" : true,
      "required" : true,
      "secondaryParam" : false,
      "hasMoreNonReadOnly" : true,
      "isPrimitiveType" : true,
      "isModel" : false,
      "isContainer" : false,
      "isNotContainer" : true,
      "isString" : true,
      "isNumeric" : false,
      "isInteger" : false,
      "isLong" : false,
      "isNumber" : false,
      "isFloat" : false,
      "isDouble" : false,
      "isByteArray" : false,
      "isBinary" : false,
      "isFile" : false,
      "isBoolean" : false,
      "isDate" : false,
      "isDateTime" : false,
      "isUuid" : false,
      "isEmail" : false,
      "isFreeFormObject" : false,
      "isListContainer" : false,
      "isMapContainer" : false,
      "isEnum" : false,
      "isReadOnly" : false,
      "isWriteOnly" : false,
      "isNullable" : false,
      "vendorExtensions" : { },
      "hasValidation" : false,
      "isInherited" : false,
      "nameInCamelCase" : "Name",
      "nameInSnakeCase" : "NAME",
      "isXmlAttribute" : false,
      "isXmlWrapped" : false,
      "datatype" : "string",
      "iexclusiveMaximum" : false
    }, {
      "baseName" : "tag",
      "getter" : "getTag",
      "setter" : "setTag",
      "dataType" : "string",
      "datatypeWithEnum" : "string",
      "name" : "Tag",
      "defaultValueWithParam" : " = data.tag;",
      "baseType" : "string",
      "example" : "null",
      "jsonSchema" : "{\n  \"type\" : \"string\"\n}",
      "exclusiveMinimum" : false,
      "exclusiveMaximum" : false,
      "hasMore" : false,
      "required" : false,
      "secondaryParam" : false,
      "hasMoreNonReadOnly" : false,
      "isPrimitiveType" : true,
      "isModel" : false,
      "isContainer" : false,
      "isNotContainer" : true,
      "isString" : true,
      "isNumeric" : false,
      "isInteger" : false,
      "isLong" : false,
      "isNumber" : false,
      "isFloat" : false,
      "isDouble" : false,
      "isByteArray" : false,
      "isBinary" : false,
      "isFile" : false,
      "isBoolean" : false,
      "isDate" : false,
      "isDateTime" : false,
      "isUuid" : false,
      "isEmail" : false,
      "isFreeFormObject" : false,
      "isListContainer" : false,
      "isMapContainer" : false,
      "isEnum" : false,
      "isReadOnly" : false,
      "isWriteOnly" : false,
      "isNullable" : false,
      "vendorExtensions" : { },
      "hasValidation" : false,
      "isInherited" : false,
      "nameInCamelCase" : "Tag",
      "nameInSnakeCase" : "TAG",
      "isXmlAttribute" : false,
      "isXmlWrapped" : false,
      "datatype" : "string",
      "iexclusiveMaximum" : false
    } ],
    "requiredVars" : [ /* id, name */ ],
    "optionalVars" : [ /* tag */ ],
    "readOnlyVars" : [ ],
    "readWriteVars" : [ /* lists metadata for all three properties */ ],
    "allVars" : [ /* lists all properties */],
    "parentVars" : [ ],
    "mandatory" : [ "id", "name" ],
    "allMandatory" : [ "id", "name" ],
    "imports" : [ ],
    "hasVars" : true,
    "emptyVars" : false,
    "hasMoreModels" : false,
    "hasEnums" : false,
    "isEnum" : false,
    "hasRequired" : true,
    "hasOptional" : true,
    "isArrayModel" : false,
    "hasChildren" : false,
    "isMapModel" : false,
    "hasOnlyReadOnly" : false,
    "vendorExtensions" : { }
  }
} ]
```

Templates are passed redundant properties, depending on the semantics of the array. For example:

* `vars` lists all defined model properties
* `requiredVars` lists all model properties marked with `required` in the spec document
* `optionalVars` lists all model properties _not_ marked with `required` in the spec document
* `readWriteVars` lists all model properties _not_ marked with `readonly` in the spec document
* `readOnlyVars` lists all model properties marked with `readonly` in the spec document
* `allVars` lists all model properties. This may include the same set as `vars`, but may also include generator-defined properties

We expose the same properties in multiple sets because this allows us to conditionally iterate over properties based on some condition ("is it required" or "is it readonly"). This is driven by the use of the logic-less Mustache templates. It is possible that models passed to the templating engine may be cleaned up as we support more template engines, but such an effort will go through a deprecation phase and would be communicated at runtime through log messages. 

### supportingFiles

> Inspect supportingFiles passed to templates with system property `-DdebugSupportingFiles`
> 
> Execute:
> 
> ```bash
> openapi-generator generate -g go \
>     -o out \
>     -i petstore-minimal.yaml \
>     -DdebugSupportingFiles
> ```
>


This is a "catch-all" which gives you the entire structure--operations, model, etc--so you can create "single-file" code from them.

Supporting files can either be processed through the templating engine or copied as-is. When creating your own templates, you're limited to the files and extensions expected by the generator implementation. For more control over the supporting files produced by a generator, see our [customization](./customization.md) documentation.

## Variables

> This is a very limited list of variable name explanations. Feel free to [open a pull request](https://github.com/OpenAPITools/openapi-generator/pull/new/master) to add to this documentation!

- **complexType**: stores the name of the model (e.g. Pet) 
- **isContainer**: true if the parameter or property is an array or a map.
- **isPrimitiveType**: true if the parameter or property type is a primitive type (e.g. string, integer, etc) as defined in the spec.

## Extensions

OpenAPI supports a concept called "Extensions". These are called "Specification Extensions" [in 3.x](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#specificationExtensions) and "Vendor Extensions" [in 2.0](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#vendorExtensions).
You'll see them referred to as "Vendor Extensions" in most places in this project.

Vendor extensions allow you to provide vendor-specific configurations to your specification document.

For example, suppose you use your specification document for code generation with a (hypothetical) C# OpenAPI generator supporting a desired operationId prefix where the extension is `x-csharp-operationid`, you can define this property alongside the object you'd like to extend (which would be a Path Object in this case). You could then apply additional extensions alongside this property, whether they're for another language or other tooling.

> Well-defined vendor extensions don't cause conflicts with other tooling.

<!-- TODO: Auto-generate this list using generator metadata -->

The following are vendor extensions supported by OpenAPI Generator. The list may not be up-to-date, the best way is to look for "x-" in the built-in mustache templates.

### All generators (core)

#### Enum

`x-enum-varnames` can be used to have an other enum name for the corresponding value.
This is used to define names of the enum items.

`x-enum-descriptions` can be used to provide an individual description for each value.
This is used for comments in the code (like javadoc if the target language is java).

`x-enum-descriptions` and `x-enum-varnames` are each expected to be list of items containing the same number of items as `enum`.
The order of the items in the list matters: their position is used to group them together.

Example:

```yaml
WeatherType:
  type: integer
  format: int32
  enum:
    - 42
    - 18
    - 56
  x-enum-descriptions:
    - 'Blue sky'
    - 'Slightly overcast'
    - 'Take an umbrella with you'
  x-enum-varnames:
    - Sunny
    - Cloudy
    - Rainy
```

In the example for the integer value `42`, the description will be `Blue sky` and the name of the enum item will be `Sunny` (some generators changes it to `SUNNY` to respect some coding convention).

### ObjC
#### x-objc-operationId

To customize the method name, you can provide a different name in x-objc-operationId, e.g.
```yaml
summary: Add a new pet to the store
description: ''
operationId: addPet
x-objc-operationId: CreateNewPet
```  

### Java (Feign)
#### x-accepts

A single `Accepts` value as the Feign API client needs a single value for `Accepts` header, e.g.
```yaml
consumes:
  - application/json
  - application/xml
x-accepts: application/json
```

### x-content-type

A single "Content-Type" value as the Feign API client needs a single value for `Content-Type` header, e.g.
```yaml
produces:
  - application/xml
  - application/json
x-content-type: application/json
```

### Rust-server

#### x-responseId

Each response may specify a unique `x-responseId`. `rust-server` will use this to name the corresponding enum variant in the code. e.g.

```yaml
paths:
  /ping:
    get:
      responses:
        200:
          description: OK
          x-responseId: Pong
```

### MySQL Schema

#### x-mysqlSchema

MySQL schema generator creates vendor extensions based on openapi `dataType` and `dataFormat`. When user defined extensions with same key already exists codegen accepts those as is. It means it won't validate properties or correct it for you. Every model in `definitions` can contain table related and column related extensions like in example below: 

```yaml
definitions:
  Order:
    description: This should be most common InnoDB table
    type: object
    properties:
      id:
        description: >-
          This column should be unsigned BIGINT with AUTO_INCREMENT
        type: integer
        format: int64
        x-mysqlSchema:
          columnDefinition:
            colName: id
            colDataType: DECIMAL
            colDataTypeArguments:
              - argumentValue: 16
                isString: false
                hasMore: true
              - argumentValue: 4
                isString: false
                hasMore: false
            colUnsigned: true
            colNotNull: true
            colDefault:
              defaultValue: AUTO_INCREMENT
              isString: false
              isNumeric: false
              isKeyword: true
            colComment: >-
              Column comment. This column should be unsigned BIGINT with AUTO_INCREMENT
    x-mysqlSchema:
      tableDefinition:
        tblName: orders
        tblStorageEngine: InnoDB
        tblComment: >-
          Table comment. This should be most common InnoDB table
```
> There are properties that are not implemented by now(`tblStorageEngine`), but you can see how generator can be enhanced in future.


## Mustache Tips

Here are a few tips we've found useful for new template authors.
For more details on Mustache see [mustache.5](https://mustache.github.io/mustache.5.html). See also [samskivert/jmustache](https://github.com/samskivert/jmustache) for implementation-specific details.

### First/Last

To access the first or last element in a list using Mustache:

```mustache
{{#vars}}{{#-first}} this is the first element {{.}} {{/-first}}{{/vars}}
{{#vars}}{{#-last}} this is the last element {{.}} {{/-last}}{{/vars}}
```

### This

Mustache evaluates template variables contextually. If the variable isn't found in the immediate object, mustache will search the parent. This is similar to JavaScript's prototype object (if you're familiar with the concept).

You can inspect this entire context by outputting `{{this}}`. For example:

```mustache
{{#operations}}{{this}}{{/operations}}
```

### Index

If you'd like a 1-based index in your array traversal, you can use `{{-index}}`:

```mustache
{{#enums}}{{-index}} {{enum}}{{/enums}}
```
