---
id: new-generator
title: Create a New Generator
---

Creating a new generator which will become a part of the officially supported generators in OpenAPI Generator is pretty simple. We've created a helper script to bootstrap the operation. Let's look at the files necessary to create a new generator, then an example of bootstrapping a generator using the `new.sh` script in the root of the repository.

## Required Files

The minimum set of files required to create a new generator are:

* A "Codegen" file
  - exists under `modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/`
  - defines language options
  - defines framework options
  - determines OpenAPI feature set
  - extends the generation workflow
* SPI registration
  - Above class must be referenced in `modules/openapi-generator/src/main/resources/META-INF/services/org.openapitools.codegen.CodegenConfig`
  - Tells the generator that this class exists
  - Allows for classpath extension (addition) of generators
* A minimal template
  - Should include a README explaining usage
  - Must include an `api.mustache`
  - Exists under `modules/openapi-generator/src/main/resources/` (plus `embeddedTemplate` dir value, see below)
* Config file under `./bin/configs`
  - Gives users a "real life" example of generated output
  - Samples are used by CI to verify generators and test for regressions in some cases
  
Now, let's generate an example generator and then walk through the pieces. At the end, we'll touch on some known sticking points for new generator authors and provide some suggestions.

## new.sh

The `new.sh` script in the root of the project is meant to simplify this process. Run `./new.sh --help`.

```text
Stubs out files for new generators

Usage:
./new.sh [options]
    Options:
    -n  Required. Specify generator name, should be kebab-cased.
    -c  Create a client generator
    -s  Create a server generator
    -d  Create a documentation generator
    -H  Create a schema generator
    -f  Create a config generator
    -t  When specified, creates test file(s) for the generator.
    -h  Display help.

Examples:
  Create a server generator for ktor:
  ./new.sh -n kotlin -s

    Creates:
    modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/KotlinServerCodegen.java
    modules/openapi-generator/src/main/resources/kotlin-server/README.mustache
    modules/openapi-generator/src/main/resources/kotlin-server/model.mustache
    modules/openapi-generator/src/main/resources/kotlin-server/api.mustache
    bin/configs/kotlin-server-petstore-new.yaml

  Create a generic C# server generator:
  ./new.sh -n csharp -s -t
    Creates:
    modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/CsharpServerCodegen.java
    modules/openapi-generator/src/main/resources/csharp-server/README.mustache
    modules/openapi-generator/src/main/resources/csharp-server/model.mustache
    modules/openapi-generator/src/main/resources/csharp-server/api.mustache
    bin/configs/csharp-server-petstore-new.yaml
    modules/openapi-generator/src/test/java/org/openapitools/codegen/csharp/CsharpServerCodegenTest.java
    modules/openapi-generator/src/test/java/org/openapitools/codegen/csharp/CsharpServerCodegenModelTest.java
    modules/openapi-generator/src/test/java/org/openapitools/codegen/csharp/CsharpServerCodegenOptionsTest.java
    modules/openapi-generator/src/test/java/org/openapitools/codegen/options/CsharpServerCodegenOptionsProvider.java
```

This script allows us to define a client, server, schema, or documentation generator. We'll focus on the simplest generator (documentation). The other generator types may require heavy extension of the "Config" base class, and these docs could very quickly become outdated. When creating a new generator, please review existing generators as a guideline for implementation.

Create a new Markdown generator, specifying CommonMark as the name to avoid conflicting with the built-in Markdown generator.

```bash
./new.sh -n common-mark -d
```

You should see output similar to the following:

```bash
Creating modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/CommonMarkDocumentationCodegen.java
Creating modules/openapi-generator/src/main/resources/common-mark-documentation/README.mustache
Creating modules/openapi-generator/src/main/resources/common-mark-documentation/model.mustache
Creating modules/openapi-generator/src/main/resources/common-mark-documentation/api.mustache
Creating bin/configs/common-mark-documentation-petstore-new.yaml
Finished.
```

### Review Generated Config

Beginning with the "Codegen" file (`CommonMarkDocumentationCodegen.java`), the constructor was created:

```java
    public CommonMarkDocumentationCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "common-mark";
        modelTemplateFiles.put("model.mustache", ".zz");
        apiTemplateFiles.put("api.mustache", ".zz");
        embeddedTemplateDir = templateDir = "common-mark-documentation";
        apiPackage = File.separator + "Apis";
        modelPackage = File.separator + "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        // TODO: Fill this out.
    }
```

These options are some defaults which may require updating. Let's look line-by-line at the config.

```java
outputFolder = "generated-code" + File.separator + "common-mark";
```

This is the default output location. This will be `generated-code/common-mark` on non-Windows machines and `generated-code\common-mark` on Windows. You may change this to any value you'd like, but a user will almost always provide an output directory.

> When joining paths, always use `File.seperator`

```java
 modelTemplateFiles.put("model.mustache", ".zz");
```

The `model.mustache` file is registered as the template for model generation. The `new.sh` script doesn't have a way to know your intended file extension, so we default to a `.zz` extension. This _must_ be changed (unless your generator's target extension is `.zz`). For this example, you'd change `.zz` to `.md` or `.markdown`, depending on your preference.

This model template registration will use `model.mustache` to generate a new file for every model defined in your API's specification document.

The path is considered relative to `embeddedTemplateDir`, `templateDir`, or a library subdirectory (refer to the Java client generator implementation for a prime example).

```java
apiTemplateFiles.put("api.mustache", ".zz");
```

This is the template used for generating API related files. Similar to the above model template, you'll want to change `.zz` to `.md` or `.markdown`.

The path is considered relative to `embeddedTemplateDir`, `templateDir`, or a library subdirectory (refer to the Java client generator implementation for a prime example).

```java
embeddedTemplateDir = templateDir = "common-mark-documentation";
```

This line sets the embedded and template directories to `common-mark-documentation`. The `embeddedTemplateDir` refers to the directory which will exist under `modules/openapi-generator/src/main/resources` and will be published with every release in which your new generator is present.

The `templateDir` variable refers to the "current" template directory setting, as defined by the user. That is, the user may invoke with `-t` or `--template-directory` (or plugin option variants), and override this directory.

Both of these variables exist because the generator will fallback to files under `embeddedTemplateDir` if they are not defined in the user's custom template directory.

```java
apiPackage = "Apis";
```

This sets the "package" location for anything considered an API document. You might want to change this setting if, for instance, your language doesn't support uppercase letters in the path. We don't need to worry about that here.

Every templated output from `api.mustache` (registered via `apiTemplateFiles` above) will end up in the directory defined by `apiPackage` here.

```java
modelPackage = "Models";
```

Similarly, this sets the package for `Models`.

Every templated output from `model.mustache` (registered via `modelTemplateFiles` above) will end up in the directory defined by `modelPackage` here.

```java
supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
```

A "supporting file" is an extra file which isn't created once for every operation or model defined in your specification document. It is a single file which may or may not be templated (determined by the extension of the filename).

A supporting file only passes through the Mustache template processor if the filename ends in `.mustache`.

The path is considered relative to `embeddedTemplateDir`, `templateDir`, or a library subdirectory (refer to the Java client generator implementation for a prime example).

> If you want your readme to be generic (not templated), just rename the file to README.md and change `README.mustache` to `README.md` above.

### Create templates

The `new.sh` created our three required files. Let's start filling out each of these files.

#### README.mustache

```mustache
# Documentation for {{appName}}

{{#generateApiDocs}}
<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *{{{basePath}}}*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
{{#apiInfo}}{{#apis}}{{#operations}}{{#operation}}*{{classname}}* | [**{{operationId}}**](Apis/{{apiDocPath}}{{classname}}.md#{{operationIdLowerCase}}) | **{{httpMethod}}** {{path}} | {{#summary}}{{{summary}}}{{/summary}}
{{/operation}}{{/operations}}{{/apis}}{{/apiInfo}}
{{/generateApiDocs}}

{{#generateModelDocs}}
<a name="documentation-for-models"></a>
## Documentation for Models

{{#modelPackage}}
{{#models}}{{#model}} - [{{{modelPackage}}}.{{{classname}}}](Models/{{modelDocPath}}{{{classname}}}.md)
{{/model}}{{/models}}
{{/modelPackage}}
{{^modelPackage}}
No model defined in this package
{{/modelPackage}}
{{/generateModelDocs}}

<a name="documentation-for-authorization"></a>{{! TODO: optional documentation for authorization? }}
## Documentation for Authorization

{{^authMethods}}
All endpoints do not require authorization.
{{/authMethods}}
{{#authMethods}}
{{#last}}
Authentication schemes defined for the API:
{{/last}}
{{/authMethods}}
{{#authMethods}}
<a name="{{name}}"></a>
### {{name}}

{{#isApiKey}}- **Type**: API key
- **API key parameter name**: {{keyParamName}}
- **Location**: {{#isKeyInQuery}}URL query string{{/isKeyInQuery}}{{#isKeyInHeader}}HTTP header{{/isKeyInHeader}}
{{/isApiKey}}
{{#isBasic}}- **Type**: HTTP basic authentication
{{/isBasic}}
{{#isOAuth}}- **Type**: OAuth
- **Flow**: {{flow}}
- **Authorization URL**: {{authorizationUrl}}
- **Scopes**: {{^scopes}}N/A{{/scopes}}
{{#scopes}}  - {{scope}}: {{description}}
{{/scopes}}
{{/isOAuth}}

{{/authMethods}}
```

Let's not focus too much on the contents of this file.  You may refer to [templating](./templating.md) for more details on the variables bound to these files and to [debugging](./debugging.md) how to debug the structures. Of note here is that we're generating structures in markdown as defined by the objects constructed by our new "Config" class.

#### api.mustache

The API documentation might look like this:

```mustache
# {{classname}}{{#description}}
{{description}}{{/description}}

All URIs are relative to *{{basePath}}*

Method | HTTP request | Description
------------- | ------------- | -------------
{{#operations}}{{#operation}}[**{{operationId}}**]({{classname}}.md#{{operationId}}) | **{{httpMethod}}** {{path}} | {{#summary}}{{summary}}{{/summary}}
{{/operation}}{{/operations}}

{{#operations}}
{{#operation}}
<a name="{{operationId}}"></a>
# **{{operationId}}**
> {{#returnType}}{{returnType}} {{/returnType}}{{operationId}}({{#allParams}}{{{paramName}}}{{^-last}}, {{/-last}}{{/allParams}})

{{summary}}{{#notes}}

{{notes}}{{/notes}}

### Parameters
{{^allParams}}This endpoint does not need any parameter.{{/allParams}}{{#allParams}}{{#-last}}
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------{{/-last}}{{/allParams}}
{{#allParams}} **{{paramName}}** | {{#isPrimitiveType}}**{{dataType}}**{{/isPrimitiveType}}{{^isPrimitiveType}}{{#isFile}}**{{dataType}}**{{/isFile}}{{^isFile}}{{#generateModelDocs}}[**{{dataType}}**]({{baseType}}.md){{/generateModelDocs}}{{^generateModelDocs}}**{{dataType}}**{{/generateModelDocs}}{{/isFile}}{{/isPrimitiveType}}| {{description}} |{{^required}} [optional]{{/required}}{{#defaultValue}} [default to {{defaultValue}}]{{/defaultValue}}{{#allowableValues}} [enum: {{#values}}{{{.}}}{{^-last}}, {{/-last}}{{/values}}]{{/allowableValues}}
{{/allParams}}

### Return type

{{#returnType}}{{#returnTypeIsPrimitive}}**{{returnType}}**{{/returnTypeIsPrimitive}}{{^returnTypeIsPrimitive}}{{#generateModelDocs}}[**{{returnType}}**]({{returnBaseType}}.md){{/generateModelDocs}}{{^generateModelDocs}}**{{returnType}}**{{/generateModelDocs}}{{/returnTypeIsPrimitive}}{{/returnType}}{{^returnType}}null (empty response body){{/returnType}}

### Authorization

{{^authMethods}}No authorization required{{/authMethods}}{{#authMethods}}[{{name}}](../README.md#{{name}}){{^-last}}, {{/-last}}{{/authMethods}}

### HTTP request headers

 - **Content-Type**: {{#consumes}}{{{mediaType}}}{{^-last}}, {{/-last}}{{/consumes}}{{^consumes}}Not defined{{/consumes}}
 - **Accept**: {{#produces}}{{{mediaType}}}{{^-last}}, {{/-last}}{{/produces}}{{^produces}}Not defined{{/produces}}

{{/operation}}
{{/operations}}

```

#### model.mustache

The models file could resemble the following.

```mustache
{{#models}}
{{#model}}
# {{{packageName}}}.{{modelPackage}}.{{{classname}}}
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
{{#parent}}
{{#parentVars}}
**{{name}}** | {{#isPrimitiveType}}**{{dataType}}**{{/isPrimitiveType}}{{^isPrimitiveType}}[**{{dataType}}**]({{complexType}}.md){{/isPrimitiveType}} | {{description}} | {{^required}}[optional] {{/required}}{{#readOnly}}[readonly] {{/readOnly}}{{#defaultValue}}[default to {{{.}}}]{{/defaultValue}}
{{/parentVars}}
{{/parent}}
{{#vars}}**{{name}}** | {{#isPrimitiveType}}**{{dataType}}**{{/isPrimitiveType}}{{^isPrimitiveType}}[**{{dataType}}**]({{complexType}}.md){{/isPrimitiveType}} | {{description}} | {{^required}}[optional] {{/required}}{{#readOnly}}[readonly] {{/readOnly}}{{#defaultValue}}[default to {{{.}}}]{{/defaultValue}}
{{/vars}}

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

{{/model}}
{{/models}}
```

### Build it

To compile quickly to test this out, you can run `mvn clean package -DskipTests`.

> When implementing a more robust generator, you'll want to run all tests as well: `mvn clean package`

### Compile Sample

The `new.sh` script created the generation config file `bin/configs/common-mark-documentation-petstore-new.yaml`:

```bash
generatorName: common-mark
outputDir: samples/documentation/petstore/common/mark
inputSpec: modules/openapi-generator/src/test/resources/3_0/petstore.yaml
templateDir: modules/openapi-generator/src/main/resources/common-mark
additionalProperties:
  hideGenerationTimestamp: "true"
```

This configuration file is passed to the generator's CLI tool during continuous integration builds, and many outputs are compiled and tested as a regression test on every build. Contributors are also asked to run `./bin/utils/ensure-up-to-date` before opening a pull request to regenerate all samples defined under `./bin/configs`. This allows maintainers to quickly verify whether changes impact other generators.

Configuration based examples allow us to test the same samples in each tooling option (CLI, Gradle Plugin, Maven Plugin, etc.). 

You can compile your generator by running:

```bash
./bin/generate-samples.sh bin/configs/common-mark-documentation-petstore-new.yaml
```

This configuration file can be used to demonstrate the default options for generation. A common option in most of these configs is to define the template directory as the generator's directory under `resources`. This allows template maintainers to modify and test out template changes which don't require recompilation of the entire project. You'd still need to recompile the project in full if you add or modify behaviors to the generator (such as adding a `CliOption`).

### Verify output

Creating a new generator will be an iterative task. Once you've generated the sample, you'll want to try it out. For compiled client/server outputs, this would mean running the code or creating a small sample project to consume your artifact just to make sure it works.

For markdown, you can open in Visual Studio Code or any other editor with a markdown preview. Not all editors support relative links to other markdown documents. To test the output in this guide, install `markserv`:

```bash
npm install --global markserv
```

Now, you can serve the output directory directly and test your links:

```bash
markserv samples/documentation/petstore/common/markdown
```

That's it! You've created your first generator!
