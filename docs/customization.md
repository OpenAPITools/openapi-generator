---
id: customization
title: Customization
---

## User-defined Templates

The most common scenario for user customization is to override the built-in templates with small modifications. That scenario's documentation is in our [templating](./templating.md) page, and differs from user-defined templates.

Prior to release 5.0.0, whenever a user wanted to include templates which weren't built-in or weren't known to the generator at compile time, they'd need to follow the more involved approach of creating a custom generator as documented later in this document. Beginning in 5.0.0, a user may now provide additional supporting files and extensions to built-in templates via configuration. This feature requires using the external configuration file feature.

Consider that you might want to add some static documentation such as `AUTHORS.md` and a custom tooling script. Rather than a single file for API definitions you also want an implementation file and a separate interface file for each.

You might have an external configuration file named `config.yaml` which defines additional properties like this for a `kotlin` client generator:

```yaml
additionalProperties:
  artifactId: kotlin-petstore-client
  serializableModel: "true"
  dateLibrary: java8
```

You would generate via CLI with the command:

```shell script
openapi-generator generate -g kotlin -i spec.yaml -o outdir -c config.yaml
```

To support the above scenario with custom templates, ensure that you're pointing to your custom template directory and add a `files` node with template file definitions to your config:

```
templateDir: my_custom_templates
additionalProperties:
  artifactId: kotlin-petstore-client
  serializableModel: "true"
  dateLibrary: java8
files:
  AUTHORS.md: {}
  api_interfaces.mustache:
    templateType: API
    destinationFilename: Interface.kt
  api.mustache:
    templateType: API
    destinationFilename: Impl.kt
  other/check.mustache:
    folder: scripts
    destinationFilename: check.sh
    templateType: SupportingFiles
```

The keys under the `files` node are your template filenames. These honor the same resolution order as all other templates.

The above configuration will do the following:

* Copy `my_custom_templates/AUTHORS.md` to the generated output directory without processing via the template engine (due to template file extension). The empty object definition following `AUTHORS.md` allows the tool to infer the target output filename in the root of the output directory.
* Compile a user-provided `my_custom_templates/api_interfaces.mustache` following our usual API template compilation logic. That is, one file will be created per API; APIs are generated defined according to tags in your spec documentation. The destination filename of `Interface.kt` will act as a suffix for the filename. So, a tag of `Equipment` will output a corresponding `EquipmentInterface.kt`.
* Because `api.mustache` is the same mustache filename as used in your target generator (`kotlin` in this example), we support the following:
  - The destination filename provides a suffix for the generated output. APIs generate per tag in your specification. So, a tag of `Equipment` will output a corresponding `EquipmentImpl.kt`. This option will be used whether `api.mustache` targets a user customized template or a built-in template.
  - The built-in template will be used if you haven't provided an customized template. The kotlin generator defines the suffix as simply `.kt`, so this scenario would modify only the generated file suffixes according to the previous bullet point.
  - Your `api.mustache` will be used if it exists in your custom template directory. For generators with library options, such as `jvm-okhttp3` in the kotlin generator, your file must exist in the same relative location as the embedded template. For kotlin using the `jvm-okhttp3` library option, this file would need to be located at `my_custom_templates/libraries/jvm-okhttp/api.mustache`. See [templating](./templating.md) for more details.
* Compile `my_custom_templates/other/check.mustache` with the supporting files bundle, and output to `scripts/check.sh` in your output directory. Note that we don't currently support setting file flags on output, so scripts such as these will either have to be sourced rather than executed, or have file flags set separately after generation (external to our tooling).

The `templateType` option will default to `SupportingFiles`, so the option for `other/check.mustache` is redundant and provided to demonstrate the full template file configuration options. The available template types are:

* API
* APIDocs
* APITests
* Model
* ModelDocs
* ModelTests
* SupportingFiles

Excluding `SupportingFiles`, each of the above options may result in multiple files. API related types create a file per API. Model related types create a file for each model.

Note that user-defined templates will merge with built-in template definitions. If a supporting file with the sample template file path exists, it will be replaced with the user-defined template, otherwise the user-defined template will be added to the list of template files to compile. If the generator's built-in template is `model_docs.mustache` and you define `model-docs.mustache`, this will result in duplicated model docs (if `destinationFilename` differs) or undefined behavior as whichever template compiles last will overwrite the previous model docs (if `destinationFilename` matches the extension or suffix in the generator's code).

## Custom Generator (and Template)
 
<a id="creating-a-new-template"></a> If none of the built-in generators suit your needs and you need to do more than just modify the mustache templates to tweak generated code, you can create a brand new generator and its associated templates. OpenAPI Generator can help with this, using the `meta` command:

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar meta \
  -o out/generators/my-codegen -n my-codegen -p com.my.company.codegen
```

This will create a new directory `out/generators/my-codegen`, with all the files you need to get started - including a `README.md`. Once modified and compiled, you can use your new codegen just like any other, with your own custom-rolled logic.

These names can be anything you like. If you are building a client for the whitespace language, maybe  you'd use the options `-o out/generators/whitespace -n whitespace`. They can be the same, or different, it doesn't matter. The `-n` value will be become the template name.

**NOTE** Convention is to use kebab casing for names passed to `-n`. Example, `scala-finatra` would become `ScalaFinatraGenerator`.

### Use your new generator with the CLI

To compile your library, enter the `out/generators/my-codegen` directory, run `mvn package`.

**NOTE** Running your custom generator requires adding it to the classpath. This differs on [Windows](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/classpath.html) slightly from [unix](https://docs.oracle.com/javase/8/docs/technotes/tools/unix/classpath.html).
If you are running a Windows Subsystem for Linux or a shell such as gitbash, and have issues with the unix variant, try the Windows syntax below.
 
Now, execute the generator:

```sh
java -cp out/generators/my-codegen/target/my-codegen-openapi-generator-1.0.0.jar:modules/openapi-generator-cli/target/openapi-generator-cli.jar org.openapitools.codegen.OpenAPIGenerator
```

For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.
```
java -cp "out/generators/my-codegen/target/my-codegen-openapi-generator-1.0.0.jar;modules/openapi-generator-cli/target/openapi-generator-cli.jar" org.openapitools.codegen.OpenAPIGenerator
```

Note the `my-codegen` is an option for `-g` now, and you can use the usual arguments for generating your code:

```sh
java -cp out/codegens/customCodegen/target/my-codegen-openapi-generator-1.0.0.jar:modules/openapi-generator-cli/target/openapi-generator-cli.jar \
  org.openapitools.codegen.OpenAPIGenerator generate -g my-codegen \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
  -o ./out/myClient
```

For Windows users:
```
java -cp "out/codegens/customCodegen/target/my-codegen-openapi-generator-1.0.0.jar;modules/openapi-generator-cli/target/openapi-generator-cli.jar" \
  org.openapitools.codegen.OpenAPIGenerator generate -g my-codegen \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
  -o ./out/myClient
```

### Use your new generator with the maven plugin

Install your library to your local maven repository by running:

```
mvn clean install -f out/generators/my-codegen
```

This will install `org.openapitools:my-codegen-openapi-generator:1.0.0` to your local maven repository.

You can use this as additional dependency of the `openapi-generator-maven-plugin` plugin and use `my-codegen` as `generatorName` value:

```xml
<plugin>
  <groupId>org.openapitools</groupId>
  <artifactId>openapi-generator-maven-plugin</artifactId>
  <version>${openapi-generator-version}</version>
  <executions>
    <execution>
      <id>generate-client-code</id>
      <goals>
        <goal>generate</goal>
      </goals>
      <configuration>
        <generatorName>my-codegen</generatorName>
        <!-- other configuration ... -->
      </configuration>
    </execution>
  </executions>
  <dependencies>
    <dependency>
      <groupId>org.openapitools</groupId>
      <artifactId>my-codegen-openapi-generator</artifactId>
      <version>1.0.0</version>
    </dependency>
  </dependencies>
</plugin>
```

If you publish your artifact to a distant maven repository, do not forget to add this repository as `pluginRepository` for your project.

## Selective generation

You may not want to generate *all* models in your project. Likewise, you may want just one or two apis to be written.  If that's the case, you can use system properties or [global properties](./global-properties.md) to control the output.

The default is generate *everything* supported by the specific library. Once you enable a feature, it will restrict the contents generated:

```sh
# generate only models
--global-property models

# generate only apis
--global-property apis

# generate only supporting files
--global-property supportingFiles

# generate models and supporting files
--global-property models,supportingFiles
```

To control the specific files being generated, you can pass a CSV list of what you want:
```sh
# generate the User and Pet models only
--global-property models="User:Pet"

# generate the User model and the supportingFile `StringUtil.java`:
--global-property models=User,supportingFiles=StringUtil.java
```

To control generation of docs and tests for api and models, pass false to the option. For api, these options are  `--global-property apiTests=false,apiDocs=false`. For models, `--global-property modelTests=false,modelDocs=false`.
These options default to true and don't limit the generation of the feature options listed above (like `--global-property api`):

```sh
# generate only models (with tests and documentation)
--global-property models

# generate only models (with tests but no documentation)
--global-property models,modelDocs=false

# generate only User and Pet models (no tests and no documentation)
--global-property models="User:Pet",modelTests=false

# generate only apis (without tests)
--global-property apis,apiTests=false

# generate only apis (modelTests option is ignored)
--global-property apis,modelTests=false
```

When using selective generation, _only_ the templates needed for the specific generation will be used.

To skip models defined as the form parameters in "requestBody", please use `skipFormModel` (default to `true`) (this option is introduced at v3.2.2 and `true` by default starting from v5.x).

```sh
--global-property skipFormModel=true
```

This option will be helpful to skip model generation due to the form parameter, which is defined differently in OAS3 as there's no form parameter in OAS3

## Ignore file format

OpenAPI Generator supports a `.openapi-generator-ignore` file, similar to `.gitignore` or `.dockerignore` you're probably already familiar with.

The ignore file allows for better control over overwriting existing files than the `--skip-overwrite` flag. With the ignore file, you can specify individual files or directories can be ignored. This can be useful, for example if you only want a subset of the generated code.

Examples:

```sh
# OpenAPI Generator Ignore
# Lines beginning with a # are comments

# This should match build.sh located anywhere.
build.sh

# Matches build.sh in the root
/build.sh

# Exclude all recursively
docs/**

# Explicitly allow files excluded by other rules
!docs/UserApi.md

# Recursively exclude directories named Api
# You can't negate files below this directory.
src/**/Api/

# When this file is nested under /Api (excluded above),
# this rule is ignored because parent directory is excluded by previous rule.
!src/**/PetApiTests.cs

# Exclude a single, nested file explicitly
src/Org.OpenAPITools.Test/Model/AnimalFarmTests.cs
```

The `.openapi-generator-ignore` file must exist in the root of the output directory.

Upon first code generation, you may also pass the CLI option `--ignore-file-override=/path/to/ignore_file` for greater control over generated outputs. Note that this is a complete override, and will override the `.openapi-generator-ignore` file in an output directory when regenerating code.

Editor support for `.openapi-generator-ignore` files is available in IntelliJ via the [.ignore plugin](https://plugins.jetbrains.com/plugin/7495--ignore).

## Customizing the generator

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```sh
$ ls -1 modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/
AbstractJavaJAXRSServerCodegen.java
AbstractTypeScriptClientCodegen.java
... (results omitted)
TypeScriptAngularClientCodegen.java
TypeScriptNodeClientCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc. you can use a json config file to pass the values.

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
  -g java \
  -o samples/client/petstore/java \
  -c path/to/config.json
```
and `config.json` contains the following as an example:
```json
{
  "apiPackage" : "petstore"
}
```
You can use also `config.yml` with following equivalent example:
```yaml
apiPackage: "petstore"
```

Supported config options can be different per language. Running `config-help -g {lang}` will show available options.
**These options are applied via configuration file (e.g. config.json or config.yml) or by passing them with `-p {optionName}={optionValue}`**. (If `-p {optionName}` does not work, please open a [ticket](https://github.com/openapitools/openapi-generator/issues/new) and we'll look into it)

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar config-help -g java
```

Output

```
CONFIG OPTIONS
	modelPackage
	    package for generated models

	apiPackage
	    package for generated api classes
...... (results omitted)
	library
	    library template (sub-template) to use:
	    jersey1 - HTTP client: Jersey client 1.18. JSON processing: Jackson 2.4.2
	    jersey2 - HTTP client: Jersey client 2.6
	    feign - HTTP client: Netflix Feign 8.1.1.  JSON processing: Jackson 2.6.3
	    okhttp-gson (default) - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1
	    retrofit - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1 (Retrofit 1.9.0)
        retrofit2 - HTTP client: OkHttp 2.5.0. JSON processing: Gson 2.4 (Retrofit 2.0.0-beta2)
        google-api-client - HTTP client: google-api-client 1.23.0. JSON processing: Jackson 2.8.9
        rest-assured - HTTP client: rest-assured : 4.3.0. JSON processing: Gson 2.8.6. Only for Java8
```

Your config file for Java can look like

```json
{
  "groupId":"com.my.company",
  "artifactId":"MyClient",
  "artifactVersion":"1.2.0",
  "library":"feign"
}
```

Or if you prefer yaml format it can look like

```yaml
groupId: "com.my.company"
artifactId: "MyClient"
artifactVersion: "1.2.0"
library: "feign"
```

For all the unspecified options default values will be used.

Another way to override default options is to extend the config class for the specific language.
To change, for example, the prefix for the Objective-C generated files, simply subclass the `ObjcClientCodegen.java`:

```java
package com.mycompany.openapitools.codegen;

import org.openapitools.codegen.languages.*;

public class MyObjcCodegen extends ObjcClientCodegen {
    static {
        PREFIX = "HELLO";
    }
}
```

and specify the `classname` when running the generator:

```
-g com.mycompany.openapitools.codegen.MyObjcCodegen
```

Your subclass will now be loaded and overrides the `PREFIX` value in the superclass.

## Bringing your own models

Sometimes you don't want a model generated.  In this case, you can simply specify an import mapping to tell
the codegen what _not_ to create.  When doing this, every location that references a specific model will
refer back to your classes.  Note, this may not apply to all languages...

To specify an import mapping, use the `--import-mappings` argument and specify the model-to-import logic as such:

```
--import-mappings Pet=my.models.MyPet
```

Or for multiple mappings:

```
--import-mappings Pet=my.models.MyPet,Order=my.models.MyOrder
```
or
```
--import-mappings Pet=my.models.MyPet --import-mappings Order=my.models.MyOrder
```
