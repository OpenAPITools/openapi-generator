---
id: customization
title: Customization
---

## Creating a new template

If none of the templates suit your needs, you can create a brand new template. OpenAPI Generator can help with this, using the `meta` command:

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar meta \
  -o out/generators/my-codegen -n my-codegen -p com.my.company.codegen
```

This will create a new directory `out/generators/my-codegen`, with all the files you need to get started - including a `README.md`. Once modified and compiled, you can use your new codegen just like any other, with your own custom-rolled logic.

These names can be anything you like. If you are building a client for the whitespace language, maybe  you'd use the options `-o out/generators/whitespace -n whitespace`. They can be the same, or different, it doesn't matter. The `-n` value will be become the template name.

**NOTE** Convention is to use kebab casing for names passed to `-n`. Example, `scala-finatra` would become `ScalaFinatraGenerator`.

### Use your new generator with the CLI

To compile your library, enter the `out/generators/my-codegen` directory, run `mvn package` and execute the generator:

```sh
java -cp out/generators/my-codegen/target/my-codegen-openapi-generator-1.0.0.jar:modules/openapi-generator-cli/target/openapi-generator-cli.jar org.openapitools.codegen.OpenAPIGenerator
```

For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.
```
java -cp out/generators/my-codegen/target/my-codegen-openapi-generator-1.0.0.jar;modules/openapi-generator-cli/target/openapi-generator-cli.jar org.openapitools.codegen.OpenAPIGenerator
```

Note the `my-codegen` is an option for `-g` now, and you can use the usual arguments for generating your code:

```sh
java -cp out/codegens/customCodegen/target/my-codegen-openapi-generator-1.0.0.jar:modules/openapi-generator-cli/target/openapi-generator-cli.jar \
  org.openapitools.codegen.OpenAPIGenerator generate -g my-codegen \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
  -o ./out/myClient
```

For Windows users:
```
java -cp out/codegens/customCodegen/target/my-codegen-openapi-generator-1.0.0.jar;modules/openapi-generator-cli/target/openapi-generator-cli.jar \
  org.openapitools.codegen.OpenAPIGenerator generate -g my-codegen \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
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
You may not want to generate *all* models in your project.  Likewise you may want just one or two apis to be written.  If that's the case, you can use system properties to control the output:

The default is generate *everything* supported by the specific library.  Once you enable a feature, it will restrict the contents generated:

```sh
# generate only models
java -Dmodels {opts}

# generate only apis
java -Dapis {opts}

# generate only supporting files
java -DsupportingFiles

# generate models and supporting files
java -Dmodels -DsupportingFiles
```

To control the specific files being generated, you can pass a CSV list of what you want:
```sh
# generate the User and Pet models only
-Dmodels=User,Pet

# generate the User model and the supportingFile `StringUtil.java`:
-Dmodels=User -DsupportingFiles=StringUtil.java
```

To control generation of docs and tests for api and models, pass false to the option. For api, these options are  `-DapiTests=false` and `-DapiDocs=false`. For models, `-DmodelTests=false` and `-DmodelDocs=false`.
These options default to true and don't limit the generation of the feature options listed above (like `-Dapi`):

```sh
# generate only models (with tests and documentation)
java -Dmodels {opts}

# generate only models (with tests but no documentation)
java -Dmodels -DmodelDocs=false {opts}

# generate only User and Pet models (no tests and no documentation)
java -Dmodels=User,Pet -DmodelTests=false {opts}

# generate only apis (without tests)
java -Dapis -DapiTests=false {opts}

# generate only apis (modelTests option is ignored)
java -Dapis -DmodelTests=false {opts}
```

When using selective generation, _only_ the templates needed for the specific generation will be used.

To skip models defined as the form parameters in "requestBody", please use `skipFormModel` (default to false) (this option is introduced at v3.2.2)

```sh
java -DskipFormModel=true
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
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
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
**These options are applied via configuration file (e.g. config.json or config.yml) or by passing them with `-D{optionName}={optionValue}`**. (If `-D{optionName}` does not work, please open a [ticket](https://github.com/openapitools/openapi-generator/issues/new) and we'll look into it)

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
        rest-assured - HTTP client: rest-assured : 3.1.0. JSON processing: Gson 2.6.1. Only for Java8
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

Or if you preffer yaml format it can look like

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
        PREFIX = "HELO";
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
