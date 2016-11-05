# Swagger Code Generator

[![Build Status](https://img.shields.io/travis/swagger-api/swagger-codegen.svg?label=Petstore%20Integration%20Test)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Run Status](https://img.shields.io/shippable/5782588a3be4f4faa56c5bea.svg?label=Mustache%20Template%20Test)](https://app.shippable.com/projects/5782588a3be4f4faa56c5bea)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/swagger-api/swagger-codegen?branch=master&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/swagger-codegen-wh2wu)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project)
[![PR Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/pr)](http://issuestats.com/github/swagger-api/swagger-codegen) [![Issue Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/issue)](http://issuestats.com/github/swagger-api/swagger-codegen)

:star::star::star: If you would like to contribute, please refer to [guidelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md) and a list of [open tasks](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aopen+is%3Aissue+label%3A%22Need+community+contribution%22).:star::star::star:

:notebook_with_decorative_cover: For more information, please refer to the [Wiki page](https://github.com/swagger-api/swagger-codegen/wiki) and [FAQ](https://github.com/swagger-api/swagger-codegen/wiki/FAQ) :notebook_with_decorative_cover:

:warning: If the OpenAPI/Swagger spec is obtained from an untrusted source, please make sure you've reviewed the spec before using Swagger Codegen to generate the API client, server stub or documentation as [code injection](https://en.wikipedia.org/wiki/Code_injection) may occur :warning:

## Overview
This is the swagger codegen project, which allows generation of API client libraries, server stubs and documentation automatically given an [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification).

Check out [Swagger-Spec](https://github.com/OAI/OpenAPI-Specification) for additional information about the Swagger project, including additional libraries with support for other languages and more.

# Table of contents

  - [Swagger Code Generator](#swagger-code-generator)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - Installation
    - [Compatibility](#compatibility)
    - [Prerequisites](#prerequisites)
      - [OS X Users](#os-x-users)
    - [Building](#building)
    - [Docker](#docker)
      - [Build and run](#build-and-run-using-docker)
      - [Run docker in Vagrant](#run-docker-in-vagrant)
      - [Public Docker image](#public-docker-image)
    - [Homebrew](#homebrew)
  - [Getting Started](#getting-started)
  - Generators
    - [To generate a sample client library](#to-generate-a-sample-client-library)
    - [Generating libraries from your server](#generating-libraries-from-your-server)
    - [Modifying the client library format](#modifying-the-client-library-format)
    - [Making your own codegen modules](#making-your-own-codegen-modules)
    - [Where is Javascript???](#where-is-javascript)
    - [Generating a client from local files](#generating-a-client-from-local-files)
    - [Customizing the generator](#customizing-the-generator)
    - [Validating your OpenAPI Spec](#validating-your-openapi-spec)
    - [Generating dynamic html api documentation](#generating-dynamic-html-api-documentation)
    - [Generating static html api documentation](#generating-static-html-api-documentation)
    - [To build a server stub](#to-build-a-server-stub)
    - [To build the codegen library](#to-build-the-codegen-library)
  - [Workflow Integration](#workflow-integration)
  - [Github Integration](#github-integration)
  - [Online Generators](#online-generators)
  - [Guidelines for Contribution](https://github.com/swagger-api/swagger-codegen/wiki/Guidelines-for-Contribution)
  - [Companies/Projects using Swagger Codegen](#companiesprojects-using-swagger-codegen)
  - [Swagger Codegen Core Team](#swagger-codegen-core-team)
  - [License](#license)


## Compatibility
The OpenAPI Specification has undergone 3 revisions since initial creation in 2010.  The swagger-codegen project has the following compatibilies with the OpenAPI Specification:

Swagger Codegen Version    | Release Date | OpenAPI Spec compatibility | Notes
-------------------------- | ------------ | -------------------------- | -----
2.3.0 (upcoming minor release) | TBD   | 1.0, 1.1, 1.2, 2.0   | Minor release with breaking changes
2.2.2 (upcoming patch release) | TBD   | 1.0, 1.1, 1.2, 2.0   | Patch release (without breaking changes)
2.2.1 (**current stable**) | 2016-08-07   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.1](https://github.com/swagger-api/swagger-codegen/tree/v2.2.1)
2.1.6 | 2016-04-06   | 1.0, 1.1, 1.2, 2.0   | [tag v2.1.6](https://github.com/swagger-api/swagger-codegen/tree/v2.1.6)
2.0.17                     | 2014-08-22   | 1.1, 1.2             | [tag v2.0.17](https://github.com/swagger-api/swagger-codegen/tree/v2.0.17)
1.0.4                      | 2012-04-12   | 1.0, 1.1             | [tag v1.0.4](https://github.com/swagger-api/swagger-codegen/tree/swagger-codegen_2.9.1-1.1)


### Prerequisites
If you're looking for the latest stable version, you can grab it directly from maven central (you'll need java 7 runtime at a minimum):

```
wget http://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.2.1/swagger-codegen-cli-2.2.1.jar -O swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar help
```

On a mac, it's even easier with `brew`:
```
brew install swagger-codegen
```

To build from source, you need the following installed and available in your $PATH:

* [Java 7 or 8](http://java.oracle.com)

* [Apache maven 3.3.3 or greater](http://maven.apache.org/)

#### OS X Users
Don't forget to install Java 7 or 8. You probably have 1.6.

Export JAVA_HOME in order to use the supported Java version:
```
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```

### Building

After cloning the project, you can build it from source with this command:
```
mvn clean package
```

### Docker
#### Build and run using docker

```
git clone https://github.com/swagger-api/swagger-codegen
cd swagger-codegen
./run-in-docker.sh mvn package
 ```



#### Run Docker in Vagrant
Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
 ```
git clone http://github.com/swagger-api/swagger-codegen.git
cd swagger-codegen
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
 ```

#### Public Docker image

https://hub.docker.com/r/swaggerapi/swagger-generator/

### Homebrew
To install, run `brew install swagger-codegen`

Here is an example usage:
```
swagger-codegen generate -i http://petstore.swagger.io/v2/swagger.json -l ruby -o /tmp/test/
```
## Getting Started

To generate a PHP client for http://petstore.swagger.io/v2/swagger.json, please run the following
```sh
git clone https://github.com/swagger-api/swagger-codegen
cd swagger-codegen
mvn clean package
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
   -i http://petstore.swagger.io/v2/swagger.json \
   -l php \
   -o /var/tmp/php_api_client
```
(if you're on Windows, replace the last command with `java -jar modules\swagger-codegen-cli\target\swagger-codegen-cli.jar generate -i http://petstore.swagger.io/v2/swagger.json -l php -o c:\temp\php_api_client`)

You can also download the JAR (latest release) directly from [maven.org](http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/2.2.1/swagger-codegen-cli-2.2.1.jar )

To get a list of **general** options available, please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar help generate`

To get a list of PHP specified options (which can be passed to the generator with a config file via the `-c` option), please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l php`

## Generators

### To generate a sample client library
You can build a client against the swagger sample [petstore](http://petstore.swagger.io) API as follows:

```
./bin/java-petstore.sh
```

(On Windows, run `.\bin\windows\java-petstore.bat` instead)

This will run the generator with this command:

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java
```

with a number of options.  You can get the options with the `help generate` command:

```
NAME
        swagger-codegen-cli generate - Generate code with chosen lang

SYNOPSIS
        swagger-codegen-cli generate
                [(-a <authorization> | --auth <authorization>)]
                [--additional-properties <additional properties>]
                [--api-package <api package>] [--artifact-id <artifact id>]
                [--artifact-version <artifact version>]
                [(-c <configuration file> | --config <configuration file>)]
                [-D <system properties>] [--group-id <group id>]
                (-i <spec file> | --input-spec <spec file>)
                [--import-mappings <import mappings>]
                [--instantiation-types <instantiation types>]
                [--invoker-package <invoker package>]
                (-l <language> | --lang <language>)
                [--language-specific-primitives <language specific primitives>]
                [--library <library>] [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)]
                [(-s | --skip-overwrite)]
                [(-t <template directory> | --template-dir <template directory>)]
                [--type-mappings <type mappings>] [(-v | --verbose)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the swagger definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

        --additional-properties <additional properties>
            sets additional properties that can be referenced by the mustache
            templates in the format of name=value,name=value

        --api-package <api package>
            package for generated api classes

        --artifact-id <artifact id>
            artifactId in generated pom.xml

        --artifact-version <artifact version>
            artifact version in generated pom.xml

        -c <configuration file>, --config <configuration file>
            Path to json configuration file. File content should be in a json
            format {"optionKey":"optionValue", "optionKey1":"optionValue1"...}
            Supported options can be different for each language. Run
            config-help -l {lang} command for language specific config options.

        -D <system properties>
            sets specified system properties in the format of
            name=value,name=value

        --group-id <group id>
            groupId in generated pom.xml

        -i <spec file>, --input-spec <spec file>
            location of the swagger spec, as URL or file (required)


        --import-mappings <import mappings>
            specifies mappings between a given class and the import that should
            be used for that class in the format of type=import,type=import

        --instantiation-types <instantiation types>
            sets instantiation type mappings in the format of
            type=instantiatedType,type=instantiatedType.For example (in Java):
            array=ArrayList,map=HashMap. In other words array types will get
            instantiated as ArrayList in generated code.

        --invoker-package <invoker package>
            root package for generated code

        -l <language>, --lang <language>
            client language to generate (maybe class name in classpath,
            required)

        --language-specific-primitives <language specific primitives>
            specifies additional language specific primitive types in the format
            of type1,type2,type3,type3. For example:
            String,boolean,Boolean,Double

        --library <library>
            library template (sub-template)

        --model-package <model package>
            package for generated models

        -o <output directory>, --output <output directory>
            where to write the generated files (current dir by default)

        -s, --skip-overwrite
            specifies if the existing files should be overwritten during the
            generation.

        -t <template directory>, --template-dir <template directory>
            folder containing the template files

        --type-mappings <type mappings>
            sets mappings between swagger spec types and generated code types in
            the format of swaggerType=generatedType,swaggerType=generatedType.
            For example: array=List,map=Map,string=String

        -v, --verbose
            verbose mode

```

You can then compile and run the client, as well as unit tests against it:

```
cd samples/client/petstore/java
mvn package
```

Other languages have petstore samples, too:
```
./bin/android-petstore.sh
./bin/java-petstore.sh
./bin/objc-petstore.sh
```

### Generating libraries from your server
It's just as easy--just use the `-i` flag to point to either a server or file.

### Modifying the client library format
Don't like the default swagger client syntax?  Want a different language supported?  No problem!  Swagger codegen processes mustache templates with the [jmustache](https://github.com/samskivert/jmustache) engine.  You can modify our templates or make your own.

You can look at `modules/swagger-codegen/src/main/resources/${your-language}` for examples.  To make your own templates, create your own files and use the `-t` flag to specify your template folder.  It actually is that easy.

### Making your own codegen modules
If you're starting a project with a new language and don't see what you need, swagger-codegen can help you create a project to generate your own libraries:

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a README.md. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

You would then compile your library in the `output/myLibrary` folder with `mvn package` and execute the codegen like such:

```
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.SwaggerCodegen
```

Note the `myClientCodegen` is an option now, and you can use the usual arguments for generating your library:

```
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar \
  io.swagger.codegen.SwaggerCodegen generate -l myClientCodegen\
  -i http://petstore.swagger.io/v2/swagger.json \
  -o myClient
```

### Where is Javascript???
See our [javascript library](http://github.com/swagger-api/swagger-js)--it's completely dynamic and doesn't require
static code generation.
There is a third-party component called [swagger-js-codegen](https://github.com/wcandillon/swagger-js-codegen) that can generate angularjs or nodejs source code from a OpenAPI Specification.

:exclamation: On Dec 7th 2015, a Javascript API client generator has been added by @jfiala.

### Generating a client from local files
If you don't want to call your server, you can save the OpenAPI Spec files into a directory and pass an argument
to the code generator like this:

```
-i ./modules/swagger-codegen/src/test/resources/2_0/petstore.json
```

Great for creating libraries on your ci server, from the [Swagger Editor](http://editor.swagger.io)... or while coding on an airplane.

### Selective generation
You may not want to generate *all* models in your project.  Likewise you may want just one or two apis to be written.  If that's the case, you can use system properties to control the output:

The default is generate *everything* supported by the specific library.  Once you enable a feature, it will restrict the contents generated:

```
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
```
# generate the User and Pet models only
-Dmodels=User,Pet

# generate the User model and the supportingFile `StringUtil.java`:
-Dmodels=User -DsupportingFiles=StringUtil.java
```

To control generation of docs and tests for api and models, pass false to the option. For api, these options are  `-DapiTests=false` and `-DapiDocs=false`. For models, `-DmodelTests=false` and `-DmodelDocs=false`.
These options default to true and don't limit the generation of the feature options listed above (like `-Dapi`):

```
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

### Ignore file format

Swagger codegen supports a `.swagger-codegen-ignore` file, similar to `.gitignore` or `.dockerignore` you're probably already familiar with.

The ignore file allows for better control over overwriting existing files than the `--skip-overwrite` flag. With the ignore file, you can specify individual files or directories can be ignored. This can be useful, for example if you only want a subset of the generated code.

Examples:

```
# Swagger Codegen Ignore
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
src/IO.Swagger.Test/Model/AnimalFarmTests.cs
```

The `.swagger-codegen-ignore` file must exist in the root of the output directory.

### Customizing the generator

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```
$ ls -1 modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/
AbstractJavaJAXRSServerCodegen.java
AbstractTypeScriptClientCodegen.java
AkkaScalaClientCodegen.java
AndroidClientCodegen.java
AspNet5ServerCodegen.java
AspNetCoreServerCodegen.java
AsyncScalaClientCodegen.java
CSharpClientCodegen.java
ClojureClientCodegen.java
CsharpDotNet2ClientCodegen.java
DartClientCodegen.java
FlashClientCodegen.java
FlaskConnexionCodegen.java
GoClientCodegen.java
HaskellServantCodegen.java
JMeterCodegen.java
JavaCXFServerCodegen.java
JavaClientCodegen.java
JavaInflectorServerCodegen.java
JavaJerseyServerCodegen.java
JavaResteasyServerCodegen.java
JavascriptClientCodegen.java
NodeJSServerCodegen.java
NancyFXServerCodegen
ObjcClientCodegen.java
PerlClientCodegen.java
PhpClientCodegen.java
PythonClientCodegen.java
Qt5CPPGenerator.java
RubyClientCodegen.java
ScalaClientCodegen.java
ScalatraServerCodegen.java
SilexServerCodegen.java
SinatraServerCodegen.java
SlimFrameworkServerCodegen.java
SpringMVCServerCodegen.java
StaticDocCodegen.java
StaticHtmlGenerator.java
SwaggerGenerator.java
SwaggerYamlGenerator.java
SwiftCodegen.java
TizenClientCodegen.java
TypeScriptAngularClientCodegen.java
TypeScriptNodeClientCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc. you can use a json config file to pass the values.

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java \
  -c path/to/config.json
```
and `config.json` contains the following as an example:
```
{
  "apiPackage" : "petstore"
}
```

Supported config options can be different per language. Running `config-help -l {lang}` will show available options.  
**These options are applied via configuration file (e.g. config.json) or by passing them with `-D{optionName}={optionValue}**. (If `-D{optionName}` does not work, please open a [ticket](https://github.com/swagger-api/swagger-codegen/issues/new) and we'll look into it)

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l java
```

Output

```
CONFIG OPTIONS
	modelPackage
	    package for generated models

	apiPackage
	    package for generated api classes

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. Default: true

	invokerPackage
	    root package for generated code

	groupId
	    groupId in generated pom.xml

	artifactId
	    artifactId in generated pom.xml

	artifactVersion
	    artifact version in generated pom.xml

	sourceFolder
	    source folder for generated code

	localVariablePrefix
	    prefix for generated code members and local variables

	serializableModel
	    boolean - toggle "implements Serializable" for generated models

	library
	    library template (sub-template) to use:
	    <default> - HTTP client: Jersey client 1.18. JSON processing: Jackson 2.4.2
	    jersey2 - HTTP client: Jersey client 2.6
	    feign - HTTP client: Netflix Feign 8.1.1.  JSON processing: Jackson 2.6.3
	    okhttp-gson - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1
	    retrofit - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1 (Retrofit 1.9.0)
        retrofit2 - HTTP client: OkHttp 2.5.0. JSON processing: Gson 2.4 (Retrofit 2.0.0-beta2)
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

For all the unspecified options default values will be used.

Another way to override default options is to extend the config class for the specific language.
To change, for example, the prefix for the Objective-C generated files, simply subclass the ObjcClientCodegen.java:

```java
package com.mycompany.swagger.codegen;

import io.swagger.codegen.languages.*;

public class MyObjcCodegen extends ObjcClientCodegen {
    static {
        PREFIX = "HELO";
    }
}
```

and specify the `classname` when running the generator:

```
-l com.mycompany.swagger.codegen.MyObjcCodegen
```

Your subclass will now be loaded and overrides the `PREFIX` value in the superclass.

### Bringing your own models

Sometimes you don't want a model generated.  In this case, you can simply specify an import mapping to tell
the codegen what _not_ to create.  When doing this, every location that references a specific model will
refer back to your classes.  Note, this may not apply to all languages...

To specify an import mapping, use the `--import-mappings` argument and specify the model-to-import logic as such:

```
--import-mappings Pet=my.models.MyPet
```

Or for multiple mappings:

```
Pet=my.models.MyPet,Order=my.models.MyOrder
```

### Validating your OpenAPI Spec

You have options.  The easiest is to use our [online validator](https://github.com/swagger-api/validator-badge) which not only will let you validate your spec, but with the debug flag, you can see what's wrong with your spec.  For example:

http://online.swagger.io/validator/debug?url=http://petstore.swagger.io/v2/swagger.json

### Generating dynamic html api documentation

To do so, just use the `-l dynamic-html` flag when reading a spec file.  This creates HTML documentation that is available as a single-page application with AJAX.  To view the documentation:

```
cd samples/dynamic-html/
npm install
node .
```

Which launches a node.js server so the AJAX calls have a place to go.


### Generating static html api documentation

To do so, just use the `-l html` flag when reading a spec file.  This creates a single, simple HTML file with embedded css so you can ship it as an email attachment, or load it from your filesystem:

```
cd samples/html/
open index.html
```


### To build a server stub

Please refer to https://github.com/swagger-api/swagger-codegen/wiki/Server-stub-generator-HOWTO for more information.

### To build the codegen library

This will create the swagger-codegen library from source.

```
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts

## Workflow integration

You can use the [swagger-codegen-maven-plugin](modules/swagger-codegen-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.

## GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)
 
 2) Generate the SDK
```
 java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
 -i modules/swagger-codegen/src/test/resources/2_0/petstore.json -l perl \
 --git-user-id "wing328" \
 --git-repo-id "petstore-perl" \
 --release-note "Github integration demo" \
 -o /var/tmp/perl/petstore
```
 3) Push the SDK to GitHub
```
cd /var/tmp/perl/petstore
/bin/sh ./git_push.sh
```

## Online generators

One can also generate API client or server using the online generators (https://generator.swagger.io)

For example, to generate Ruby API client, simply send the following HTTP request using curl:
```
curl -X POST -H "content-type:application/json" -d '{"swaggerUrl":"http://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/ruby
```
Then you will receieve a JSON response with the URL to download the zipped code.

To customize the SDK, you can `POST` to `https://generator.swagger.io/gen/clients/{language}` with the following HTTP body:
```
{
  "options": {},
  "swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"
}
```
in which the `options` for a language can be obtained by submitting a `GET` request to `https://generator.swagger.io/api/gen/clients/{language}`:

For example, `curl https://generator.swagger.io/api/gen/clients/python` returns
```
{
  "packageName":{
    "opt":"packageName",
    "description":"python package name (convention: snake_case).",
    "type":"string",
    "default":"swagger_client"
  },
  "packageVersion":{
    "opt":"packageVersion",
    "description":"python package version.",
    "type":"string",
    "default":"1.0.0"
  },
  "sortParamsByRequiredFlag":{
    "opt":"sortParamsByRequiredFlag",
    "description":"Sort method arguments to place required parameters before optional parameters.",
    "type":"boolean",
    "default":"true"
  }
}
```
To set package name to `pet_store`, the HTTP body of the request is as follows:
```
{
  "options": {
    "packageName": "pet_store"
  },
  "swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"
}
```
and here is the curl command:
```
curl -H "Content-type: application/json" -X POST -d '{"options": {"packageName": "pet_store"},"swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/python
```

Guidelines for Contribution
---------------------------

Please refer to this [page](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md)

Companies/Projects using Swagger Codegen
----------------------------------------
Here are some companies/projects using Swagger Codegen in production. To add your company/project to the list, please visit [README.md](https://github.com/swagger-api/swagger-codegen/blob/master/README.md) and click on the icon to edit the page.
- [Activehours](https://www.activehours.com/)
- [Acunetix](https://www.acunetix.com/)
- [Atlassian](https://www.atlassian.com/)
- [Avenida Compras S.A.](https://www.avenida.com.ar)
- [beemo](http://www.beemo.eu)
- [bitly](https://bitly.com)
- [Bufferfly Network](https://www.butterflynetinc.com/)
- [Cachet Financial](http://www.cachetfinancial.com/)
- [carpolo](http://www.carpolo.co/)
- [CloudBoost](https://www.CloudBoost.io/)
- [Conplement](http://www.conplement.de/)
- [Cummins] (http://www.cummins.com/)
- [Cupix](http://www.cupix.com)
- [DBBest Technologies](https://www.dbbest.com)
- [DecentFoX](http://decentfox.com/)
- [DocuSign](https://www.docusign.com)
- [Ergon](http://www.ergon.ch/)
- [EMC](https://www.emc.com/)
- [eureka](http://eure.jp/)
- [everystory.us](http://everystory.us)
- [Expected Behavior](http://www.expectedbehavior.com/)
- [Finder](http://en.finder.pl/)
- [FH Münster - University of Applied Sciences](http://www.fh-muenster.de)
- [Gear Zero Network](https://www.gearzero.ca)
- [Germin8](http://www.germin8.com)
- [goTransverse](http://www.gotransverse.com/api)
- [GraphHopper](https://graphhopper.com/)
- [Gravitate Solutions](http://gravitatesolutions.com/)
- [IMS Health](http://www.imshealth.com/en/solution-areas/technology-and-applications)
- [Interactive Intelligence](http://developer.mypurecloud.com/)
- [LANDR Audio](https://www.landr.com/)
- [Lascaux](http://www.lascaux.it/)
- [LiveAgent](https://www.ladesk.com/)
- [Kabuku](http://www.kabuku.co.jp/en)
- [Kuroi](http://kuroiwebdesign.com/)
- [Kuary](https://kuary.com/)
- [Mindera](http://mindera.com/)
- [Mporium](http://mporium.com/) 
- [nViso](http://www.nviso.ch/)
- [Okiok](https://www.okiok.com)
- [Onedata](http://onedata.org)
- [OrderCloud.io](http://ordercloud.io)
- [OSDN](https://osdn.jp)
- [PagerDuty](https://www.pagerduty.com)
- [Pepipost](https://www.pepipost.com)
- [Plexxi](http://www.plexxi.com)
- [Pixoneye](http://www.pixoneye.com/)
- [PostAffiliatePro](https://www.postaffiliatepro.com/)
- [QAdept](http://qadept.com/)
- [QuantiModo](https://quantimo.do/)
- [Rapid7](https://rapid7.com/)
- [Reload! A/S](https://reload.dk/) 
- [REstore](https://www.restore.eu)
- [Revault Sàrl](http://revault.ch)
- [Royal Bank of Canada (RBC)](http://www.rbc.com/canada.html)
- [SCOOP Software GmbH](http://www.scoop-software.de)
- [Skurt](http://www.skurt.com)
- [SmartRecruiters](https://www.smartrecruiters.com/)
- [StyleRecipe](http://stylerecipe.co.jp)
- [Svenska Spel AB](https://www.svenskaspel.se/)
- [ThoughtWorks](https://www.thoughtworks.com)
- [uShip](https://www.uship.com/)
- [W.UP](http://wup.hu/?siteLang=en)
- [Wealthfront](https://www.wealthfront.com/)
- [WEXO A/S](https://www.wexo.dk/)
- [Zalando](https://tech.zalando.com)
- [ZEEF.com](https://zeef.com/)


# Swagger Codegen Core Team

Swagger Codegen core team members are contributors who have been making significant contributions (review issues, fix bugs, make enhancements, etc) to the project on a regular basis.

## API Clients
| Languages     | Core Team (join date) | 
|:-------------|:-------------| 
| ActionScript | |
| C++      |  |  
| C#      | @jimschubert (2016/05/01) |  |
| Clojure | @xhh (2016/05/01) |
| Dart      |  |  
| Groovy     |  |  
| Go     |  @guohuang (2016/05/01) @neilotoole (2016/05/01) |  
| Java      | @cbornet (2016/05/01) @xhh (2016/05/01) @epaul (2016/06/04) |
| Java (Spring Cloud) | @cbornet (2016/07/19) |
| NodeJS/Javascript | @xhh (2016/05/01) | 
| ObjC      | @mateuszmackowiak (2016/05/09) |
| Perl      | @wing328 (2016/05/01) |
| PHP      | @arnested (2016/05/01) |
| Python   | @scottrw93 (2016/05/01) |
| Ruby      | @wing328 (2016/05/01) @zlx (2016/05/22) |
| Scala     |  |
| Swift     | @jaz-ah (2016/05/01)  @Edubits (2016/05/01) |
| TypeScript (Node) | @Vrolijkx (2016/05/01) | 
| TypeScript (Angular1) | @Vrolijkx (2016/05/01) | 
| TypeScript (Angular2) | @Vrolijkx (2016/05/01) |
| TypeScript (Fetch) |  |
## Server Stubs
| Languages     | Core Team (date joined) | 
|:------------- |:-------------| 
| C# ASP.NET5 |  @jimschubert (2016/05/01) |
| Go Server | @guohuang (2016/06/13) |
| Haskell Servant |  |
| Java Spring Boot | @cbornet (2016/07/19) |
| Java Spring MVC | @kolyjjj (2016/05/01) @cbornet (2016/07/19) |
| Java JAX-RS |  |
| NancyFX |  |
| NodeJS | @kolyjjj (2016/05/01) |  
| PHP Lumen | @abcsum (2016/05/01) |
| PHP Silex |  |
| PHP Slim  |  |
| Python Flask  |  |
| Ruby Sinatra     | @wing328 (2016/05/01) |  |
| Scala Scalatra |  |  |

## Template Creator
Here is a list of template creators:
 * API Clients:
   * Akka-Scala: @cchafer 
   * C++ REST: @Danielku15
   * C# (.NET 2.0): @who
   * Clojure: @xhh
   * Dart: @yissachar  
   * Groovy: @victorgit  
   * Go: @wing328  
   * Java (Retrofit): @0legg
   * Java (Retrofi2): @emilianobonassi
   * Java (Jersey2): @xhh 
   * Java (okhttp-gson): @xhh
   * Javascript/NodeJS: @jfiala  
   * Javascript (Closure-annotated Angular) @achew22
   * Perl: @wing328
   * Swift: @tkqubo
   * Swift 3: @hexelon
   * TypeScript (Node):  @mhardorf 
   * TypeScript (Angular1):  @mhardorf 
   * TypeScript (Fetch): @leonyu
   * TypeScript (Angular2): @roni-frantchi
 * Server Stubs
   * C# ASP.NET5: @jimschubert
   * C# NancyFX: @mstefaniuk
   * Erlang Server: @galaxie
   * Go Server: @guohuang
   * Haskell Servant: @algas
   * Java MSF4J: @sanjeewa-malalgoda
   * Java Spring Boot: @diyfr
   * Java Undertow: @stevehu
   * JAX-RS RestEasy: @chameleon82
   * JAX-RS CXF: @hiveship
   * JAX-RS CXF (CDI): @nickcmaynard
   * PHP Lumen: @abcsum
   * PHP Slim: @jfastnacht
   * Ruby on Rails 5: @zlx 
 * Documentation
   * HTML Doc 2: @jhitchcock
   * Confluence Wiki: @jhitchcock

## How to join the core team

Here are the requirements to become a core team member:
- rank within top 50 in https://github.com/swagger-api/swagger-codegen/graphs/contributors
  - to contribute, here are some good [starting points](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aopen+is%3Aissue+label%3A%22Need+community+contribution%22) 
- regular contributions to the project
  - about 3 hours per week
  - for contribution, it can be addressing issues, reviewing PRs submitted by others, submitting PR to fix bugs or make enhancements, etc

 To join the core team, please reach out to wing328hk@gmail.com (@wing328) for more information.
 
 To become a Template Creator, simply submit a PR for new API client (e.g. Rust, Elixir) or server stub (e.g. Ruby Grape) generator.

## License information on Generated Code

The Swagger Codegen project is intended as a benefit for users of the Swagger / Open API Specification.  The project itself has the [License](#license) as specified.  In addition, please understand the following points:

* The templates included with this project are subject to the [License](#license).
* Generated code is intentionally _not_ subject to the parent project license

When code is generated from this project, it shall be considered **AS IS** and owned by the user of the software.  There are no warranties--expressed or implied--for generated code.  You can do what you wish with it, and once generated, the code is your responsibility and subject to the licensing terms that you deem appropriate.

License
-------

Copyright 2016 SmartBear Software

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---
<img src="http://swagger.io/wp-content/uploads/2016/02/logo.jpg"/>
