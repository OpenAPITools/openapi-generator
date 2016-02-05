# Swagger Code Generator

[![Build Status](https://travis-ci.org/swagger-api/swagger-codegen.svg)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project)
[![PR Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/pr)](http://issuestats.com/github/swagger-api/swagger-codegen) [![Issue Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/issue)](http://issuestats.com/github/swagger-api/swagger-codegen)

:star::star::star: If you would like to contribute, please refer to [guidelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md) and a list of [open tasks](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aopen+is%3Aissue+label%3A%22Need+community+contribution%22).:star::star::star:

## Overview
This is the swagger codegen project, which allows generation of client libraries automatically from a Swagger-compliant server.

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
      - [Node.js](#nodejs)
      - [PHP Slim](#php-slim)
      - [PHP Silex](#php-silex)
      - [Python Flask (Connexion)](#python-flask-connexion)
      - [Ruby Sinatra](#ruby-sinatra)
      - [Scala Scalatra](#scala-scalatra)
      - [Java JAX-RS (Java JAX-RS (Jersey v1.18)](#java-jax-rs-jersey-v118)
      - [Java JAX-RS (Apache CXF 3)](#java-jax-rs-apache-cxf-3)
      - [Java Spring MVC](#java-spring-mvc)
      - [Haskell Servant](#haskell-servant)
    - [To build the codegen library](#to-build-the-codegen-library)
  - [Workflow Integration](#workflow-integration)
  - [Online Generators](#online-generators)
  - [Guidelines for Contribution](https://github.com/swagger-api/swagger-codegen/wiki/Guidelines-for-Contribution)
  - [License](#license)


## Compatibility
The OpenAPI Specification has undergone 3 revisions since initial creation in 2010.  The swagger-codegen project has the following compatibilies with the OpenAPI Specification:

Swagger Codegen Version    | Release Date | OpenAPI Spec compatibility | Notes
-------------------------- | ------------ | -------------------------- | -----
2.1.6-SNAPSHOT             |              | 1.0, 1.1, 1.2, 2.0   | [master](https://github.com/swagger-api/swagger-codegen)
2.1.5 (**current stable**) | 2015-01-06   | 1.0, 1.1, 1.2, 2.0   | [tag v2.1.5](https://github.com/swagger-api/swagger-codegen/tree/v2.1.5)
2.0.17                     | 2014-08-22   | 1.1, 1.2             | [tag v2.0.17](https://github.com/swagger-api/swagger-codegen/tree/v2.0.17)
1.0.4                      | 2012-04-12   | 1.0, 1.1             | [tag v1.0.4](https://github.com/swagger-api/swagger-codegen/tree/swagger-codegen_2.9.1-1.1)


### Prerequisites
If you're looking for the latest stable version, you can grab it directly from maven central (you'll need java 7 runtime at a minimum):

```
wget http://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.1.4/swagger-codegen-cli-2.1.4.jar swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar help
```

On a mac, it's even easier with `brew`:
```
brew install swagger-codegen
```

To build from source, you need the following installed and available in your $PATH:

* [Java 7 or 8](http://java.oracle.com)

* [Apache maven 3.0.3 or greater](http://maven.apache.org/)

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
mvn package
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


### To generate a sample client library
You can build a client against the swagger sample [petstore](http://petstore.swagger.io) API as follows:

```
./bin/java-petstore.sh
```

(On Windows, run `./bin/windows/java-petstore.bat` instead)

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
        swagger generate - Generate code with chosen lang

SYNOPSIS
        swagger generate [(-a <authorization> | --auth <authorization>)]
                [(-c <configuration file> | --config <configuration file>)]
                [-D <system properties>]
                (-i <spec file> | --input-spec <spec file>)
                (-l <language> | --lang <language>)
                [(-o <output directory> | --output <output directory>)]
                [(-t <template directory> | --template-dir <template directory>)]
                [(-v | --verbose)]
                [(-s | --skip-overwrite)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the swagger definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

        -c <configuration file>, --config <configuration file>
            Path to json configuration file. File content should be in a json
            format {"optionKey":"optionValue", "optionKey1":"optionValue1"...}
            Supported options can be different for each language. Run
            config-help -l {lang} command for language specific config options.

        -D <system properties>
            sets specified system properties in the format of
            name=value,name=value

        -i <spec file>, --input-spec <spec file>
            location of the OpenAPI Spec, as URL or file (required)

        -l <language>, --lang <language>
            client language to generate (maybe class name in classpath,
            required)

        -o <output directory>, --output <output directory>
            where to write the generated files (current dir by default)

        -t <template directory>, --template-dir <template directory>
            folder containing the template files

        -v, --verbose
            verbose mode

        -s , --skip-overwrite
            specifies if the existing files should be overwritten during
            the generation
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

When using selective generation, _only_ the templates needed for the specific generation will be used.

### Customizing the generator

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```
$ ls -1 modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/
AbstractJavaJAXRSServerCodegen.java
AbstractTypeScriptClientCodegen.java
AkkaScalaClientCodegen.java
AndroidClientCodegen.java
AspNet5ServerCodegen.java
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
JavascriptClientCodegen.java
NodeJSServerCodegen.java
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
Supported config options can be different per language. Running `config-help -l {lang}` will show available options.  **These options are applied
by passing them with `-D{optionName}={optionValue}**.

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

You can also use the codegen to generate a server for a couple different frameworks.  Take a look here:

### Node.js

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l nodejs \
  -o samples/server/petstore/nodejs
```

### PHP Slim

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l slim \
  -o samples/server/petstore/slim
```

### PHP Silex

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l silex \
  -o samples/server/petstore/silex
```

### Python Flask (Connexion)

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l python-flask \
  -o samples/server/petstore/flaskConnexion
```

### Ruby Sinatra

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l sinatra \
  -o samples/server/petstore/sinatra
```

### Scala Scalatra
```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l scalatra \
  -o samples/server/petstore/scalatra
```

### Java JAX-RS (Jersey v1.18)

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l jaxrs \
  -o samples/server/petstore/jaxrs-jersey
```

### Java JAX-RS (Apache CXF 3)

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l jaxrs-cxf \
  -o samples/server/petstore/jaxrs-cxf
```

### Java Spring MVC

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l spring-mvc \
  -o samples/server/petstore/spring-mvc
```

### Haskell Servant

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l haskell-servant \
  -o samples/server/petstore/haskell-servant
```

### ASP.NET 5 Web API

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l aspnet5 \
  -o samples/server/petstore/aspnet5
```

### To build the codegen library

This will create the swagger-codegen library from source.

```
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts

## Workflow integration

You can use the [swagger-codegen-maven-plugin](modules/swagger-codegen-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.


## Online generators

One can also generate API client or server using the online generators (https://generator.swagger.io)

For example, to generate Ruby API client, simply send the following HTTP request using curl:
```
curl -X POST -H "content-type:application/json" -d '{"swaggerUrl":"http://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/ruby
```
Then you will receieve a JSON response with the URL to download the zipped code.

Guidelines for Contribution
---------------------------

Please refer to this [page](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md)

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
