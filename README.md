# Swagger Code Generator

- Master (2.4.0): [![Build Status](https://img.shields.io/travis/swagger-api/swagger-codegen/master.svg?label=Petstore%20Integration%20Test)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Run Status](https://img.shields.io/shippable/5782588a3be4f4faa56c5bea.svg?label=Mustache%20Template%20Test)](https://app.shippable.com/projects/5782588a3be4f4faa56c5bea)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/swagger-api/swagger-codegen?branch=master&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/swagger-codegen-wh2wu)
[![Java Test](https://circleci.com/gh/swagger-api/swagger-codegen.svg?style=shield)](https://circleci.com/gh/swagger-api/swagger-codegen)
- 3.0.0:  [![Build Status](https://img.shields.io/travis/swagger-api/swagger-codegen/3.0.0.svg?label=Petstore%20Integration%20Test)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Run Status](https://img.shields.io/shippable/5782588a3be4f4faa56c5bea/3.0.0.svg?label=Mustache%20Template%20Test)](https://app.shippable.com/projects/5782588a3be4f4faa56c5bea)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/swagger-api/swagger-codegen?branch=3.0.0&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/swagger-codegen-wh2wu)
[![Java Test](https://circleci.com/gh/swagger-api/swagger-codegen/tree/3.0.0.svg?style=shield)](https://circleci.com/gh/swagger-api/swagger-codegen)


[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.swagger/swagger-codegen-project)
[![PR Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/pr)](http://issuestats.com/github/swagger-api/swagger-codegen) [![Issue Stats](http://issuestats.com/github/swagger-api/swagger-codegen/badge/issue)](http://issuestats.com/github/swagger-api/swagger-codegen)

:star::star::star: If you would like to contribute, please refer to [guidelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md) and a list of [open tasks](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22).:star::star::star:

:notebook_with_decorative_cover: For more information, please refer to the [Wiki page](https://github.com/swagger-api/swagger-codegen/wiki) and [FAQ](https://github.com/swagger-api/swagger-codegen/wiki/FAQ) :notebook_with_decorative_cover:

:warning: If the OpenAPI/Swagger spec is obtained from an untrusted source, please make sure you've reviewed the spec before using Swagger Codegen to generate the API client, server stub or documentation as [code injection](https://en.wikipedia.org/wiki/Code_injection) may occur :warning:

:rocket: ProductHunt: https://producthunt.com/posts/swagger-codegen :rocket:

## Overview
This is the swagger codegen project, which allows generation of API client libraries (SDK generation), server stubs and documentation automatically given an [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification). Currently, the following languages/frameworks are supported:

- **API clients**: **ActionScript**, **Ada**, **Apex**, **Bash**, **C#** (.net 2.0, 3.5 or later), **C++** (cpprest, Qt5, Tizen), **Clojure**, **Dart**, **Elixir**, **Elm**, **Eiffel**, **Erlang**, **Go**, **Groovy**, **Haskell** (http-client, Servant), **Java** (Jersey1.x, Jersey2.x, OkHttp, Retrofit1.x, Retrofit2.x, Feign, RestTemplate, RESTEasy, Vertx, Google API Client Library for Java), **Kotlin**, **Lua**, **Node.js** (ES5, ES6, AngularJS with Google Closure Compiler annotations) **Objective-C**, **Perl**, **PHP**, **PowerShell**, **Python**, **R**, **Ruby**, **Rust** (rust, rust-server), **Scala** (akka, http4s, swagger-async-httpclient), **Swift** (2.x, 3.x, 4.x), **Typescript** (Angular1.x, Angular2.x, Fetch, jQuery, Node)
- **Server stubs**: **C#** (ASP.NET Core, NancyFx), **C++** (Pistache, Restbed), **Erlang**, **Go**, **Haskell** (Servant), **Java** (MSF4J, Spring, Undertow, JAX-RS: CDI, CXF, Inflector, RestEasy, Play Framework, [PKMST](https://github.com/ProKarma-Inc/pkmst-getting-started-examples)), **PHP** (Lumen, Slim, Silex, [Symfony](https://symfony.com/), [Zend Expressive](https://github.com/zendframework/zend-expressive)), **Python** (Flask), **NodeJS**, **Ruby** (Sinatra, Rails5), **Rust** (rust-server), **Scala** ([Finch](https://github.com/finagle/finch), [Lagom](https://github.com/lagom/lagom), Scalatra)
- **API documentation generators**: **HTML**, **Confluence Wiki**
- **Configuration files**: [**Apache2**](https://httpd.apache.org/)
- **Others**: **JMeter**

Check out [OpenAPI-Spec](https://github.com/OAI/OpenAPI-Specification) for additional information about the OpenAPI project.

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
      - [Development in Docker](#development-in-docker)
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
    - [Maven Integration](#maven-integration)
    - [Gradle Integration](#gradle-integration)
  - [Github Integration](#github-integration)
  - [Online Generators](#online-generators)
  - [Guidelines for Contribution](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md#guidelines-for-contributing)
  - [Companies/Projects using Swagger Codegen](#companiesprojects-using-swagger-codegen)
  - [Presentations/Videos/Tutorials/Books](#presentationsvideostutorialsbooks)
  - [Swagger Codegen Core Team](#swagger-codegen-core-team)
  - [Swagger Codegen Technical Committee](#swagger-codegen-technical-committee)
  - [License](#license)


## Compatibility
The OpenAPI Specification has undergone 3 revisions since initial creation in 2010.  The swagger-codegen project has the following compatibilities with the OpenAPI Specification:

Swagger Codegen Version    | Release Date | OpenAPI Spec compatibility | Notes
-------------------------- | ------------ | -------------------------- | -----
3.0.0 (upcoming major release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/io/swagger/swagger-codegen-cli/3.0.0-SNAPSHOT/)| TBD | 1.0, 1.1, 1.2, 2.0, 3.0 | Major release with breaking changes
2.4.0 (current master, upcoming minor release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/io/swagger/swagger-codegen-cli/2.4.0-SNAPSHOT/)| TBD   | 1.0, 1.1, 1.2, 2.0   | Minor release with breaking changes
[2.3.0](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.3.0) (**current stable**) | 2017-12-21   | 1.0, 1.1, 1.2, 2.0   | [tag v2.3.0](https://github.com/swagger-api/swagger-codegen/tree/v2.3.0)
[2.2.3](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.3) | 2017-07-15   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.3](https://github.com/swagger-api/swagger-codegen/tree/v2.2.3)
[2.2.2](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.2) | 2017-03-01   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.2](https://github.com/swagger-api/swagger-codegen/tree/v2.2.2)
[2.2.1](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.1) | 2016-08-07   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.1](https://github.com/swagger-api/swagger-codegen/tree/v2.2.1)
[2.1.6](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.1.6) | 2016-04-06   | 1.0, 1.1, 1.2, 2.0   | [tag v2.1.6](https://github.com/swagger-api/swagger-codegen/tree/v2.1.6)
2.0.17                     | 2014-08-22   | 1.1, 1.2             | [tag v2.0.17](https://github.com/swagger-api/swagger-codegen/tree/2.0.17)
1.0.4                      | 2012-04-12   | 1.0, 1.1             | [tag v1.0.4](https://github.com/swagger-api/swagger-codegen/tree/swagger-codegen_2.9.1-1.1)


### Prerequisites
If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 7 runtime at a minimum):

```sh
wget http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/2.3.0/swagger-codegen-cli-2.3.0.jar -O swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar help
```

For Windows users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+), e.g. `Invoke-WebRequest -OutFile swagger-codegen-cli.jar http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/2.3.0/swagger-codegen-cli-2.3.0.jar`

On a mac, it's even easier with `brew`:
```sh
brew install swagger-codegen
```

To build from source, you need the following installed and available in your $PATH:

* [Java 7 or 8](http://java.oracle.com)

* [Apache maven 3.3.3 or greater](http://maven.apache.org/)

#### OS X Users
Don't forget to install Java 7 or 8. You probably have 1.6.

Export JAVA_HOME in order to use the supported Java version:
```sh
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```

### Building

After cloning the project, you can build it from source with this command:
```sh
mvn clean package
```

### Homebrew

To install, run `brew install swagger-codegen`

Here is an example usage:
```sh
swagger-codegen generate -i http://petstore.swagger.io/v2/swagger.json -l ruby -o /tmp/test/
```

### Docker

#### Development in docker

You can use `run-in-docker.sh` to do all development. This script maps your local repository to `/gen`
in the docker container. It also maps `~/.m2/repository` to the appropriate container location.

To execute `mvn package`:

```sh
git clone https://github.com/swagger-api/swagger-codegen
cd swagger-codegen
./run-in-docker.sh mvn package
```

Build artifacts are now accessible in your working directory.

Once built, `run-in-docker.sh` will act as an executable for swagger-codegen-cli. To generate code, you'll need to output to a directory under `/gen` (e.g. `/gen/out`). For example:

```sh
./run-in-docker.sh help # Executes 'help' command for swagger-codegen-cli
./run-in-docker.sh langs # Executes 'langs' command for swagger-codegen-cli
./run-in-docker.sh /gen/bin/go-petstore.sh  # Builds the Go client
./run-in-docker.sh generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml \
    -l go -o /gen/out/go-petstore -DpackageName=petstore # generates go client, outputs locally to ./out/go-petstore
```

#### Run Docker in Vagrant
Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
 ```sh
git clone http://github.com/swagger-api/swagger-codegen.git
cd swagger-codegen
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
 ```

#### Public Pre-built Docker images

 - https://hub.docker.com/r/swaggerapi/swagger-generator/ (official web service)
 - https://hub.docker.com/r/swaggerapi/swagger-codegen-cli/ (official CLI)


##### Swagger Generator Docker Image

The Swagger Generator image can act as a self-hosted web application and API for generating code. This container can be  incorporated into a CI pipeline, and requires at least two HTTP requests and some docker orchestration to access generated code.

Example usage (note this assumes `jq` is installed for command line processing of JSON):

```sh
# Start container and save the container id
CID=$(docker run -d swaggerapi/swagger-generator)
# allow for startup
sleep 5
# Get the IP of the running container
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  $CID)
# Execute an HTTP request and store the download link
RESULT=$(curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{
  "swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"
}' 'http://localhost:8188/api/gen/clients/javascript' | jq '.link' | tr -d '"')
# Download the generated zip and redirect to a file
curl $RESULT > result.zip
# Shutdown the swagger generator image
docker stop $CID && docker rm $CID
```

In the example above, `result.zip` will contain the generated client.

##### Swagger Codegen CLI Docker Image

The Swagger Codegen image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code with this image, you'll need to mount a local location as a volume.

Example:

```sh
docker run --rm -v ${PWD}:/local swaggerapi/swagger-codegen-cli generate \
    -i http://petstore.swagger.io/v2/swagger.json \
    -l go \
    -o /local/out/go
```

The generated code will be located under `./out/go` in the current directory.

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

You can also download the JAR (latest release) directly from [maven.org](http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/2.3.0/swagger-codegen-cli-2.3.0.jar)

To get a list of **general** options available, please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar help generate`

To get a list of PHP specified options (which can be passed to the generator with a config file via the `-c` option), please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l php`

## Generators

### To generate a sample client library
You can build a client against the swagger sample [petstore](http://petstore.swagger.io) API as follows:

```sh
./bin/java-petstore.sh
```

(On Windows, run `.\bin\windows\java-petstore.bat` instead)

This will run the generator with this command:

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java
```

with a number of options. You can get the options with the `help generate` command (below only shows partal results):

```
NAME
        swagger-codegen-cli generate - Generate code with chosen lang

SYNOPSIS
        swagger-codegen-cli generate
                [(-a <authorization> | --auth <authorization>)]
                [--additional-properties <additional properties>...]
                [--api-package <api package>] [--artifact-id <artifact id>]
                [--artifact-version <artifact version>]
                [(-c <configuration file> | --config <configuration file>)]
                [-D <system properties>...] [--git-repo-id <git repo id>]
                [--git-user-id <git user id>] [--group-id <group id>]
                [--http-user-agent <http user agent>]
                (-i <spec file> | --input-spec <spec file>)
                [--ignore-file-override <ignore file override location>]
                [--import-mappings <import mappings>...]
                [--instantiation-types <instantiation types>...]
                [--invoker-package <invoker package>]
                (-l <language> | --lang <language>)
                [--language-specific-primitives <language specific primitives>...]
                [--library <library>] [--model-name-prefix <model name prefix>]
                [--model-name-suffix <model name suffix>]
                [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)]
                [--release-note <release note>] [--remove-operation-id-prefix]
                [--reserved-words-mappings <reserved word mappings>...]
                [(-s | --skip-overwrite)]
                [(-t <template directory> | --template-dir <template directory>)]
                [--type-mappings <type mappings>...] [(-v | --verbose)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the swagger definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

...... (results omitted)

        -v, --verbose
            verbose mode

```

You can then compile and run the client, as well as unit tests against it:

```sh
cd samples/client/petstore/java
mvn package
```

Other languages have petstore samples, too:
```sh
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

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a README.md. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

You would then compile your library in the `output/myLibrary` folder with `mvn package` and execute the codegen like such:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.SwaggerCodegen
```
For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.
```
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar;modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.SwaggerCodegen
```

Note the `myClientCodegen` is an option now, and you can use the usual arguments for generating your library:

```sh
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

### Ignore file format

Swagger codegen supports a `.swagger-codegen-ignore` file, similar to `.gitignore` or `.dockerignore` you're probably already familiar with.

The ignore file allows for better control over overwriting existing files than the `--skip-overwrite` flag. With the ignore file, you can specify individual files or directories can be ignored. This can be useful, for example if you only want a subset of the generated code.

Examples:

```sh
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

Upon first code generation, you may also pass the CLI option `--ignore-file-override=/path/to/ignore_file` for greater control over generated outputs. Note that this is a complete override, and will override the `.swagger-codegen-ignore` file in an output directory when regenerating code.

Editor support for `.swagger-codegen-ignore` files is available in IntelliJ via the [.ignore plugin](https://plugins.jetbrains.com/plugin/7495--ignore).

### Customizing the generator

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```sh
$ ls -1 modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/
AbstractJavaJAXRSServerCodegen.java
AbstractTypeScriptClientCodegen.java
... (results omitted)
TypeScriptAngularClientCodegen.java
TypeScriptNodeClientCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc. you can use a json config file to pass the values.

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java \
  -c path/to/config.json
```
and `config.json` contains the following as an example:
```json
{
  "apiPackage" : "petstore"
}
```

Supported config options can be different per language. Running `config-help -l {lang}` will show available options.
**These options are applied via configuration file (e.g. config.json) or by passing them with `-D{optionName}={optionValue}`**. (If `-D{optionName}` does not work, please open a [ticket](https://github.com/swagger-api/swagger-codegen/issues/new) and we'll look into it)

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l java
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
--import-mappings Pet=my.models.MyPet,Order=my.models.MyOrder
```
or
```
--import-mappings Pet=my.models.MyPet --import-mappings Order=my.models.MyOrder
```


### Validating your OpenAPI Spec

You have options.  The easiest is to use our [online validator](https://github.com/swagger-api/validator-badge) which not only will let you validate your spec, but with the debug flag, you can see what's wrong with your spec.  For example:

http://online.swagger.io/validator/debug?url=http://petstore.swagger.io/v2/swagger.json

### Generating dynamic html api documentation

To do so, just use the `-l dynamic-html` flag when reading a spec file.  This creates HTML documentation that is available as a single-page application with AJAX.  To view the documentation:

```sh
cd samples/dynamic-html/
npm install
node .
```

Which launches a node.js server so the AJAX calls have a place to go.


### Generating static html api documentation

To do so, just use the `-l html` flag when reading a spec file.  This creates a single, simple HTML file with embedded css so you can ship it as an email attachment, or load it from your filesystem:

```sh
cd samples/html/
open index.html
```


### To build a server stub

Please refer to https://github.com/swagger-api/swagger-codegen/wiki/Server-stub-generator-HOWTO for more information.

### To build the codegen library

This will create the swagger-codegen library from source.

```sh
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts

## Workflow Integration

### Maven Integration

You can use the [swagger-codegen-maven-plugin](modules/swagger-codegen-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.

### Gradle Integration

[Gradle Swagger Generator Plugin](https://github.com/int128/gradle-swagger-generator-plugin) is available for generating source code and API document.

## GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)

 2) Generate the SDK
```sh
 java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
 -i modules/swagger-codegen/src/test/resources/2_0/petstore.json -l perl \
 --git-user-id "wing328" \
 --git-repo-id "petstore-perl" \
 --release-note "Github integration demo" \
 -o /var/tmp/perl/petstore
```
 3) Push the SDK to GitHub
```sh
cd /var/tmp/perl/petstore
/bin/sh ./git_push.sh
```

## Online generators

One can also generate API client or server using the online generators (https://generator.swagger.io)

For example, to generate Ruby API client, simply send the following HTTP request using curl:
```sh
curl -X POST -H "content-type:application/json" -d '{"swaggerUrl":"http://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/ruby
```
Then you will receieve a JSON response with the URL to download the zipped code.

To customize the SDK, you can `POST` to `https://generator.swagger.io/gen/clients/{language}` with the following HTTP body:
```json
{
  "options": {},
  "swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"
}
```
in which the `options` for a language can be obtained by submitting a `GET` request to `https://generator.swagger.io/api/gen/clients/{language}`:

For example, `curl https://generator.swagger.io/api/gen/clients/python` returns
```json
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
```json
{
  "options": {
    "packageName": "pet_store"
  },
  "swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"
}
```
and here is the curl command:
```sh
curl -H "Content-type: application/json" -X POST -d '{"options": {"packageName": "pet_store"},"swaggerUrl": "http://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/python
```

Instead of using `swaggerUrl` with an URL to the OpenAPI/Swagger spec, one can include the spec in the JSON payload with `spec`, e.g.
```json
{
  "options": {},
  "spec": {
    "swagger": "2.0",
    "info": {
      "version": "1.0.0",
      "title": "Test API"
    },
    ...
  }
}
```

Guidelines for Contribution
---------------------------

Please refer to this [page](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md)

Companies/Projects using Swagger Codegen
----------------------------------------
Here are some companies/projects using Swagger Codegen in production. To add your company/project to the list, please visit [README.md](https://github.com/swagger-api/swagger-codegen/blob/master/README.md) and click on the icon to edit the page.
- [Accengage](https://www.accengage.com/)
- [Activehours](https://www.activehours.com/)
- [Acunetix](https://www.acunetix.com/)
- [Atlassian](https://www.atlassian.com/)
- [Autodesk](http://www.autodesk.com/)
- [Avenida Compras S.A.](https://www.avenida.com.ar)
- [AYLIEN](http://aylien.com/)
- [Balance Internet](https://www.balanceinternet.com.au/)
- [beemo](http://www.beemo.eu)
- [bitly](https://bitly.com)
- [BeezUP](http://www.beezup.com)
- [Box](https://box.com)
- [Bufferfly Network](https://www.butterflynetinc.com/)
- [Cachet Financial](http://www.cachetfinancial.com/)
- [carpolo](http://www.carpolo.co/)
- [Cisco](http://www.cisco.com/)
- [CloudBoost](https://www.CloudBoost.io/)
- [Cloudsmith](https://cloudsmith.io/)
- [Conplement](http://www.conplement.de/)
- [Cummins](http://www.cummins.com/)
- [Cupix](http://www.cupix.com)
- [DBBest Technologies](https://www.dbbest.com)
- [DecentFoX](http://decentfox.com/)
- [DocRaptor](https://docraptor.com)
- [DocuSign](https://www.docusign.com)
- [Elastic](https://www.elastic.co/)
- [Ergon](http://www.ergon.ch/)
- [Dell EMC](https://www.emc.com/)
- [eureka](http://eure.jp/)
- [everystory.us](http://everystory.us)
- [Expected Behavior](http://www.expectedbehavior.com/)
- [Fastly](https://www.fastly.com/)
- [FINRA](https://github.com/FINRAOS/herd/)
- [Flat](https://flat.io)
- [Finder](http://en.finder.pl/)
- [Fitwell](https://fitwell.co/)
- [FH Münster - University of Applied Sciences](http://www.fh-muenster.de)
- [Fotition](https://www.fotition.com/)
- [Gear Zero Network](https://www.gearzero.ca)
- [General Electric](https://www.ge.com/)
- [Genesys - PureCloud](http://developer.mypurecloud.com/)
- [Germin8](http://www.germin8.com)
- [GigaSpaces](http://www.gigaspaces.com)
- [GMO Pepabo](https://pepabo.com/en/)
- [goTransverse](http://www.gotransverse.com/api)
- [GraphHopper](https://graphhopper.com/)
- [Gravitate Solutions](http://gravitatesolutions.com/)
- [HashData](http://www.hashdata.cn/)
- [Hewlett Packard Enterprise](https://hpe.com)
- [High Technologies Center](http://htc-cs.com)
- [Hootsuite](https://hootsuite.com/)
- [Huawei Cloud](https://www.huaweicloud.com) - [Cloud Stream Service](http://www.huaweicloud.com/en-us/product/cs.html)
- [IBM](https://www.ibm.com)
- [IMS Health](http://www.imshealth.com/en/solution-areas/technology-and-applications)
- [Individual Standard IVS](http://www.individual-standard.com)
- [Intent HQ](http://www.intenthq.com)
- [Kabuku](http://www.kabuku.co.jp/en)
- [Kurio](https://kurio.co.id)
- [Kuroi](http://kuroiwebdesign.com/)
- [Kuary](https://kuary.com/)
- [Kubernetes](https://kubernetes.io/)
- [LANDR Audio](https://www.landr.com/)
- [Lascaux](http://www.lascaux.it/)
- [Leanix](http://www.leanix.net/)
- [Leica Geosystems AG](http://leica-geosystems.com)
- [LiveAgent](https://www.ladesk.com/)
- [LXL Tech](http://lxltech.com)
- [Lyft](https://www.lyft.com/developers)
- [MailMojo](https://mailmojo.no/)
- [Metaswitch](https://www.metaswitch.com/)
- [Mindera](http://mindera.com/)
- [Mporium](http://mporium.com/)
- [Neverfail](https://neverfail.com/)
- [NTT DATA](http://www.nttdata.com/)
- [nViso](http://www.nviso.ch/)
- [Okiok](https://www.okiok.com)
- [Onedata](http://onedata.org)
- [Open International Systems](https://openintl.com/)
- [OrderCloud.io](http://ordercloud.io)
- [OSDN](https://osdn.jp)
- [PagerDuty](https://www.pagerduty.com)
- [PagerTree](https://pagertree.com)
- [Pepipost](https://www.pepipost.com)
- [Peatio Tech](https://www.peatio.tech)
- [Plexxi](http://www.plexxi.com)
- [Pixoneye](http://www.pixoneye.com/)
- [PostAffiliatePro](https://www.postaffiliatepro.com/)
- [PracticeBird](https://www.practicebird.com/)
- [Prill Tecnologia](http://www.prill.com.br)
- [Prokarma](https://www.prokarma.com)
- [QAdept](http://qadept.com/)
- [QuantiModo](https://quantimo.do/)
- [QuickBlox](https://quickblox.com/)
- [Rapid7](https://rapid7.com/)
- [Red Hat](https://www.redhat.com/)
- [Reload! A/S](https://reload.dk/)
- [REstore](https://www.restore.eu)
- [Revault Sàrl](http://revault.ch)
- [Riffyn](https://riffyn.com)
- [Royal Bank of Canada (RBC)](http://www.rbc.com/canada.html)
- [Saritasa](https://www.saritasa.com/)
- [SAS](https://www.sas.com)
- [SCOOP Software GmbH](http://www.scoop-software.de)
- [Shine Solutions](https://shinesolutions.com/)
- [Simpfony](https://www.simpfony.com/)
- [Skurt](http://www.skurt.com)
- [Slamby](https://www.slamby.com/)
- [SmartRecruiters](https://www.smartrecruiters.com/)
- [snapCX](https://snapcx.io)
- [SPINEN](http://www.spinen.com)
- [Sponsoo](https://www.sponsoo.de)
- [SRC](https://www.src.si/)
- [Stardog Ventures](https://www.stardog.io)
- [Stingray](http://www.stingray.com)
- [StyleRecipe](http://stylerecipe.co.jp)
- [Svenska Spel AB](https://www.svenskaspel.se/)
- [Switch Database](https://www.switchdatabase.com/)
- [TaskData](http://www.taskdata.com/)
- [ThirdWatch.ai](https://www.thirdwatch.ai/)
- [ThoughtWorks](https://www.thoughtworks.com)
- [Tpay](https://tpay.com)
- [Trexle](https://trexle.com/)
- [Upwork](http://upwork.com/)
- [uShip](https://www.uship.com/)
- [VMware](https://vmware.com/)
- [Viavi Solutions Inc.](https://www.viavisolutions.com)
- [W.UP](http://wup.hu/?siteLang=en)
- [Wealthfront](https://www.wealthfront.com/)
- [Webever GmbH](https://www.webever.de/)
- [WEXO A/S](https://www.wexo.dk/)
- [XSky](http://www.xsky.com/)
- [Yelp](http://www.yelp.com/)
- [Zalando](https://tech.zalando.com)
- [ZEEF.com](https://zeef.com/)
- [zooplus](https://www.zooplus.com/)

Presentations/Videos/Tutorials/Books
----------------------------------------
- 2011/08/31 - [Introducing Swagger](https://www.slideshare.net/fehguy/introducing-swagger) by [Tony Tam](https://twitter.com/fehguy)
- 2014/05/22 - [Swagger APIs for Humans and Robots](https://www.slideshare.net/fehguy/swagger-apis-for-humans-and-robots-gluecon) by [Tony Tam](https://twitter.com/fehguy) at [Gluecon](http://gluecon.com/)
- 2014/11/11 - [Genie 2.0: Second Wish Granted!](https://medium.com/netflix-techblog/genie-2-0-second-wish-granted-d888d79455c6) by [Tom Gianos](http://www.linkedin.com/in/thomasgianos/) and [Amit Sharma](https://www.linkedin.com/pub/amit-sharma/5/163/a83) @ [Netflix](https://www.netflix.com/) Big Data Platform Team
- 2015/07/28 - [Enriching RESTful Services with Swagger](https://blog.philipphauer.de/enriching-restful-services-swagger/) by [Philipp Hauer](https://blog.philipphauer.de/)
- 2015/11/11 - [Generate client stubs & document your REST-API using Swagger & Spring](https://www.youtube.com/watch?v=43GhBbP--oI) by [Johannes Fiala](https://github.com/jfiala) @ Devoxx Belgium 2015
- 2015/12/03 - [こんなに簡単! Swagger Codegenのカスタマイズ](http://qiita.com/Quramy/items/c583f3213f0b77ff1bac) by [Quramy](http://qiita.com/Quramy)
- 2016/01/12 - [Generate client side code using Swagger Codegen](http://rnavagamuwa.com/open-source/generate-client-side-code-using-swagger-codegen/) by [RNAVAGAMUWA](http://rnavagamuwa.com/author/admin/)
- 2016/01/15 - [How to end manual REST-API client coding](https://www.youtube.com/watch?v=RzZRdqZp6Oo) by [Johannes Fiala](https://github.com/jfiala) @ dotJS 2015
- 2016/04/27 - [Automated REST API Development](https://yos.io/2016/04/27/automated-api-development/) by [Yos Riady](https://www.linkedin.com/in/yosriady)
- 2016/05/29 - [Generating Java Spring-MVC code from Swagger Spec](https://www.clianz.com/2016/05/29/java-mvc-swagger-gen/) by [@icha024](https://github.com/icha024)
- 2016/08/23 - [Generating Dreamfactory Client SDKs based on Swagger API Definitions](http://blog.dreamfactory.com/generating-dreamfactory-client-sdks-based-on-swagger-api-definitions) by [Phil Schuler](https://github.com/philicious/)
- 2016/09/28 - [1 UNBELIEVABLE TRICK TO CREATE AN EASY TO CONSUME API IN .NET](https://stapp.space/1-simple-trick-to-create-a-good-api-in-net/) by [Piotr Stapp](https://stapp.space/author/piotr-stapp/)
- 2016/10/10 - [Using swagger-codegen with Marketo](http://developers.marketo.com/blog/using-swagger-codegen-with-marketo/) by [Kenny Elkington](http://developers.marketo.com/blog/using-swagger-codegen-with-marketo/)
- 2016/10/12 - [Designing a Swagger API](https://sookocheff.com/post/api/swagger/) by [Kevin Sookocheff](https://sookocheff.com/)
- 2016/11/05 - [How to generate a REST Application](https://www.youtube.com/watch?v=iyC9BWMe75Q) by [Johannes Fiala](https://github.com/jfiala) @ DevFest Vienna 2016
- 2016/11/10 - [Building an AEM API clients ecosystem](http://blog.cliffano.com/2016/11/10/adobe-marketing-cloud-community-expo/) by Cliffano Subagio, Michael Diender, Stephen Shim from [Shine Solutions](https://shinesolutions.com/) @ [Adobe Marketing Cloud Community Expo (AMCCE)](https://www.meetup.com/Melbourne-AEM-CQ-Meetup/events/233363101/)
- 2016/11/18 - [How to generate a REST CXF3 application from Swagger-Contract](https://www.slideshare.net/johannes_fiala/how-to-generate-a-rest-cxf3-application-from-swagger-apacheconeu-2016) by [Johannes Fiala](https://github.com/jfiala) @ ApacheConEU 2016
- 2016/11/25 - [Swagger Codegen for Swift3 and NodeJS](https://normand1.github.io/blog/swift/swagger/codegen/2016/11/25/Swagger-Codegen-for-Swift3-and-NodeJS.html) by [David Norman](https://github.com/normand1)
- 2016/12/08 - [Building an API with Swagger](https://www.youtube.com/watch?v=PbwQWw7xSOM)
- 2016/12/08 - [Generate client side code using Swagger Codegen](https://carra-lucia-ltd.co.uk/2016/12/08/generate-client-side-code-using-swagger-codegen/) by [theFerkel](https://carra-lucia-ltd.co.uk/author/theferkel/)
- 2017/01/16 - [Zero to API in 4 minutes](https://cidrblock.github.io/zero-to-api-in-4-minutes.html) by [Bradley A. Thornton](https://github.com/cidrblock)
- 2017/02/09 - [「Swaggerを利用した新規サービス開発」というタイトルで登壇して来ました](https://techblog.recochoku.jp/1055) by [recotech](https://www.slideshare.net/recotech)
- 2017/03/03 - [Swagger Codegen の使い方の簡単な説明です](https://speakerdeck.com/wagyu298/swagger-codegen) by [wagyu298](https://github.com/wagyu298)
- 2017/03/24 - [Using Open API Specification To Put Lyft SDK Support in the Fast Lane](https://medium.com/lyft-developer-platform/using-open-api-specification-to-put-lyft-sdk-support-in-the-fast-lane-7b623218e4ee) by [Val Polouchkine](https://github.com/vpolouchkine)
- 2017/04/13 - [Automatically Generating your API Client with Swagger and Swagger Codegen](https://www.youtube.com/watch?v=EzKwi-u9jQo) by [Jesse Collis](https://github.com/jessedc) @ Melbourne Cocoaheads
- 2017/04/27 - [Swagger Codegen のPHP実装があまりにアレだったので、ライブラリ自作して公開してみた](http://qiita.com/imunew/items/2e9c472e0097e329f2cd) by [imunew](http://qiita.com/imunew)
- 2017/05/17 - [Diseño de APIs con OpenAPI](https://www.slideshare.net/pjmolina/diseo-de-apis-con-openapi) by [Pedro J. Molina](https://github.com/pjmolina) @ [JSDayES 2017](http://2017.jsday.es/)
- 2017/05/22 - [Presentation of the Vert.x-Swagger project](http://vertx.io/blog/presentation-of-the-vert-x-swagger-project/) by [@phiz71](http://github.com/phiz71)
- 2017/05/22 - [Automatically generating your API from a swagger file using gradle](https://www.jcore.com/2017/05/22/automatically-generating-api-using-swagger-and-gradle/) by [Deniz Turan](https://www.jcore.com/author/deniz/)
- 2017/06/21 - [Swagger Presentation - Warsaw Ruby Users Group](https://www.youtube.com/watch?v=uCnnDMFQB8U) by [@rafalpetryka](http://github.com/rafalpetryka)
- 2017/06/29 - [Making SDKs: the bespoke, the hopeful and the generated](https://devrel.net/developer-experience/making-sdks-bespoke-hopeful-generated) by [Tristan Sokol](https://github.com/tristansokol) ([Square](https://github.com/square)) at DevXcon 2017
- 2017/07/11 - [OpenAPI development with Python](https://www.slideshare.net/TakuroWada/20170711-euro-python2017) by [和田拓朗](https://github.com/taxpon) at [EuroPython 2017](https://ep2017.europython.eu/en/)
- 2017/07/29 - [How Square makes its SDKs](https://medium.com/square-corner-blog/how-square-makes-its-sdks-6a0fd7ea4b2d) by [Tristan Sokol](https://github.com/tristansokol) ([Square](https://github.com/square))
- 2017/07/31 - [How to Generate a Deployable REST CXF3 Application from a Swagger-Contract](https://www.youtube.com/watch?v=gM63rJlUHZQ) by [Johannes Fiala](https://github.com/jfiala) @ Voxxed Days Vienna
- 2017/08/11 - [Swagger Codegen 自动生成Retrofit 代码](https://juejin.im/entry/598d8eb86fb9a03c52459e2a) by [徐磊](http://www.jianshu.com/u/792c738b33fc)
- 2017/08/24 - [APIs First](https://engineering.squarespace.com/blog/2017/apis-first) by [roykachouh](https://github.com/roykachouh) ([Square](https://github.com/square))
- 2017/08/31 - [Bringing Jenkins Remote Access API To The Masses](http://blog.cliffano.com/2017/09/01/jenkins-world-2017/) by [Cliffano Subagio](http://cliffano.com) from [Shine Solutions](https://shinesolutions.com/) @ [Jenkins World 2017](https://jenkinsworld20162017.sched.com/)
- 2017/09/08 - [Swagger Codegen で自動生成したクライアントSDKを使う(iOS編)](http://blog.techium.jp/entry/2017/09/08/071650) by [kfurue](http://profile.hatena.ne.jp/kfurue/)
- 2017/09/09 - [Swagger Codegen で自動生成したクライアントSDKを使う(RxSwift 編)](http://blog.techium.jp/entry/2017/09/09/113003) by [kfurue](http://profile.hatena.ne.jp/kfurue/)
- 2017/09/09 - [OpenAPIを利用したPythonWebアプリケーション開発](https://www.slideshare.net/TakuroWada/openapipythonweb) by [和田拓朗](https://github.com/taxpon) at [PyCon JP 2017](https://pycon.jp/2017/ja/)
- 2017/09/21 - [Generating an Elastic Cloud Enterprise Client](https://www.elastic.co/blog/generating-an-elastic-cloud-enterprise-client) by [Greg Marzouka](https://github.com/gmarz) ([Elastic](https://www.elastic.co/))
- 2017/09/26 - [How to Use IoT Application Enablement Api Hub JSON Descriptor in Postman and How to Generate Client Code](https://www.linkedin.com/in/radu-simen/) by [Radu Simen](https://www.linkedin.com/in/radu-simen/) ([SAP](https://www.sap.com/))
- 2017/09/28 - [Swagger Codegenで APIクライアントgem 自動生成 #m3kt](https://speakerdeck.com/juntaki/swagger-codegende-apikuraiantogem-zi-dong-sheng-cheng-number-m3kt) by [Jumpei Takiyasu](https://github.com/juntaki)
- 2017/09/30 - [Swaggerのテンプレートを魔改造した話 #渋谷java](https://www.slideshare.net/int128/swagger-80309224) by [Hidetake Iwata](https://github.com/int128) ([NTT DATA Corporation](http://www.nttdata.com/global/en/))
- 2017/10/04 - [Enterprise-friendly Java client for Microsoft Machine Learning Server](https://blogs.msdn.microsoft.com/mlserver/2017/10/04/enterprise-friendly-java-client-for-microsoft-machine-learning-server/) by [Pratik Palnitkar](https://www.linkedin.com/in/pratikpalnitkar/) ([Microsoft](https://www.microsoft.com/))
- 2017/10/08 - [Generating a REST Ada client with OpenAPI and Swagger Codegen](https://blog.vacs.fr/vacs/blogs/post.html?post=2017/10/08/Generating-a-REST-Ada-client-with-OpenAPI-and-Swagger-Codegen) by [Stephane Carrez](https://github.com/stcarrez)
- 2017/11/08 - [A Beginner's Guide to Code Generation for REST APIs](https://gum.co/swagger_codegen_beginner)(eBook) by [William Cheng](https://twitter.com/wing328)
- 2017/11/18 - [10年前のレガシーシステムをサーバーサイドKotlinでフルリニューアルしている話 #jjug_ccc #ccc_g2](https://speakerdeck.com/maeharin/10nian-qian-falseregasisisutemuwosabasaidokotlindehururiniyuarusiteiruhua-number-jjug-ccc-number-ccc-g2) by [Hidenori Maehara](https://github.com/maeharin)
- 2017/11/21 - [swagger-codegen から眺める Swift4](https://speakerdeck.com/d_date/swagger-codegen-karatiao-meru-swift4) by [Daiki Matsudate](https://github.com/d-date)
- 2017/12/07 - [API-driven development with OpenAPI and Swagger, Part 2](https://www.itworld.com/article/3199190/apis/api-driven-development-with-openapi-and-swagger-part-2.html) by Matthew Tyson
- 2017/12/29 - [REST APIs代码生成指南](https://gumroad.com/l/swagger_codegen_beginner_zh)(eBook) by [William Cheng](https://github.com/wing328), [Xin Meng](https://github.com/xmeng1)
- 2017/12/21 - [Using Protocol Buffer to Generate SDK at Hootsuite](http://code.hootsuite.com/using-protocol-buffer-to-generate-sdk-at-hoosuite/?lipi=urn%3Ali%3Apage%3Ad_flagship3_messaging%3By4ATz3SDRXyvXJJ14LQysQ%3D%3D) by [Joy Zhang](https://www.linkedin.com/in/joy8zhang/)

# Swagger Codegen Core Team

Swagger Codegen core team members are contributors who have been making significant contributions (review issues, fix bugs, make enhancements, etc) to the project on a regular basis.

## API Clients
| Languages     | Core Team (join date) |
|:-------------|:-------------|
| ActionScript | |
| C++      |  |
| C#      | @jimschubert (2016/05/01) |
| Clojure | @xhh (2016/05/01) |
| Dart      |  |
| Groovy     |  |
| Go     |  @guohuang (2016/05/01) @neilotoole (2016/05/01) |
| Java      | @cbornet (2016/05/01) @xhh (2016/05/01) @epaul (2016/06/04) |
| Java (Spring Cloud) | @cbornet (2016/07/19) |
| Kotlin      | @jimschubert (2016/05/01) |
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
| Java Play Framework |  |
| NancyFX |  |
| NodeJS | @kolyjjj (2016/05/01) |
| PHP Lumen | @abcsun (2016/05/01) |
| PHP Silex |  |
| PHP Slim  |  |
| Python Flask  |  |
| Ruby Sinatra     | @wing328 (2016/05/01) |  |
| Scala Scalatra |  |  |
| Scala Finch | @jimschubert (2017/01/28) |


## Template Creator
Here is a list of template creators:
 * API Clients:
   * Ada: @stcarrez
   * Akka-Scala: @cchafer
   * Apex: @asnelling
   * Bash: @bkryza
   * C++ REST: @Danielku15
   * C# (.NET 2.0): @who
   * C# (.NET Standard 1.3 ): @Gronsak
   * C# (.NET 4.5 refactored): @jimschubert
   * Clojure: @xhh
   * Dart: @yissachar
   * Elixir: @niku
   * Elm: @trenneman
   * Eiffel: @jvelilla
   * Erlang: @tsloughter
   * Groovy: @victorgit
   * Go: @wing328
   * Go (rewritten in 2.3.0): @antihax
   * Haskell (http-client): @jonschoning
   * Java (Feign): @davidkiss
   * Java (Retrofit): @0legg
   * Java (Retrofit2): @emilianobonassi
   * Java (Jersey2): @xhh
   * Java (okhttp-gson): @xhh
   * Java (RestTemplate): @nbruno
   * Java (RESTEasy): @gayathrigs
   * Java (Vertx): @lopesmcc
   * Java (Google APIs Client Library): @charlescapps
   * Javascript/NodeJS: @jfiala
   * Javascript (Closure-annotated Angular) @achew22
   * JMeter: @davidkiss
   * Kotlin: @jimschubert
   * Lua: @daurnimator
   * Perl: @wing328
   * PHP (Guzzle): @baartosz
   * PowerShell: @beatcracker
   * R: @ramnov
   * Rust: @farcaller
   * Rust (rust-server): @metaswitch
   * Scala (scalaz & http4s): @tbrown1979
   * Swift: @tkqubo
   * Swift 3: @hexelon
   * Swift 4: @ehyche
   * TypeScript (Node):  @mhardorf
   * TypeScript (Angular1):  @mhardorf
   * TypeScript (Fetch): @leonyu
   * TypeScript (Angular2): @roni-frantchi
   * TypeScript (jQuery): @bherila
 * Server Stubs
   * C# ASP.NET5: @jimschubert
   * C# NancyFX: @mstefaniuk
   * C++ Pistache: @sebymiano
   * C++ Restbed: @stkrwork
   * Erlang Server: @galaxie
   * Go Server: @guohuang
   * Haskell Servant: @algas
   * Java MSF4J: @sanjeewa-malalgoda
   * Java Spring Boot: @diyfr
   * Java Undertow: @stevehu
   * Java Play Framework: @JFCote
   * Java PKMST: @anshu2185 @sanshuman @rkumar-pk @ninodpillai
   * JAX-RS RestEasy: @chameleon82
   * JAX-RS CXF: @hiveship
   * JAX-RS CXF (CDI): @nickcmaynard
   * JAX-RS RestEasy (JBoss EAP): @jfiala
   * PHP Lumen: @abcsun
   * PHP Slim: @jfastnacht
   * PHP Symfony: @ksm2
   * PHP Zend Expressive (with Path Handler): @Articus
   * Ruby on Rails 5: @zlx
   * Rust (rust-server): @metaswitch
   * Scala Finch: @jimschubert
   * Scala Lagom: @gmkumar2005
 * Documentation
   * HTML Doc 2: @jhitchcock
   * Confluence Wiki: @jhitchcock
 * Configuration
   * Apache2: @stkrwork

## How to join the core team

Here are the requirements to become a core team member:
- rank within top 50 in https://github.com/swagger-api/swagger-codegen/graphs/contributors
  - to contribute, here are some good [starting points](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)
- regular contributions to the project
  - about 3 hours per week
  - for contribution, it can be addressing issues, reviewing PRs submitted by others, submitting PR to fix bugs or make enhancements, etc

 To join the core team, please reach out to wing328hk@gmail.com (@wing328) for more information.

 To become a Template Creator, simply submit a PR for new API client (e.g. Rust, Elixir) or server stub (e.g. Ruby Grape) generator.

# Swagger Codegen Technical Committee

Members of the Swagger Codegen technical committee shoulder the following responsibilities:

- Provides guidance and direction to other users
- Reviews pull requests and issues
- Improves the generator by making enhancements, fixing bugs or updating documentations
- Sets the technical direction of the generator

Who is eligible? Those who want to join must have at least 3 PRs merged into a generator. (Exceptions can be granted to template creators or contributors who have made a lot of code changes with less than 3 merged PRs)

If you want to join the committee, please kindly apply by sending an email to wing328hk@gmail.com ([@wing328](https://github.com/wing328)) with your Github ID.

## Members of Technical Committee

| Languages     | Member (join date) |
|:-------------|:-------------|
| ActionScript |      |
| Android | @jaz-ah (2017/09) |
| Apex  |     |
| Bash  | @frol (2017/07) @bkryza (2017/08) @kenjones-cisco (2017/09) |
| C++      | @ravinikam (2017/07) @stkrwork (2017/07) @fvarose (2017/11) |
| C#        | @mandrean (2017/08) @jimschubert (2017/09) |
| Clojure   |  |
| Dart      | @ircecho (2017/07) |
| Eiffel    | @jvelilla (2017/09) |
| Elixir    |  |
| Elm       |  |
| Erlang    | @tsloughter (2017/11) |
| Groovy    |  |
| Go        | @antihax (2017/11) @bvwells (2017/12) |
| Haskell   | |
| Java      | @bbdouglas (2017/07) @JFCote (2017/08) @sreeshas (2017/08) @jfiala (2017/08) @lukoyanov (2017/09) @cbornet (2017/09) |
| Kotlin     | @jimschubert (2017/09) |
| Lua     | @daurnimator (2017/08) |
| NodeJS/Javascript | @CodeNinjai (2017/07) @frol (2017/07) @cliffano (2017/07) |
| ObjC      |  |
| Perl      | @wing328 (2017/07) |
| PHP      | @jebentier (2017/07) @dkarlovi (2017/07) @mandrean (2017/08) @jfastnacht (2017/09) @ackintosh (2017/09) |
| Python   | @taxpon (2017/07) @frol (2017/07) @mbohlool (2017/07) @cbornet (2017/09) @kenjones-cisco (2017/11)|
| R        |  |
| Ruby      | @cliffano (2017/07) @zlx (2017/09) |
| Rust      | @frol (2017/07) @farcaller (2017/08) @bjgill (2017/12) |
| Scala     | @clasnake (2017/07) @jimschubert (2017/09) |
| Swift     | @jgavris (2017/07) @ehyche (2017/08) @Edubits (2017/09) @jaz-ah (2017/09) |
| TypeScript | @TiFu (2017/07) @taxpon (2017/07) @sebastianhaas (2017/07) @kenisteward (2017/07) @Vrolijkx (2017/09) |


# License information on Generated Code

The Swagger Codegen project is intended as a benefit for users of the Swagger / Open API Specification.  The project itself has the [License](#license) as specified.  In addition, please understand the following points:

* The templates included with this project are subject to the [License](#license).
* Generated code is intentionally _not_ subject to the parent project license

When code is generated from this project, it shall be considered **AS IS** and owned by the user of the software.  There are no warranties--expressed or implied--for generated code.  You can do what you wish with it, and once generated, the code is your responsibility and subject to the licensing terms that you deem appropriate.

License
-------

Copyright 2018 SmartBear Software

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

