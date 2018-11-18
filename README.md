<h1 align="center">OpenAPI Generator</h1>

<div align="center">

[Master](https://github.com/OpenAPITools/openapi-generator/tree/master) (`3.3.3`): [![Build Status](https://img.shields.io/travis/OpenAPITools/openapi-generator/master.svg?label=Integration%20Test)](https://travis-ci.org/OpenAPITools/openapi-generator)
[![Integration Test2](https://circleci.com/gh/OpenAPITools/openapi-generator.svg?style=shield)](https://circleci.com/gh/OpenAPITools/openapi-generator)
[![Run Status](https://api.shippable.com/projects/5af6bf74e790f4070084a115/badge?branch=master)](https://app.shippable.com/github/OpenAPITools/openapi-generator)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/openapitools/openapi-generator?branch=master&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/openapi-generator-wh2wu)

[`4.0.x`](https://github.com/OpenAPITools/openapi-generator/tree/4.0.x) branch: [![Build Status](https://img.shields.io/travis/OpenAPITools/openapi-generator/4.0.x.svg?label=Integration%20Test)](https://travis-ci.org/OpenAPITools/openapi-generator)
[![Integration Test2](https://circleci.com/gh/OpenAPITools/openapi-generator/tree/4.0.x.svg?style=shield)](https://circleci.com/gh/OpenAPITools/openapi-generator)
[![Run Status](https://api.shippable.com/projects/5af6bf74e790f4070084a115/badge?branch=4.0.x)](https://app.shippable.com/github/OpenAPITools/openapi-generator)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/openapitools/openapi-generator?branch=4.0.x&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/openapi-generator-wh2wu)
</div>

<div align="center">

[![Join the chat at https://gitter.im/OpenAPITools/openapi-generator](https://badges.gitter.im/OpenAPITools/openapi-generator.svg)](https://gitter.im/OpenAPITools/openapi-generator?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Stable releaases in the Maven store](https://img.shields.io/maven-metadata/v/http/central.maven.org/maven2/org/openapitools/openapi-generator/maven-metadata.xml.svg)](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.openapitools%22%20AND%20a%3A%22openapi-generator%22)
[![Follow OpenAPI Generator Twitter account to get the latest update](https://img.shields.io/twitter/follow/oas_generator.svg?style=social&label=Follow)](https://twitter.com/oas_generator)

</div>

<div align="center">

:star::star::star: If you would like to contribute, please refer to [guidelines](CONTRIBUTING.md) and a list of [open tasks](https://github.com/openapitools/openapi-generator/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22).:star::star::star:

:bangbang: To migrate from Swagger Codegen to OpenAPI Generator, please refer to the [migration guide](docs/migration-from-swagger-codegen.md) :bangbang:

:notebook_with_decorative_cover: For more information, please refer to the [Wiki page](https://github.com/openapitools/openapi-generator/wiki) and [FAQ](https://github.com/openapitools/openapi-generator/wiki/FAQ) :notebook_with_decorative_cover:

:notebook_with_decorative_cover: The eBook [A Beginner's Guide to Code Generation for REST APIs](https://gumroad.com/l/swagger_codegen_beginner) is a good starting point for beginners :notebook_with_decorative_cover:

:warning: If the OpenAPI spec, templates or any input (e.g. options, envirionment variables) is obtained from an untrusted source or environment, please make sure you've reviewed these inputs before using OpenAPI Generator to generate the API client, server stub or documentation to avoid potential security issues (e.g. [code injection](https://en.wikipedia.org/wiki/Code_injection)) :warning:

:bangbang: Both "OpenAPI Tools" (https://OpenAPITools.org - the parent organization of OpenAPI Generator) and "OpenAPI Generator" are not affiliated with OpenAPI Initiative (OAI) :bangbang:

</div>

## Overview
OpenAPI Generator allows generation of API client libraries (SDK generation), server stubs,  documentation and configuration automatically given an [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification) (both 2.0 and 3.0 are supported). Currently, the following languages/frameworks are supported:

|                                | Languages/Frameworks |
|-|-|
**API clients**                  | **ActionScript**, **Ada**, **Apex**, **Bash**, **C**, **C#** (.net 2.0, 3.5 or later), **C++** (cpprest, Qt5, Tizen), **Clojure**, **Dart (1.x, 2.x)**, **Elixir**, **Elm**, **Eiffel**, **Erlang**, **Go**, **Groovy**, **Haskell** (http-client, Servant), **Java** (Jersey1.x, Jersey2.x, OkHttp, Retrofit1.x, Retrofit2.x, Feign, RestTemplate, RESTEasy, Vertx, Google API Client Library for Java, Rest-assured, Spring 5 Web Client), **Kotlin**, **Lua**, **Node.js** (ES5, ES6, AngularJS with Google Closure Compiler annotations, Flow types) **Objective-C**, **Perl**, **PHP**, **PowerShell**, **Python**, **R**, **Ruby**, **Rust** (rust, rust-server), **Scala** (akka, http4s, scalaz, swagger-async-httpclient), **Swift** (2.x, 3.x, 4.x), **Typescript** (AngularJS, Angular (2.x - 7.x), Aurelia, Axios, Fetch, Inversify, jQuery, Node)
**Server stubs**                 | **Ada**, **C#** (ASP.NET Core, NancyFx), **C++** (Pistache, Restbed), **Erlang**, **Go** (net/http, Gin), **Haskell** (Servant), **Java** (MSF4J, Spring, Undertow, JAX-RS: CDI, CXF, Inflector, RestEasy, Play Framework, [PKMST](https://github.com/ProKarma-Inc/pkmst-getting-started-examples)), **Kotlin** (Spring Boot), **PHP** (Laravel, Lumen, Slim, Silex, [Symfony](https://symfony.com/), [Zend Expressive](https://github.com/zendframework/zend-expressive)), **Python** (Flask), **NodeJS**, **Ruby** (Sinatra, Rails5), **Rust** (rust-server), **Scala** ([Finch](https://github.com/finagle/finch), [Lagom](https://github.com/lagom/lagom), Scalatra)
**API documentation generators** | **HTML**, **Confluence Wiki**
**Configuration files**          | [**Apache2**](https://httpd.apache.org/)
**Others**                       | **JMeter**, **MySQL Schema**

## Table of contents

  - [OpenAPI Generator](#openapi-generator)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [1 - Installation](#1---installation)
    - [1.1 - Compatibility](#11---compatibility)
    - [1.2 - Artifacts on Maven Central](#12---artifacts-on-maven-central)
    - [1.3 - Download JAR](#13---download-jar)
    - [1.4 - Build Projects](#14---build-projects)
    - [1.5 - Homebrew](#15---homebrew)
    - [1.6 - Docker](#16---docker)
    - [1.7 - NPM](#17---npm)    
  - [2 - Getting Started](#2---getting-started)
  - [3 - Usage](#3---usage)
    - [3.1 - Customization](#31---customization)
    - [3.2 - Workflow Integration](#32---workflow-integration-maven-gradle-github-cicd)
    - [3.3 - Online Generators](#33---online-openapi-generator)
    - [3.4 - License Information on Generated Code](#34---license-information-on-generated-code)
  - [4 - Companies/Projects using OpenAPI Generator](#4---companiesprojects-using-openapi-generator)
  - [5 - Presentations/Videos/Tutorials/Books](#5---presentationsvideostutorialsbooks)
  - [6 - About Us](#6---about-us)
    - [6.1 - OpenAPI Generator Core Team](#61---openapi-generator-core-team)
    - [6.2 - OpenAPI Generator Technical Committee](#62---openapi-generator-technical-committee)
    - [6.3 - History of OpenAPI Generator](#63---history-of-openapi-generator)
  - [7 - License](#7---license)

## [1 - Installation](#table-of-contents)

### [1.1 - Compatibility](#table-of-contents)

The OpenAPI Specification has undergone 3 revisions since initial creation in 2010.  The openapi-generator project has the following compatibilities with the OpenAPI Specification:

OpenAPI Generator Version    | Release Date | Notes
---------------------------- | ------------ | -----
4.0.0 (upcoming major release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/org/openapitools/openapi-generator-cli/4.0.0-SNAPSHOT/)| 20.12.2018 | Major release with breaking changes (with or without fallback)
3.3.4 (current master, upcoming patch release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/org/openapitools/openapi-generator-cli/3.3.4-SNAPSHOT/) | 01.12.2018 | Bugfix release
[3.3.3](https://github.com/OpenAPITools/openapi-generator/releases/tag/v3.3.3) (latest stable release) | 15.11.2018 | Bugfix release

OpenAPI Spec compatibility: 1.0, 1.1, 1.2, 2.0, 3.0

For old releases, please refer to the [**Release**](https://github.com/OpenAPITools/openapi-generator/releases) page.

## [1.2 - Artifacts on Maven Central](#table-of-contents)

You can find our released artefacts on maven central:

**Core:**
```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator</artifactId>
    <version>${openapi-generator-version}</version>
</dependency>
```
See the different versions of the [openapi-generator](https://mvnrepository.com/artifact/org.openapitools/openapi-generator) artifact available on maven central.

**Cli:**
```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-cli</artifactId>
    <version>${openapi-generator-version}</version>
</dependency>
```
See the different versions of the [openapi-generator-cli](https://mvnrepository.com/artifact/org.openapitools/openapi-generator-cli) artifact available on maven central.

**Maven plugin:**
```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
    <version>${openapi-generator-version}</version>
</dependency>
```
* See the different versions of the [openapi-generator-maven-plugin](https://mvnrepository.com/artifact/org.openapitools/openapi-generator-maven-plugin) artifact available on maven central.
* [Readme](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-maven-plugin/README.md)

**Gradle plugin:**
```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-gradle-plugin</artifactId>
    <version>${openapi-generator-version}</version>
</dependency>
```
* See the different versions of the [openapi-generator-gradle-plugin](https://mvnrepository.com/artifact/org.openapitools/openapi-generator-gradle-plugin) artifact available on maven central.
* [Readme](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-gradle-plugin/README.adoc)

### [1.3 - Download JAR](#table-of-contents)

If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 8 runtime at a minimum):

JAR location: `http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/3.3.3/openapi-generator-cli-3.3.3.jar`

For **Mac/Linux** users:
```sh
wget http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/3.3.3/openapi-generator-cli-3.3.3.jar -O openapi-generator-cli.jar
```

For **Windows** users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+), e.g.
```
Invoke-WebRequest -OutFile openapi-generator-cli.jar http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/3.3.3/openapi-generator-cli-3.3.3.jar
```

After downloading the JAR, run `java -jar openapi-generator-cli.jar help` to show the usage.

For Mac users, please make sure Java 8 is installed (Tips: run `java -version` to check the version), and export `JAVA_HOME` in order to use the supported Java version:
```sh
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```

### Launcher Script

One downside to manual jar downloads is that you don't keep up-to-date with the latest released version. We have a Bash launcher script at [bin/utils/openapi-generator.cli.sh](./bin/utils/openapi-generator-cli.sh) which resolves this issue.

To install the launcher script, copy the contents of the script to a location on your path and make the script executable.

An example of setting this up (NOTE: Always evaluate scripts curled from external systems before executing them).

```
mkdir -p ~/bin/openapitools
curl https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/bin/utils/openapi-generator-cli.sh > ~/bin/openapitools/openapi-generator-cli
chmod u+x ~/bin/openapitools/openapi-generator-cli
export PATH=$PATH:~/bin/openapitools/
```

Now, `openapi-generator-cli` is "installed". On invocation, it will query the GitHub repository for the most recently released version. If this matches the last downloaded jar,
it will execute as normal. If a newer version is found, the script will download the latest release and execute it.

If you need to invoke an older version of the generator, you can define the variable `OPENAPI_GENERATOR_VERSION` either ad hoc or globally. You can export this variable if you'd like to persist a specific release version.

Examples:

```
# Execute latest released openapi-generator-cli
openapi-generator-cli version

# Execute version 3.1.0 for the current invocation, regardless of the latest released version
OPENAPI_GENERATOR_VERSION=3.1.0 openapi-generator-cli version

# Execute version 3.1.0-SNAPSHOT for the current invocation
OPENAPI_GENERATOR_VERSION=3.1.0-SNAPSHOT openapi-generator-cli version

# Execute version 3.0.2 for every invocation in the current shell session
export OPENAPI_GENERATOR_VERSION=3.0.2
openapi-generator-cli version # is 3.0.2
openapi-generator-cli version # is also 3.0.2

# To "install" a specific version, set the variable in .bashrc/.bash_profile
echo "export OPENAPI_GENERATOR_VERSION=3.0.2" >> ~/.bashrc
source ~/.bashrc
openapi-generator-cli version # is always 3.0.2, unless any of the above overrides are done ad hoc
```

### [1.4 - Build Projects](#table-of-contents)

To build from source, you need the following installed and available in your `$PATH:`

* [Java 8](http://java.oracle.com)

* [Apache maven 3.3.3 or greater](http://maven.apache.org/)

After cloning the project, you can build it from source with this command:
```sh
mvn clean install
```

If you don't have maven installed, you may directly use the included [maven wrapper](https://github.com/takari/maven-wrapper), and build with the command:
```sh
./mvnw clean install
```

### [1.5 - Homebrew](#table-of-contents)

To install, run `brew install openapi-generator`

Here is an example usage to generate a Ruby client:
```sh
openapi-generator generate -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g ruby -o /tmp/test/
```

To reinstall with the latest master, run `brew reinstall --HEAD openapi-generator`

### [1.6 - Docker](#table-of-contents)

#### Public Pre-built Docker images

 - [https://hub.docker.com/r/openapitools/openapi-generator-cli/](https://hub.docker.com/r/openapitools/openapi-generator-cli/) (official CLI)
 - [https://hub.docker.com/r/openapitools/openapi-generator-online/](https://hub.docker.com/r/openapitools/openapi-generator-online/) (official web service)


#### OpenAPI Generator CLI Docker Image

The OpenAPI Generator image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code with this image, you'll need to mount a local location as a volume.

Example:

```sh
docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli generate \
    -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
    -g go \
    -o /local/out/go
```

The generated code will be located under `./out/go` in the current directory.

#### OpenAPI Generator Online Docker Image

The openapi-generator-online image can act as a self-hosted web application and API for generating code. This container can be incorporated into a CI pipeline, and requires at least two HTTP requests and some docker orchestration to access generated code.

Example usage:

```sh
# Start container at port 8888 and save the container id
> CID=$(docker run -d -p 8888:8080 -e GENERATOR_HOST=http://localhost:8888 openapitools/openapi-generator-online)

# allow for startup
> sleep 10

# Get the IP of the running container (optional)
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  $CID)

# Execute an HTTP request to generate a Ruby client
> curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' \
-d '{"openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml"}' \
'http://localhost:8888/api/gen/clients/ruby'

{"code":"c2d483d3-3672-40e9-91df-b9ffd18d22b8","link":"http://localhost:8888/api/gen/download/c2d483d3-3672-40e9-91df-b9ffd18d22b8"}

# Download the generated zip file  
> wget http://localhost:8888/api/gen/download/c2d483d3-3672-40e9-91df-b9ffd18d22b8

# Unzip the file
> unzip c2d483d3-3672-40e9-91df-b9ffd18d22b8

# Shutdown the openapi generator image
> docker stop $CID && docker rm $CID
```

#### Development in docker

You can use `run-in-docker.sh` to do all development. This script maps your local repository to `/gen`
in the docker container. It also maps `~/.m2/repository` to the appropriate container location.

To execute `mvn package`:

```sh
git clone https://github.com/openapitools/openapi-generator
cd openapi-generator
./run-in-docker.sh mvn package
```

Build artifacts are now accessible in your working directory.

Once built, `run-in-docker.sh` will act as an executable for openapi-generator-cli. To generate code, you'll need to output to a directory under `/gen` (e.g. `/gen/out`). For example:

```sh
./run-in-docker.sh help # Executes 'help' command for openapi-generator-cli
./run-in-docker.sh list # Executes 'list' command for openapi-generator-cli
./run-in-docker.sh /gen/bin/go-petstore.sh  # Builds the Go client
./run-in-docker.sh generate -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
    -g go -o /gen/out/go-petstore -DpackageName=petstore # generates go client, outputs locally to ./out/go-petstore
```

##### Troubleshooting

If an error like this occurs, just execute the **mvn clean install -U** command:

> org.apache.maven.lifecycle.LifecycleExecutionException: Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:2.19.1:test (default-test) on project openapi-generator: A type incompatibility occurred while executing org.apache.maven.plugins:maven-surefire-plugin:2.19.1:test: java.lang.ExceptionInInitializerError cannot be cast to java.io.IOException

```sh
./run-in-docker.sh mvn clean install -U
```

> Failed to execute goal org.fortasoft:gradle-maven-plugin:1.0.8:invoke (default) on project openapi-generator-gradle-plugin-mvn-wrapper: org.gradle.tooling.BuildException: Could not execute build using Gradle distribution 'https://services.gradle.org/distributions/gradle-4.7-bin.zip'

Right now: no solution for this one :|

#### Run Docker in Vagrant
Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
 ```sh
git clone http://github.com/openapitools/openapi-generator.git
cd openapi-generator
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
```

### [1.7 - NPM](#table-of-contents)

There is also an [NPM package wrapper](https://github.com/HarmoWatch/openapi-generator-cli), available.
Please see the [docs](https://github.com/HarmoWatch/openapi-generator-cli) there for more information.

Install it globally to get the CLI available on the command line:

```sh
npm install @harmowatch/openapi-generator-cli -g
openapi-generator version
```

Or you install it as dev-dependency like this:

```sh
npm install @harmowatch/openapi-generator-cli -D
```

## [2 - Getting Started](#table-of-contents)

To generate a PHP client for [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml), please run the following
```sh
git clone https://github.com/openapitools/openapi-generator
cd openapi-generator
mvn clean package
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
   -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
   -g php \
   -o /var/tmp/php_api_client
```
(if you're on Windows, replace the last command with `java -jar modules\openapi-generator-cli\target\openapi-generator-cli.jar generate -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g php -o c:\temp\php_api_client`)

You can also download the JAR (latest release) directly from [maven.org](http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/3.0.0/openapi-generator-cli-3.0.0.jar)

To get a list of **general** options available, please run `java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar help generate`

To get a list of PHP specified options (which can be passed to the generator with a config file via the `-c` option), please run `java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar config-help -g php`

## [3 - Usage](#table-of-contents)

### To generate a sample client library
You can build a client against the [Petstore API](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml) as follows:

```sh
./bin/java-petstore-okhttp-gson.sh
```

(On Windows, run `.\bin\windows\java-petstore-okhttp-gson.bat` instead)

This script uses the default library, which is `okhttp-gson`. It will run the generator with this command:

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
  -g java \
  -o samples/client/petstore/java/okhttp-gson
```

with a number of options. [The java options are documented here.](docs/generators/java.md)

You can also get the options with the `help generate` command (below only shows partial results):

```
NAME
        openapi-generator-cli generate - Generate code with the specified
        generator.

SYNOPSIS
        openapi-generator-cli generate
                [(-a <authorization> | --auth <authorization>)]
                [--additional-properties <additional properties>...]
                [--api-package <api package>] [--artifact-id <artifact id>]
                [--artifact-version <artifact version>]
                [(-c <configuration file> | --config <configuration file>)]
                [-D <system properties>...]
                [(-g <generator name> | --generator-name <generator name>)]
                [--git-repo-id <git repo id>] [--git-user-id <git user id>]
                [--group-id <group id>] [--http-user-agent <http user agent>]
                (-i <spec file> | --input-spec <spec file>)
                [--ignore-file-override <ignore file override location>]
                [--import-mappings <import mappings>...]
                [--instantiation-types <instantiation types>...]
                [--invoker-package <invoker package>]
                [(-l <language> | --lang <language>)]
                [--language-specific-primitives <language specific primitives>...]
                [--library <library>] [--log-to-stderr]
                [--model-name-prefix <model name prefix>]
                [--model-name-suffix <model name suffix>]
                [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)]
                [--release-note <release note>] [--remove-operation-id-prefix]
                [--reserved-words-mappings <reserved word mappings>...]
                [(-s | --skip-overwrite)] [--skip-validate-spec]
                [(-t <template directory> | --template-dir <template directory>)]
                [--type-mappings <type mappings>...] [(-v | --verbose)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the OpenAPI definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

...... (results omitted)

        -v, --verbose
            verbose mode

```

You can then compile and run the client, as well as unit tests against it:

```sh
cd samples/client/petstore/java/okhttp-gson
mvn package
```

Other languages have petstore samples, too:
```sh
./bin/android-petstore-all.sh
./bin/java-petstore-all.sh
./bin/objc-petstore.sh
```

... and others. [Here is a list of all scripts.](https://github.com/OpenAPITools/openapi-generator/wiki/Samples-folder#scripts)

### [3.1 - Customization](#table-of-contents)

Please refer to [customization.md](docs/customization.md) on how to customize the output (e.g. package name, version)

### [3.2 - Workflow Integration (Maven, Gradle, Github, CI/CD)](#table-of-contents)

Please refer to [integration.md](docs/integration.md) on how to integrate OpenAPI generator with Maven, Gradle, Github and CI/CD.

### [3.3 - Online OpenAPI generator](#table-of-contents)

Please refer to [online-openapi-generator.md](docs/online-openapi-generator.md) on how to run and use the `openapi-generator-online` - a web service for `openapi-generator`.

### [3.4 - License information on Generated Code](#table-of-contents)

The OpenAPI Generator project is intended as a benefit for users of the Open API Specification.  The project itself has the [License](#license) as specified. In addition, please understand the following points:

* The templates included with this project are subject to the [License](#license).
* Generated code is intentionally _not_ subject to the parent project license

When code is generated from this project, it shall be considered **AS IS** and owned by the user of the software.  There are no warranties--expressed or implied--for generated code.  You can do what you wish with it, and once generated, the code is your responsibility and subject to the licensing terms that you deem appropriate.
 
## [4 - Companies/Projects using OpenAPI Generator](#table-of-contents)
Here are some companies/projects (alphabetical order) using OpenAPI Generator in production. To add your company/project to the list, please visit [README.md](README.md) and click on the icon to edit the page.

- [Angular.Schule](https://angular.schule/)
- [Bithost GmbH](https://www.bithost.ch)
- [Boxever](https://www.boxever.com/)
- [GMO Pepabo](https://pepabo.com/en/)
- [JustStar](https://www.juststarinfo.com)
- [Klarna](https://www.klarna.com/)
- [Myworkout](https://myworkout.com)
- [Raiffeisen Schweiz Genossenschaft](https://www.raiffeisen.ch)
- [RepreZen API Studio](https://www.reprezen.com/swagger-openapi-code-generation-api-first-microservices-enterprise-development)
- [REST United](https://restunited.com)
- [Suva](https://www.suva.ch/)
- [Telstra](https://dev.telstra.com)
- [TUI InfoTec GmbH](http://www.tui-infotec.com/)
- [unblu inc.](https://www.unblu.com/)
- [Zalando](https://www.zalando.com)

## [5 - Presentations/Videos/Tutorials/Books](#table-of-contents)

- 2018/05/12 - [OpenAPI Generator - community drivenで成長するコードジェネレータ](https://ackintosh.github.io/blog/2018/05/12/openapi-generator/) by [中野暁人](https://github.com/ackintosh)
- 2018/05/15 - [Starting a new open-source project](http://jmini.github.io/blog/2018/2018-05-15_new-open-source-project.html) by [Jeremie Bresson](https://github.com/jmini)
- 2018/05/15 - [REST API仕様からAPIクライアントやスタブサーバを自動生成する「OpenAPI Generator」オープンソースで公開。Swagger Codegenからのフォーク](https://www.publickey1.jp/blog/18/rest_apiapiopenapi_generatorswagger_generator.html) by [Publickey](https://www.publickey1.jp)
- 2018/06/08 - [Swagger Codegen is now OpenAPI Generator](https://angular.schule/blog/2018-06-swagger-codegen-is-now-openapi-generator) by [JohannesHoppe](https://github.com/JohannesHoppe)
- 2018/06/21 - [Connect your JHipster apps to the world of APIs with OpenAPI and gRPC](https://fr.slideshare.net/chbornet/jhipster-conf-2018-connect-your-jhipster-apps-to-the-world-of-apis-with-openapi-and-grpc) by [Christophe Bornet](https://github.com/cbornet) at [JHipster Conf 2018](https://jhipster-conf.github.io/)
- 2018/06/27 - [Lessons Learned from Leading an Open-Source Project Supporting 30+ Programming Languages](https://speakerdeck.com/wing328/lessons-learned-from-leading-an-open-source-project-supporting-30-plus-programming-languages) - [William Cheng](https://github.com/wing328) at [LinuxCon + ContainerCon + CloudOpen China 2018](http://bit.ly/2waDKKX)
- 2018/07/19 - [OpenAPI Generator Contribution Quickstart - RingCentral Go SDK](https://medium.com/ringcentral-developers/openapi-generator-for-go-contribution-quickstart-8cc72bf37b53) by [John Wang](https://github.com/grokify)
- 2018/08/22 - [OpenAPI Generatorのプロジェクト構成などのメモ](https://yinm.info/20180822/) by [Yusuke Iinuma](https://github.com/yinm)
- 2018/10/31 - [A node package wrapper for openapi-generator](https://github.com/HarmoWatch/openapi-generator-cli)
- 2018/11/03 - [OpenAPI Generator + golang + Flutter でアプリ開発](http://ryuichi111std.hatenablog.com/entry/2018/11/03/214005) by [Ryuichi Daigo](https://github.com/ryuichi111)


## [6 - About Us](#table-of-contents)

### [6.1 - OpenAPI Generator Core Team](#table-of-contents)

OpenAPI Generator core team members are contributors who have been making significant contributions (review issues, fix bugs, make enhancements, etc) to the project on a regular basis.

#### Core Team Members
* [@wing328](https://github.com/wing328) (2015/07)
* [@jimschubert](https://github.com/jimschubert) (2016/05)
* [@cbornet](https://github.com/cbornet) (2016/05)
* [@jaz-ah](https://github.com/jaz-ah) (2016/05)
* [@ackintosh](https://github.com/ackintosh) (2018/02)
* [@JFCote](https://github.com/JFCote) (2018/03)
* [@jmini](https://github.com/jmini) (2018/04)

#### Template Creator
Here is a list of template creators:
 * API Clients:
   * Ada: @stcarrez
   * Akka-Scala: @cchafer
   * Apex: @asnelling
   * Bash: @bkryza
   * C: @PowerOfCreation @zhemant
   * C++ REST: @Danielku15
   * C# (.NET 2.0): @who
   * C# (.NET Standard 1.3 ): @Gronsak
   * C# (.NET 4.5 refactored): @jimschubert
   * Clojure: @xhh
   * Dart: @yissachar
   * Dart (refactor): @joernahrens
   * Dart 2: @swipesight
   * Dart (Jaguar): @jaumard
   * Elixir: @niku
   * Elm: @trenneman
   * Eiffel: @jvelilla
   * Erlang: @tsloughter
   * Erlang (PropEr): @jfacorro @robertoaloi
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
   * Java (Spring 5 WebClient): @daonomic
   * Java (RESTEasy): @gayathrigs
   * Java (Vertx): @lopesmcc
   * Java (Google APIs Client Library): @charlescapps
   * Java (Rest-assured): @viclovsky
   * Javascript/NodeJS: @jfiala
   * Javascript (Closure-annotated Angular) @achew22
   * Javascript (Flow types) @jaypea
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
   * TypeScript (Angular1): @mhardorf
   * TypeScript (Angular2): @roni-frantchi
   * TypeScript (Angular6): @akehir 
   * TypeScript (Angular7): @topce
   * TypeScript (Axios): @nicokoenig
   * TypeScript (Fetch): @leonyu
   * TypeScript (jQuery): @bherila
   * TypeScript (Node):  @mhardorf
 * Server Stubs
   * Ada: @stcarrez
   * C# ASP.NET5: @jimschubert
   * C# NancyFX: @mstefaniuk
   * C++ (Qt5 QHttpEngine): @etherealjoy
   * C++ Pistache: @sebymiano
   * C++ Restbed: @stkrwork
   * Erlang Server: @galaxie
   * Go Server: @guohuang
   * Go (Gin) Server: @kemokemo
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
   * Kotlin: @jimschubert
   * Kotlin (Spring Boot): @dr4ke616
   * PHP Laravel: @renepardon
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
 * Schema
   * MySQL: @ybelenko

#### How to join the core team

Here are the requirements to become a core team member:
- rank within top 50 in https://github.com/openapitools/openapi-generator/graphs/contributors
  - to contribute, here are some good [starting points](https://github.com/openapitools/openapi-generator/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)
- regular contributions to the project
  - about 3 hours per week
  - for contribution, it can be addressing issues, reviewing PRs submitted by others, submitting PR to fix bugs or make enhancements, etc
  - must be active in the past 3 months at the time of application

 To join the core team, please reach out to team@openapitools.org for more information.

 To become a Template Creator, simply submit a PR for new API client (e.g. Rust, Elixir) or server stub (e.g. Ruby Grape) generator.

### [6.2 - OpenAPI Generator Technical Committee](#table-of-contents)

Members of the OpenAPI Generator technical committee shoulder the following responsibilities:

- Provides guidance and direction to other users
- Reviews pull requests and issues
- Improves the generator by making enhancements, fixing bugs or updating documentations
- Sets the technical direction of the generator

Who is eligible? Those who want to join must have at least 3 PRs merged into a generator. (Exceptions can be granted to template creators or contributors who have made a lot of code changes with less than 3 merged PRs)

If you want to join the committee, please kindly apply by sending an email to team@openapitools.org with your Github ID.

#### Members of Technical Committee

| Languages    | Member (join date) |
|:-------------|:-------------|
| ActionScript |      |
| Ada       | @stcarrez (2018/02) @micheleISEP (2018/02) |
| Android   | @jaz-ah (2017/09) |
| Apex      |  |
| Bash      | @frol (2017/07) @bkryza (2017/08) @kenjones-cisco (2017/09) |
| C         | @zhemant (2018/11) |
| C++       | @ravinikam (2017/07) @stkrwork (2017/07) @fvarose (2017/11) @etherealjoy (2018/02) @martindelille (2018/03) |
| C#        | @mandrean (2017/08) @jimschubert (2017/09) |
| Clojure   |  |
| Dart      | @ircecho (2017/07) @swipesight (2018/09) @jaumard (2018/09) |
| Eiffel    | @jvelilla (2017/09) |
| Elixir    |  |
| Elm       | @trenneman (2018/09) |
| Erlang    | @tsloughter (2017/11) @jfacorro (2018/10) @robertoaloi (2018/10) |
| Go        | @antihax (2017/11) @bvwells (2017/12) @grokify (2018/07) @kemokemo (2018/09 |
| Groovy    |  |
| Haskell   |  |
| Java      | @bbdouglas (2017/07) @JFCote (2017/08) @sreeshas (2017/08) @jfiala (2017/08) @lukoyanov (2017/09) @cbornet (2017/09) @jeff9finger (2018/01) |
| Kotlin    | @jimschubert (2017/09) @dr4ke616 (2018/08) |
| Lua       | @daurnimator (2017/08) |
| NodeJS/Javascript | @CodeNinjai (2017/07) @frol (2017/07) @cliffano (2017/07) |
| ObjC      |  |
| Perl      | @wing328 (2017/07) |
| PHP       | @jebentier (2017/07) @dkarlovi (2017/07) @mandrean (2017/08) @jfastnacht (2017/09) @ackintosh (2017/09) @ybelenko (2018/07) |
| PowerShell | |
| Python    | @taxpon (2017/07) @frol (2017/07) @mbohlool (2017/07) @cbornet (2017/09) @kenjones-cisco (2017/11) @tomplus (2018/10) |
| R         |  |
| Ruby      | @cliffano (2017/07) @zlx (2017/09) |
| Rust      | @frol (2017/07) @farcaller (2017/08) @bjgill (2017/12) |
| Scala     | @clasnake (2017/07) @jimschubert (2017/09) @shijinkui  (2018/01) @ramzimaalej (2018/03) |
| Swift     | @jgavris (2017/07) @ehyche (2017/08) @Edubits (2017/09) @jaz-ah (2017/09) @d-date  (2018/03) |
| TypeScript | @TiFu (2017/07) @taxpon (2017/07) @sebastianhaas (2017/07) @kenisteward (2017/07) @Vrolijkx (2017/09) @macjohnny (2018/01) @nicokoenig (2018/09) @topce (2018/10) |

### [6.3 - History of OpenAPI Generator](#table-of-contents)

OpenAPI Generator is a fork of [Swagger Codegen](https://github.com/swagger-api/swagger-codegen). In view of the issues with the Swagger Codegen 3.0.0 (beta) release and the disagreement on the project's direction, more than 40 top contributors and template creators of Swagger Codegen decided to fork Swagger Codegen and maintain a community-driven version called "OpenAPI Generator". Please refer to the [Q&A](docs/qna.md) for more information.

#### Founding Members (alphabetical order):

- [Akihito Nakano](https://github.com/ackintosh)
- [Artem Ocheredko](https://github.com/galaxie)
- [Arthur Mogliev](https://github.com/Articus)
- [Bartek Kryza](https://github.com/bkryza)
- [Ben Wells](https://github.com/bvwells)
- [Benjamin Gill](https://github.com/bjgill)
- [Christophe Bornet](https://github.com/cbornet)
- [Cliffano Subagio](https://github.com/cliffano)
- [Daiki Matsudate](https://github.com/d-date)
- [Daniel](https://github.com/Danielku15)
- [Emiliano Bonassi](https://github.com/emilianobonassi)
- [Erik Timmers](https://github.com/trenneman)
- [Esteban Marin](https://github.com/macjohnny)
- [Gustavo Paz](https://github.com/gustavoapaz)
- [Javier Velilla](https://github.com/jvelilla)
- [Jean-François Côté](https://github.com/JFCote)
- [Jim Schubert](https://github.com/jimschubert)
- [Jon Schoning](https://github.com/jonschoning)
- [Jérémie Bresson](https://github.com/jmini)
- [Jörn Ahrens](https://github.com/jayearn)
- [Keni Steward](https://github.com/kenisteward)
- [Marcin Stefaniuk](https://github.com/mstefaniuk)
- [Martin Delille](https://github.com/MartinDelille)
- [Masahiro Yamauchi](https://github.com/algas)
- [Michele Albano](https://github.com/micheleISEP)
- [Ramzi Maalej](https://github.com/ramzimaalej)
- [Ravindra Nikam](https://github.com/ravinikam)
- [Ricardo Cardona](https://github.com/ricardona)
- [Sebastian Haas](https://github.com/sebastianhaas)
- [Sebastian Mandrean](https://github.com/mandrean)
- [Sreenidhi Sreesha](https://github.com/sreeshas)
- [Stefan Krismann](https://github.com/stkrwork)
- [Stephane Carrez](https://github.com/stcarrez)
- [Takuro Wada](https://github.com/taxpon)
- [Tomasz Prus](https://github.com/tomplus)
- [Tristan Sloughter](https://github.com/tsloughter)
- [Victor Orlovsky](https://github.com/viclovsky)
- [Victor Trakhtenberg](https://github.com/victorgit)
- [Vlad Frolov](https://github.com/frol)
- [Vladimir Pouzanov](https://github.com/farcaller)
- [William Cheng](https://github.com/wing328)
- [Xin Meng](https://github.com/xmeng1)
- [Xu Hui Hui](https://github.com/xhh)
- [antihax](https://github.com/antihax)
- [beatcracker](https://github.com/beatcracker)
- [daurnimator](https:/github.com/daurnimator)
- [etherealjoy](https://github.com/etherealjoy)
- [jfiala](https://github.com/jfiala)
- [lukoyanov](https://github.com/lukoyanov)


## [7 - License](#table-of-contents)
-------

Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)  
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
