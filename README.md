<h1 align="center">OpenAPI Generator</h1>


<div align="center">

[![Stable releases in Maven Central](https://img.shields.io/maven-metadata/v/https/repo1.maven.org/maven2/org/openapitools/openapi-generator/maven-metadata.xml.svg)](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.openapitools%22%20AND%20a%3A%22openapi-generator%22) [![Apache 2.0 License](https://img.shields.io/badge/License-Apache%202.0-orange)](./LICENSE) [![Open Collective backers](https://img.shields.io/opencollective/backers/openapi_generator?color=orange&label=OpenCollective%20Backers)](https://opencollective.com/openapi_generator) [![Join the Slack chat room](https://img.shields.io/badge/Slack-Join%20the%20chat%20room-orange)](https://join.slack.com/t/openapi-generator/shared_invite/zt-12jxxd7p2-XUeQM~4pzsU9x~eGLQqX2g) [![Follow OpenAPI Generator Twitter account to get the latest update](https://img.shields.io/twitter/follow/oas_generator.svg?style=social&label=Follow)](https://twitter.com/oas_generator)

</div>

<div align="center">

[Master](https://github.com/OpenAPITools/openapi-generator/tree/master) (`6.0.x`):
[![Build Status](https://img.shields.io/travis/OpenAPITools/openapi-generator/master.svg?label=Integration%20Test)](https://travis-ci.com/OpenAPITools/openapi-generator)
[![Integration Test2](https://circleci.com/gh/OpenAPITools/openapi-generator.svg?style=shield)](https://circleci.com/gh/OpenAPITools/openapi-generator)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/openapitools/openapi-generator?branch=master&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/WilliamCheng/openapi-generator)
[![JDK11 Build](https://cloud.drone.io/api/badges/OpenAPITools/openapi-generator/status.svg?ref=refs/heads/master)](https://cloud.drone.io/OpenAPITools/openapi-generator)
[![Bitrise](https://img.shields.io/bitrise/4a2b10a819d12b67/master?label=bitrise%3A%20Swift+4,5&token=859FMDR8QHwabCzwvZK6vQ)](https://app.bitrise.io/app/4a2b10a819d12b67)
[![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/openapitools/openapi-generator/Check%20Supported%20Java%20Versions/master?label=Check%20Supported%20Java%20Versions&logo=github&logoColor=green)](https://github.com/OpenAPITools/openapi-generator/actions?query=workflow%3A%22Check+Supported+Java+Versions%22)

</div>

<div align="center">

:star::star::star: If you would like to contribute, please refer to [guidelines](CONTRIBUTING.md) and a list of [open tasks](https://github.com/openapitools/openapi-generator/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22).:star::star::star:

:bangbang: To migrate from Swagger Codegen to OpenAPI Generator, please refer to the [migration guide](docs/migration-from-swagger-codegen.md) :bangbang:

:notebook_with_decorative_cover: For more information, please refer to the [Wiki page](https://github.com/openapitools/openapi-generator/wiki) and [FAQ](https://github.com/openapitools/openapi-generator/wiki/FAQ) :notebook_with_decorative_cover:

:notebook_with_decorative_cover: The eBook [A Beginner's Guide to Code Generation for REST APIs](https://gum.co/openapi_generator_ebook) is a good starting point for beginners :notebook_with_decorative_cover:

:warning: If the OpenAPI spec, templates or any input (e.g. options, environment variables) is obtained from an untrusted source or environment, please make sure you've reviewed these inputs before using OpenAPI Generator to generate the API client, server stub or documentation to avoid potential security issues (e.g. [code injection](https://en.wikipedia.org/wiki/Code_injection)). For security vulnerabilities, please contact [team@openapitools.org](mailto:team@openapitools.org). :warning:

:bangbang: Both "OpenAPI Tools" (https://OpenAPITools.org - the parent organization of OpenAPI Generator) and "OpenAPI Generator" are not affiliated with OpenAPI Initiative (OAI) :bangbang:

</div>

## Sponsors

If you find OpenAPI Generator useful for work, please consider asking your company to support this Open Source project by [becoming a sponsor](https://opencollective.com/openapi_generator). You can also individually sponsor the project by [becoming a backer](https://opencollective.com/openapi_generator).

#### Thank you to our bronze sponsors!

[![NamSor](https://openapi-generator.tech/img/companies/namsor.png)](https://www.namsor.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[![LightBow](https://openapi-generator.tech/img/companies/lightbow.png)](https://www.lightbow.net/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/docspring.png" width="128" height="128">](https://docspring.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/datadog.png" width="128" height="128">](https://datadoghq.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/thales.jpg" width="128" height="128">](https://cpl.thalesgroup.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/apideck.jpg" width="128" height="128">](https://www.apideck.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/pexa.png" width="128" height="128">](https://www.pexa.com.au/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://openapi-generator.tech/img/companies/numary.png" width="128" height="128">](https://www.numary.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)

#### Thank you GoDaddy for sponsoring the domain names, Linode for sponsoring the VPS and Checkly for sponsoring the API monitoring

[<img src="https://openapi-generator.tech/img/companies/godaddy.png" width="150">](https://www.godaddy.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[![Linode](https://www.linode.com/media/images/logos/standard/light/linode-logo_standard_light_small.png)](https://www.linode.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)
[<img src="https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcRAhEYadUyZYzGUotZiSdXkVMqqLGuohyixLl4eUpUV6pAbUULL" width="150">](https://checklyhq.com/?utm_source=openapi_generator&utm_medium=github_webpage&utm_campaign=sponsor)


## Overview
OpenAPI Generator allows generation of API client libraries (SDK generation), server stubs,  documentation and configuration automatically given an [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification) (both 2.0 and 3.0 are supported). Currently, the following languages/frameworks are supported:

|                                  | Languages/Frameworks                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| -------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **API clients**                  | **ActionScript**, **Ada**, **Apex**, **Bash**, **C**, **C#** (.net 2.0, 3.5 or later, .NET Standard 1.3 - 2.1, .NET Core 3.1, .NET 5.0. Libraries: RestSharp, GenericHost, HttpClient), **C++** (Arduino, cpp-restsdk, Qt5, Tizen, Unreal Engine 4), **Clojure**, **Crystal**, **Dart**, **Elixir**, **Elm**, **Eiffel**, **Erlang**, **Go**, **Groovy**, **Haskell** (http-client, Servant), **Java** (Apache HttpClient, Jersey1.x, Jersey2.x, OkHttp, Retrofit1.x, Retrofit2.x, Feign, RestTemplate, RESTEasy, Vertx, Google API Client Library for Java, Rest-assured, Spring 5 Web Client, MicroProfile Rest Client), **k6**, **Kotlin**, **Lua**, **Nim**, **Node.js/JavaScript** (ES5, ES6, AngularJS with Google Closure Compiler annotations, Flow types, Apollo GraphQL DataStore), **Objective-C**, **OCaml**, **Perl**, **PHP**, **PowerShell**, **Python**, **R**, **Ruby**, **Rust** (hyper, reqwest, rust-server), **Scala** (akka, http4s, scalaz, sttp, swagger-async-httpclient), **Swift** (2.x, 3.x, 4.x, 5.x), **Typescript** (AngularJS, Angular (2.x - 11.x), Aurelia, Axios, Fetch, Inversify, jQuery, Nestjs, Node, redux-query, Rxjs) |
| **Server stubs**                 | **Ada**, **C#** (ASP.NET Core, Azure Functions), **C++** (Pistache, Restbed, Qt5 QHTTPEngine), **Erlang**, **F#** (Giraffe), **Go** (net/http, Gin, Echo), **Haskell** (Servant, Yesod), **Java** (MSF4J, Spring, Undertow, JAX-RS: CDI, CXF, Inflector, Jersey, RestEasy, Play Framework, [PKMST](https://github.com/ProKarma-Inc/pkmst-getting-started-examples), [Vert.x](https://vertx.io/), [Apache Camel](https://camel.apache.org/)), **Kotlin** (Spring Boot, Ktor, Vertx), **PHP** (Laravel, Lumen, [Mezzio (fka Zend Expressive)](https://github.com/mezzio/mezzio), Slim, Silex, [Symfony](https://symfony.com/)), **Python** (FastAPI, Flask), **NodeJS**, **Ruby** (Sinatra, Rails5), **Rust** (rust-server), **Scala** (Akka, [Finch](https://github.com/finagle/finch), [Lagom](https://github.com/lagom/lagom), [Play](https://www.playframework.com/), Scalatra)                                                                                                                                                                                         |
| **API documentation generators** | **HTML**, **Confluence Wiki**, **Asciidoc**, **Markdown**, **PlantUML**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| **Configuration files**          | [**Apache2**](https://httpd.apache.org/)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| **Others**                       | **GraphQL**, **JMeter**, **Ktorm**, **MySQL Schema**, **Protocol Buffer**, **WSDL**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

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
    - [3.5 - IDE Integration](#35---ide-integration)
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

| OpenAPI Generator Version                                                                                                                                 | Release Date | Notes                                             |
| --------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------ | ------------------------------------------------- |
| 6.0.0 (upcoming major release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/org/openapitools/openapi-generator-cli/6.0.0-SNAPSHOT/) | Feb/Mar 2022   | Major release with breaking changes (no fallback) |
| [5.4.0](https://github.com/OpenAPITools/openapi-generator/releases/tag/v5.4.0) (latest stable release)                                                    | 31.01.2021   | Minor release with breaking changes (with fallback) |
| [4.3.1](https://github.com/OpenAPITools/openapi-generator/releases/tag/v4.3.1)                                                    | 06.05.2020   | Patch release (enhancements, bug fixes, etc)                       |

OpenAPI Spec compatibility: 1.0, 1.1, 1.2, 2.0, 3.0

For old releases, please refer to the [**Release**](https://github.com/OpenAPITools/openapi-generator/releases) page.

## [1.2 - Artifacts on Maven Central](#table-of-contents)

You can find our released artifacts on maven central:

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
<!-- RELEASE_VERSION -->
If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 8 runtime at a minimum):

JAR location: `https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.4.0/openapi-generator-cli-5.4.0.jar`

For **Mac/Linux** users:
```sh
wget https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.4.0/openapi-generator-cli-5.4.0.jar -O openapi-generator-cli.jar
```

For **Windows** users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+), e.g.
```
Invoke-WebRequest -OutFile openapi-generator-cli.jar https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.4.0/openapi-generator-cli-5.4.0.jar
```

After downloading the JAR, run `java -jar openapi-generator-cli.jar help` to show the usage.

For Mac users, please make sure Java 8 is installed (Tips: run `java -version` to check the version), and export `JAVA_HOME` in order to use the supported Java version:
```sh
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```
<!-- /RELEASE_VERSION -->
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

# Execute version 4.1.0 for the current invocation, regardless of the latest released version
OPENAPI_GENERATOR_VERSION=4.1.0 openapi-generator-cli version

# Execute version 4.1.0-SNAPSHOT for the current invocation
OPENAPI_GENERATOR_VERSION=4.1.0-SNAPSHOT openapi-generator-cli version

# Execute version 4.0.2 for every invocation in the current shell session
export OPENAPI_GENERATOR_VERSION=4.0.2
openapi-generator-cli version # is 4.0.2
openapi-generator-cli version # is also 4.0.2

# To "install" a specific version, set the variable in .bashrc/.bash_profile
echo "export OPENAPI_GENERATOR_VERSION=4.0.2" >> ~/.bashrc
source ~/.bashrc
openapi-generator-cli version # is always 4.0.2, unless any of the above overrides are done ad hoc
```

### [1.4 - Build Projects](#table-of-contents)

To build from source, you need the following installed and available in your `$PATH:`

* [Java 8](https://www.oracle.com/technetwork/java/index.html)

* [Apache Maven 3.3.4 or greater](https://maven.apache.org/)

After cloning the project, you can build it from source with this command:
```sh
mvn clean install
```

If you don't have maven installed, you may directly use the included [maven wrapper](https://github.com/takari/maven-wrapper), and build with the command:
```sh
./mvnw clean install
```

The default build contains minimal static analysis (via CheckStyle). To run your build with PMD and Spotbugs, use the `static-analysis` profile:

```sh
mvn -Pstatic-analysis clean install
```

### [1.5 - Homebrew](#table-of-contents)

To install, run `brew install openapi-generator`

Here is an example usage to generate a Ruby client:
```sh
openapi-generator generate -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g ruby -o /tmp/test/
```

To reinstall with the latest master, run `brew uninstall openapi-generator && brew install --HEAD openapi-generator`

To install OpenJDK (pre-requisites), please run
```sh
brew tap AdoptOpenJDK/openjdk
brew install --cask adoptopenjdk12
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-12.0.2.jdk/Contents/Home/
```

To install Maven, please run
```sh
brew install maven
```

### [1.6 - Docker](#table-of-contents)

#### Public Pre-built Docker images

 - [https://hub.docker.com/r/openapitools/openapi-generator-cli/](https://hub.docker.com/r/openapitools/openapi-generator-cli/) (official CLI)
 - [https://hub.docker.com/r/openapitools/openapi-generator-online/](https://hub.docker.com/r/openapitools/openapi-generator-online/) (official web service)


#### OpenAPI Generator CLI Docker Image

The OpenAPI Generator image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code with this image, you'll need to mount a local location as a volume.

Example:

```sh
docker run --rm -v "${PWD}:/local" openapitools/openapi-generator-cli generate \
    -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
    -g go \
    -o /local/out/go
```

The generated code will be located under `./out/go` in the current directory.

#### OpenAPI Generator Online Docker Image

The openapi-generator-online image can act as a self-hosted web application and API for generating code. This container can be incorporated into a CI pipeline, and requires at least two HTTP requests and some docker orchestration to access generated code.

Example usage:

```sh
# Start container at port 8888 and save the container id
> CID=$(docker run -d -p 8888:8080 openapitools/openapi-generator-online)

# allow for startup
> sleep 10

# Get the IP of the running container (optional)
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  $CID)

# Execute an HTTP request to generate a Ruby client
> curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' \
-d '{"openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"}' \
'http://localhost:8888/api/gen/clients/ruby'

{"code":"c2d483.3.4672-40e9-91df-b9ffd18d22b8","link":"http://localhost:8888/api/gen/download/c2d483.3.4672-40e9-91df-b9ffd18d22b8"}

# Download the generated zip file
> wget http://localhost:8888/api/gen/download/c2d483.3.4672-40e9-91df-b9ffd18d22b8

# Unzip the file
> unzip c2d483.3.4672-40e9-91df-b9ffd18d22b8

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
./run-in-docker.sh generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
    -g go -o /gen/out/go-petstore --package-name=petstore # generates go client, outputs locally to ./out/go-petstore
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
git clone https://github.com/openapitools/openapi-generator.git
cd openapi-generator
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
```

### [1.7 - NPM](#table-of-contents)

There is also an [NPM package wrapper](https://www.npmjs.com/package/@openapitools/openapi-generator-cli) available for different platforms (e.g. Linux, Mac, Windows). (JVM is still required)
Please see the [project's README](https://github.com/openapitools/openapi-generator-cli) there for more information.

Install it globally to get the CLI available on the command line:

```sh
npm install @openapitools/openapi-generator-cli -g
openapi-generator-cli version
```

<!-- RELEASE_VERSION -->
To use a specific version of "openapi-generator-cli"

```sh
openapi-generator-cli version-manager set 5.4.0
```

Or install it as dev-dependency:

```sh
npm install @openapitools/openapi-generator-cli -D
```
<!-- /RELEASE_VERSION -->
## [2 - Getting Started](#table-of-contents)

To generate a PHP client for [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml), please run the following
```sh
git clone https://github.com/openapitools/openapi-generator
cd openapi-generator
mvn clean package
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
   -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
   -g php \
   -o /var/tmp/php_api_client
```
(if you're on Windows, replace the last command with `java -jar modules\openapi-generator-cli\target\openapi-generator-cli.jar generate -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g php -o c:\temp\php_api_client`)

<!-- RELEASE_VERSION -->
You can also download the JAR (latest release) directly from [maven.org](https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.4.0/openapi-generator-cli-5.4.0.jar)
<!-- /RELEASE_VERSION -->

To get a list of **general** options available, please run `java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar help generate`

To get a list of PHP specified options (which can be passed to the generator with a config file via the `-c` option), please run `java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar config-help -g php`

## [3 - Usage](#table-of-contents)

### To generate a sample client library
You can build a client against the [Petstore API](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml) as follows:

```sh
./bin/generate-samples.sh ./bin/configs/java-okhttp-gson.yaml
```

(On Windows, please install [GIT Bash for Windows](https://gitforwindows.org/) to run the command above)

This script uses the default library, which is `okhttp-gson`. It will run the generator with this command:

```sh
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
  -g java \
  -t modules/openapi-generator/src/main/resources/Java \
  --additional-properties artifactId=petstore-okhttp-gson,hideGenerationTimestamp:true \
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
                [--api-name-suffix <api name suffix>] [--api-package <api package>]
                [--artifact-id <artifact id>] [--artifact-version <artifact version>]
                [(-c <configuration file> | --config <configuration file>)] [--dry-run]
                [(-e <templating engine> | --engine <templating engine>)]
                [--enable-post-process-file]
                [(-g <generator name> | --generator-name <generator name>)]
                [--generate-alias-as-model] [--git-host <git host>]
                [--git-repo-id <git repo id>] [--git-user-id <git user id>]
                [--global-property <global properties>...] [--group-id <group id>]
                [--http-user-agent <http user agent>]
                [(-i <spec file> | --input-spec <spec file>)]
                [--ignore-file-override <ignore file override location>]
                [--import-mappings <import mappings>...]
                [--instantiation-types <instantiation types>...]
                [--invoker-package <invoker package>]
                [--language-specific-primitives <language specific primitives>...]
                [--legacy-discriminator-behavior] [--library <library>]
                [--log-to-stderr] [--minimal-update]
                [--model-name-prefix <model name prefix>]
                [--model-name-suffix <model name suffix>]
                [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)] [(-p <additional properties> | --additional-properties <additional properties>)...]
                [--package-name <package name>] [--release-note <release note>]
                [--remove-operation-id-prefix]
                [--reserved-words-mappings <reserved word mappings>...]
                [(-s | --skip-overwrite)] [--server-variables <server variables>...]
                [--skip-validate-spec] [--strict-spec <true/false strict behavior>]
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

Other generators have [samples](https://github.com/OpenAPITools/openapi-generator/tree/master/samples) too.

### [3.1 - Customization](#table-of-contents)

Please refer to [customization.md](docs/customization.md) on how to customize the output (e.g. package name, version)

### [3.2 - Workflow Integration (Maven, Gradle, Github, CI/CD)](#table-of-contents)

Please refer to [integration.md](docs/integration.md) on how to integrate OpenAPI generator with Maven, Gradle, sbt, Bazel, Github and CI/CD.

### [3.3 - Online OpenAPI generator](#table-of-contents)

Here are the public online services:

- latest stable version: https://api.openapi-generator.tech
- latest master: https://api-latest-master.openapi-generator.tech (updated with latest master every hour)

The server is sponsored by [Linode](https://www.linode.com/) [![Linode Logo](https://www.linode.com/media/images/logos/standard/light/linode-logo_standard_light_small.png)](https://www.linode.com/)

(These services are beta and do not have any guarantee on service level)

Please refer to [online.md](docs/online.md) on how to run and use the `openapi-generator-online` - a web service for `openapi-generator`.

### [3.4 - License information on Generated Code](#table-of-contents)

The OpenAPI Generator project is intended as a benefit for users of the Open API Specification.  The project itself has the [License](#license) as specified. In addition, please understand the following points:

* The templates included with this project are subject to the [License](#license).
* Generated code is intentionally _not_ subject to the parent project license

When code is generated from this project, it shall be considered **AS IS** and owned by the user of the software.  There are no warranties--expressed or implied--for generated code.  You can do what you wish with it, and once generated, the code is your responsibility and subject to the licensing terms that you deem appropriate.

### [3.5 - IDE Integration](#table-of-contents)

Here is a list of community-contributed IDE plug-ins that integrate with OpenAPI Generator:

- Eclipse: [Codewind OpenAPI Tools for Eclipse](https://www.eclipse.org/codewind/open-api-tools-for-eclipse.html) by [IBM](https://www.ibm.com)
- IntelliJ IDEA: [OpenAPI Generator](https://plugins.jetbrains.com/plugin/8433-openapi-generator) by [Jim Schubert](https://jimschubert.us/#/)
- IntelliJ IDEA: [Senya Editor](https://plugins.jetbrains.com/plugin/10690-senya-editor) by [senya.io](https://senya.io)
- [RepreZen API Studio](https://www.reprezen.com/)
- Visual Studio: [REST API Client Code Generator](https://marketplace.visualstudio.com/items?itemName=ChristianResmaHelle.ApiClientCodeGenerator) by [Christian Resma Helle](https://christian-helle.blogspot.com/)
- Visual Studio Code: [Codewind OpenAPI Tools](https://marketplace.visualstudio.com/items?itemName=IBM.codewind-openapi-tools) by [IBM](https://marketplace.visualstudio.com/publishers/IBM)


## [4 - Companies/Projects using OpenAPI Generator](#table-of-contents)
Here are some companies/projects (alphabetical order) using OpenAPI Generator in production. To add your company/project to the list, please visit [README.md](README.md) and click on the icon to edit the page.

- [Aalborg University](https://www.aau.dk)
- [Adaptant Solutions AG](https://www.adaptant.io/)
- [adesso SE](https://www.adesso.de/)
- [Agoda](https://www.agoda.com/)
- [Airthings](https://www.airthings.com/)
- [Allianz](https://www.allianz.com)
- [Angular.Schule](https://angular.schule/)
- [Aqovia](https://aqovia.com/)
- [Australia and New Zealand Banking Group (ANZ)](http://www.anz.com/)
- [ASKUL](https://www.askul.co.jp)
- [Arduino](https://www.arduino.cc/)
- [b<>com](https://b-com.com/en)
- [百度营销](https://e.baidu.com)
- [Banzai Cloud](https://banzaicloud.com)
- [BIMData.io](https://bimdata.io)
- [Bithost GmbH](https://www.bithost.ch)
- [Bosch Connected Industry](https://www.bosch-connected-industry.com)
- [Boxever](https://www.boxever.com/)
- [Bunker Holding Group](https://www.bunker-holding.com/)
- [California State University, Northridge](https://www.csun.edu)
- [CAM](https://www.cam-inc.co.jp/)
- [Camptocamp](https://www.camptocamp.com/en)
- [Cisco](https://www.cisco.com/)
- [codecentric AG](https://www.codecentric.de/)
- [CoinAPI](https://www.coinapi.io/)
- [Commencis](https://www.commencis.com/)
- [Crossover Health](https://crossoverhealth.com/)
- [Cupix](https://www.cupix.com/)
- [Datadog](https://www.datadoghq.com)
- [DB Systel](https://www.dbsystel.de)
- [Devsupply](https://www.devsupply.com/)
- [DocSpring](https://docspring.com/)
- [dwango](https://dwango.co.jp/)
- [Edge Impulse](https://www.edgeimpulse.com/)
- [Element AI](https://www.elementai.com/)
- [Embotics](https://www.embotics.com/)
- [emineo](https://www.emineo.ch)
- [Fenergo](https://www.fenergo.com/)
- [freee](https://corp.freee.co.jp/en/)
- [FreshCells](https://www.freshcells.de/)
- [Fuse](https://www.fuse.no/)
- [Gantner](https://www.gantner.com)
- [GenFlow](https://github.com/RepreZen/GenFlow)
- [GetYourGuide](https://www.getyourguide.com/)
- [GMO Pepabo](https://pepabo.com/en/)
- [GoDaddy](https://godaddy.com)
- [Gumtree](https://gumtree.com)
- [Here](https://developer.here.com/)
- [IBM](https://www.ibm.com/)
- [Instana](https://www.instana.com)
- [Interxion](https://www.interxion.com)
- [Inquisico](https://inquisico.com)
- [JustStar](https://www.juststarinfo.com)
- [k6.io](https://k6.io/)
- [Klarna](https://www.klarna.com/)
- [Kronsoft Development](https://www.kronsoft.ro/home/)
- [Kubernetes](https://kubernetes.io)
- [Linode](https://www.linode.com/)
- [Logicdrop](https://www.logicdrop.com)
- [Lumeris](https://www.lumeris.com)
- [LVM Versicherungen](https://www.lvm.de)
- [MailSlurp](https://www.mailslurp.com)
- [Manticore Search](https://manticoresearch.com)
- [Médiavision](https://www.mediavision.fr/)
- [Metaswitch](https://www.metaswitch.com/)
- [MoonVision](https://www.moonvision.io/)
- [Myworkout](https://myworkout.com)
- [NamSor](https://www.namsor.com/)
- [Neverfail](https://www.neverfail.com/)
- [NeuerEnergy](https://neuerenergy.com)
- [Nokia](https://www.nokia.com/)
- [Options Clearing Corporation (OCC)](https://www.theocc.com/)
- [Openet](https://www.openet.com/)
- [openVALIDATION](https://openvalidation.io/)
- [Oracle](https://www.oracle.com/)
- [Paxos](https://www.paxos.com)
- [Plaid](https://plaid.com)
- [PLAID, Inc.](https://plaid.co.jp/)
- [Ponicode](https://ponicode.dev/)
- [Pricefx](https://www.pricefx.com/)
- [PrintNanny](https://www.print-nanny.com/)
- [Prometheus/Alertmanager](https://github.com/prometheus/alertmanager)
- [Qavar](https://www.qavar.com)
- [QEDIT](https://qed-it.com)
- [Qulix Systems](https://www.qulix.com)
- [Raksul](https://corp.raksul.com)
- [Raiffeisen Schweiz Genossenschaft](https://www.raiffeisen.ch)
- [RedHat](https://www.redhat.com)
- [RepreZen API Studio](https://www.reprezen.com/swagger-openapi-code-generation-api-first-microservices-enterprise-development)
- [REST United](https://restunited.com)
- [Robotinfra](https://www.robotinfra.com)
- [SmartHR](https://smarthr.co.jp/)
- [Sony Interactive Entertainment](https://www.sie.com/en/index.html)
- [Splitit](https://www.splitit.com/)
- [Stingray](http://www.stingray.com)
- [Suva](https://www.suva.ch/)
- [Telstra](https://dev.telstra.com)
- [The University of Aizu](https://www.u-aizu.ac.jp/en/)
- [TravelTime platform](https://www.traveltimeplatform.com/)
- [TribalScale](https://www.tribalscale.com)
- [TUI InfoTec GmbH](http://www.tui-infotec.com/)
- [Twitter](https://twitter.com)
- [unblu inc.](https://www.unblu.com/)
- [Veamly](https://www.veamly.com/)
- [VMWare](https://www.vmware.com/)
- [wbt-solutions](https://www.wbt-solutions.de/)
- [Woleet](https://www.woleet.io/)
- [WSO2](https://wso2.com/)
- [Vouchery.io](https://vouchery.io)
- [Xero](https://www.xero.com/)
- [Yahoo Japan](https://www.yahoo.co.jp/)
- [viadee](https://www.viadee.de/)
- [Vonage](https://vonage.com)
- [YITU Technology](https://www.yitutech.com/)
- [Yelp](https://www.yelp.com/)
- [Zalando](https://www.zalando.com)

## [5 - Presentations/Videos/Tutorials/Books](#table-of-contents)

- 2018/05/12 - [OpenAPI Generator - community drivenで成長するコードジェネレータ](https://ackintosh.github.io/blog/2018/05/12/openapi-generator/) by [中野暁人](https://github.com/ackintosh)
- 2018/05/15 - [Starting a new open-source project](http://jmini.github.io/blog/2018/2018-05-15_new-open-source-project.html) by [Jeremie Bresson](https://github.com/jmini)
- 2018/05/15 - [REST API仕様からAPIクライアントやスタブサーバを自動生成する「OpenAPI Generator」オープンソースで公開。Swagger Codegenからのフォーク](https://www.publickey1.jp/blog/18/rest_apiapiopenapi_generatorswagger_generator.html) by [Publickey](https://www.publickey1.jp)
- 2018/06/08 - [Swagger Codegen is now OpenAPI Generator](https://angular.schule/blog/2018-06-swagger-codegen-is-now-openapi-generator) by [JohannesHoppe](https://github.com/JohannesHoppe)
- 2018/06/21 - [Connect your JHipster apps to the world of APIs with OpenAPI and gRPC](https://fr.slideshare.net/chbornet/jhipster-conf-2018-connect-your-jhipster-apps-to-the-world-of-apis-with-openapi-and-grpc) by [Christophe Bornet](https://github.com/cbornet) at [JHipster Conf 2018](https://jhipster-conf.github.io/)
- 2018/06/22 - [OpenAPI Generator で Gatling Client を生成してみた](https://rohki.hatenablog.com/entry/2018/06/22/073000) at [ソモサン](https://rohki.hatenablog.com/)
- 2018/06/27 - [Lessons Learned from Leading an Open-Source Project Supporting 30+ Programming Languages](https://speakerdeck.com/wing328/lessons-learned-from-leading-an-open-source-project-supporting-30-plus-programming-languages) - [William Cheng](https://github.com/wing328) at [LinuxCon + ContainerCon + CloudOpen China 2018](http://bit.ly/2waDKKX)
- 2018/07/19 - [OpenAPI Generator Contribution Quickstart - RingCentral Go SDK](https://medium.com/ringcentral-developers/openapi-generator-for-go-contribution-quickstart-8cc72bf37b53) by [John Wang](https://github.com/grokify)
- 2018/08/22 - [OpenAPI Generatorのプロジェクト構成などのメモ](https://yinm.info/20180822/) by [Yusuke Iinuma](https://github.com/yinm)
- 2018/09/12 - [RepreZen and OpenAPI 3.0: Now is the Time](https://www.reprezen.com/blog/reprezen-openapi-3.0-upgrade-now-is-the-time) by [Miles Daffin](https://www.reprezen.com/blog/author/miles-daffin)
- 2018/10/31 - [A node package wrapper for openapi-generator](https://github.com/HarmoWatch/openapi-generator-cli)
- 2018/11/03 - [OpenAPI Generator + golang + Flutter でアプリ開発](http://ryuichi111std.hatenablog.com/entry/2018/11/03/214005) by [Ryuichi Daigo](https://github.com/ryuichi111)
- 2018/11/15 - [基于openapi3.0的yaml文件生成java代码的一次实践](https://blog.csdn.net/yzy199391/article/details/84023982) by [焱魔王](https://me.csdn.net/yzy199391)
- 2018/11/18 - [Generating PHP library code from OpenAPI](https://lornajane.net/posts/2018/generating-php-library-code-from-openapi) by [Lorna Jane](https://lornajane.net/) at [LORNAJANE Blog](https://lornajane.net/blog)
- 2018/11/19 - [OpenAPIs are everywhere](https://youtu.be/-lDot4Yn7Dg) by [Jeremie Bresson (Unblu)](https://github.com/jmini) at [EclipseCon Europe 2018](https://www.eclipsecon.org/europe2018)
- 2018/12/09 - [openapi-generator をカスタマイズする方法](https://qiita.com/watiko/items/0961287c02eac9211572) by [@watiko](https://qiita.com/watiko)
- 2019/01/03 - [Calling a Swagger service from Apex using openapi-generator](https://lekkimworld.com/2019/01/03/calling-a-swagger-service-from-apex-using-openapi-generator/) by [Mikkel Flindt Heisterberg](https://lekkimworld.com)
- 2019/01/13 - [OpenAPI GeneratorでRESTful APIの定義書から色々自動生成する](https://ky-yk-d.hatenablog.com/entry/2019/01/13/234108) by [@ky_yk_d](https://twitter.com/ky_yk_d)
- 2019/01/20 - [Contract-First API Development with OpenAPI Generator and Connexion](https://medium.com/commencis/contract-first-api-development-with-openapi-generator-and-connexion-b21bbf2f9244) by [Anil Can Aydin](https://github.com/anlcnydn)
- 2019/01/30 - [Rapid Application Development With API First Approach Using Open-API Generator](https://dzone.com/articles/rapid-api-development-using-open-api-generator) by [Milan Sonkar](https://dzone.com/users/828329/milan_sonkar.html)
- 2019/02/02 - [平静を保ち、コードを生成せよ 〜 OpenAPI Generator誕生の背景と軌跡 〜](https://speakerdeck.com/akihito_nakano/gunmaweb34) by [中野暁人](https://github.com/ackintosh) at [Gunma.web #34 スキーマ駆動開発](https://gunmaweb.connpass.com/event/113974/)
- 2019/02/20 - [An adventure in OpenAPI V3 code generation](https://mux.com/blog/an-adventure-in-openapi-v3-api-code-generation/) by [Phil Cluff](https://mux.com/blog/author/philc/)
- 2019/02/26 - [Building API Services: A Beginner’s Guide](https://medium.com/google-cloud/building-api-services-a-beginners-guide-7274ae4c547f) by [Ratros Y.](https://medium.com/@ratrosy) in [Google Cloud Platform Blog](https://medium.com/google-cloud)
- 2019/02/26 - [Building APIs with OpenAPI: Continued](https://medium.com/@ratrosy/building-apis-with-openapi-continued-5d0faaed32eb) by [Ratros Y.](https://medium.com/@ratrosy) in [Google Cloud Platform Blog](https://medium.com/google-cloud)
- 2019-03-07 - [OpenAPI Generator で Spring Boot と Angular をタイプセーフに繋ぐ](https://qiita.com/chibato/items/e4a748db12409b40c02f) by [Tomofumi Chiba](https://github.com/chibat)
- 2019-03-16 - [A Quick introduction to manual OpenAPI V3](https://vadosware.io/post/quick-intro-to-manual-openapi-v3/) by [vados](https://github.com/t3hmrman) at [VADOSWARE](https://vadosware.io)
- 2019-03-25 - [Access any REST service with the SAP S/4HANA Cloud SDK](https://blogs.sap.com/2019/03/25/integrate-sap-s4hana-cloud-sdk-with-open-api/) by [Alexander Duemont](https://people.sap.com/alexander.duemont)
- 2019-03-25 - [OpenAPI generatorを試してみる](https://qiita.com/amuyikam/items/e8a45daae59c68be0fc8) by [@amuyikam](https://twitter.com/amuyikam)
- 2019-03-27 - [OpenAPI3を使ってみよう！Go言語でクライアントとスタブの自動生成まで！](https://techblog.zozo.com/entry/openapi3/go) by [@gold_kou](https://twitter.com/gold_kou)
- 2019-04-17 - [OpenAPIによるスキーマファースト開発の実施サンプルとCloud Runについて](https://tech-blog.optim.co.jp/entry/2019/04/17/174000) by [@yukey1031](https://twitter.com/yukey1031)
- 2019-04-18 - [How to use OpenAPI3 for API developer (RubyKaigi 2019)](https://speakerdeck.com/ota42y/how-to-use-openapi3-for-api-developer) by [@ota42y](https://twitter.com/ota42y) at [RubyKaigi 2019](https://rubykaigi.org/2019)
- 2019-04-29 - [A Beginner's Guide to Code Generation for REST APIs (OpenAPI Generator)](https://gum.co/openapi_generator_ebook) by [William Cheng](https://twitter.com/wing328)
- 2019-05-01 - [Design and generate a REST API from Swagger / OpenAPI in Java, Python, C# and more](https://simply-how.com/design-and-generate-api-code-from-openapi) by [Simply How](https://simply-how.com/)
- 2019-05-17 - [Generate Spring Boot REST API using Swagger/OpenAPI](https://www.47northlabs.com/knowledge-base/generate-spring-boot-rest-api-using-swagger-openapi/) by [Antonie Zafirov](https://www.47northlabs.com/author/antonie-zafirov/)
- 2019-05-22 - [REST APIs代码生成指南(OpenAPI Generator)](https://gum.co/openapi_generator_ebook_gb) by [William Cheng](https://twitter.com/wing328), [Xin Meng](https://github.com/xmeng1)
- 2019-05-24 - [REST API 代碼生成指南 (OpenAPI Generator)](https://gum.co/openapi_generator_ebook_big5) by [William Cheng](https://twitter.com/wing328)
- 2019-06-24 - [Kubernetes Clients and OpenAPI Generator](https://speakerdeck.com/wing328/kubernetes-clients-and-openapi-generator) by [William Cheng](https://twitter.com/wing328) at [Kubernetes Contributor Summits Shanghai 2019](https://www.lfasiallc.com/events/contributors-summit-china-2019/)
- 2019-06-28 [Codewind OpenAPI Tools](https://marketplace.eclipse.org/content/codewind-openapi-tools) in [Eclipse Marketplace](https://marketplace.eclipse.org/) by IBM
- 2019-06-29 [Codewind OpenAPI Tools](https://marketplace.visualstudio.com/items?itemName=IBM.codewind-openapi-tools) in [Visual Studio Marketplace](https://marketplace.visualstudio.com/) by IBM
- 2019-07-04 - [REST API のためのコード生成入門 (OpenAPI Generator)](https://gum.co/openapi_generator_ebook_big5) by [William Cheng](https://twitter.com/wing328), [中野暁人](https://github.com/ackintosh), [和田拓朗](https://github.com/taxpon)
- 2019-07-08 - [OpenAPI Generator にコントリビュートしたら社名が載った話。(CAM) - CAM TECH BLOG](https://tech.cam-inc.co.jp/entry/2019/07/08/140000) by [CAM, Inc.](https://www.cam-inc.co.jp/)
- 2019-07-14 - [OpenAPI GeneratorでPythonのクライアントライブラリを作成した](https://qiita.com/yuji38kwmt/items/dfb929316a1335a161c0) by [yuji38kwmt](https://qiita.com/yuji38kwmt)
- 2019-07-19 - [Developer Experience (DX) for Open-Source Projects: How to Engage Developers and Build a Growing Developer Community](https://speakerdeck.com/wing328/developer-experience-dx-for-open-source-projects-english-japanese) by [William Cheng](https://twitter.com/wing328), [中野暁人](https://github.com/ackintosh) at [Open Source Summit Japan 2019](https://events.linuxfoundation.org/events/open-source-summit-japan-2019/)
- 2019-08-14 - [Our OpenAPI journey with Standardizing SDKs](https://bitmovin.com/our-openapi-journey-with-standardizing-sdks/) by [Sebastian Burgstaller](https://bitmovin.com/author/sburgstaller/) at [Bitmovin](https://www.bitmovin.com)
- 2019-08-15 - [APIのコードを自動生成させたいだけならgRPCでなくてもよくない?](https://www.m3tech.blog/entry/2019/08/15/110000) by [M3, Inc.](https://corporate.m3.com/)
- 2019-08-22 - [マイクロサービスにおけるWeb APIスキーマの管理─ GraphQL、gRPC、OpenAPIの特徴と使いどころ](https://employment.en-japan.com/engineerhub/entry/2019/08/22/103000) by [@ota42y](https://twitter.com/ota42y)
- 2019-08-24 - [SwaggerドキュメントからOpenAPI Generatorを使ってモックサーバー作成](https://qiita.com/masayoshi0222/items/4845e4c715d04587c104) by [坂本正義](https://qiita.com/masayoshi0222)
- 2019-08-29 - [OpenAPI初探](https://cloud.tencent.com/developer/article/1495986) by [peakxie](https://cloud.tencent.com/developer/user/1113152) at [腾讯云社区](https://cloud.tencent.com/developer)
- 2019-08-29 - [全面进化：Kubernetes CRD 1.16 GA前瞻](https://www.servicemesher.com/blog/kubernetes-1.16-crd-ga-preview/) by [Min Kim](https://github.com/yue9944882) at [ServiceMesher Blog](https://www.servicemesher.com/blog/)
- 2019-09-01 - [Creating a PHP-Slim server using OpenAPI (Youtube video)](https://www.youtube.com/watch?v=5cJtbIrsYkg) by [Daniel Persson](https://www.youtube.com/channel/UCnG-TN23lswO6QbvWhMtxpA)
- 2019-09-06 - [Vert.x and OpenAPI](https://wissel.net/blog/2019/09/vertx-and-openapi.html) by [Stephan H Wissel](https://twitter.com/notessensei) at [wissel.net blog](https://wissel.net)
- 2019-09-09 - [Cloud-native development - Creating RESTful microservices](https://cloud.ibm.com/docs/cloud-native?topic=cloud-native-rest-api) in [IBM Cloud Docs](https://cloud.ibm.com/docs)
- 2019-09-14 - [Generating and Configuring a Mastercard API Client](https://developer.mastercard.com/platform/documentation/generating-and-configuring-a-mastercard-api-client/) at [Mastercard Developers Platform](https://developer.mastercard.com/platform/documentation/)
- 2019-09-15 - [OpenAPI(Swagger)導入下調べ](https://qiita.com/ShoichiKuraoka/items/f1f7a3c2376f7cd9c56a) by [Shoichi Kuraoka](https://qiita.com/ShoichiKuraoka)
- 2019-09-17 - [Tutorial: Documenting http4k APIs with OpenApi3](https://www.http4k.org/tutorials/documenting_apis_with_openapi/) by [http4k](https://www.http4k.org/)
- 2019-09-22 - [OpenAPI 3を完全に理解できる本](https://booth.pm/ja/items/1571902) by [@ota42y](https://twitter.com/ota42y)
- 2019-09-22 - [RESTful APIs: Tutorial of OpenAPI Specification](https://medium.com/@amirm.lavasani/restful-apis-tutorial-of-openapi-specification-eeada0e3901d) by [Amir Lavasani](https://medium.com/@amirm.lavasani)
- 2019-09-22 - [Redefining SDKs as software diversity kits](https://devrel.net/dev-rel/redefining-sdks-as-software-diversity-kits) by [Sid Maestre (Xero)](https://twitter.com/sidneyallen) at [DevRelCon San Francisco 2019](https://sf2019.devrel.net/)
- 2019-09-23 - [swaggerからOpenApi GeneratorでSpringのコードを自動生成](https://qiita.com/littleFeet/items/492df2ad68a0799a5e5e) by [@littleFeet](https://qiita.com/littleFeet) at [Qiita](https://qiita.com/)
- 2019-09-24 - [Eine Stunde was mit Api First!](https://www.slideshare.net/JanWeinschenker/eine-stunde-was-mit-api-first) by [@janweinschenker](https://twitter.com/janweinschenker) at [Java Forum Nord](https://javaforumnord.de/)
- 2019-10-09 - [openapi-generator で生成した Go クライアントで Bearer 認証をする](https://autopp-tech.hatenablog.com/entry/2019/10/09/222039) by [Akira Tanimura](https://github.com/autopp)
- 2019-10-10 - [Automatic Generation of REST Clients](https://www.meetup.com/fr-FR/Criteo-Labs-Tech-Talks/events/264775768/) by Thomas Peyrard, Senior Software Engineer at Criteo in [Full-Stack Tech Talks (Meetup)](https://www.meetup.com/fr-FR/Criteo-Labs-Tech-Talks/events/264775768/)
- 2019-10-12 - [OpenApi自动生成client](https://blog.csdn.net/wxid2798226/article/details/102527467) by [郑泽洲](https://me.csdn.net/wxid2798226)
- 2019-10-16 - [How to ship APIs faster?](https://medium.com/@accounts_76224/how-to-ship-apis-faster-cabef2f819e4) by [Simon Guilliams @ PoniCode](https://ponicode.dev)
- 2019-10-22 - [OpenAPI + Spring Boot(Kotlin)でファイルダウンロードAPIを作成する](https://qiita.com/boronngo/items/4b78b92526209daeaee9) by [Yuki Furukawa](https://twitter.com/yuki_furukawa5)
- 2019-10-24 - [Microprofile OpenAPI - Code First or Design First?](https://github.com/pe-st/apidocs/blob/master/MicroProfile-OpenAPI-all-slides.pdf) by [Peter [pɛʃə] Steiner](https://twitter.com/pesche) at [eclipsecon Europe 2019](https://www.eclipsecon.org/europe2019/sessions/microprofile-openapi-code-first-or-design-first)
- 2019-11-06 - [Generating API clients based on OpenAPI v3 specifications](https://98elements.com/blog/generating-api-clients-based-on-openapi-v3-specifications) by [Dominik Jastrzębski @ 98elements](https://98elements.com)
- 2019-11-06 - [OpenAPIを利用して自前のAPIサーバー(Sinatra)を移植した時のメモ](https://qiita.com/YasuhiroABE/items/c73920eab2d9d6e97fd9) by [Yasuhiro ABE](https://twitter.com/YasuhiroABE)
- 2019-11-07 - [API First development with OpenAPI - You should you practise it !?](https://www.youtube.com/watch?v=F9iF3a1Z8Y8) by [Nick Van Hoof](https://www.nickvanhoof.com/) at [Devoxx Belgium 2019](https://devoxx.be/)
- 2019-11-08 - [JHipster beyond CRUD - API-First for Enterprises by Enrico Costanzi](https://www.youtube.com/watch?v=m28JFovKQ20) by [Enrico Costanzi](https://twitter.com/enricocostanzi) at [JHipster Conf 2019 in Paris](https://jhipster-conf.github.io/)
- 2019-11-11 - [TypeScript REST APIクライアント](https://qiita.com/unhurried/items/7b74f7d3c43545dadd2b) by [@unhurried](https://qiita.com/unhurried)
- 2019-11-11 - [One Spec to Rule them all - OpenAPI in Action](https://www.youtube.com/watch?v=MMay_nht8ec) by [Andreas Litt](https://github.com/littldr) at [code.talks 2019](https://www.codetalks.com/)
- 2019-11-13 - [OpenAPI 3.0 Editor And Generator With A Spring Boot Example](https://simply-how.com/design-and-generate-api-code-from-openapi) at [Simply How](https://simply-how.com/)
- 2019-11-17 - [OpenAPI Generator YouTube playlist](https://www.youtube.com/playlist?list=PLtJyHVMdzfF6fBkOUV5VDVErP23CGgHIy) at [YouTube](https://www.youtube.com)
- 2019-11-20 - [Introduction to OpenAPI](https://noti.st/lornajane/HvDH7U/introduction-to-openapi) by [Lorna Mitchell](https://twitter.com/lornajane) at [GOTO Copenhagen 2019](https://gotocph.com/2019/)
- 2019-11-20 - [How to Generate Angular code from OpenAPI specifications](https://dotnetthoughts.net/how-to-generate-angular-code-from-openapi-specifications/) by Anuraj
- 2019-11-23 - [Swagger ではない OpenAPI Specification 3.0 による API サーバー開発](https://www.slideshare.net/techblogyahoo/swagger-openapi-specification-30-api) by [Tetsuya Morimoto](https://github.com/t2y) at [JJUG CCC 2019 Fall](https://ccc2019fall.java-users.jp/)
- 2019-11-24 - [Accelerate Flutter development with OpenAPI and Dart code generation](https://medium.com/@irinasouthwell_220/accelerate-flutter-development-with-openapi-and-dart-code-generation-1f16f8329a6a) by [Irina Southwell](https://medium.com/@irinasouthwell_220)
- 2019-11-25 - [openapi-generatorで手軽にスタブサーバとクライアントの生成](https://qiita.com/pochopocho13/items/8db662e1934fb2b408b8) by [@pochopocho13](https://twitter.com/pochopocho13)
- 2019-11-26 - [CordaCon 2019 Highlights: Braid Server and OpenAPI Generator for Corda Client API’s](https://blog.b9lab.com/cordacon-2019-highlights-braid-server-and-openapi-generator-for-corda-flows-api-s-d24179ccb27c) by [Adel Rustum](https://blog.b9lab.com/@adelrestom) at [B9lab](https://blog.b9lab.com/)
- 2019-12-03 - [A Road to Less Coding: Auto-Generate APILibrary](https://www.corda.net/blog/a-road-to-less-coding-auto-generate-apilibrary/) at [Corda Blog](https://www.corda.net/blog/)
- 2019-12-04 - [Angular＋NestJS＋OpenAPI（Swagger）でマイクロサービスを視野に入れた環境を考える](https://qiita.com/teracy55/items/0327c7a170ec772970c6) by [てらしー](https://twitter.com/teracy55)
- 2019-12-05 - [Code generation on the Java VM](https://speakerdeck.com/sullis/code-generation-on-the-java-vm-2019-12-05) by [Sean Sullivan](https://speakerdeck.com/sullis)
- 2019-12-17 - [OpenAPI Generator で OAuth2 アクセストークン発行のコードまで生成してみる](https://www.techscore.com/blog/2019/12/17/openapi-generator-oauth2-accesstoken/) by [TECHSCORE](https://www.techscore.com/blog/)
- 2019-12-23 - [Use Ada for Your Web Development](https://www.electronicdesign.com/technologies/embedded-revolution/article/21119177/use-ada-for-your-web-development) by [Stephane Carrez](https://github.com/stcarrez)
- 2019-12-23 - [OpenAPIのスキーマを分割・構造化していく方法](https://gift-tech.co.jp/articles/structured-openapi-schema) by [小飯塚達也](https://github.com/t2h5) at [GiFT, Inc](https://gift-tech.co.jp/)
- 2020-01-17 - [OpenAPI demo for Pulp 3.0 GA](https://www.youtube.com/watch?v=mFBP-M0ZPfw&t=178s) by [Pulp](https://www.youtube.com/channel/UCI43Ffs4VPDv7awXvvBJfRQ) at [Youtube](https://www.youtube.com/)
- 2020-01-19 - [Why document a REST API as code?](https://dev.to/rolfstreefkerk/why-document-a-rest-api-as-code-5e7p) by [Rolf Streefkerk](https://github.com/rpstreef) at [DEV Community](https://dev.to)
- 2020-01-28 - [Get Your Serverless Swagger Back with OpenAPI](https://dev.to/matttyler/get-your-serverless-swagger-back-with-openapi-48gc) by [Matt Tyler](https://dev.to/matttyler)
- 2020-01-30 - [OpenAPI Generatorへのコントリビュート](https://www.yutaka0m.work/entry/2020/01/30/163905) by [yutaka0m](https://github.com/yutaka0m)
- 2020-02-01 - [Using OpenAPI to Maximise Your Pulp 3 Experience](https://fosdem.org/2020/schedule/event/openapi/) by [Dennis Kliban](https://github.com/dkliban/) at [FOSDEM](https://fosdem.org/)
- 2020-02-07 - [Why you should use OpenAPI for your API design](https://www.youtube.com/watch?v=zhb7vUApLW8&t=927s) by [Nick Van Hoof](https://apiconference.net/speaker/nick-van-hoof/) at [API Conference](https://apiconference.net/)
- 2020-02-17 - [Rubynetes: using OpenAPI to validate Kubernetes configs](https://www.brightbox.com/blog/2020/02/17/using-openapi-to-validate-kubernetes-configs/) by Neil Wilson at [Brightbox](https://www.brightbox.com/)
- 2020-02-20 - [Building SDKs for the future](https://devblog.xero.com/building-sdks-for-the-future-b79ff726dfd6) by [Sid Maestre (Xero)](https://twitter.com/sidneyallen)
- 2020-02-27 - [Nuxt利用プロダクトでIE11と仲良くするためのE2E](https://tech.medpeer.co.jp/entry/e2e-ie11) at [Medpeer.co.jp Tech Blog](https://tech.medpeer.co.jp/)
- 2020-02-29 - [Providing Support to IoT Devices Deployed in Disconnected Rural Environment (Conference paper)](https://link.springer.com/chapter/10.1007/978-3-030-41494-8_14) by Sergio Laso, Daniel Flores-Martín, Juan Luis HerreraCarlos, CanalJuan Manuel, MurilloJavier Berrocal
- 2020-03-02 - [How To Generate Angular & Spring Code From OpenAPI Specification](https://www.mokkapps.de/blog/how-to-generate-angular-and-spring-code-from-open-api-specification/) by [Michael Hoffmann](https://www.mokkapps.de/)
- 2020-03-02 - [OpenAPI Generator + TypeScript で始める自動生成の型に守られた豊かなクライアント生活](https://gift-tech.co.jp/articles/openapi-generator-typescript) by [五百蔵 直樹](https://gift-tech.co.jp/members/naokiioroi) at [GiFT株式会社](https://gift-tech.co.jp/)
- 2020-03-10 - [OpenAPI Generator Meetup #1](https://speakerdeck.com/akihito_nakano/openapi-generator-meetup-number-1) by [中野暁人](https://github.com/ackintosh) at [OpenAPI Generator Meetup #1](https://openapi-generator-meetup.connpass.com/event/168187/)
- 2020-03-15 - [Load Testing Your API with Swagger/OpenAPI and k6](https://k6.io/blog/load-testing-your-api-with-swagger-openapi-and-k6)
- 2020-04-13 - [俺的【OAS】との向き合い方 (爆速でOpenAPIと友達になろう)](https://tech-blog.optim.co.jp/entry/2020/04/13/100000) in [OPTim Blog](https://tech-blog.optim.co.jp/)
- 2020-04-22 - [Introduction to OpenAPI Generator](https://nordicapis.com/introduction-to-openapi-generator/) by [Kristopher Sandoval](https://nordicapis.com/author/sandovaleffect/) in [Nordic APIs](https://nordicapis.com/)
- 2020-04-27 - [How we use Open API v3 specification to auto-generate API documentation, code-snippets and clients](https://medium.com/pdf-generator-api/how-we-use-open-api-v3-specification-to-auto-generate-api-documentation-code-snippets-and-clients-d127a3cea784) by [Tanel Tähepõld](https://medium.com/@tanel.tahepold)
- 2020-05-09 - [OpenAPIでお手軽にモックAPIサーバーを動かす](https://qiita.com/kasa_le/items/97ca6a8dd4605695c25c) by [Sachie Kamba](https://qiita.com/kasa_le)
- 2020-05-18 - [Spring Boot REST with OpenAPI 3](https://dev.to/alfonzjanfrithz/spring-boot-rest-with-openapi-3-59jm) by [Alfonz Jan Frithz](https://dev.to/alfonzjanfrithz)
- 2020-05-19 - [Dead Simple APIs with Open API](https://www.youtube.com/watch?v=sIaXmR6xRAw) by [Chris Tankersley](https://github.com/dragonmantank) at [Nexmo](https://developer.nexmo.com/)
- 2020-05-22 - [TypeScript REST API Client](https://dev.to/unhurried/typescript-rest-api-client-4in3) by ["unhurried"](https://dev.to/unhurried)
- 2020-05-28 - [【使用 lotify + Swagger 建置可共用的 LINE Notify bot】 - #NiJia @ Chatbot Developer Taiwan 第 #19 小聚](https://www.youtube.com/watch?v=agYVz6dzh1I) by [Chatbot Developer Taiwan](https://www.youtube.com/channel/UCxeYUyZNnHmpX23YNF-ewvw)
- 2020-05-28 - [Building APIs with Laravel using OpenAPI](https://www.youtube.com/watch?v=xexLvQqAhiA) by [Chris Tankersley](https://github.com/dragonmantank) at [Laracon EU](https://laracon.eu/)
- 2020-06-12 - [Interoperability by construction: code generation for Arrowhead Clients](https://ieeexplore.ieee.org/document/9274746) by Michele Albano, Brian Nielsen at [2020 IEEE Conference on Industrial Cyberphysical Systems (ICPS)](https://ieeexplore.ieee.org/xpl/conhome/9274544/proceeding)
- 2020-06-23 - [新規サーバーアプリケーションにTypeScriptを採用してみた](https://www.cam-inc.co.jp/news/20200623) at [CAM Tech Blog](https://www.cam-inc.co.jp/news/tech-blog/)
- 2020-06-29 - [Artifact Abstract: Deployment of APIs on Android Mobile Devices and Microcontrollers](https://ieeexplore.ieee.org/document/9127353) by [Sergio Laso ; Marino Linaje ; Jose Garcia-Alonso ; Juan M. Murillo ; Javier Berrocal](https://ieeexplore.ieee.org/document/9127353/authors#authors) at [2020 IEEE International Conference on Pervasive Computing and Communications (PerCom)](https://ieeexplore.ieee.org/xpl/conhome/9125449/proceeding)
- 2020-07-07 - [5 Best API Documentation Tools](https://blog.dreamfactory.com/5-best-api-documentation-tools/) by Susanna Bouse at [DreamFactory Blog](https://blog.dreamfactory.com/)
- 2020-07-12 - [Open API 3.0の定義からgolangのサーバコードのスケルトンを作成する](https://qiita.com/professor/items/4cbd04ec084d13057bc2) by [@professor (Qiita Blog)](https://qiita.com/professor)
- 2020-07-20 - [Datadog API client libraries now available for Java and Go](https://www.datadoghq.com/blog/java-go-libraries/) by Jordan Obey at [Datadog Blog](https://www.datadoghq.com/blog)
- 2020-07-23 - [Generate Client SDK for .NET Core using Open Api](https://dev.to/no0law1/generate-client-sdk-for-net-core-using-open-api-2dgh) by [Nuno Reis](https://dev.to/no0law1)
- 2020-07-26 - [Dartのhttp_interceptorライブラリを使うと配列のクエリパラメータが消えてしまう件の応急処置](https://qiita.com/gyamoto/items/eeeff81b6770487319ed) by [@gyamoto](https://qiita.com/gyamoto)
- 2020-08-01 - [Generate Angular ReactiveForms from Swagger/OpenAPI](https://dev.to/martinmcwhorter/generate-angular-reactiveforms-from-swagger-openapi-35h9) by [Martin McWhorter](https://dev.to/martinmcwhorter)
- 2020-08-03 - [Criando Bibliotecas para APIs RESTful com OpenAPI, Swagger Editor e OpenAPI Generator](https://medium.com/@everisBrasil/criando-bibliotecas-para-apis-restful-com-openapi-swagger-editor-e-openapi-generator-75349a6420fd) by [everis Brasil (an NTT DATA Company)](https://medium.com/@everisBrasil)
- 2020-08-19 - [マイクロサービスを連携してみよう](https://thinkit.co.jp/article/17704) by [岡井 裕矢(おかい ゆうや)](https://thinkit.co.jp/author/17588), [泉 勝(いずみ まさる)](https://thinkit.co.jp/author/17705) at [Think IT（シンクイット）](https://thinkit.co.jp/)
- 2020-08-25 - [OpenAPI Generator と TypeScript で型安全にフロントエンド開発をしている話](https://tech.smarthr.jp/entry/2020/08/25/135631) at [SmartHR Tech Blog](https://tech.smarthr.jp/)
- 2020-09-10 - [Introduction to OpenAPI with Instana](https://www.instana.com/blog/introduction-to-openapi-with-instana/) by [Cedric Ziel](https://www.instana.com/blog/author/cedricziel/) at [Instana Blog](https://www.instana.com/blog/)
- 2020-09-17 - [Generate PowerShellSDK using openapi-generator](https://medium.com/@ghufz.learn/generate-powershellsdk-using-openapi-generator-33b700891e33) by [Ghufran Zahidi](https://medium.com/@ghufz.learn)
- 2020-09-24 - [How to automate API code generation (OpenAPI/Swagger) and boost productivity - Tutorial with React Native featuring TypeScript](https://medium.com/@sceleski/how-to-automate-api-code-generation-openapi-swagger-and-boost-productivity-1176a0056d8a) by [Sanjin Celeski](https://medium.com/@sceleski)
- 2020-09-25 - [Generate OpenAPI Angular Client](https://medium.com/@pguso/generate-openapi-angular-client-8c9288e8bbd4) by [Patric](https://medium.com/@pguso)
- 2020-10-24 - [Working with Microsoft Identity - React Native Client](https://www.josephguadagno.net/2020/10/24/working-with-microsoft-identity-react-native-client) by [Joseph Guadagno](https://www.josephguadagno.net/)
- 2020-10-31 - [[B2] OpenAPI Specification으로 타입-세이프하게 API 개발하기: 희망편 VS 절망편](https://www.youtube.com/watch?v=J4JHLESAiFk) by 최태건 at [FEConf 2020](https://2020.feconf.kr/)
- 2020-11-05 - [Automated REST-Api Code Generation: Wie IT-Systeme miteinander sprechen](https://www.massiveart.com/blog/automated-rest-api-code-generation-wie-it-systeme-miteinander-sprechen) by Stefan Rottensteiner at [MASSIVE ART Blog](https://www.massiveart.com/blog)
- 2020-12-01 - [OpenAPI GeneratorでGoのAPIサーバー/クライアントコードを自動生成する](https://qiita.com/saki-engineering/items/b20d8b6074c4da9664a5) by [@saki-engineering](https://qiita.com/saki-engineering)
- 2020-12-04 - [Scaling the Test Coverage of OpenAPI Generator for 30+ Programming Languages](https://www.youtube.com/watch?v=7Lke9dHRqT0) by [William Cheng](https://github.com/wing328) at [Open Source Summit Japan + Automotive Linux Summit 2020](https://events.linuxfoundation.org/archive/2020/open-source-summit-japan/) ([Slides](https://speakerdeck.com/wing328/scaling-the-test-coverage-of-openapi-generator-for-30-plus-programming-languages))
- 2020-12-09 - [プロジェクトにOpenAPI Generatorで自動生成された型付きAPI Clientを導入した話](https://qiita.com/yoshifujiT/items/905c18700ede23f40840) by [@yoshifujiT](https://github.com/yoshifujiT)
- 2020-12-15 - [Next.js + NestJS + GraphQLで変化に追従するフロントエンドへ 〜 ショッピングクーポンの事例紹介](https://techblog.yahoo.co.jp/entry/2020121530052952/) by [小倉 陸](https://github.com/ogugu9) at [Yahoo! JAPAN Tech Blog](https://techblog.yahoo.co.jp/)
- 2021-01-08 - [Hello, New API – Part 1](https://www.nginx.com/blog/hello-new-api-part-1/) by [Jeremy Schulman](https://www.nginx.com/people/jeremy-schulman/) at [Major League Baseball](https://www.mlb.com)
- 2021-01-18 - [「アプリ開発あるある」を疑うことから始まった、API Clientコードの自動生成【デブスト2020】](https://codezine.jp/article/detail/13406?p=2) by [CodeZine編集部](https://codezine.jp/author/1)
- 2021-02-05 - [REST-API-Roundtrip with SpringDoc and OpenAPI Generator](https://blog.viadee.de/en/rest-api-roundtrip) by [Benjamin Klatt](https://twitter.com/benklatt) at [viadee](https://www.viadee.de/en/)
- 2021-02-17 - [REST-API-Roundtrip with SpringDoc and OpenAPI Generator](https://medium.com/nerd-for-tech/rest-api-roundtrip-with-springdoc-and-openapi-generator-30bd27ccf698) by [cloud @viadee](https://cloud-viadee.medium.com/)
- 2021-03-08 - [OpenAPI Generator 工具的躺坑尝试](https://blog.csdn.net/u013019701/article/details/114531975) by [独家雨天](https://blog.csdn.net/u013019701) at [CSDN官方博客](https://blog.csdn.net/) 
- 2021-03-16 - [如何基于 Swagger 使用 OpenAPI Generator 生成 JMeter 脚本？](https://cloud.tencent.com/developer/article/1802704) by [高楼Zee](https://cloud.tencent.com/developer/user/5836255) at [腾讯云专栏](https://cloud.tencent.com/developer/column)
- 2021-03-24 - [openapi-generator-cli による TypeScript 型定義](https://zenn.dev/takepepe/articles/openapi-generator-cli-ts) by [Takefumi Yoshii](https://zenn.dev/takepepe)
- 2021-03-28 - [Trying out NestJS part 4: Generate Typescript clients from OpenAPI documents](https://dev.to/arnaudcortisse/trying-out-nestjs-part-4-generate-typescript-clients-from-openapi-documents-28mk) by [Arnaud Cortisse](https://dev.to/arnaudcortisse) 
- 2021-03-31 - [Open API Server Implementation Using OpenAPI Generator](https://www.baeldung.com/java-openapi-generator-server) at [Baeldung](https://www.baeldung.com/)
- 2021-03-31 - [使用OpenAPI Generator實現Open API Server](https://www.1ju.org/article/java-openapi-generator-server) at [億聚網](https://www.1ju.org/)
- 2021-04-19 - [Introducing Twilio’s OpenAPI Specification Beta](https://www.twilio.com/blog/introducing-twilio-open-api-specification-beta) by [GARETH PAUL JONES](https://www.twilio.com/blog/author/gpj) at [Twilio Blog](https://www.twilio.com/blog)
- 2021-04-22 - [Leveraging OpenApi strengths in a Micro-Service environment](https://medium.com/unibuddy-technology-blog/leveraging-openapi-strengths-in-a-micro-service-environment-3d7f9e7c26ff) by Nicolas Jellab at [Unibuddy Technology Blog](https://medium.com/unibuddy-technology-blog)
- 2021-04-27 - [From zero to publishing PowerShell API clients in PowerShell Gallery within minutes](https://speakerdeck.com/wing328/from-zero-to-publishing-powershell-api-clients-in-powershell-gallery-within-minutes) by [William Cheng](https://github.com/wing328) at [PowerShell + DevOps Global Summit 2021](https://events.devopscollective.org/event/powershell-devops-global-summit-2021/)
- 2021-05-31 - [FlutterでOpen Api Generator(Swagger)を使う](https://aakira.app/blog/2021/05/flutter-open-api/) by [AAkira](https://twitter.com/_a_akira)
- 2021-06-22 - [Rest API Documentation and Client Generation With OpenAPI](https://dzone.com/articles/rest-api-documentation-and-client-generation-with) by [Prasanth Gullapalli](https://dzone.com/users/1011797/prasanthnath.g@gmail.com.html) 
- 2021-07-16 - [銀行事業のサーバーサイド開発について / LINE 京都開発室 エンジニア採用説明会](https://www.youtube.com/watch?v=YrrKQHxLPpQ) by 野田誠人, Robert Mitchell
- 2021-07-19 - [OpenAPI code generation with kotlin](https://sylhare.github.io/2021/07/19/Openapi-swagger-codegen-with-kotlin.html) by [sylhare](https://github.com/sylhare)
- 2021-07-29 - [How To Rewrite a Huge Codebase](https://dzone.com/articles/how-to-rewrite-a-huge-code-base) by [Curtis Poe](https://dzone.com/users/4565446/publiusovidius.html)
- 2021-08-21 - [Generating Client APIs using Swagger Part 1](https://medium.com/@flowsquad/generating-client-apis-using-swagger-part-1-2d46f13f5e92) by [FlowSquad.io](https://medium.com/@flowsquad)
- 2021-09-11 - [Invoking AWS ParallelCluster API](https://docs.aws.amazon.com/parallelcluster/latest/ug/api-reference-v3.html) at [AWS ParallelCluster API official documentation](https://docs.aws.amazon.com/parallelcluster/latest/ug/api-reference-v3.html)
- 2021-09-20 - [OpenAPI Generator - The Babel Fish of the API World](https://www.youtube.com/watch?v=s2zMtwd5klg) by [Cliffano Subagio (Principal Engineer at Shine Solutions)](https://github.com/cliffano) at [Apidays LIVE Australia 2021](https://www.apidays.global/australia2021/)
- 2021-10-02 - [How to Write Fewer Lines of Code with the OpenAPI Generator](https://hackernoon.com/how-to-write-fewer-lines-of-code-with-the-openapi-generator) by [Mikhail Alfa](https://hackernoon.com/u/alphamikle)
- 2021-10-12 - [OpenAPI Generator : 4000 étoiles sur GitHub et des spaghettis](https://www.youtube.com/watch?v=9hEsNBSqTFk) by [Jérémie Bresson](https://github.com/jmini) at [Devoxx FR 2021](https://cfp.devoxx.fr/2021/speaker/jeremie_bresson)
- 2021-10-17 - [Generate a TypeScript HTTP Client From An OpenAPI Spec In DotNET 5](https://richardwillis.info/blog/generate-a-type-script-http-client-from-an-open-api-spec-in-dot-net-5) by [Richard Willis](https://github.com/badsyntax)
- 2021-11-06 - [スタートアップの開発で意識したこと](https://zenn.dev/woo_noo/articles/5cb09f8e2899ae782ad1) by [woo-noo](https://zenn.dev/woo_noo)
- 2021-11-09 - [Effective Software Development using OpenAPI Generator](https://apexlabs.ai/post/effective-software-development-using-openapi-generator) by Ajil Oomme
- 2021-12-07 - [An Introduction to OpenAPI](https://betterprogramming.pub/4-use-cases-of-openapi-which-are-good-to-know-1a041f4ad71e) by [Na'aman Hirschfeld](https://naamanhirschfeld.medium.com/)
- 2021-01-02 - [Towards a secure API client generator for IoT devices](https://arxiv.org/abs/2201.00270) by Anders Aaen Springborg, Martin Kaldahl Andersen, Kaare Holland Hattel, Michele Albano


## [6 - About Us](#table-of-contents)

### [6.1 - OpenAPI Generator Core Team](#table-of-contents)

OpenAPI Generator core team members are contributors who have been making significant contributions (review issues, fix bugs, make enhancements, etc) to the project on a regular basis.

#### Core Team Members
* [@wing328](https://github.com/wing328) (2015/07) [:heart:](https://www.patreon.com/wing328)
* [@jimschubert](https://github.com/jimschubert) (2016/05) [:heart:](https://www.patreon.com/jimschubert)
* [@cbornet](https://github.com/cbornet) (2016/05)
* [@ackintosh](https://github.com/ackintosh) (2018/02) [:heart:](https://www.patreon.com/ackintosh/overview)
* [@jmini](https://github.com/jmini) (2018/04)  [:heart:](https://www.patreon.com/jmini)
* [@etherealjoy](https://github.com/etherealjoy) (2019/06)
* [@spacether](https://github.com/spacether) (2020/05) [:heart:][spacether sponsorship]

:heart: = Link to support the contributor directly

[spacether sponsorship]: https://github.com/sponsors/spacether/

#### Template Creator

**NOTE**: Embedded templates are only supported in _Mustache_ format. Support for all other formats is experimental and subject to change at any time.

Here is a list of template creators:
 * API Clients:
   * Ada: @stcarrez
   * Apex: @asnelling
   * Bash: @bkryza
   * C: @PowerOfCreation @zhemant [:heart:](https://www.patreon.com/zhemant)
   * C++ REST: @Danielku15
   * C++ Tiny: @AndersSpringborg @kaareHH @michelealbano @mkakbas 
   * C++ UE4: @Kahncode
   * C# (.NET 2.0): @who
   * C# (.NET Standard 1.3 ): @Gronsak
   * C# (.NET 4.5 refactored): @jimschubert [:heart:](https://www.patreon.com/jimschubert)
   * C# (GenericHost): @devhl-labs
   * C# (HttpClient): @Blackclaws 
   * Clojure: @xhh
   * Crystal: @wing328
   * Dart: @yissachar
   * Dart (refactor): @joernahrens
   * Dart 2: @swipesight
   * Dart (Jaguar): @jaumard
   * Dart (Dio): @josh-burton
   * Elixir: @niku
   * Elm: @eriktim
   * Eiffel: @jvelilla
   * Erlang: @tsloughter
   * Erlang (PropEr): @jfacorro @robertoaloi
   * Groovy: @victorgit
   * Go: @wing328 [:heart:](https://www.patreon.com/wing328)
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
   * Java (Java 11 Native HTTP client): @bbdouglas
   * Java (Apache HttpClient): @harrywhite4
   * Javascript/NodeJS: @jfiala
   * JavaScript (Apollo DataSource): @erithmetic
   * JavaScript (Closure-annotated Angular) @achew22
   * JavaScript (Flow types) @jaypea
   * JMeter: @davidkiss
   * Kotlin: @jimschubert [:heart:](https://www.patreon.com/jimschubert)
   * Kotlin (MultiPlatform): @andrewemery
   * Kotlin (Volley): @alisters
   * Lua: @daurnimator
   * Nim: @hokamoto
   * OCaml: @cgensoul
   * Perl: @wing328 [:heart:](https://www.patreon.com/wing328)
   * PHP (Guzzle): @baartosz
   * PHP (with Data Transfer): @Articus
   * PowerShell: @beatcracker
   * PowerShell (refactored in 5.0.0): @wing328
   * Python: @spacether [:heart:][spacether sponsorship]
   * Python-Experimental: @spacether [:heart:][spacether sponsorship]
   * R: @ramnov
   * Ruby (Faraday): @meganemura @dkliban
   * Rust: @farcaller
   * Rust (rust-server): @metaswitch
   * Scala (scalaz & http4s): @tbrown1979
   * Scala (Akka): @cchafer
   * Scala (sttp): @chameleon82
   * Swift: @tkqubo
   * Swift 3: @hexelon
   * Swift 4: @ehyche
   * Swift 5: @4brunu
   * TypeScript (Angular1): @mhardorf
   * TypeScript (Angular2): @roni-frantchi
   * TypeScript (Angular6): @akehir
   * TypeScript (Angular7): @topce
   * TypeScript (Axios): @nicokoenig
   * TypeScript (Fetch): @leonyu
   * TypeScript (Inversify): @gualtierim
   * TypeScript (jQuery): @bherila
   * TypeScript (Nestjs): @vfrank66
   * TypeScript (Node):  @mhardorf
   * TypeScript (Rxjs): @denyo
   * TypeScript (redux-query): @petejohansonxo
 * Server Stubs
   * Ada: @stcarrez
   * C# ASP.NET 5: @jimschubert [:heart:](https://www.patreon.com/jimschubert)
   * C# ASP.NET Core 3.0: @A-Joshi
   * C# APS.NET Core 3.1: @phatcher
   * C# Azure functions: @Abrhm7786
   * C# NancyFX: @mstefaniuk
   * C++ (Qt5 QHttpEngine): @etherealjoy
   * C++ Pistache: @sebymiano
   * C++ Restbed: @stkrwork
   * Erlang Server: @galaxie
   * F# (Giraffe) Server: @nmfisher
   * Go Server: @guohuang
   * Go (Echo) Server: @ph4r5h4d
   * Go (Gin) Server: @kemokemo
   * GraphQL Express Server: @renepardon
   * Haskell Servant: @algas
   * Haskell Yesod: @yotsuya
   * Java Camel: @carnevalegiacomo
   * Java MSF4J: @sanjeewa-malalgoda
   * Java Spring Boot: @diyfr
   * Java Undertow: @stevehu
   * Java Play Framework: @JFCote
   * Java PKMST: @anshu2185 @sanshuman @rkumar-pk @ninodpillai
   * Java Vert.x: @lwlee2608
   * Java Micronaut: @andriy-dmytruk
   * JAX-RS RestEasy: @chameleon82
   * JAX-RS CXF: @hiveship
   * JAX-RS CXF (CDI): @nickcmaynard
   * JAX-RS RestEasy (JBoss EAP): @jfiala
   * Kotlin: @jimschubert [:heart:](https://www.patreon.com/jimschubert)
   * Kotlin (Spring Boot): @dr4ke616
   * Kotlin (Vertx): @Wooyme
   * Kotlin (JAX-RS): @anttileppa
   * NodeJS Express: @YishTish
   * PHP Laravel: @renepardon
   * PHP Lumen: @abcsun
   * PHP Mezzio (with Path Handler): @Articus
   * PHP Slim: @jfastnacht
   * PHP Slim4: @ybelenko
   * PHP Symfony: @ksm2
   * Python FastAPI: @krjakbrjak
   * Python AIOHTTP: @Jyhess
   * Ruby on Rails 5: @zlx
   * Rust (rust-server): @metaswitch
   * Scala Akka: @Bouillie
   * Scala Finch: @jimschubert [:heart:](https://www.patreon.com/jimschubert)
   * Scala Lagom: @gmkumar2005
   * Scala Play: @adigerber
 * Documentation
   * AsciiDoc: @man-at-home
   * HTML Doc 2: @jhitchcock
   * Confluence Wiki: @jhitchcock
   * PlantUML: @pburls
 * Configuration
   * Apache2: @stkrwork
   * k6: @mostafa
 * Schema
   * Avro: @sgadouar
   * GraphQL: @wing328 [:heart:](https://www.patreon.com/wing328)
   * Ktorm: @Luiz-Monad
   * MySQL: @ybelenko
   * Protocol Buffer: @wing328
   * WSDL @adessoDpd

:heart: = Link to support the contributor directly

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

| Languages/Generators         | Member (join date)                                                                                                                                                                                                                |
| :---------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ActionScript      |                                                                                                                                                                                                                                   |
| Ada               | @stcarrez (2018/02) @michelealbano (2018/02)                                                                                                                                                                                        |
| Android           | @jaz-ah (2017/09)                                                                                                                                                                                                                 |
| Apex              |                                                                                                                                                                                                                                   |
| Bash              | @frol (2017/07) @bkryza (2017/08) @kenjones-cisco (2017/09)                                                                                                                                                                       |
| C                 | @zhemant (2018/11) @ityuhui (2019/12) @michelealbano (2020/03)                                                                                                                                                                   |
| C++               | @ravinikam (2017/07) @stkrwork (2017/07) @etherealjoy (2018/02) @martindelille (2018/03) @muttleyxd (2019/08)                                                                                                                     |
| C#                | @mandrean (2017/08) @frankyjuang (2019/09) @shibayan (2020/02) @Blackclaws (2021/03) @lucamazzanti (2021/05)                                                                               |
| Clojure           |                                                                                                                                                                                                                                   |
| Crystal           | @cyangle (2021/01) |
| Dart              | @jaumard (2018/09) @josh-burton (2019/12) @amondnet (2019/12) @sbu-WBT (2020/12) @kuhnroyal (2020/12) @agilob (2020/12) @ahmednfwela (2021/08)                                                                                                      |
| Eiffel            | @jvelilla (2017/09)                                                                                                                                                                                                               |
| Elixir            | @mrmstn (2018/12)                                                                                                                                                                                                                 |
| Elm               | @eriktim (2018/09)                                                                                                                                                                                                                |
| Erlang            | @tsloughter (2017/11) @jfacorro (2018/10) @robertoaloi (2018/10)                                                                                                                                                                  |
| F#                | @nmfisher (2019/05)                                                                                                                                                                                                               |
| Go                | @antihax (2017/11) @grokify (2018/07) @kemokemo (2018/09) @jirikuncar (2021/01) @ph4r5h4d (2021/04)                                                                                                                                   |
| GraphQL           | @renepardon (2018/12)                                                                                                                                                                                                             |
| Groovy            |                                                                                                                                                                                                                                   |
| Haskell           |                                                                                                                                                                                                                                   |
| Java              | @bbdouglas (2017/07) @sreeshas (2017/08) @jfiala (2017/08) @lukoyanov (2017/09) @cbornet (2017/09) @jeff9finger (2018/01) @karismann (2019/03) @Zomzog (2019/04) @lwlee2608 (2019/10)                        |
| Java Spring             | @cachescrubber (2022/02) @welshm (2022/02) @MelleD (2022/02) @atextor (2022/02) @manedev79 (2022/02) @javisst (2022/02) @borsch (2022/02) @banlevente (2022/02)                    |
| JMeter               | @kannkyo (2021/01)                                                                                                                                                                                                            |
| Kotlin            | @jimschubert (2017/09) [:heart:](https://www.patreon.com/jimschubert), @dr4ke616 (2018/08) @karismann (2019/03) @Zomzog (2019/04) @andrewemery (2019/10) @4brunu (2019/11) @yutaka0m (2020/03)                                                        |
| Lua               | @daurnimator (2017/08)                                                                                                                                                                                                            |
| Nim               |                                                                                                                                                                                                                                   |
| NodeJS/Javascript | @CodeNinjai (2017/07) @frol (2017/07) @cliffano (2017/07)                                                                                                                                                                         |
| ObjC              |                                                                                                                                                                                                                                   |
| OCaml             | @cgensoul (2019/08)                                                                                                                                                                                                               |
| Perl              | @wing328 (2017/07) [:heart:](https://www.patreon.com/wing328) @yue9944882 (2019/06)                                                                                                                                               |
| PHP               | @jebentier (2017/07), @dkarlovi (2017/07), @mandrean (2017/08), @jfastnacht (2017/09), @ackintosh (2017/09) [:heart:](https://www.patreon.com/ackintosh/overview), @ybelenko (2018/07), @renepardon (2018/12)                     |
| PowerShell        | @wing328 (2020/05)                                                                                                                                                                                                                                  |
| Python            | @taxpon (2017/07) @frol (2017/07) @mbohlool (2017/07) @cbornet (2017/09) @kenjones-cisco (2017/11) @tomplus (2018/10) @Jyhess (2019/01) @arun-nalla (2019/11) @spacether (2019/11) [:heart:][spacether sponsorship]               |
| R                 | @Ramanth (2019/07) @saigiridhar21 (2019/07)                                                                                                                                                                                       |
| Ruby              | @cliffano (2017/07) @zlx (2017/09) @autopp (2019/02)                                                                                                                                                                             |
| Rust              | @frol (2017/07) @farcaller (2017/08) @richardwhiuk (2019/07) @paladinzh (2020/05)                                                                                                                               |
| Scala             | @clasnake (2017/07), @jimschubert (2017/09) [:heart:](https://www.patreon.com/jimschubert), @shijinkui  (2018/01), @ramzimaalej (2018/03), @chameleon82 (2020/03), @Bouillie (2020/04)                                                          |
| Swift             | @jgavris (2017/07) @ehyche (2017/08) @Edubits (2017/09) @jaz-ah (2017/09) @4brunu (2019/11)                                                                                                                                       |
| TypeScript        | @TiFu (2017/07) @taxpon (2017/07) @sebastianhaas (2017/07) @kenisteward (2017/07) @Vrolijkx (2017/09) @macjohnny (2018/01) @topce (2018/10) @akehir (2019/07) @petejohansonxo (2019/11) @amakhrov (2020/02) @davidgamero (2022/03) |

:heart: = Link to support the contributor directly

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
- [Erik Timmers](https://github.com/eriktim)
- [Esteban Gehring](https://github.com/macjohnny)
- [Gustavo Paz](https://github.com/gustavoapaz)
- [Javier Velilla](https://github.com/jvelilla)
- [Jean-François Côté](https://github.com/JFCote)
- [Jim Schubert](https://github.com/jimschubert)
- [Jon Schoning](https://github.com/jonschoning)
- [Jérémie Bresson](https://github.com/jmini) [:heart:](https://www.patreon.com/jmini)
- [Jörn Ahrens](https://github.com/jayearn)
- [Keni Steward](https://github.com/kenisteward)
- [Marcin Stefaniuk](https://github.com/mstefaniuk)
- [Martin Delille](https://github.com/MartinDelille)
- [Masahiro Yamauchi](https://github.com/algas)
- [Michele Albano](https://github.com/michelealbano)
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
- [Xin Meng](https://github.com/xmeng1) [:heart:](https://www.patreon.com/user/overview?u=16435385)
- [Xu Hui Hui](https://github.com/xhh)
- [antihax](https://github.com/antihax)
- [beatcracker](https://github.com/beatcracker)
- [daurnimator](https:/github.com/daurnimator)
- [etherealjoy](https://github.com/etherealjoy)
- [jfiala](https://github.com/jfiala)
- [lukoyanov](https://github.com/lukoyanov)

:heart: = Link to support the contributor directly

## [7 - License](#table-of-contents)
-------

Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
Copyright 2018 SmartBear Software

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](https://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---
