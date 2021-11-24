---
id: contribute-building
title: Building the code
---

## Using Maven

To build from source, you need the following installed and available in your `$PATH:`

* [Java 8](https://java.oracle.com)

* [Apache maven 3.3.4 or greater](https://maven.apache.org/)

After cloning the project, you can build it from source with this command:

```bash
mvn clean install
```

If you don't have maven installed, you may directly use the included [maven wrapper](https://github.com/takari/maven-wrapper), and build with the command:

```bash
./mvnw clean install
```

## Using Docker

You can use `run-in-docker.sh` to do all development. This script maps your local repository to `/gen`
in the docker container. It also maps `~/.m2/repository` to the appropriate container location.

To execute `mvn package`:

```bash
git clone https://github.com/openapitools/openapi-generator
cd openapi-generator
./run-in-docker.sh mvn package
```

Build artifacts are now accessible in your working directory.

Once built, `run-in-docker.sh` will act as an executable for openapi-generator-cli. To generate code, you'll need to output to a directory under `/gen` (e.g. `/gen/out`). For example:

```bash
./run-in-docker.sh help # Executes 'help' command for openapi-generator-cli
./run-in-docker.sh list # Executes 'list' command for openapi-generator-cli
./run-in-docker.sh /gen/bin/generate-samples.sh /gen/bin/configs/go-petstore.yaml  # Builds the Go client
./run-in-docker.sh generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
    -g go -o /gen/out/go-petstore --packageName=petstore # generates go client, outputs locally to ./out/go-petstore
```

### Docker in Vagrant

Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).

```bash
git clone https://github.com/openapitools/openapi-generator.git
cd openapi-generator
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
```

### Troubleshooting

If an error like this occurs, just execute the **mvn clean install -U** command:

> org.apache.maven.lifecycle.LifecycleExecutionException: Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:2.19.1:test (default-test) on project openapi-generator: A type incompatibility occurred while executing org.apache.maven.plugins:maven-surefire-plugin:2.19.1:test: java.lang.ExceptionInInitializerError cannot be cast to java.io.IOException

```bash
./run-in-docker.sh mvn clean install -U
```

> Failed to execute goal org.fortasoft:gradle-maven-plugin:1.0.8:invoke (default) on project openapi-generator-gradle-plugin-mvn-wrapper: org.gradle.tooling.BuildException: Could not execute build using Gradle distribution 'https://services.gradle.org/distributions/gradle-4.7-bin.zip'

Right now: no solution for this one :|
