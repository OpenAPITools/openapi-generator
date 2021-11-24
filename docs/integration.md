---
id: integrations
title: Workflow Integrations
---
## Workflow Integration (Gradle, Maven, GitHub, CI/CD)

### Gradle Integration

See the [openapi-generator-gradle-plugin README](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-gradle-plugin/README.adoc) for details related to configuring and using the Gradle Plugin.

Supported tasks include:

* Listing generators
* Validation of Open API 2.0 and 3.0 Specs
* Generating "Meta" generators
* Generating all generators supported by OpenAPI Generator

### Maven Integration

See the [openapi-generator-maven-plugin README](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-maven-plugin/README.md) for details related to configuring and using the Maven Plugin.

### sbt Integration

Please refer to https://github.com/upstart-commerce/sbt-openapi-generator

### Bazel Integration

Please refer to https://github.com/OpenAPITools/openapi-generator-bazel

### Cake Addin

3rd-party Cake Addin provided by [Lukas Körfer](https://github.com/lukoerfer): https://github.com/lukoerfer/cake-openapi-generator

### GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)

 2) Generate the SDK
```sh
 java -jar openapi-generator-cli.jar generate \
 -i modules/openapi-generator/src/test/resources/3_0/petstore.json -g perl \
 --git-user-id "wing328" \
 --git-repo-id "petstore-perl" \
 --release-note "GitHub integration demo" \
 -o /var/tmp/perl/petstore
```
 3) Push the SDK to GitHub
```sh
cd /var/tmp/perl/petstore
/bin/sh ./git_push.sh
```
### CI/CD

Some generators also generate CI/CD configuration files (.travis.yml) so that the output will be ready to be tested by the CI (e.g. Travis)

If you're looking for the configuration files of a particular CI that is not yet supported, please open an [issue](https://github.com/openapitools/openapi-generator/issues/new) to let us know.
