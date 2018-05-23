## Workflow Integration (Maven, Github, CI/CD)

### Maven Integration

You can use the [openapi-generator-maven-plugin](../modules/openapi-generator-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.

### GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)

 2) Generate the SDK
```sh
 java -jar openapi-generator-cli.jar generate \
 -i modules/openapi-generator/src/test/resources/2_0/petstore.json -g perl \
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
### CI/CD

Some generators also generate CI/CD configuration files (.travis.yml) so that the output will be ready to be tested by the CI (e.g. Travis)

If you're looking for the configuration files of a particular CI that is not yet supported, please open an [issue](https://github.com/openapitools/openapi-generator/issues/new) to let us know.

[Back to OpenAPI-Generator's README page](../README.md)
