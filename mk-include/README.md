# Confluent Cloud Makefile Includes
This is a set of Makefile include targets that are used in cloud applications.

The purpose of cc-mk-include is to present a consistent developer experience across repos/projects:
```
make deps
make build
make test
make clean
```

It also helps standardize our CI pipeline across repos:
```
make init-ci
make build
make test
make release-ci
make epilogue-ci
```

## Install
Add this repo to your repo with the command:
```shell
git subtree add --prefix mk-include git@github.com:confluentinc/cc-mk-include.git master --squash
```

To exclude these makefiles from your project language summary on GitHub, add this to your `.gitattributes`:
```
mk-include/** linguist-vendored
```

Then update your makefile like so:

### Go + Docker + Helm Service
```make
SERVICE_NAME := scraper
CHART_NAME := cc-$(SERVICE_NAME)
IMAGE_NAME := cc-$(SERVICE_NAME)
GO_BINS := cmd/scraper/main.go=cc-scraper

include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-go.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-helm.mk
include ./mk-include/cc-deployer.mk
include ./mk-include/cc-testbreak.mk
include ./mk-include/cc-vault.mk
include ./mk-include/cc-end.mk
```

### Docker + Helm Only Service
```make
IMAGE_NAME := cc-example
CHART_NAME := $(IMAGE_NAME)

include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-helm.mk
include ./mk-include/cc-deployer.mk
include ./mk-include/cc-end.mk
```

### Java (Maven) + Docker + Helm Service

#### Maven-orchestrated Docker build
```make
IMAGE_NAME := cc-java-example
CHART_NAME := cc-java-example
BUILD_DOCKER_OVERRIDE := mvn-docker-package

include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-maven.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-helm.mk
include ./mk-include/cc-deployer.mk
include ./mk-include/cc-end.mk
```

#### Make-orchestrated Docker build
```make
IMAGE_NAME := cc-java-example
CHART_NAME := cc-java-example
MAVEN_INSTALL_PROFILES += docker

build-docker: mvn-install

include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-maven.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-helm.mk
include ./mk-include/cc-deployer.mk
include ./mk-include/cc-end.mk
```

In this scenario, the `docker` profile from `io.confluent:common` is leveraged to assemble the filesystem layout
for the Docker build.  However, `cc-docker.mk` is used to invoke the actual `docker build` command.

You must also configure your project's `pom.xml` to skip the `dockerfile-maven-plugin`:
```xml
  <properties>
    <docker.skip-build>false</docker.skip-build>
  </properties>
  <profiles>
    <profile>
      <id>docker</id>
      <build>
        <plugins>
          <!--
          Skip dockerfile-maven-plugin since we do the actual docker build from make
          Note that we still leverage the `docker` profile to do the filesystem assembly
          -->
          <plugin>
            <groupId>com.spotify</groupId>
            <artifactId>dockerfile-maven-plugin</artifactId>
            <executions>
              <execution>
                <id>package</id>
                <configuration>
                  <skip>true</skip>
                </configuration>
              </execution>
            </executions>
          </plugin>
        </plugins>
      </build>
    </profile>
  </profiles>
```

## Updating
Once you have the make targets installed, you can update at any time by running

```shell
make update-mk-include
```
### Update to a specific version

Add
```shell
MK_INCLUDE_UPDATE_VERSION := v<version>
```
to you Makefile and commit the change. Then run 
```shell
make update-mk-include
```
It will update to that specific tag version of mk-include.

## Auto Update
The cc-mk-include now provides a solution to auto-sync your repo with the newest or pinned version of cc-mk-include. It will *auto open* a PR if your master branch is not at the same version with newest or pinned version. And it will *auto merged* if the CI passed. To use it, first you must have cc-begin.mk and cc-semver.mk in your toplevel Makefile. Then you need to add
```shell
UPDATE_MK_INCLUDE := true
```
To disable auto merge CI
```shell
UPDATE_MK_INCLUDE_AUTO_MERGE := false
```
in your toplevel Makefile. The default sync version will be master which is the newest version, you can pin whatever version you want to by enable
```shell
MK_INCLUDE_UPDATE_VERSION := <tag>
```

### Auto Merge
Leverage gh cli, cc-mk-include now support auto merge, add
```shell
make auto-merge
```
in the end semaphore.yml, once all CI passed

## Passing Credentials Into A Docker Build

If your docker build requires ssh, aws, netrc, or other credentials to be passed into the
docker build, see [the secrets readme](BuildKitSecrets.md).

## Standardized Dependencies

If you need to install a standardized dependency, just include its file.
```
include ./mk-include/cc-librdkafka.mk
include ./mk-include/cc-sops.mk
```

## OpenAPI Spec
Include `cc-api.mk` for make targets supporting OpenAPI spec development:
```
API_SPEC_DIRS := src/main/resources/openapi

include ./mk-include/cc-api.mk
```

This integration looks in `API_SPEC_DIRS` for files named `minispec.yaml` and/or `openapi.yaml`.
All generated files are output into the same directory as the input files.

This will automatically integrate into the `build` and `test` top-level make targets:
* [`build` phase] Generate API-related artifacts.
  * Generate OpenAPI specification using Minispec (target: `api-spec` or `openapi`)
  * Generate HTML API documentation using ReDoc (target: `api-docs`)
* [`test` phase] Lint the API spec using:
  * [`yamllint`](https://github.com/adrienverge/yamllint) (target: `api-lint-yaml`)
  * [`openapi-spec-validator`](https://github.com/p1c2u/openapi-spec-validator) (target: `api-lint-openapi-spec-validator`)
  * [`spectral`](https://github.com/stoplightio/spectral) (target: `api-lint-spectral`)
* [`clean` phase] Remove all generated artifacts.

This also provides integration with the following tools:
  * (POC) Lint your API spec using [`speccy`](https://github.com/wework/speccy) (target: `api-lint-speccy`)
  * (POC) Auto-reload API docs using [`redoc-cli`](https://github.com/Redocly/redoc/tree/master/cli) (target: `redoc-serve` or `redoc-start`/`redoc-stop`)
  * (POC) Run a mock API server using [`prism`](https://github.com/stoplightio/prism) (target: `api-mock`)
  * (POC) Generate API load tests using [`gatling`](https://gatling.io/) (target: `api-loadtest`)
  * (POC) Generate Postman collections using [`openapi-to-postmanv2`](https://www.npmjs.com/package/openapi-to-postmanv2) (target: `api-postman`)
  * (POC) Generate SDK in Golang using [`openapi-generator`](https://github.com/OpenAPITools/openapi-generator) (target: `sdk/go`)
  * (POC) Generate SDK in Java using [`openapi-generator`](https://github.com/OpenAPITools/openapi-generator) (target: `sdk/java`)


## Add github templates

To add the github PR templates to your repo

```shell
make add-github-templates
```

## Developing

If you're developing an app that uses cc-mk-include or needs to extend it, it's useful
to understand how the "library" is structured.

The consistent developer experience of `make build`, `make test`, etc. is enabled by exposing a
handful of extension points that are used internally and available for individual apps as well.
For example, when you include `cc-go.mk` it adds `clean-go` to `CLEAN_TARGETS`, `build-go` to
`BUILD_TARGETS`, and so on. Each of these imports (like `semver`, `go`, `docker`, etc) is
essentially a standardized extension.

**The ultimate effect is to be able to "mix and match" different extensions
(e.g., semver, docker, go, helm, cpd) for different applications.**

You can run `make show-args` when you're inside any given project to see what extensions
are enabled for a given standard extensible command. For example, we can see that when you
run `make build` in the `cc-scheduler-service`, it'll run `build-go`, `build-docker`, and
`helm-package`.
```
cc-scheduler-service cody$ make show-args
INIT_CI_TARGETS:      seed-local-mothership deps cpd-update gcloud-install helm-setup-ci
CLEAN_TARGETS:         clean-go clean-images clean-terraform clean-cc-system-tests helm-clean
BUILD_TARGETS:         build-go build-docker helm-package
TEST_TARGETS:          lint-go test-go test-cc-system helm-lint
RELEASE_TARGETS:      set-tf-bumped-version helm-set-bumped-version get-release-image commit-release tag-release cc-cluster-spec-service push-docker
RELEASE_MAKE_TARGETS:  bump-downstream-tf-consumers helm-release
CI_BIN:
```

This also shows the full list of supported extension points (`INIT_CI_TARGETS`, `CLEAN_TARGETS`, and so on).

Applications themselves may also use these extension points; for example, you can append
your custom `clean-myapp` target to `CLEAN_TARGETS` to invoke as part of `make clean`.

We also expose a small number of override points for special cases (e.g., `BUILD_DOCKER_OVERRIDE`)
but these should be rather rare.
