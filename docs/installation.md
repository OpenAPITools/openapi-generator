---
id: installation
title: CLI Installation
---

There are a number of ways to use OpenAPI Generator. This page documents how to install the CLI artifact.
Installing OpenAPI Generator's CLI tool allows users to generate all available generators from the command line.

Some of the following are cross-platform options and some are not, these are called out where possible.

## npm

> **Platform(s)**: Linux, macOS, Windows

The [npm package wrapper](https://github.com/openapitools/openapi-generator-cli) is cross-platform wrapper around the .jar artifact. It works by providing a CLI wrapper atop the JAR's command line options. This gives a simple interface layer which normalizes usage of the command line across operating systems, removing some differences in how options or switches are passed to the tool (depending on OS).
**Install** the latest version of the tool globally, exposing the CLI on the command line:

```bash
npm install @openapitools/openapi-generator-cli -g
```

To install a specific version of the tool, pass the version during installation:
<!-- RELEASE_VERSION -->
```bash
openapi-generator-cli version-manager set 5.3.0
```
<!-- /RELEASE_VERSION -->
To install the tool as a dev dependency in your current project:

```bash
npm install @openapitools/openapi-generator-cli -D
```
Then, **generate** a ruby client from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml) doc:

```bash
npx @openapitools/openapi-generator-cli generate -i petstore.yaml -g ruby -o /tmp/test/
```

> `npx` will execute a globally available `openapi-generator`, and if not found it will fall back to project-local commands. The result is that the above command will work regardless of which installation method you've chosen.

## Homebrew

> **Platform(s)**: macOS

**Install** via [homebrew](https://brew.sh/):

```bash
brew install openapi-generator
```

Then, **generate** a ruby client from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml) doc:

```bash
openapi-generator generate -i petstore.yaml -g ruby -o /tmp/test/
```

## Docker

> **Platform(s)**: Linux, macOS, Windows

The OpenAPI Generator Docker image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml) doc with this image, you'll need to mount a local location as a volume.
You'll then need to output the generated code to this mapped volume. Everything else works the same as if you ran the command on your host machine.

Here's an example generating a Go client:

```bash
docker run --rm \
  -v ${PWD}:/local openapitools/openapi-generator-cli generate \
  -i /local/petstore.yaml \
  -g go \
  -o /local/out/go
```

## JAR

> **Platform(s)**: Linux, macOS, Windows

<!-- RELEASE_VERSION -->
If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 8 runtime at a minimum):

JAR location: `https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.3.0/openapi-generator-cli-5.3.0.jar`

For **Mac/Linux** users:

```bash
wget https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.3.0/openapi-generator-cli-5.3.0.jar -O openapi-generator-cli.jar
```

For **Windows** users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+), e.g.

```powershell
Invoke-WebRequest -OutFile openapi-generator-cli.jar https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/5.3.0/openapi-generator-cli-5.3.0.jar
```
<!-- /RELEASE_VERSION -->

After downloading the JAR, run `java -jar openapi-generator-cli.jar help` to show the usage.

For Mac users, please make sure Java 8 is installed (Tips: run `java -version` to check the version), and export `JAVA_HOME` in order to use the supported Java version:

```bash
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```

## Bash Launcher Script

> **Platform(s)**: Linux, macOS, Windows (variable)

One downside to manual JAR downloads is that you don't keep up-to-date with the latest released version. We have a Bash launcher script at [bin/utils/openapi-generator.cli.sh](https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/bin/utils/openapi-generator-cli.sh) which solves this problem.

To install the launcher script, copy the contents of the script to a location on your path and make the script executable.

An example of setting this up (NOTE: Always evaluate scripts curled from external systems before executing them).

```bash
mkdir -p ~/bin/openapitools
curl https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/bin/utils/openapi-generator-cli.sh > ~/bin/openapitools/openapi-generator-cli
chmod u+x ~/bin/openapitools/openapi-generator-cli
export PATH=$PATH:~/bin/openapitools/
```

Now, `openapi-generator-cli` is "installed". On invocation, it will query the GitHub repository for the most recently released version. If this matches the last downloaded jar,
it will execute as normal. If a newer version is found, the script will download the latest release and execute it.

If you need to invoke an older version of the generator, you can define the variable `OPENAPI_GENERATOR_VERSION` either ad hoc or globally. You can export this variable if you'd like to persist a specific release version.

Examples:

```bash
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
