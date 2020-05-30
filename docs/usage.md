---
id: usage
title: Usage
---

Options for OpenAPI Generator are the same whether you're using the CLI, Maven/Gradle Plugins, or Online generation options.
This page demonstrates navigating the options via CLI. Commands are presented here in a logical progression as a tutorial, but you're welcome to skip directly to the [generate](#generate) command.

## help

The `help` option lists all commands available to the CLI.

```bash
openapi-generator help
usage: openapi-generator-cli <command> [<args>]

The most commonly used openapi-generator-cli commands are:
    author        Utilities for authoring generators or customizing templates.
    config-help   Config help for chosen lang
    generate      Generate code with the specified generator.
    help          Display help information about openapi-generator
    list          Lists the available generators
    meta          MetaGenerator. Generator for creating a new template set and configuration for Codegen.  The output will be based on the language you specify, and includes default templates to include.
    validate      Validate specification
    version       Show version information used in tooling

See 'openapi-generator-cli help <command>' for more information on a specific
command.
```

## version

The version command provides version information, returning either the semver version by default or the git sha when passed `--sha`.

```bash
NAME
        openapi-generator-cli version - Show version information

SYNOPSIS
        openapi-generator-cli version [--sha]

OPTIONS
        --sha
            Git commit SHA version

```

## list

The `list` command outputs a formatted list of every available generator. Pass the `-s/--short` option if you would like a CSV output for easy parsing.

```bash
openapi-generator help list
NAME
        openapi-generator-cli list - Lists the available generators

SYNOPSIS
        openapi-generator-cli list [(-i <include> | --include <include>)]
                [(-s | --short)]

OPTIONS
        -i <include>, --include <include>
            comma-separated list of stability indexes to include (value:
            all,beta,stable,experimental,deprecated). Excludes deprecated by
            default.

        -s, --short
            shortened output (suitable for scripting)

```

Example:

```bash
openapi-generator list -s | tr ',' '\n'
```

For the full list of generators, refer to the [Generators List](./generators.md).

## config-help

The `config-help` option provides details about 

```bash
openapi-generator help config-help
NAME
        openapi-generator-cli config-help - Config help for chosen lang

SYNOPSIS
        openapi-generator-cli config-help
                [(-f <output format> | --format <output format>)]
                [(-g <generator name> | --generator-name <generator name>)]
                [--markdown-header] [--named-header]
                [(-o <output location> | --output <output location>)]

OPTIONS
        -f <output format>, --format <output format>
            Write output files in the desired format. Options are 'text',
            'markdown' or 'yamlsample'. Default is 'text'.

        -g <generator name>, --generator-name <generator name>
            generator to get config help for

        --markdown-header
            When format=markdown, include this option to write out markdown
            headers (e.g. for docusaurus).

        --named-header
            Header includes the generator name, for clarity in output

        -o <output location>, --output <output location>
            Optionally write help to this location, otherwise default is
            standard output

```

The option of note is `-g/--generator-name` (other options are exposed for tooling).

You may pass any generator name (see [list](#list) command) to `-g`, and options specific to that generator will be displayed. Some generators have _many_ options, while others may have only a few.

Example:

```bash
openapi-generator config-help -g go
```

Outputs:

```text
CONFIG OPTIONS
	packageName
	    Go package name (convention: lowercase). (Default: openapi)

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

	packageVersion
	    Go package version. (Default: 1.0.0)

	withGoCodegenComment
	    whether to include Go codegen comment to disable Go Lint and collapse by default in GitHub PRs and diffs (Default: false)

	withXml
	    whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML) (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)
```

To pass these go client generator-specific options to the `generate` command for a go client, use the `--additional-properties` option. See the [generate](#generate) command section for an example.

## meta

The `meta` command creates a new Java class and template files, used for creating your own custom templates.

```bash
openapi-generator help meta
NAME
        openapi-generator-cli meta - MetaGenerator. Generator for creating a new
        template set and configuration for Codegen. The output will be based on
        the language you specify, and includes default templates to include.

SYNOPSIS
        openapi-generator-cli meta [(-n <name> | --name <name>)]
                [(-o <output directory> | --output <output directory>)]
                [(-p <package> | --package <package>)] [(-t <type> | --type <type>)]

OPTIONS
        -n <name>, --name <name>
            the human-readable name of the generator

        -o <output directory>, --output <output directory>
            where to write the generated files (current dir by default)

        -p <package>, --package <package>
            the package to put the main class into (defaults to
            org.openapitools.codegen)

        -t <type>, --type <type>
            the type of generator that is created
```

For an in-depth example of using the `meta` command, see [Customization](./customization.md).

## validate

The `validate` command allows you to validate an input specification, optionally providing recommendations for error fixes or other improvements (if available).

```bash
openapi-generator help validate
NAME
        openapi-generator-cli validate - Validate specification

SYNOPSIS
        openapi-generator-cli validate
                (-i <spec file> | --input-spec <spec file>) [--recommend]

OPTIONS
        -i <spec file>, --input-spec <spec file>
            location of the OpenAPI spec, as URL or file (required)

        --recommend

```

Valid Spec Example (using [petstore-v3.0.yaml](https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator-gradle-plugin/samples/local-spec/petstore-v3.0.yaml))
```bash
openapi-generator validate -i petstore-v3.0.yaml
```
```text
Validating spec (petstore-v3.0.yaml)
No validation issues detected.
```

Invalid Spec Example (using [petstore-v3.0-invalid.yaml](https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator-gradle-plugin/samples/local-spec/petstore-v3.0-invalid.yaml)):

```bash
openapi-generator validate -i petstore-v3.0-invalid.yaml
```
```text
Validating spec (petstore-v3.0-invalid.yaml)
Errors:
	-attribute info is missing

[error] Spec has 1 errors.
```

## completion

Although not documented in the `help` output, the CLI offers a `completion` command, which can be used for auto-completion.

This command takes one or more parameters representing the args list you would otherwise pass to `openapi-generator`. For example:

```bash
openapi-generator completion config-help
-o
--output
--named-header
-g
--generator-name
-f
--format
--markdown-header
```

An example bash completion script can be found in the repo at [scripts/openapi-generator-cli-completion.bash](https://github.com/OpenAPITools/openapi-generator/blob/master/scripts/openapi-generator-cli-completion.bash).

## generate

The `generate` command is the workhorse of the generator toolset. As such, it has _many_ more options available than the previous commands. The abbreviated options are below, but you may expand the full descriptions.


```bash
openapi-generator help generate
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
                (-i <spec file> | --input-spec <spec file>)
                [--ignore-file-override <ignore file override location>]
                [--import-mappings <import mappings>...]
                [--instantiation-types <instantiation types>...]
                [--invoker-package <invoker package>]
                [--language-specific-primitives <language specific primitives>...]
                [--library <library>] [--log-to-stderr] [--minimal-update]
                [--model-name-prefix <model name prefix>]
                [--model-name-suffix <model name suffix>]
                [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)] 
                [(-p <additional properties> | --additional-properties <additional properties>)...]
                [--package-name <package name>] [--release-note <release note>]
                [--remove-operation-id-prefix]
                [--reserved-words-mappings <reserved word mappings>...]
                [(-s | --skip-overwrite)] [--server-variables <server variables>...]
                [--skip-validate-spec] [--strict-spec <true/false strict behavior>]
                [(-t <template directory> | --template-dir <template directory>)]
                [--type-mappings <type mappings>...] [(-v | --verbose)]
```

<details>
<summary>generate OPTIONS</summary>

```bash
OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the OpenAPI definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

        --api-name-suffix <api name suffix>
            Suffix that will be appended to all API names ('tags'). Default:
            Api. e.g. Pet => PetApi. Note: Only ruby, python, jaxrs generators
            suppport this feature at the moment.

        --api-package <api package>
            package for generated api classes

        --artifact-id <artifact id>
            artifactId in generated pom.xml. This also becomes part of the
            generated library's filename

        --artifact-version <artifact version>
            artifact version in generated pom.xml. This also becomes part of the
            generated library's filename

        -c <configuration file>, --config <configuration file>
            Path to configuration file. It can be JSON or YAML. If file is JSON,
            the content should have the format {"optionKey":"optionValue",
            "optionKey1":"optionValue1"...}. If file is YAML, the content should
            have the format optionKey: optionValue. Supported options can be
            different for each language. Run config-help -g {generator name}
            command for language-specific config options.

        --dry-run
            Try things out and report on potential changes (without actually
            making changes).

        -e <templating engine>, --engine <templating engine>
            templating engine: "mustache" (default) or "handlebars" (beta)

        --enable-post-process-file
            Enable post-processing file using environment variables.

        -g <generator name>, --generator-name <generator name>
            generator to use (see list command for list)

        --generate-alias-as-model
            Generate model implementation for aliases to map and array schemas.
            An 'alias' is an array, map, or list which is defined inline in a
            OpenAPI document and becomes a model in the generated code. A 'map'
            schema is an object that can have undeclared properties, i.e. the
            'additionalproperties' attribute is set on that object. An 'array'
            schema is a list of sub schemas in a OAS document

        --git-host <git host>
            Git host, e.g. gitlab.com.

        --git-repo-id <git repo id>
            Git repo ID, e.g. openapi-generator.

        --git-user-id <git user id>
            Git user ID, e.g. openapitools.

        --global-property <global properties>
            sets specified global properties (previously called 'system
            properties') in the format of name=value,name=value (or multiple
            options, each with name=value)

        --group-id <group id>
            groupId in generated pom.xml

        --http-user-agent <http user agent>
            HTTP user agent, e.g. codegen_csharp_api_client, default to
            'OpenAPI-Generator/{packageVersion}/{language}'

        -i <spec file>, --input-spec <spec file>
            location of the OpenAPI spec, as URL or file (required)

        --ignore-file-override <ignore file override location>
            Specifies an override location for the .openapi-generator-ignore
            file. Most useful on initial generation.

        --import-mappings <import mappings>
            specifies mappings between a given class and the import that should
            be used for that class in the format of type=import,type=import. You
            can also have multiple occurrences of this option.

        --instantiation-types <instantiation types>
            sets instantiation type mappings in the format of
            type=instantiatedType,type=instantiatedType.For example (in Java):
            array=ArrayList,map=HashMap. In other words array types will get
            instantiated as ArrayList in generated code. You can also have
            multiple occurrences of this option.

        --invoker-package <invoker package>
            root package for generated code

        --language-specific-primitives <language specific primitives>
            specifies additional language specific primitive types in the format
            of type1,type2,type3,type3. For example:
            String,boolean,Boolean,Double. You can also have multiple
            occurrences of this option.

        --library <library>
            library template (sub-template)

        --log-to-stderr
            write all log messages (not just errors) to STDOUT. Useful for
            piping the JSON output of debug options (e.g. `-DdebugOperations`)
            to an external parser directly while testing a generator.

        --minimal-update
            Only write output files that have changed.

        --model-name-prefix <model name prefix>
            Prefix that will be prepended to all model names.

        --model-name-suffix <model name suffix>
            Suffix that will be appended to all model names.

        --model-package <model package>
            package for generated models

        -o <output directory>, --output <output directory>
            where to write the generated files (current dir by default)

        -p <additional properties>, --additional-properties <additional
        properties>
            sets additional properties that can be referenced by the mustache
            templates in the format of name=value,name=value. You can also have
            multiple occurrences of this option.

        --package-name <package name>
            package for generated classes (where supported)

        --release-note <release note>
            Release note, default to 'Minor update'.

        --remove-operation-id-prefix
            Remove prefix of operationId, e.g. config_getId => getId

        --reserved-words-mappings <reserved word mappings>
            specifies how a reserved name should be escaped to. Otherwise, the
            default _<name> is used. For example id=identifier. You can also
            have multiple occurrences of this option.

        -s, --skip-overwrite
            specifies if the existing files should be overwritten during the
            generation.

        --server-variables <server variables>
            sets server variables overrides for spec documents which support
            variable templating of servers.

        --skip-validate-spec
            Skips the default behavior of validating an input specification.

        --strict-spec <true/false strict behavior>
            'MUST' and 'SHALL' wording in OpenAPI spec is strictly adhered to.
            e.g. when false, no fixes will be applied to documents which pass
            validation but don't follow the spec.

        -t <template directory>, --template-dir <template directory>
            folder containing the template files

        --type-mappings <type mappings>
            sets mappings between OpenAPI spec types and generated code types in
            the format of OpenAPIType=generatedType,OpenAPIType=generatedType.
            For example: array=List,map=Map,string=String. You can also have
            multiple occurrences of this option.

        -v, --verbose
            verbose mode

```

</details>


At a minimum, `generate` requires:

* `-g` to specify the generator
* `-o` to specify a meaningful output directory (defaults to the current directory!)
* `-i` to specify the input OpenAPI document

> **NOTE** You may also pass `-Dcolor` as a system property to colorize terminal outputs.

### Examples

The following examples use [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml).

#### Additional Properties

Generator-specific options should be passed as `--additional-properties`:

```bash
openapi-generator generate -g go --additional-properties=prependFormOrBodyParameters=true \
    -o out -i petstore.yaml
```

Pass more options via comma delimited key/value pairs:

```bash
--additional-properties=key1=value1,key2=value2
```

For the full list of generator-specific parameters, refer to [generators docs](./generators.md).

#### Type Mappings and Import Mappings

Most generators allow for types bound to the OpenAPI Specification's types to be remapped to a user's desired types. Not _all_ type mappings can be reassigned, as some generators define mappings which are tightly coupled to the built-in templates.

If you're not using your own templates with star/glob package imports, you will most likely need to combine `--type-mappings` and `--import-mappings` together.

* `--type-mappings` Defines the user's target type
* `--import-mappings` Informs the template of the type to be imported

Here's how one might change the `kotlin-spring` server generator's default of `OffsetDateTime` to `LocalDateTime`:

```bash
openapi-generator generate \
    -i petstore.yaml \
    -g kotlin-spring \
    -o out \
    --additional-properties=library=spring-boot,beanValidations=true,swaggerAnnotations=true,serviceImplementation=true \
    --import-mappings=DateTime=java.time.LocalDateTime \
    --type-mappings=DateTime=java.time.LocalDateTime
```

> NOTE: mappings are applied to `DateTime`, as this is the representation of the primitive type. See [DefaultCodegen](https://github.com/OpenAPITools/openapi-generator/blob/7cee999543fcc00b7c1eb9f70f0456b707c7f9e2/modules/openapi-generator/src/main/java/org/openapitools/codegen/DefaultCodegen.java#L1431).

### Target External Models

Sometimes you don't want the codegen to make a model for you--you might want to just include one that already exists in your codebase.  Say you already have a `User` object and want to reuse that, which has a different model package from the other generated files:

First, indicate that the class is already included by default. This will keep the codegen from trying to generate the class.

```bash
--language-specific-primitives=Pet
```

This command line option will tell the generator to consider `Pet` a "primitive" type.

Next, if the `Pet` class is a different package, add an `--import-mapping` to tell the generator to include that import wherever `Pet` is used:

```bash
--import-mappings=Pet=com.yourpackage.models.Pet
```

Now the codegen will know what to import from that specific package.

NOTE: `import-mappings` is assigned a key-value pair in this example, but multiple values can be comma-separate. For instance:

```bash
--import-mappings=Pet=com.yourpackage.models.Pet,User=com.yourpackage.models.User
```


#### Configuration File

Rather than passing generator options in a CSV of `--additional-properties`, you may also provide the settings via JSON file or YAML file.

For example, one of our typescript samples has the following configuration file:

```json
{
  "npmName": "@swagger/typescript-fetch-petstore",
  "npmVersion": "1.0.0",
  "npmRepository" : "https://skimdb.npmjs.com/registry",
  "snapshot" : false,
  "supportsES6": true
}
```

These settings can be passed via `-c filename`. Here, we've saved the above as `config.json`:

```bash
openapi-generator generate -i petstore.yaml -g typescript-fetch -o out \
    -c config.json
```

Same configuration file can be passed into YAML format having following equivalent content:

```yaml
npmName: "@swagger/typescript-fetch-petstore"
npmVersion: "1.0.0"
npmRepository: "https://skimdb.npmjs.com/registry"
snapshot: false
supportsES6: true
```

The settings are passed exactly the same as for `config.json`. The most important part is the file extension. Supported values are `yml` or `yaml`. 
The name of the file should be `config.yml` or `config.yaml` (in our example it will be `config.yaml`.

```bash
openapi-generator generate -i petstore.yaml -g typescript-fetch -o out \
    -c config.yaml
```


## batch

The `batch` command allows you to move all CLI arguments supported by the `generate` command into a YAML or JSON file.

*NOTE*: This command supports an additional `!include` property which may point to another "shared" file, the base path to which can be
modified by `--includes-base-dir`.

```bash
openapi-generator help batch
NAME
        openapi-generator-cli batch - Generate code in batch via external
        configs.

SYNOPSIS
        openapi-generator-cli batch [--fail-fast]
                [--includes-base-dir <includes>] [(-r <threads> | --threads <threads>)]
                [--root-dir <root>] [--timeout <timeout>] [(-v | --verbose)] [--]
                <configs>...

OPTIONS
        --fail-fast
            fail fast on any errors

        --includes-base-dir <includes>
            base directory used for includes

        -r <threads>, --threads <threads>
            thread count

        --root-dir <root>
            root directory used output/includes (includes can be overridden)

        --timeout <timeout>
            execution timeout (minutes)

        -v, --verbose
            verbose mode

        --
            This option can be used to separate command-line options from the
            list of argument, (useful when arguments might be mistaken for
            command-line options

        <configs>
            Generator configuration files.
```

Example:

```bash
# create "shared" config
mkdir shared && cat > shared/common.yaml <<EOF
inputSpec: https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml
additionalProperties:
    x-ext-name: "Your Name"
EOF

# create "standard" configs
cat > kotlin.yaml <<EOF
'!include': 'shared/common.yaml'
outputDir: out/kotlin
generatorName: kotlin
artifactId: kotlin-petstore-string
additionalProperties:
  dateLibrary: string
  serializableModel: "true"
EOF

cat > csharp.yaml <<EOF
'!include': 'shared/common.yaml'
outputDir: out/csharp-netcore
generatorName: csharp-netcore
additionalProperties:
  packageGuid: "{321C8C3F-0156-40C1-AE42-D59761FB9B6C}"
  useCompareNetObjects: "true"
EOF

# Generate them
openapi-generator batch *.yaml
```

## author

This command group contains utilities for authoring generators or customizing templates.

```
openapi-generator help author
NAME
        openapi-generator-cli author - Utilities for authoring generators or
        customizing templates.

SYNOPSIS
        openapi-generator-cli author
        openapi-generator-cli author template [(-v | --verbose)]
                [(-o <output directory> | --output <output directory>)]
                [--library <library>]
                (-g <generator name> | --generator-name <generator name>)

OPTIONS
        --help
            Display help about the tool

        --version
            Display full version output

COMMANDS
        With no arguments, Display help information about openapi-generator

        template
            Retrieve templates for local modification

            With --verbose option, verbose mode

            With --output option, where to write the template files (defaults to
            'out')

            With --library option, library template (sub-template)

            With --generator-name option, generator to use (see list command for
            list)
```

### template

This command allows user to extract templates from the CLI jar which simplifies customization efforts.

```
NAME
        openapi-generator-cli author template - Retrieve templates for local
        modification

SYNOPSIS
        openapi-generator-cli author template
                (-g <generator name> | --generator-name <generator name>)
                [--library <library>]
                [(-o <output directory> | --output <output directory>)]
                [(-v | --verbose)]

OPTIONS
        -g <generator name>, --generator-name <generator name>
            generator to use (see list command for list)

        --library <library>
            library template (sub-template)

        -o <output directory>, --output <output directory>
            where to write the template files (defaults to 'out')

        -v, --verbose
            verbose mode
```

Example:

Extract Java templates, limiting to the `webclient` library.

```
openapi-generator author template -g java --library webclient
```

Extract all Java templates:

```
openapi-generator author template -g java
```
