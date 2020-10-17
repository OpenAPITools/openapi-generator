---
id: file-post-processing
title: File post-processing
---

Each tool (CLI and plugins) supports enabling file post-processing at a high-level. Enabling this option allows for generators which support post-processing to call some external process for each generated file, passing the file path to that tool. The external tool must be defined in an environment variable supported by the generator.

Note that:

* this option is `--enable-post-process-file` in the CLI and `enablePostProcessFile` in plugins
* we require _both_ specifying the environment variable _and_ enabling the option at the tooling level; this feature is opt-in for security 
* file processing occurs one at a time
* the external tool may be a custom script which invokes multiple tools

Also refer to the relevant documentation for [CLI](./usage.md), [Maven Plugin](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-maven-plugin/README.md), [Gradle Plugin](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-gradle-plugin/README.adoc), or [SBT Plugin](https://github.com/OpenAPITools/sbt-openapi-generator/blob/master/README.md).

## Supported Environment Variables

The following environment variables are supported by their respective generators:
<!-- query with: grep -Rn '_POST_PROCESS_FILE"' modules | grep -Eo '[^"]+_POST_PROCESS_FILE' | sort -u -->

* `CPP_POST_PROCESS_FILE`
* `CSHARP_POST_PROCESS_FILE`
* `C_POST_PROCESS_FILE`
* `DART_POST_PROCESS_FILE`
* `FSHARP_POST_PROCESS_FILE`
* `GO_POST_PROCESS_FILE`
* `HASKELL_POST_PROCESS_FILE`
* `JAVA_POST_PROCESS_FILE`
* `JS_POST_PROCESS_FILE`
* `KOTLIN_POST_PROCESS_FILE`
* `OCAML_POST_PROCESS_FILE`
* `PERL_POST_PROCESS_FILE`
* `PHP_POST_PROCESS_FILE`
* `POWERSHELL_POST_PROCESS_FILE`
* `PYTHON_POST_PROCESS_FILE`
* `RUBY_POST_PROCESS_FILE`
* `RUST_POST_PROCESS_FILE`
* `SCALA_POST_PROCESS_FILE`
* `SWIFT_POST_PROCESS_FILE`
* `TS_POST_PROCESS_FILE`

## Example

Let's see how to pass Ruby generated files to Rubocop, a static code analysis/linter/formatter tool.

```
# First, export the required environment variable
export RUBY_POST_PROCESS_FILE="/usr/local/bin/rubocop -a"

export OPENAPI_DOC="https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"

# Invoke the generator with --enable-post-process-file
openapi-generator generate --enable-post-process-file -i $OPENAPI_DOC -g ruby -o .out-ruby/
```

You will now see messages logged about which files have been processed:

```
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/.rspec
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/spec/spec_helper.rb
[main] INFO  o.o.c.languages.AbstractRubyCodegen - Successfully executed: /usr/local/bin/rubocopy -a /Users/jim/projects/openapi-generator/.out-ruby/spec/spec_helper.rb
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/spec/configuration_spec.rb
[main] INFO  o.o.c.languages.AbstractRubyCodegen - Successfully executed: /usr/local/bin/rubocopy -a /Users/jim/projects/openapi-generator/.out-ruby/spec/configuration_spec.rb
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/spec/api_client_spec.rb
[main] INFO  o.o.c.languages.AbstractRubyCodegen - Successfully executed: /usr/local/bin/rubocopy -a /Users/jim/projects/openapi-generator/.out-ruby/spec/api_client_spec.rb
[main] INFO  o.o.codegen.TemplateManager - Skipped /Users/jim/projects/openapi-generator/.out-ruby/.openapi-generator-ignore (Skipped by supportingFiles options supplied by user.)
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/.openapi-generator/VERSION
[main] INFO  o.o.codegen.TemplateManager - writing file /Users/jim/projects/openapi-generator/.out-ruby/.openapi-generator/FILES
```
