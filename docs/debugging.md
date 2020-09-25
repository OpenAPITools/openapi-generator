---
id: debugging
title: Debugging
---

## Generation

As a user there may be times when generated outputs don't match your expectations it's unclear why. The CLI supports a `--dry-run` option which may be used to inspect the anticipated file operations without making changes to the file system.

Suppose you generate using the `--minimal-update` option, and you notice on subsequent generations of a client that no files have changed. This is by design.

For example, if you generate the aspnetcore generator passing `--minimal-update --dry-run` to the sample generation script in the code repository:

```bash
export JAVA_OPTS="-Dlog.level=off"
./bin/generate-samples.sh ./bin/configs/lua.yaml -- --minimal-update --dry-run
```

You'll see the output similar to the following:

```bash
$ ./bin/generate-samples.sh ./bin/configs/lua.yaml -- --minimal-update --dry-run
# START SCRIPT: ./bin/generate-samples.sh
This script generates all configs under bin/configs by default.
You may generate a targeted script or set of scripts using glob patterns.

For example:
    ./bin/generate-samples.sh bin/configs/java-*

You may generate a single config with additional options if you use -- to
separate the single config file from the generator arguments.

For example:
    ./bin/generate-samples.sh bin/configs/java-vertx.yaml -- --global-property debugModels=true


[main] INFO  o.o.codegen.DefaultGenerator - Generating with dryRun=true
[main] INFO  o.o.codegen.DefaultGenerator - OpenAPI Generator: lua (client)
[main] INFO  o.o.codegen.DefaultGenerator - Generator 'lua' is considered beta.
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] INFO  o.o.codegen.DefaultGenerator - Model inline_object (marked as unused due to form parameters) is generated due to the system property skipFormModel=false (default)
[main] INFO  o.o.codegen.DefaultGenerator - Model inline_object_1 (marked as unused due to form parameters) is generated due to the system property skipFormModel=false (default)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/json)
[main] WARN  o.o.codegen.DefaultCodegen - Multiple MediaTypes found, using only the first one
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/json)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] WARN  o.o.codegen.utils.ModelUtils - Multiple schemas found in the OAS 'content' section, returning only the first one (application/xml)
[main] ERROR o.o.codegen.DefaultGenerator - 

Dry Run Results:

k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/.openapi-generator-ignore
n /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/.openapi-generator/VERSION
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/api_response_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/category_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/inline_object_1_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/inline_object_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/order_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/pet_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/pet_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/store_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/tag_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/user_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/user_spec.lua


States:

  - w Write
  - n Write if New/Updated
  - i Ignored
  - s Skipped Overwrite
  - k Skipped by user option(s)
  - e Error evaluating file write state


[main] ERROR o.o.codegen.DefaultGenerator - 

Dry Run Results:

k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/.openapi-generator-ignore
n /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/.openapi-generator/VERSION
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/api_response_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/category_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/inline_object_1_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/inline_object_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/order_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/pet_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/pet_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/store_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/tag_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/user_api_spec.lua
k /Users/williamcheng/Code/openapi-generator/samples/client/petstore/lua/spec/user_spec.lua


States:

  - w Write
  - n Write if New/Updated
  - i Ignored
  - s Skipped Overwrite
  - k Skipped by user option(s)
  - e Error evaluating file write state

```

The output lists the files which would be written in a normal run of the tool. Notice that we skip `.openapi-generator-ignore` because the file exists and we don't want to blow away the user's generation rules. Most of these files will overwrite output files only if the contents slated for write are different from those on the filesystem; this is denoted by an `n` preceding the filename. Some of the above lines begin with a `w`, meaning these files will _always_ result in a write operation.

If you find an operation that you feel should result in a different state, please [open an issue](https://github.com/OpenAPITools/openapi-generator/issues/new/choose) or [submit a pull request](https://github.com/OpenAPITools/openapi-generator/compare) to change the behavior (we welcome all contributions).


## Templates

Sometimes, you may have issues with variables in your templates. As discussed in the [templating](./templating.md) docs, we offer a variety of system properties for inspecting the models bound to templates.

<dl>
<dt><code>--global-property debugOpenAPI</code></dt>
<dd>Prints out the JSON model of the OpenAPI Document, as seen by OpenAPI Generator</dd>
<dt><code>--global-property debugModels</code></dt>
<dd>Prints out the JSON model passed to model templates</dd>
<dt><code>--global-property debugOperations</code></dt>
<dd>Prints out the JSON model passed to operation (api) templates</dd>
<dt><code>--global-property debugSupportingFiles</code></dt>
<dd>Prints out the JSON model passed to supporting files</dd>
</dl>

One or more of these properties can be passed alongside other command line options:

```bash
openapi-generator generate -g go \
    -o out \
    -i petstore-minimal.yaml \
    --global-property debugModels,debugOperations
```

Or you can add these to your `JAVA_OPTS` environment variable (this applies to every invocation of the tool):

```bash
export JAVA_OPTS="${JAVA_OPTS} --global-property debugModels,debugOperations"
```

> NOTE: Globally available system options like these will apply to all invocations of the generator (CLI and plugins)

## Runtime

When you're working with a custom generator, a new generator, or otherwise trying to understand the behavior of the toolset, you may need to attach a remote debugger in order to step through the code.

The steps are shown here for a specific version of the generator, but apply the same if you're working off master or a feature branch.

* Determine the version of `openapi-generator` you're using. For the CLI, this is:  
  ```
  openapi-generator version
  ```
* Navigate to the `openapi-generator` source directory (see [building](./building.md) docs for obtaining source code and brief introduction).
* Checkout the branch/tag for the target version. Branches are not prefixed, but tags are prefixed with a `v`. For instance if you're using version `3.3.0`, you will execute:
  ```
  git checkout v3.3.0
  ```
* Open the project in your IDE.
* Setup your IDE for remote debugging. You'll want to define a port used for connecting the remote debugger. For this example, we'll use `5005`. See external tutorials for [IntelliJ](https://www.jetbrains.com/help/idea/run-debug-configuration-remote-debug.html) and [Eclipse](https://www.ibm.com/developerworks/library/os-eclipse-javadebug/index.html)
* Export the debug configuration, specifying `suspend=y` so you have time to attach a remote debugger. These are passed as Java system properties, either on command line or as part of the `JAVA_OPTS` environment variable. This will look like:
  ```
  export JAVA_OPTS="${JAVA_OPTS} -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
  ```
* Execute the generator with your desired options. You should see the application output _only_
  ```
  Listening for transport dt_socket at address: 5005
  ```
* Set breakpoints in code, and then attach your remote debugger from your IDE (see above). The generator will automatically unblock once the remote debugger is attached. You can now step through the code.

## Logs

You can try to enable debugging log with `-Dlog.level=debug` option to the `JAVA_OPTS` environment variable to see more information:

```bash
export JAVA_OPTS="${JAVA_OPTS} -Dlog.level=debug"
```

Set the option then DEBUG logs are printed out:

```bash
openapi-generator generate -g go ...

...
...
[main] DEBUG o.o.codegen.DefaultCodegen - debugging fromProperty for files : class Schema {
    type: null
    format: null
    $ref: #/components/schemas/File
...
...
```
