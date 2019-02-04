---
id: debugging
title: Debugging
---

## Templates

Sometimes, you may have issues with variables in your templates. As discussed in the [templating](./templating.md) docs, we offer a variety of system properties for inspecting the models bound to templates.

<dl>
<dt><code>-DdebugOpenAPI</code></dt>
<dd>Prints out the JSON model of the OpenAPI Document, as seen by OpenAPI Generator</dd>
<dt><code>-DdebugModels</code></dt>
<dd>Prints out the JSON model passed to model templates</dd>
<dt><code>-DdebugOperations</code></dt>
<dd>Prints out the JSON model passed to operation (api) templates</dd>
<dt><code>-DdebugSupportingFiles</code></dt>
<dd>Prints out the JSON model passed to supporting files</dd>
</dl>

One or more of these properties can be passed alongside other command line options:

```bash
openapi-generator generate -g go \
    -o out \
    -i petstore-minimal.yaml \
    -DdebugModels \
    -DdebugOperations
```

Or you can add these to your `JAVA_OPTS` environment variable (this applies to every invocation of the tool):

```bash
export JAVA_OPTS="${JAVA_OPTS} -DdebugModels -DdebugOperations"
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
