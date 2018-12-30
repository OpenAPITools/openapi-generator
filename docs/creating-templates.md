---
id: creating-templates
title: Creating Templates
---

It's easy to build new templates for codegen!  Here are a few steps you should follow to do so.

The generator workflow has [transforming logic](https://github.com/openapitools/openapi-generator/tree/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/languages) as well as templates for each generation of code.

Each generator will create a data structure from the OpenAPI document; OpenAPI 2.0 and OpenAPI 3.x documents are normalized into the same API model within the generator. This model is then applied to the templates.  While generators do not need to perform transformations, it's often necessary in order to add more advanced support for your language or framework. You may need to refer to the generator implementation to understand some of the logic while creating or customizing templates (see [FinchServerCodegen.java](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/FinchServerCodegen.java) for an advanced example).

The transform logic needs to implement [CodegenConfig.java](https://github.com/openapitools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/CodegenConfig.java) and is most easily done by extending [DefaultCodegen.java](https://github.com/openapitools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/DefaultCodegen.java).  Take a look at the various implementations as a guideline while the instructions get more complete.

## Modifying Templates

> OpenAPI Generator applies user-defined templates via options:  
> * CLI: `-t/--template` CLI options
> * Maven Plugin: `templateDirectory`
> * Gradle Plugin: `templateDir` 

Built-in templates are written in Mustache and processed by [jmustache](https://github.com/samskivert/jmustache). We plan to eventually support Handlebars and user-defined template engines via plugins.

OpenAPI Generator supports user-defined templates. This approach is often the easiest when creating a custom template. Our generators implement a combination of language and framework features, and it's fully possible to use an existing generator to implement a custom template for a different framework. Suppose you have internal utilities which you'd like to incorporate into generated code (e.g. logging, monitoring, fault-handling)... this is easy to add via custom templates.

### Custom Logic

For this example, let's modify a Java client to use AOP via [jcabi/jcabi-aspects](https://github.com/jcabi/jcabi-aspects). We'll log API method execution at the `INFO` level, and retry every API call a maximum of 2 times.

The Java generator supports a `library` option. This option works by defining base templates, then applying library-specific template overrides. This allows for template reuse for libraries sharing the same programming language. Templates defined as a library need only modify or extend the templates concerning the library, and generation falls back to the root templates (the "defaults") when not extended by the library. Generators which support the `library` option will only support the libraries known by the generator at compile time, and will throw a runtime error if you try to provide a custom library name.

To get started, we will need to copy our target generator's directory in full. The directory will be located under `modules/opeanpi-generator/src/main/resources/{generator}`. In general, the generator directory matches the generator name (what you would pass to the `generator` option), but this is not a requirement-- if you are having a hard time finding the template directory, look at the `embeddedTemplateDir` option in your target generator's implementation.

If you've already cloned openapi-generator, find and copy the `modules/opeanpi-generator/src/main/resources/Java` directory. If you have the [Refined GitHub](https://github.com/sindresorhus/refined-github) Chrome or Firefox Extension, you can navigate to this directory on GitHub and click the "Download" button. Or, to pull the directory from latest master:

```bash
mkdir -p ~/.openapi-generator/templates/ && cd $_
curl -L https://api.github.com/repos/OpenAPITools/openapi-generator/tarball | tar xz
mv `ls`/modules/openapi-generator/src/main/resources/Java ./Java
\rm -rf OpenAPITools-openapi-generator-*
cd Java
```

**Optional**: Before modifying your templates, you may want to `git init && git add . && git commit -am 'initial'` so you can easily revert to the base templates.

At this point, you have _every_ Java library's template locally. Let's delete all libraries except the `resteasy` library we'll be extending:

```bash
ls -d libraries/* | grep -v resteasy | xargs rm -rf
```

Execute `tree` in this Java directory and inspect the mustache files and directory structure. You'll notice there are quite a few templates in the directory root, but extending this root to support resteasy only requires modifying a handful of files:

```bash
tree libraries/resteasy/
libraries/resteasy/
├── ApiClient.mustache
├── JSON.mustache
├── api.mustache
├── build.gradle.mustache
├── build.sbt.mustache
└── pom.mustache

0 directories, 6 files
```

> NOTE: Some generators may be sensitive to _which_ files exist. If you're concerned with redundant files like `pom.mustache` and `build.sbt.mustache`, you can try deleting them. If the generator you're customizing fails at runtime, just `touch` these files to create an empty file.



First, let's add our new dependency to `libraries/resteasy/build.gradle.mustache`:

```diff
diff --git a/libraries/resteasy/build.gradle.mustache b/libraries/resteasy/build.gradle.mustache
index 3b40702..a6d12e0 100644
--- a/libraries/resteasy/build.gradle.mustache
+++ b/libraries/resteasy/build.gradle.mustache
@@ -134,6 +134,7 @@ ext {
 }
 
 dependencies {
+    compile "com.jcabi:jcabi-aspects:0.22.6"
     compile "io.swagger:swagger-annotations:$swagger_annotations_version"
     compile "org.jboss.resteasy:resteasy-client:$resteasy_version"
     compile "org.jboss.resteasy:resteasy-multipart-provider:$resteasy_version"

```

Then, we'll add the necessary import to `api.mustache`. This file is the template which becomes the API invoking class (e.g. `PetApi` or `StoreApi`).

```diff
diff --git a/libraries/resteasy/api.mustache b/libraries/resteasy/api.mustache
index a4d0f9f..49b17c7 100644
--- a/libraries/resteasy/api.mustache
+++ b/libraries/resteasy/api.mustache
@@ -1,5 +1,6 @@
 package {{package}};
 
+import com.jcabi.aspects.Loggable;
 import {{invokerPackage}}.ApiException;
 import {{invokerPackage}}.ApiClient;
 import {{invokerPackage}}.Configuration;

```

Next, we'll find the code which generates API methods. You'll see `{{#operations}}{{#operation}}` which is a mustache "loop" which executes the template logic if the model applied to the template has an `operations` array, and a non-null `operation` instance in that array. You can pass `-DdebugOpenAPI` when generating via CLI to inspect the full object model.

Further down in `api.mustache`, find implementation of the method call, and add the `@Loggable` annotation. This template is easy because it has a single method implementation.

```diff
diff --git a/libraries/resteasy/api.mustache b/libraries/resteasy/api.mustache
index 49b17c7..16ee191 100644
--- a/libraries/resteasy/api.mustache
+++ b/libraries/resteasy/api.mustache
@@ -57,6 +57,7 @@ public class {{classname}} {
   {{#isDeprecated}}
   @Deprecated
   {{/isDeprecated}}
+  @Loggable(Loggable.INFO)
   public {{#returnType}}{{{returnType}}} {{/returnType}}{{^returnType}}void {{/returnType}}{{operationId}}({{#allParams}}{{{dataType}}} {{paramName}}{{#hasMore}}, {{/hasMore}}{{/allParams}}) throws ApiException {
     Object {{localVariablePrefix}}localVarPostBody = {{#bodyParam}}{{paramName}}{{/bodyParam}}{{^bodyParam}}new Object(){{/bodyParam}};
     {{#allParams}}{{#required}}

```

Finally, because our new dependency relies on AspectJ and code weaving, let's modify the `build.gradle.mustache` again to set this up.

```diff
diff --git a/build.gradle.mustache b/build.gradle.mustache
index 04a9d55..7a93c50 100644
--- a/build.gradle.mustache
+++ b/build.gradle.mustache
@@ -1,5 +1,6 @@
 apply plugin: 'idea'
 apply plugin: 'eclipse'
+apply plugin: 'aspectj'
 
 group = '{{groupId}}'
 version = '{{artifactVersion}}'
@@ -12,6 +13,7 @@ buildscript {
     dependencies {
         classpath 'com.android.tools.build:gradle:2.3.+'
         classpath 'com.github.dcendents:android-maven-gradle-plugin:1.5'
+        classpath "net.uberfoo.gradle:gradle-aspectj:2.2"
     }
 }
 
@@ -140,9 +142,18 @@ ext {
     jersey_version = "1.19.4"
     jodatime_version = "2.9.9"
     junit_version = "4.12"
+    aspectjVersion = '1.9.0'
 }
 
+sourceCompatibility = '1.8'
+targetCompatibility = '1.8'
+
 dependencies {
+    compile "com.jcabi:jcabi-aspects:0.22.6"
+    aspectpath "com.jcabi:jcabi-aspects:0.22.6"
+    // usually, client code leaves logging implementation to the consumer code
+    compile "org.apache.logging.log4j:log4j-slf4j-impl:2.8.2"
+    compile "org.apache.logging.log4j:log4j-core:2.8.2"
     compile "io.swagger:swagger-annotations:$swagger_annotations_version"
     compile "com.sun.jersey:jersey-client:$jersey_version"
     compile "com.sun.jersey.contribs:jersey-multipart:$jersey_version"

```

> NOTE: This example includes log4j-slf4j-impl to demonstrate our new code working. Generally you'll want to leave logging implementations up to your consumers.

And because the java client generates with an outdated Gradle 2.6, let's update the gradle version in the default template (`Java/gradle-wrapper.properties.mustache`):

```diff
diff --git a/gradle-wrapper.properties.mustache b/gradle-wrapper.properties.mustache
index b7a3647..3d9d088 100644
--- a/gradle-wrapper.properties.mustache
+++ b/gradle-wrapper.properties.mustache
@@ -3,4 +3,4 @@ distributionBase=GRADLE_USER_HOME
 distributionPath=wrapper/dists
 zipStoreBase=GRADLE_USER_HOME
 zipStorePath=wrapper/dists
-distributionUrl=https\://services.gradle.org/distributions/gradle-2.6-bin.zip
+distributionUrl=https\://services.gradle.org/distributions/gradle-4.8-bin.zip

```

Now we're ready to generate the client with our simple changes. When we pass the template directory option to our toolset, we _must_ pass the generator's root directory and _not_ the library-only directory.

```bash
openapi-generator generate -g java --library resteasy \
    -t ~/.openapi-generator/templates/Java \
    -o ~/.openapi-generator/example \
    -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml
```

Make sure your custom template compiles:

```bash
cd ~/.openapi-generator/example
gradle wrapper --gradle-version 4.8 --distribution-type all
./gradlew assemble
```

You should see a log message showing our added dependency being downloaded:

```text
…
Download https://jcenter.bintray.com/com/jcabi/jcabi-aspects/0.22.6/jcabi-aspects-0.22.6.pom
…
```

And for the sake of verifying our AOP modifications work, let's create a `src/main/resources/log4j2.properties` file in our new client code:

```properties
status = error
dest = err
name = PropertiesConfig

property.filename = target/rolling/rollingtest.log

filter.threshold.type = ThresholdFilter
filter.threshold.level = debug

appender.console.type = Console
appender.console.name = STDOUT
appender.console.layout.type = PatternLayout
appender.console.layout.pattern = %m%n
appender.console.filter.threshold.type = ThresholdFilter
appender.console.filter.threshold.level = error

appender.rolling.type = RollingFile
appender.rolling.name = RollingFile
appender.rolling.fileName = ${filename}
appender.rolling.filePattern = target/rolling2/test1-%d{MM-dd-yy-HH-mm-ss}-%i.log.gz
appender.rolling.layout.type = PatternLayout
appender.rolling.layout.pattern = %d %p %C{1.} [%t] %m%n
appender.rolling.policies.type = Policies
appender.rolling.policies.time.type = TimeBasedTriggeringPolicy
appender.rolling.policies.time.interval = 2
appender.rolling.policies.time.modulate = true
appender.rolling.policies.size.type = SizeBasedTriggeringPolicy
appender.rolling.policies.size.size=100MB
appender.rolling.strategy.type = DefaultRolloverStrategy
appender.rolling.strategy.max = 5

logger.rolling.name = org.openapitools.client.api.PetApi
logger.rolling.level = debug
logger.rolling.additivity = false
logger.rolling.appenderRef.rolling.ref = RollingFile

rootLogger.level = info
rootLogger.appenderRef.stdout.ref = STDOUT
```

Execute `./gradlew build` and then `cat target/rolling/rollingtest.log`. You should see messages logged for every call in PetApi with a stubbed unit test.

Congratulations! You've now modified one of the built-in templates to meet your client code's needs. Adding/modifying template logic simply requires a little bit of [mustache](https://mustache.github.io/), for which you can use existing templates as a guide.

### Custom Framework

## Structures

Aside from transforming an API document, the implementing class gets to decide how to apply the data structure to templates. We can decide which data structure to apply to which template files. You have the following structures at your disposal.

### Operations

> Dump operation structures passed to templates with `-DdebugOpenAPI`

There is a data structure which represents all the operations that are defined in the OpenAPI specification.  A single API file is created for each `OperationGroup`, which is essentially a grouping of different operations.  See the `addOperationToGroup` in `DefaultCodegen.java` for details on this operation.

You can have many files created for each `OperationGroup` by processing multiple templates and assigning a different file naming pattern to them.  So for a single file per operation:

```java
// process the `api.mustache` template and output a single file with suffix `.java`:
apiTemplateFiles.put("api.mustache", ".java");
```

For C-like languages which also require header files, you may create two files per operation.

```objc
// create a header and implementation for each operation group:
apiTemplateFiles.put("api-header.mustache", ".h");
apiTemplateFiles.put("api-body.mustache", ".m");
```
Here, an Operation with tag `Pet` will generate two files: `SWGPetApi.h` and `SWGPetApi.m`. The `SWG` prefix and `Api` suffix are options specific to the Objective-C generator.

### Models

> Dump models passed to templates with `-DdebugModels`

Each model identified inside the generator will be passed into the `Models` data structure and will generate a new model file (or files) for each model.

### supportingFiles

> Dump supportingFiles passed to templates with `-DdebugSupportingFiles`

This is a "catch-all" which gives you the entire structure--operations, model, etc--so you can create "single-file" code from them.

