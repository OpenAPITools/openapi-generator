---
id: plugins
title: Plugins
---

## Maven

A Maven plugin to support the OpenAPI generator project

### Example

Add to your `build->plugins` section (default phase is `generate-sources` phase)

```xml
<plugin>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
    <!-- RELEASE_VERSION -->
    <version>5.1.0</version>
    <!-- /RELEASE_VERSION -->
    <executions>
        <execution>
            <goals>
                <goal>generate</goal>
            </goals>
            <configuration>
                <inputSpec>${project.basedir}/src/main/resources/api.yaml</inputSpec>
                <generatorName>java</generatorName>
                <configOptions>
                   <sourceFolder>src/gen/java/main</sourceFolder>
                </configOptions>
            </configuration>
        </execution>
    </executions>
</plugin>
```

Followed by:

```bash
mvn clean compile
```

For full details of all options, see the [plugin README](https://github.com/OpenAPITools/openapi-generator/tree/master/modules/openapi-generator-maven-plugin).

### Dependencies

The generated models use commonly used Swagger v2 annotations like `@ApiModelProperty`. A user may add Swagger v3 annotations:

```xml
<dependency>
    <groupId>io.swagger.core.v3</groupId>
    <artifactId>swagger-annotations</artifactId>
</dependency>
```

But this will not work. This dependency is not binary compatible with Swagger v2 annotations. The resulting code will fail to compile.

As alternative instead use the following dependency:

```xml
<dependency>
    <groupId>io.swagger.parser.v3</groupId>
    <artifactId>swagger-parser</artifactId>
</dependency>
```

## Gradle

This gradle plugin offers a declarative DSL via extensions (these are Gradle project extensions). These map almost fully 1:1 with the options you’d pass to the CLI or Maven plugin. The plugin maps the extensions to a task of the same name to provide a clean API. If you’re interested in the extension/task mapping concept from a high-level, you can check out [Gradle’s docs](https://docs.gradle.org/current/userguide/custom_plugins.html#sec:mapping_extension_properties_to_task_properties).

To include in your project, add the following to `build.gradle`:

```groovy
buildscript {
  repositories {
    mavenLocal()
    maven { url "https://repo1.maven.org/maven2" }
  }
  dependencies {
    classpath "org.openapitools:openapi-generator-gradle-plugin:5.0.0"
  }
}

apply plugin: 'org.openapi.generator'
```

This gives access to the following tasks:

| Task | Description |
| ---- | ----------- |
| openApiGenerate | Generate code via Open API Tools Generator for Open API 2.0 or 3.x specification documents. |
| openApiGenerators | Lists generators available via Open API Generators. |
| openApiMeta  | Generates a new generator to be consumed via Open API Generator. | 
| openApiValidate  | Validates an Open API 2.0 or 3.x specification document. |

> The plugin implements the above tasks as project extensions of the same name. If you’d like to declare these tasks as dependencies to other tasks (using `dependsOn`), you’ll need a task reference. e.g.:
> ```groovy
> compileJava.dependsOn tasks.openApiGenerate
> ```

For full details of all options, see the [plugin README](https://github.com/OpenAPITools/openapi-generator/tree/master/modules/openapi-generator-gradle-plugin).

### Example

An example task for generating a kotlin client:

```groovy
openApiGenerate {
    generatorName = "kotlin"
    inputSpec = "$rootDir/specs/petstore-v3.0.yaml".toString()
    outputDir = "$buildDir/generated".toString()
    apiPackage = "org.openapi.example.api"
    invokerPackage = "org.openapi.example.invoker"
    modelPackage = "org.openapi.example.model"
    configOptions = [
        dateLibrary: "java8"
    ]
}
```
