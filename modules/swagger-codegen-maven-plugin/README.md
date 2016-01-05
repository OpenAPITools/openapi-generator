swagger-codegen-maven-plugin
============================

A Maven plugin to support the [swagger](http://swagger.io) code generation project

Usage
============================

Add to your `build->plugins` section (default phase is `generate-sources` phase)
```xml
<plugin>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>2.1.5-SNAPSHOT</version>
    <executions>
        <execution>
            <goals>
                <goal>generate</goal>
            </goals>
            <configuration>
                <inputSpec>src/main/resources/api.yaml</inputSpec>
                <language>java</language>
                <configOptions>
                   <sourceFolder>src/gen/java/main</sourceFolder>
                </configOptions>
            </configuration>
        </execution>
    </executions>
</plugin>
```

Followed by:

```
mvn clean compile
```

### General Configuration parameters

- `inputSpec` - OpenAPI Spec file path
- `language` - target generation language
- `output` - target output path (default is `${project.build.directory}/generated-sources/swagger`)
- `templateDirectory` - directory with mustache templates
- `addCompileSourceRoot` - add the output directory to the project as a source root (`true` by default)
- `modelPackage` - the package to use for generated model objects/classes
- `apiPackage` - the package to use for generated api objects/classes
- `invokerPackage` - the package to use for the generated invoker objects
- `configOptions` - a map of language-specific parameters (see below)
- `configHelp` - dumps the configuration help for the specified library (generates no sources)

### Sample configuration

- Please see [an example configuration](examples) for using the plugin