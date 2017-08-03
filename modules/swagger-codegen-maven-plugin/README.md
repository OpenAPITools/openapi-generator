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
    <version>2.2.2</version>
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
- `modelNamePrefix` and `modelNameSuffix` - Sets the pre- or suffix for model classes and enums
- `withXml` - enable XML annotations inside the generated models and API (only works with Java `language` and libraries that provide support for JSON and XML)
- `configOptions` - a map of language-specific parameters (see below)
- `configHelp` - dumps the configuration help for the specified library (generates no sources)
- `ignoreFileOverride` - specifies the full path to a `.swagger-codegen-ignore` used for pattern based overrides of generated outputs
- `generateApis` - generate the apis (`true` by default)
- `generateApiTests` - generate the api tests (`true` by default. Only available if `generateApis` is `true`)
- `generateApiDocumentation` - generate the api documentation (`true` by default. Only available if `generateApis` is `true`)
- `generateModels` - generate the models (`true` by default)
- `modelsToGenerate` - A comma separated list of models to generate.  All models is the default.
- `generateModelTests` - generate the model tests (`true` by default. Only available if `generateModels` is `true`)
- `generateModelDocumentation` - generate the model documentation (`true` by default. Only available if `generateModels` is `true`)
- `generateSupportingFiles` - generate the supporting files (`true` by default)
- `supportingFilesToGenerate` - A comma separated list of supporting files to generate.  All files is the default.
- `skip` - skip code generation (`false` by default. Can also be set globally through the `codegen.skip` property)

### Custom Generator

Specifying a custom generator is a bit different. It doesn't support the classpath:/ syntax, but it does support the fully qualified name of the package. You can also specify your custom templates, which also get pulled in. Notice the dependency on a project, in the plugin scope. That would be your generator/template jar.

```xml
<plugin>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>${swagger-codegen-maven-plugin-version}</version>
    <executions>
        <execution>
            <goals>
                <goal>generate</goal>
            </goals>
            <configuration>
                <inputSpec>src/main/resources/yaml/yamlfilename.yaml</inputSpec>
                <!-- language file, like e.g. JavaJaxRSCodegen shipped with swagger -->
                <language>com.my.package.for.GeneratorLanguage</language>
                <templateDirectory>myTemplateDir</templateDirectory>

                <output>${project.build.directory}/generated-sources</output>
                <apiPackage>${default.package}.handler</apiPackage>
                <modelPackage>${default.package}.model</modelPackage>
                <invokerPackage>${default.package}.handler</invokerPackage>
            </configuration>
        </execution>
    </executions>

    <dependencies>
        <dependency>
            <groupId>com.my.generator</groupId>
            <artifactId>customgenerator</artifactId>
            <version>1.0-SNAPSHOT</version>
        </dependency>
    </dependencies>
</plugin>
```

### Sample configuration

- Please see [an example configuration](examples) for using the plugin
