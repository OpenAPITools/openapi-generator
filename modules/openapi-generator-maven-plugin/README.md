openapi-generator-maven-plugin
============================

A Maven plugin to support the OpenAPI generator project

Usage
============================

Add to your `build->plugins` section (default phase is `generate-sources` phase)
```xml
<plugin>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
    <version>3.3.3</version>
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

```
mvn clean compile
```

### General Configuration parameters

:bulb: These **general** configurations should be in the same level

- `inputSpec` - OpenAPI Spec file path
- `validateSpec` - Whether or not to validate the input spec prior to generation. Invalid specifications will result in an error.
- `language` - target generation language (deprecated, replaced by `generatorName` as values here don't represent only 'language' any longer)
- `generatorName` - target generator name
- `output` - target output path (default is `${project.build.directory}/generated-sources/swagger`)
- `templateDirectory` - directory with mustache templates
- `addCompileSourceRoot` - add the output directory to the project as a source root (`true` by default)
- `modelPackage` - the package to use for generated model objects/classes
- `apiPackage` - the package to use for generated api objects/classes
- `invokerPackage` - the package to use for the generated invoker objects
- `modelNamePrefix` and `modelNameSuffix` - Sets the pre- or suffix for model classes and enums
- `withXml` - enable XML annotations inside the generated models and API (only works with Java `language` and libraries that provide support for JSON and XML)
- `configOptions` - a map of language-specific parameters. To show a full list of generator-specified parameters (options), please use `configHelp` (explained below)
- `configHelp` - dumps the configuration help for the specified library (generates no sources)
- `ignoreFileOverride` - specifies the full path to a `.openapi-generator-ignore` used for pattern based overrides of generated outputs
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
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-generator-maven-plugin</artifactId>
    <version>${openapi-generator-maven-plugin-version}</version>
    <executions>
        <execution>
            <goals>
                <goal>generate</goal>
            </goals>
            <configuration>
                <inputSpec>${project.basedir}/src/main/resources/yaml/yamlfilename.yaml</inputSpec>
                <!-- language file, like e.g. JavaJaxRSCodegen -->
                <generatorName>com.my.package.for.GeneratorLanguage</generatorName>
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

Please see [an example configuration](examples) for using the plugin. To run these examples, explicitly pass the file to maven. Example:

```bash
mvn -f non-java.xml compile
```
