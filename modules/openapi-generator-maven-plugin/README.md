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
    <version>3.3.4</version>
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
- `output` - target output path (default is `${project.build.directory}/generated-sources/openapi`. Can also be set globally through the `openapi.generator.maven.plugin.output` property)
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
- `removeOperationIdPrefix` - remove operationId prefix (e.g. user_getName => getName)
- `logToStderr` - write all log messages (not just errors) to STDOUT
- `enablePostProcessFile` - enable file post-processing hook
- `skipValidateSpec` - skip spec validation
- `generateAliasAsModel` - generate alias (array, map) as model
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
- `verbose` - verbose mode (`false` by default)
- `groupId`, `artifactId` and `artifactVersion`  - sets project information in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators
- `gitUserId` and `gitRepoId` - sets git information of the project
- `auth` - adds authorization headers when fetching the OpenAPI definitions remotely. Pass in a URL-encoded string of `name:header` with a comma separating multiple values
- `configurationFile` - Path to separate json configuration file. File content should be in a json format {"optionKey":"optionValue", "optionKey1":"optionValue1"...} Supported options can be different for each language. Run `config-help -g {generator name}` command for language specific config options
- `skipOverwrite` - Specifies if the existing files should be overwritten during the generation. (`false` by default)
- `library` - library template (sub-template)
- `instantiationTypes` - sets instantiation type mappings in the format of type=instantiatedType,type=instantiatedType. For example (in Java): `array=ArrayList,map=HashMap`. In other words array types will get instantiated as ArrayList in generated code. You can also have multiple occurrences of this option
- `importMappings` - specifies mappings between a given class and the import that should be used for that class in the format of type=import,type=import. You can also have multiple occurrences of this option
- `typeMappings` - sets mappings between OpenAPI spec types and generated code types in the format of OpenAPIType=generatedType,OpenAPIType=generatedType. For example: `array=List,map=Map,string=String`. You can also have multiple occurrences of this option
- `languageSpecificPrimitives` - specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: `String,boolean,Boolean,Double`. You can also have multiple occurrences of this option
- `additionalProperties` - sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value. You can also have multiple occurrences of this option
- `reservedWordsMappings` - specifies how a reserved name should be escaped to. Otherwise, the default `_<name>` is used. For example `id=identifier`. You can also have multiple occurrences of this option
- `skipIfSpecIsUnchanged` - Skip the execution if the source file is older than the output folder (`false` by default. Can also be set globally through the `codegen.skipIfSpecIsUnchanged` property)


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
