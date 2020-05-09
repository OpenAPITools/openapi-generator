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
    <!-- RELEASE_VERSION -->
    <version>4.3.1</version>
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

```
mvn clean compile
```

### General Configuration parameters

:bulb: These **general** configurations should be in the same level

| Option | Property | Description |
|--------|----------|-------------|
| `verbose` |  `openapi.generator.maven.plugin.verbose` | verbose mode (`false` by default)
| `inputSpec` |  `openapi.generator.maven.plugin.inputSpec` | OpenAPI Spec file path
| `language` |  `openapi.generator.maven.plugin.language` | target generation language (deprecated, replaced by `generatorName` as values here don't represent only 'language' any longer)
| `generatorName` |  `openapi.generator.maven.plugin.generatorName` | target generator name
| `output` |  `openapi.generator.maven.plugin.output` | target output path (default is `${project.build.directory}/generated-sources/openapi`. Can also be set globally through the `openapi.generator.maven.plugin.output` property)
| `gitHost` | `openapi.generator.maven.plugin.gitHost` | The git host, e.g. gitlab.com
| `gitUserId` |  `openapi.generator.maven.plugin.gitUserId` | sets git information of the project
| `gitRepoId` | `openapi.generator.maven.plugin.gitRepoId` | sets the repo ID (e.g. openapi-generator)
| `templateDirectory` |  `openapi.generator.maven.plugin.templateDirectory` | directory with mustache templates
| `templateResourcePath` |  `openapi.generator.maven.plugin.templateResourcePath` | directory with mustache templates via resource path. This option will overwrite any option defined in `templateDirectory`.
| `engine` | `openapi.generator.maven.plugin.engine` | The name of templating engine to use, "mustache" (default) or "handlebars" (beta)
| `auth` |  `openapi.generator.maven.plugin.auth` | adds authorization headers when fetching the OpenAPI definitions remotely. Pass in a URL-encoded string of `name:header` with a comma separating multiple values
| `configurationFile` |  `openapi.generator.maven.plugin.configurationFile` | Path to separate json configuration file. File content should be in a json format {"optionKey":"optionValue", "optionKey1":"optionValue1"...} Supported options can be different for each language. Run `config-help -g {generator name}` command for language specific config options
| `skipOverwrite` |  `openapi.generator.maven.plugin.skipOverwrite` | Specifies if the existing files should be overwritten during the generation. (`false` by default)
| `apiPackage` |  `openapi.generator.maven.plugin.apiPackage` | the package to use for generated api objects/classes
| `modelPackage` |  `openapi.generator.maven.plugin.modelPackage` | the package to use for generated model objects/classes
| `invokerPackage` |  `openapi.generator.maven.plugin.invokerPackage` | the package to use for the generated invoker objects
| `packageName` | `openapi.generator.maven.plugin.packageName` | the default package name to use for the generated objects
| `groupId` | `openapi.generator.maven.plugin.groupId`  | sets project information in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators
| `artifactId` |  `openapi.generator.maven.plugin.artifactId` | sets project information in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators
| `artifactVersion` |  `openapi.generator.maven.plugin.artifactVersion` | sets project information in generated pom.xml/build.gradle or other build script. Language-specific conversions occur in non-jvm generators
| `library` |  `openapi.generator.maven.plugin.library` | library template (sub-template)
| `modelNamePrefix` |  `openapi.generator.maven.plugin.modelNamePrefix` | Sets the prefix for model classes and enums
| `modelNameSuffix` |  `openapi.generator.maven.plugin.modelNameSuffix` | Sets the suffix for model classes and enums
| `ignoreFileOverride` |  `openapi.generator.maven.plugin.ignoreFileOverride` | specifies the full path to a `.openapi-generator-ignore` used for pattern based overrides of generated outputs
| `httpUserAgent` | `openapi.generator.maven.plugin.httpUserAgent` | Sets custom User-Agent header value
| `removeOperationIdPrefix` |  `openapi.generator.maven.plugin.removeOperationIdPrefix` | remove operationId prefix (e.g. user_getName => getName)
| `logToStderr` |  `openapi.generator.maven.plugin.logToStderr` | write all log messages (not just errors) to STDOUT
| `enablePostProcessFile` |  `openapi.generator.maven.plugin.` | enable file post-processing hook
| `skipValidateSpec` |  `openapi.generator.maven.plugin.skipValidateSpec` | Whether or not to skip validating the input spec prior to generation. By default, invalid specifications will result in an error.
| `strictSpec` |  `openapi.generator.maven.plugin.strictSpec` | Whether or not to treat an input document strictly against the spec. 'MUST' and 'SHALL' wording in OpenAPI spec is strictly adhered to. e.g. when false, no fixes will be applied to documents which pass validation but don't follow the spec.
| `generateAliasAsModel` |  `openapi.generator.maven.plugin.generateAliasAsModel` | generate alias (array, map) as model
| `configOptions` |  N/A | a **map** of language-specific parameters. To show a full list of generator-specified parameters (options), please use `configHelp` (explained below)
| `instantiationTypes` |  `openapi.generator.maven.plugin.instantiationTypes` | sets instantiation type mappings in the format of type=instantiatedType,type=instantiatedType. For example (in Java): `array=ArrayList,map=HashMap`. In other words array types will get instantiated as ArrayList in generated code. You can also have multiple occurrences of this option
| `importMappings` |  `openapi.generator.maven.plugin.importMappings` | specifies mappings between a given class and the import that should be used for that class in the format of type=import,type=import. You can also have multiple occurrences of this option
| `typeMappings` |  `openapi.generator.maven.plugin.typeMappings` | sets mappings between OpenAPI spec types and generated code types in the format of OpenAPIType=generatedType,OpenAPIType=generatedType. For example: `array=List,map=Map,string=String`. You can also have multiple occurrences of this option
| `languageSpecificPrimitives` |  `openapi.generator.maven.plugin.languageSpecificPrimitives` | specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: `String,boolean,Boolean,Double`. You can also have multiple occurrences of this option
| `additionalProperties` |  `openapi.generator.maven.plugin.additionalProperties` | sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value. You can also have multiple occurrences of this option
| `serverVariableOverrides` | `openapi.generator.maven.plugin.serverVariableOverrides` | A map of server variable overrides for specs that support server URL templating
| `reservedWordsMappings` |  `openapi.generator.maven.plugin.reservedWordsMappings` | specifies how a reserved name should be escaped to. Otherwise, the default `_<name>` is used. For example `id=identifier`. You can also have multiple occurrences of this option
| `generateApis` |  `openapi.generator.maven.plugin.generateApis` | generate the apis (`true` by default). Specific apis may be defined as a CSV via `apisToGenerate`.
| `apisToGenerate` |  `openapi.generator.maven.plugin.apisToGenerate` | A comma separated list of apis to generate.  All apis is the default.
| `generateModels` |  `openapi.generator.maven.plugin.generateModels` | generate the models (`true` by default). Specific models may be defined as a CSV via `modelsToGenerate`.
| `modelsToGenerate` |  `openapi.generator.maven.plugin.modelsToGenerate` | A comma separated list of models to generate.  All models is the default.
| `generateSupportingFiles` |  `openapi.generator.maven.plugin.generateSupportingFiles` | generate the supporting files (`true` by default)
| `supportingFilesToGenerate` |  `openapi.generator.maven.plugin.supportingFilesToGenerate` | A comma separated list of supporting files to generate.  All files is the default.
| `generateModelTests` |  `openapi.generator.maven.plugin.generateModelTests` | generate the model tests (`true` by default. Only available if `generateModels` is `true`)
| `generateModelDocumentation` |  `openapi.generator.maven.plugin.generateModelDocumentation` | generate the model documentation (`true` by default. Only available if `generateModels` is `true`)
| `generateApiTests` |  `openapi.generator.maven.plugin.generateApiTests` | generate the api tests (`true` by default. Only available if `generateApis` is `true`)
| `generateApiDocumentation` |  `openapi.generator.maven.plugin.generateApiDocumentation` | generate the api documentation (`true` by default. Only available if `generateApis` is `true`)
| `withXml` |  `openapi.generator.maven.plugin.withXml` | enable XML annotations inside the generated models and API (only works with Java `language` and libraries that provide support for JSON and XML)
| `skip` |  `codegen.skip` | skip code generation (`false` by default. Can also be set globally through the `codegen.skip` property)
| `skipIfSpecIsUnchanged` |  `codegen.skipIfSpecIsUnchanged` | Skip the execution if the source file is older than the output folder (`false` by default. Can also be set globally through the `codegen.skipIfSpecIsUnchanged` property)
| `addCompileSourceRoot` |  `openapi.generator.maven.plugin.addCompileSourceRoot` | Add the output directory to the project as a source root, so that the generated java types are compiled and included in the project artifact (`true` by default). Mutually exclusive with `addTestCompileSourceRoot`.
| `addTestCompileSourceRoot` |  `openapi.generator.maven.plugin.addTestCompileSourceRoot` | Add the output directory to the project as a test source root, so that the generated java types are compiled only for the test classpath of the project (`false` by default). Mutually exclusive with `addCompileSourceRoot`.
| `environmentVariables` | N/A | A **map** of items conceptually similar to "environment variables" or "system properties". These are merged into a map of global settings available to all aspects of the generation flow. Use this map for any options documented elsewhere as `systemProperties`.
| `configHelp` |  `codegen.configHelp` | dumps the configuration help for the specified library (generates no sources)

### Configuring **map** structures

For configuration options documented as a **map** above, the key/value options may be configured as free-form nodes under these options. This takes the format:

```xml
<configuration>
    <option>
       <key>value</key>
    </option>
</configuration>
```

This configuration node location will match that of the plugin configuration examples at the top of this document and in the section below. Here, `option` matches in option name in the first column in the table from the previous section.
The `key` and `value` text are any values you'd like to provide for that option. As an example, to configure `environmentVariables` to match the `-Dmodels=User,Pet` example from our [Selective Generation](https://openapi-generator.tech/docs/customization#selective-generation) documentation, see below.

```xml
<configuration>
    <environmentVariables>
       <models>User,Pet</models>
    </environmentVariables>
</configuration>
```

Not that some of these environment variable options may overwrite or conflict with other options available to the maven plugin. For example, the above `environmentVariables` example is equivalent to the following:

```xml
<configuration>
    <generateModels>true</generateModels>
    <modelsToGenerate>User,Pet</modelsToGenerate>
</configuration>
```

The difference here is that you may define `generateModels` and `modelsToGenerate` as properties, while `environmentVariables` may only be configured as a configuration node.

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
