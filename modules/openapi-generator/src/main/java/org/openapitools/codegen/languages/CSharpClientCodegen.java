/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class CSharpClientCodegen extends AbstractCSharpCodegen {
    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final String NUNIT = "nunit";
    private static final String RESTSHARP = "restsharp";
    private static final String NEWTONSOFT_JSON = "newtonsoft-json";
    private static final String JSON_SUBTYPES = "json-subtypes";
    private static final String FODY = "fody";
    private static final String PROPERTYCHANGED_FODY = "propertychanged-fody";
    private static final String NET452 = "v4.5.2";
    private static final String NET45 = "v4.5";
    private static final String NET40 = "v4.0";
    private static final String NET35 = "v3.5";
    private static final String NETSTANDARD = "netstandard1.3";
    private static final String UWP = "uwp";

    // Defines the sdk option for targeted frameworks, which differs from targetFramework and targetFrameworkNuget
    private static final String MCS_NET_VERSION_KEY = "x-mcs-sdk";

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
    protected String clientPackage = "Org.OpenAPITools.Client";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    // Defines TargetFrameworkVersion in csproj files
    protected String targetFramework = NET45;

    // Defines nuget identifiers for target framework
    protected String targetFrameworkNuget = "net45";
    protected boolean supportsAsync = Boolean.TRUE;
    protected boolean supportsUWP = Boolean.FALSE;
    protected boolean netStandard = Boolean.FALSE;
    protected boolean generatePropertyChanged = Boolean.FALSE;

    protected boolean validatable = Boolean.TRUE;
    protected Map<Character, String> regexModifiers;
    protected final Map<String, String> frameworks;

    // By default, generated code is considered public
    protected boolean nonPublicApi = Boolean.FALSE;

    // use KellermanSoftware.CompareNetObjects for deep recursive object comparison
    protected boolean useCompareNetObjects = Boolean.FALSE;

    // To make API response's headers dictionary case insensitive
    protected boolean caseInsensitiveResponseHeaders = Boolean.FALSE;

    public CSharpClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        supportsInheritance = true;
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        hideGenerationTimestamp = Boolean.TRUE;

        cliOptions.clear();

        typeMapping.put("boolean", "bool");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long");
        typeMapping.put("double", "double");
        typeMapping.put("number", "decimal");
        typeMapping.put("decimal", "decimal");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("UUID", "Guid");
        typeMapping.put("URI", "string");

        setSupportNullable(Boolean.TRUE);

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Title.Case).",
                this.packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.",
                this.packageVersion);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        addOption(CodegenConstants.INTERFACE_PREFIX,
                CodegenConstants.INTERFACE_PREFIX_DESC,
                interfacePrefix);

        CliOption framework = new CliOption(
                CodegenConstants.DOTNET_FRAMEWORK,
                CodegenConstants.DOTNET_FRAMEWORK_DESC
        );
        frameworks = new ImmutableMap.Builder<String, String>()
                .put(NET35, ".NET Framework 3.5 compatible")
                .put(NET40, ".NET Framework 4.0 compatible")
                .put(NET45, ".NET Framework 4.5 compatible")
                .put(NET452, ".NET Framework 4.5.2+ compatible")
                .put(NETSTANDARD, ".NET Standard 1.3 compatible (DEPRECATED. Please use `csharp-netcore` generator instead)")
                .put(UWP, "Universal Windows Platform (DEPRECATED. Please use `csharp-netcore` generator instead)")
                .build();
        framework.defaultValue(this.targetFramework);
        framework.setEnum(frameworks);
        cliOptions.add(framework);

        CliOption modelPropertyNaming = new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC);
        cliOptions.add(modelPropertyNaming.defaultValue("PascalCase"));

        // CLI Switches
        addSwitch(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC,
                this.hideGenerationTimestamp);

        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                this.sortParamsByRequiredFlag);

        addSwitch(CodegenConstants.USE_DATETIME_OFFSET,
                CodegenConstants.USE_DATETIME_OFFSET_DESC,
                this.useDateTimeOffsetFlag);

        addSwitch(CodegenConstants.USE_COLLECTION,
                CodegenConstants.USE_COLLECTION_DESC,
                this.useCollection);

        addSwitch(CodegenConstants.RETURN_ICOLLECTION,
                CodegenConstants.RETURN_ICOLLECTION_DESC,
                this.returnICollection);

        addSwitch(CodegenConstants.OPTIONAL_METHOD_ARGUMENT,
                "C# Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).",
                this.optionalMethodArgumentFlag);

        addSwitch(CodegenConstants.OPTIONAL_ASSEMBLY_INFO,
                CodegenConstants.OPTIONAL_ASSEMBLY_INFO_DESC,
                this.optionalAssemblyInfoFlag);

        addSwitch(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES,
                CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES_DESC,
                this.optionalEmitDefaultValuesFlag);

        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE,
                CodegenConstants.OPTIONAL_PROJECT_FILE_DESC,
                this.optionalProjectFileFlag);

        addSwitch(CodegenConstants.GENERATE_PROPERTY_CHANGED,
                CodegenConstants.PACKAGE_DESCRIPTION_DESC,
                this.generatePropertyChanged);

        // NOTE: This will reduce visibility of all public members in templates. Users can use InternalsVisibleTo
        // https://msdn.microsoft.com/en-us/library/system.runtime.compilerservices.internalsvisibletoattribute(v=vs.110).aspx
        // to expose to shared code if the generated code is not embedded into another project. Otherwise, users of codegen
        // should rely on default public visibility.
        addSwitch(CodegenConstants.NON_PUBLIC_API,
                CodegenConstants.NON_PUBLIC_API_DESC,
                this.nonPublicApi);

        addSwitch(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS,
                CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC,
                this.allowUnicodeIdentifiers);

        addSwitch(CodegenConstants.NETCORE_PROJECT_FILE,
                CodegenConstants.NETCORE_PROJECT_FILE_DESC,
                this.netCoreProjectFileFlag);

        addSwitch(CodegenConstants.VALIDATABLE,
                CodegenConstants.VALIDATABLE_DESC,
                this.validatable);

        addSwitch(CodegenConstants.USE_COMPARE_NET_OBJECTS,
                CodegenConstants.USE_COMPARE_NET_OBJECTS_DESC,
                this.useCompareNetObjects);

        addSwitch(CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS,
                CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS_DESC,
                this.caseInsensitiveResponseHeaders);

        regexModifiers = new HashMap<Character, String>();
        regexModifiers.put('i', "IgnoreCase");
        regexModifiers.put('m', "Multiline");
        regexModifiers.put('s', "Singleline");
        regexModifiers.put('x', "IgnorePatternWhitespace");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        /*
         * NOTE: When supporting boolean additionalProperties, you should read the value and write it back as a boolean.
         * This avoids oddities where additionalProperties contains "false" rather than false, which will cause the
         * templating engine to behave unexpectedly.
         *
         * Use the pattern:
         *     if (additionalProperties.containsKey(prop)) convertPropertyToBooleanAndWriteBack(prop);
         */

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }


        if (isEmpty(apiPackage)) {
            setApiPackage("Api");
        }
        if (isEmpty(modelPackage)) {
            setModelPackage("Model");
        }
        clientPackage = "Client";

        Boolean excludeTests = false;
        if (additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = convertPropertyToBooleanAndWriteBack(CodegenConstants.EXCLUDE_TESTS);
        }

        if (additionalProperties.containsKey(CodegenConstants.VALIDATABLE)) {
            setValidatable(convertPropertyToBooleanAndWriteBack(CodegenConstants.VALIDATABLE));
        } else {
            additionalProperties.put(CodegenConstants.VALIDATABLE, validatable);
        }

        if (additionalProperties.containsKey(CodegenConstants.DOTNET_FRAMEWORK)) {
            setTargetFramework((String) additionalProperties.get(CodegenConstants.DOTNET_FRAMEWORK));
        } else {
            // Ensure default is set.
            setTargetFramework(NET45);
            additionalProperties.put(CodegenConstants.DOTNET_FRAMEWORK, this.targetFramework);
        }

        if (NET35.equals(this.targetFramework)) {
            // This is correct, mono will require you build .NET 3.5 sources using 4.0 SDK
            additionalProperties.put(MCS_NET_VERSION_KEY, "4");
            additionalProperties.put("net35", true);
            if (additionalProperties.containsKey(CodegenConstants.SUPPORTS_ASYNC)) {
                LOGGER.warn(".NET 3.5 generator does not support async.");
                additionalProperties.remove(CodegenConstants.SUPPORTS_ASYNC);
            }

            setTargetFrameworkNuget("net35");
            setValidatable(Boolean.FALSE);
            setSupportsAsync(Boolean.FALSE);
        } else if (NETSTANDARD.equals(this.targetFramework)) {
            LOGGER.warn(".NET Standard 1.3 support has been DEPRECATED in this generator. Please use `csharp-netcore` generator instead.");
            additionalProperties.put(MCS_NET_VERSION_KEY, "4.6-api");
            if (additionalProperties.containsKey("supportsUWP")) {
                LOGGER.warn(".NET {} generator does not support UWP.", NETSTANDARD);
                additionalProperties.remove("supportsUWP");
            }

            setTargetFrameworkNuget("netstandard1.3");
            setSupportsAsync(Boolean.TRUE);
            setSupportsUWP(Boolean.FALSE);
            setNetStandard(Boolean.TRUE);

            //Tests not yet implemented for .NET Standard codegen
            //Todo implement it
            excludeTests = true;
        } else if (UWP.equals(this.targetFramework)) {
            LOGGER.warn("UWP support has been DEPRECATED in this generator. Please use `csharp-netcore` generator instead.");
            // TODO: NETSTANDARD here is misrepresenting a PCL v5.0 which supports .NET Framework 4.6+, .NET Core 1.0, and Windows Universal 10.0
            setTargetFrameworkNuget("uwp");
            setSupportsAsync(Boolean.TRUE);
            setSupportsUWP(Boolean.TRUE);
        } else if (NET40.equals(this.targetFramework)) {
            additionalProperties.put(MCS_NET_VERSION_KEY, "4");
            if (additionalProperties.containsKey(CodegenConstants.SUPPORTS_ASYNC)) {
                LOGGER.warn(".NET {} generator does not support async.", NET40);
                additionalProperties.remove(CodegenConstants.SUPPORTS_ASYNC);
            }

            setTargetFrameworkNuget("net40");
            setSupportsAsync(Boolean.FALSE);
        } else if (NET452.equals(this.targetFramework)) {
            additionalProperties.put(MCS_NET_VERSION_KEY, "4.5.2-api");
            setTargetFrameworkNuget("net452");
            setSupportsAsync(Boolean.TRUE);
        } else {
            additionalProperties.put(MCS_NET_VERSION_KEY, "4.5.2-api");
            setTargetFrameworkNuget("net45");
            setSupportsAsync(Boolean.TRUE);
        }

        if (additionalProperties.containsKey(CodegenConstants.GENERATE_PROPERTY_CHANGED)) {
            if (NET35.equals(targetFramework)) {
                LOGGER.warn("{} is only supported by generated code for .NET 4+.", CodegenConstants.GENERATE_PROPERTY_CHANGED);
                additionalProperties.remove(CodegenConstants.GENERATE_PROPERTY_CHANGED);
            } else if (NETSTANDARD.equals(targetFramework)) {
                LOGGER.warn("{} is not supported in .NET Standard generated code.", CodegenConstants.GENERATE_PROPERTY_CHANGED);
                additionalProperties.remove(CodegenConstants.GENERATE_PROPERTY_CHANGED);
            } else if (Boolean.TRUE.equals(netCoreProjectFileFlag)) {
                LOGGER.warn("{} is not supported in .NET Core csproj project format.", CodegenConstants.GENERATE_PROPERTY_CHANGED);
                additionalProperties.remove(CodegenConstants.GENERATE_PROPERTY_CHANGED);
            } else {
                setGeneratePropertyChanged(convertPropertyToBooleanAndWriteBack(CodegenConstants.GENERATE_PROPERTY_CHANGED));
            }
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put("clientPackage", clientPackage);

        additionalProperties.put(CodegenConstants.EXCLUDE_TESTS, excludeTests);
        additionalProperties.put(CodegenConstants.VALIDATABLE, this.validatable);
        additionalProperties.put(CodegenConstants.SUPPORTS_ASYNC, this.supportsAsync);
        additionalProperties.put("supportsUWP", this.supportsUWP);
        additionalProperties.put("netStandard", this.netStandard);
        additionalProperties.put("targetFrameworkNuget", this.targetFrameworkNuget);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_PROJECT_FILE, optionalProjectFileFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_PROJECT_GUID, packageGuid);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_METHOD_ARGUMENT)) {
            setOptionalMethodArgumentFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_METHOD_ARGUMENT));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_METHOD_ARGUMENT, optionalMethodArgumentFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_ASSEMBLY_INFO)) {
            setOptionalAssemblyInfoFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_ASSEMBLY_INFO));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_ASSEMBLY_INFO, optionalAssemblyInfoFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES)) {
            setOptionalEmitDefaultValuesFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, optionalEmitDefaultValuesFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            setNonPublicApi(convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API));
        } else {
            additionalProperties.put(CodegenConstants.NON_PUBLIC_API, isNonPublicApi());
        }

        final String testPackageName = testPackageName();
        String packageFolder = sourceFolder + File.separator + packageName;
        String clientPackageDir = packageFolder + File.separator + clientPackage;
        String testPackageFolder = testFolder + File.separator + testPackageName;

        additionalProperties.put("testPackageName", testPackageName);

        //Compute the relative path to the bin directory where the external assemblies live
        //This is necessary to properly generate the project file
        int packageDepth = packageFolder.length() - packageFolder.replace(java.io.File.separator, "").length();
        String binRelativePath = "..\\";
        for (int i = 0; i < packageDepth; i = i + 1)
            binRelativePath += "..\\";
        binRelativePath += "vendor";
        additionalProperties.put("binRelativePath", binRelativePath);

        supportingFiles.add(new SupportingFile("IApiAccessor.mustache",
                clientPackageDir, "IApiAccessor.cs"));
        supportingFiles.add(new SupportingFile("Configuration.mustache",
                clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));
        supportingFiles.add(new SupportingFile("ExceptionFactory.mustache",
                clientPackageDir, "ExceptionFactory.cs"));
        supportingFiles.add(new SupportingFile("OpenAPIDateConverter.mustache",
                clientPackageDir, "OpenAPIDateConverter.cs"));

        if (NET40.equals(this.targetFramework)) {
            // .net 4.0 doesn't include ReadOnlyDictionary…
            supportingFiles.add(new SupportingFile("ReadOnlyDictionary.mustache",
                    clientPackageDir, "ReadOnlyDictionary.cs"));
        }

        if (Boolean.FALSE.equals(this.netStandard) && Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("compile.mustache", "", "build.bat"));
            supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "build.sh"));

            // copy package.config to nuget's standard location for project-level installs
            supportingFiles.add(new SupportingFile("packages.config.mustache", packageFolder + File.separator, "packages.config"));
            // .travis.yml for travis-ci.org CI
            supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        } else if (Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("project.json.mustache", packageFolder + File.separator, "project.json"));
        }

        supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache",
                clientPackageDir, "IReadableConfiguration.cs"));
        supportingFiles.add(new SupportingFile("GlobalConfiguration.mustache",
                clientPackageDir, "GlobalConfiguration.cs"));

        // Only write out test related files if excludeTests is unset or explicitly set to false (see start of this method)
        if (Boolean.FALSE.equals(excludeTests)) {
            // shell script to run the nunit test
            supportingFiles.add(new SupportingFile("mono_nunit_test.mustache", "", "mono_nunit_test.sh"));

            modelTestTemplateFiles.put("model_test.mustache", ".cs");
            apiTestTemplateFiles.put("api_test.mustache", ".cs");

            if (Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
                supportingFiles.add(new SupportingFile("packages_test.config.mustache", testPackageFolder + File.separator, "packages.config"));
            }

            if (NET40.equals(this.targetFramework)) {
                // Include minimal tests for modifications made to JsonSubTypes, since code is quite different for .net 4.0 from original implementation
                supportingFiles.add(new SupportingFile("JsonSubTypesTests.mustache",
                        testPackageFolder + File.separator + "Client",
                        "JsonSubTypesTests.cs"));
            }
        }

        if (Boolean.TRUE.equals(generatePropertyChanged)) {
            supportingFiles.add(new SupportingFile("FodyWeavers.xml", packageFolder, "FodyWeavers.xml"));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (optionalAssemblyInfoFlag && Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));

            if (Boolean.TRUE.equals(this.netCoreProjectFileFlag)) {
                supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));
            } else {
                supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, packageName + ".csproj"));
                if (Boolean.FALSE.equals(this.netStandard)) {
                    supportingFiles.add(new SupportingFile("nuspec.mustache", packageFolder, packageName + ".nuspec"));
                }
            }

            if (Boolean.FALSE.equals(excludeTests)) {
                // NOTE: This exists here rather than previous excludeTests block because the test project is considered an optional project file.
                if (Boolean.TRUE.equals(this.netCoreProjectFileFlag)) {
                    supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
                } else {
                    supportingFiles.add(new SupportingFile("TestProject.mustache", testPackageFolder, testPackageName + ".csproj"));
                }
            }
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) ||
                "PascalCase".equals(naming) || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" +
                    naming + "'. Must be 'original', 'camelCase', " +
                    "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "csharp";
    }

    @Override
    public String getHelp() {
        return "Generates a CSharp client library.";
    }

    public void setOptionalAssemblyInfoFlag(boolean flag) {
        this.optionalAssemblyInfoFlag = flag;
    }

    public void setOptionalEmitDefaultValuesFlag(boolean flag) {
        this.optionalEmitDefaultValuesFlag = flag;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel codegenModel = super.fromModel(name, model);
        if (allDefinitions != null && codegenModel != null && codegenModel.parent != null) {
            final Schema parentModel = allDefinitions.get(toModelName(codegenModel.parent));
            if (parentModel != null) {
                final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
                if (codegenModel.hasEnums) {
                    codegenModel = this.reconcileInlineEnums(codegenModel, parentCodegenModel);
                }

                Map<String, CodegenProperty> propertyHash = new HashMap<>(codegenModel.vars.size());
                for (final CodegenProperty property : codegenModel.vars) {
                    propertyHash.put(property.name, property);
                }

                for (final CodegenProperty property : codegenModel.readWriteVars) {
                    if (property.defaultValue == null && parentCodegenModel.discriminator != null && property.name.equals(parentCodegenModel.discriminator.getPropertyName())) {
                        property.defaultValue = "\"" + name + "\"";
                    }
                }

                CodegenProperty last = null;
                for (final CodegenProperty property : parentCodegenModel.vars) {
                    // helper list of parentVars simplifies templating
                    if (!propertyHash.containsKey(property.name)) {
                        final CodegenProperty parentVar = property.clone();
                        parentVar.isInherited = true;
                        last = parentVar;
                        LOGGER.info("adding parent variable {}", property.name);
                        codegenModel.parentVars.add(parentVar);
                    }
                }
            }
        }

        // Cleanup possible duplicates. Currently, readWriteVars can contain the same property twice. May or may not be isolated to C#.
        if (codegenModel != null && codegenModel.readWriteVars != null && codegenModel.readWriteVars.size() > 1) {
            int length = codegenModel.readWriteVars.size() - 1;
            for (int i = length; i > (length / 2); i--) {
                final CodegenProperty codegenProperty = codegenModel.readWriteVars.get(i);
                // If the property at current index is found earlier in the list, remove this last instance.
                if (codegenModel.readWriteVars.indexOf(codegenProperty) < i) {
                    codegenModel.readWriteVars.remove(i);
                }
            }
        }

        return codegenModel;
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
        postProcessEmitDefaultValue(parameter.vendorExtensions);
        super.postProcessParameter(parameter);

        if (nullableType.contains(parameter.dataType)) {
            if (!parameter.required) { //optional
                parameter.dataType = parameter.dataType + "?";
            } else {
                parameter.vendorExtensions.put("x-csharp-value-type", true);
            }
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
        postProcessEmitDefaultValue(property.vendorExtensions);
        super.postProcessModelProperty(model, property);

        if (!property.isContainer && (nullableType.contains(property.dataType) || property.isEnum)) {
            property.vendorExtensions.put("x-csharp-value-type", true);
        }
    }

    /*
     * The pattern spec follows the Perl convention and style of modifiers. .NET
     * does not support this syntax directly so we need to convert the pattern to a .NET compatible
     * format and apply modifiers in a compatible way.
     * See https://msdn.microsoft.com/en-us/library/yd1hzczs(v=vs.110).aspx for .NET options.
     */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            int i = pattern.lastIndexOf('/');

            //Must follow Perl /pattern/modifiers convention
            if (pattern.charAt(0) != '/' || i < 2) {
                throw new IllegalArgumentException("Pattern must follow the Perl "
                        + "/pattern/modifiers convention. " + pattern + " is not valid.");
            }

            String regex = pattern.substring(1, i).replace("'", "\'");
            List<String> modifiers = new ArrayList<String>();

            // perl requires an explicit modifier to be culture specific and .NET is the reverse.
            modifiers.add("CultureInvariant");

            for (char c : pattern.substring(i).toCharArray()) {
                if (regexModifiers.containsKey(c)) {
                    String modifier = regexModifiers.get(c);
                    modifiers.add(modifier);
                } else if (c == 'l') {
                    modifiers.remove("CultureInvariant");
                }
            }

            vendorExtensions.put("x-regex", regex);
            vendorExtensions.put("x-modifiers", modifiers);
        }
    }

    public void postProcessEmitDefaultValue(Map<String, Object> vendorExtensions) {
        vendorExtensions.put("x-emit-default-value", optionalEmitDefaultValuesFlag);
    }

    public void setTargetFramework(String dotnetFramework) {
        if (!frameworks.containsKey(dotnetFramework)) {
            LOGGER.warn("Invalid .NET framework version, defaulting to {}", this.targetFramework);
        } else {
            this.targetFramework = dotnetFramework;
        }
        switch (targetFramework) {
            case NET35:
                additionalProperties.put(RESTSHARP, new LibraryDependency("105.1.0", "net35"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "net35"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "net35"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "net35"));
                break;
            case NET40:
                additionalProperties.put(RESTSHARP, new LibraryDependency("105.1.0", "net4"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "net40"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "net40"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "net40"));
                break;
            case NET45:
                additionalProperties.put(RESTSHARP, new LibraryDependency("105.1.0", "net45"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "net45"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "net45"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "net45"));
                break;
            case NET452:
                additionalProperties.put(RESTSHARP, new LibraryDependency("106.10.1", "net452"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "net45"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "net45"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "net45"));
                additionalProperties.put("isRestSharp_106_10_1_above", true);
                break;
            case UWP:
                additionalProperties.put(RESTSHARP, new LibraryDependency("105.1.0", "uwp"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "uwp"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "uwp"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "uwp"));
                break;
            case NETSTANDARD:
                additionalProperties.put(RESTSHARP, new LibraryDependency("105.1.0", "netstandard1.3"));
                additionalProperties.put(JSON_SUBTYPES, new LibraryDependency("1.6.0", "netstandard1.3"));
                additionalProperties.put(NEWTONSOFT_JSON, new LibraryDependency("12.0.3", "netstandard1.3"));
                additionalProperties.put(NUNIT, new LibraryDependency("3.11.0", "netstandard1.3"));
                break;
        }

        LOGGER.info("Generating code for .NET Framework {}", this.targetFramework);
    }

    private CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        if (parentCodegenModel.hasEnums) {

            // Get the properties for the parent and child models
            final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
            List<CodegenProperty> codegenProperties = codegenModel.vars;

            // Iterate over all of the parent model properties
            boolean removedChildEnum = false;
            for (CodegenProperty parentModelCodegenProperty : parentModelCodegenProperties) {
                // Look for enums
                if (parentModelCodegenProperty.isEnum) {
                    // Now that we have found an enum in the parent class,
                    // and search the child class for the same enum.
                    Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                    while (iterator.hasNext()) {
                        CodegenProperty codegenProperty = iterator.next();
                        if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenProperty)) {
                            // We found an enum in the child class that is
                            // a duplicate of the one in the parent, so remove it.
                            iterator.remove();
                            removedChildEnum = true;
                        }
                    }
                }
            }

            if (removedChildEnum) {
                codegenModel.vars = codegenProperties;
            }
        }

        return codegenModel;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return camelize(getSymbolName(value));
        }

        // number
        if (datatype.startsWith("int") || datatype.startsWith("long") ||
                datatype.startsWith("double") || datatype.startsWith("float")) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String var = value.replaceAll("_", " ");
        //var = WordUtils.capitalizeFully(var);
        var = camelize(var);
        var = var.replaceAll("\\W+", "");

        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }
    }

    @Override
    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    @Override
    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setTargetFrameworkNuget(String targetFrameworkNuget) {
        this.targetFrameworkNuget = targetFrameworkNuget;
    }

    public void setSupportsAsync(Boolean supportsAsync) {
        this.supportsAsync = supportsAsync;
    }

    public void setSupportsUWP(Boolean supportsUWP) {
        this.supportsUWP = supportsUWP;
    }

    public void setNetStandard(Boolean netStandard) {
        this.netStandard = netStandard;
    }

    public void setGeneratePropertyChanged(final Boolean generatePropertyChanged) {
        this.generatePropertyChanged = generatePropertyChanged;
        if (this.generatePropertyChanged) {
            additionalProperties.put(FODY, new LibraryDependency("1.29.4", targetFrameworkNuget));
            additionalProperties.put(PROPERTYCHANGED_FODY, new LibraryDependency("1.51.3", targetFrameworkNuget));
        }
    }

    public void setUseCompareNetObjects(final Boolean useCompareNetObjects) {
        this.useCompareNetObjects = useCompareNetObjects;
    }

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    public boolean isNonPublicApi() {
        return nonPublicApi;
    }

    public void setNonPublicApi(final boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
    }

    public void setValidatable(boolean validatable) {
        this.validatable = validatable;
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelFilename(name);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + apiPackage();
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + modelPackage();
    }

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        // To avoid unexpected behaviors when options are passed programmatically such as { "supportsAsync": "" }
        return super.processCompiler(compiler).emptyStringIsFalse(true);
    }

    /**
     * Return the instantiation type of the property, especially for map and array
     *
     * @param schema property schema
     * @return string presentation of the instantiation type of the property
     */
    @Override
    public String toInstantiationType(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            Schema additionalProperties = getAdditionalProperties(schema);
            String inner = getSchemaType(additionalProperties);
            if (ModelUtils.isMapSchema(additionalProperties)) {
                inner = toInstantiationType(additionalProperties);
            }
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arraySchema = (ArraySchema) schema;
            String inner = getSchemaType(arraySchema.getItems());
            return instantiationTypes.get("array") + "<" + inner + ">";
        } else {
            return null;
        }
    }

    @Override
    public String getNullableType(Schema p, String type) {
        if (languageSpecificPrimitives.contains(type)) {
            if (isSupportNullable() && ModelUtils.isNullable(p) && nullableType.contains(type)) {
                return type + "?";
            } else {
                return type;
            }
        } else {
            return null;
        }
    }

}

class LibraryDependency {
    String version;
    String targetFramework;

    public LibraryDependency(String version, String targetFramework) {
        this.version = version;
        this.targetFramework = targetFramework;
    }
}
