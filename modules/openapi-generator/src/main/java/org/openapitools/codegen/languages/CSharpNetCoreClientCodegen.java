/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

@SuppressWarnings("Duplicates")
public class CSharpNetCoreClientCodegen extends AbstractCSharpCodegen {
    // Defines the sdk option for targeted frameworks, which differs from targetFramework and targetFrameworkNuget
    protected static final String MCS_NET_VERSION_KEY = "x-mcs-sdk";
    protected static final String SUPPORTS_UWP = "supportsUWP";

    protected static final String NET_STANDARD = "netStandard";

    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_IDENTIFIER = "targetFrameworkIdentifier";
    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_VERSION = "targetFrameworkVersion";

    @SuppressWarnings({"hiding"})
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final List<FrameworkStrategy> frameworkStrategies = Arrays.asList(
            FrameworkStrategy.NETSTANDARD_1_3,
            FrameworkStrategy.NETSTANDARD_1_4,
            FrameworkStrategy.NETSTANDARD_1_5,
            FrameworkStrategy.NETSTANDARD_1_6,
            FrameworkStrategy.NETSTANDARD_2_0,
            FrameworkStrategy.NETSTANDARD_2_1,
            FrameworkStrategy.NETCOREAPP_2_0,
            FrameworkStrategy.NETCOREAPP_2_1,
            FrameworkStrategy.NETCOREAPP_3_0,
            FrameworkStrategy.NETCOREAPP_3_1
    );
    private static FrameworkStrategy defaultFramework = FrameworkStrategy.NETSTANDARD_2_0;
    protected final Map<String, String> frameworks;
    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
    protected String clientPackage = "Org.OpenAPITools.Client";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    // Defines TargetFrameworkVersion in csproj files
    protected String targetFramework = defaultFramework.name;
    protected String testTargetFramework = defaultFramework.testTargetFramework;

    // Defines nuget identifiers for target framework
    protected String targetFrameworkNuget = targetFramework;

    protected boolean supportsAsync = Boolean.TRUE;
    protected boolean netStandard = Boolean.FALSE;

    protected boolean validatable = Boolean.TRUE;
    protected Map<Character, String> regexModifiers;
    // By default, generated code is considered public
    protected boolean nonPublicApi = Boolean.FALSE;

    protected boolean caseInsensitiveResponseHeaders = Boolean.FALSE;
    protected String releaseNote = "Minor update";
    protected String licenseId;
    protected String packageTags;

    public CSharpNetCoreClientCodegen() {
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
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        // mapped non-nullable type without ?
        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("boolean", "bool");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long");
        typeMapping.put("double", "double");
        typeMapping.put("number", "decimal");
        typeMapping.put("BigDecimal", "decimal");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");
        typeMapping.put("UUID", "Guid");
        typeMapping.put("URI", "string");
        typeMapping.put("AnyType", "Object");

        setSupportNullable(Boolean.TRUE);
        hideGenerationTimestamp = Boolean.TRUE;
        supportsInheritance = true;
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "csharp-netcore";

        cliOptions.clear();

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

        addOption(CodegenConstants.LICENSE_ID,
                CodegenConstants.LICENSE_ID_DESC,
                this.licenseId);

        addOption(CodegenConstants.RELEASE_NOTE,
                CodegenConstants.RELEASE_NOTE_DESC,
                this.releaseNote);

        addOption(CodegenConstants.PACKAGE_TAGS,
                CodegenConstants.PACKAGE_TAGS_DESC,
                this.packageTags);

        CliOption framework = new CliOption(
                CodegenConstants.DOTNET_FRAMEWORK,
                CodegenConstants.DOTNET_FRAMEWORK_DESC
        );

        ImmutableMap.Builder<String, String> frameworkBuilder = new ImmutableMap.Builder<>();
        for (FrameworkStrategy frameworkStrategy : frameworkStrategies) {
            frameworkBuilder.put(frameworkStrategy.name, frameworkStrategy.description);
        }

        frameworks = frameworkBuilder.build();

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

        addSwitch(CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS,
                CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS_DESC,
                this.caseInsensitiveResponseHeaders);

        regexModifiers = new HashMap<>();
        regexModifiers.put('i', "IgnoreCase");
        regexModifiers.put('m', "Multiline");
        regexModifiers.put('s', "Singleline");
        regexModifiers.put('x', "IgnorePatternWhitespace");
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + apiPackage();
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
                    if (property.defaultValue == null && parentCodegenModel.discriminator != null && property.baseName.equals(parentCodegenModel.discriminator.getPropertyName())) {
                        property.defaultValue = "\"" + name + "\"";
                    }
                }

                CodegenProperty last = null;
                for (final CodegenProperty property : parentCodegenModel.vars) {
                    // helper list of parentVars simplifies templating
                    if (!propertyHash.containsKey(property.name)) {
                        final CodegenProperty parentVar = property.clone();
                        parentVar.isInherited = true;
                        parentVar.hasMore = true;
                        last = parentVar;
                        LOGGER.debug("adding parent variable {}", property.name);
                        codegenModel.parentVars.add(parentVar);
                    }
                }

                if (last != null) {
                    last.hasMore = false;
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

    @Override
    public String getHelp() {
        return "Generates a C# client library (.NET Standard, .NET Core).";
    }

//    private void syncStringProperty(Map<String, Object> properties, String key)

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
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

    @Override
    public String getName() {
        return "csharp-netcore";
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

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public boolean isNonPublicApi() {
        return nonPublicApi;
    }

    public void setNonPublicApi(final boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + modelPackage();
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
        postProcessEmitDefaultValue(property.vendorExtensions);

        super.postProcessModelProperty(model, property);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.returnType != null) {
                        operation.returnContainer = operation.returnType;
                        if (this.returnICollection && (
                                operation.returnType.startsWith("List") ||
                                        operation.returnType.startsWith("Collection"))) {
                            // NOTE: ICollection works for both List<T> and Collection<T>
                            int genericStart = operation.returnType.indexOf("<");
                            if (genericStart > 0) {
                                operation.returnType = "ICollection" + operation.returnType.substring(genericStart);
                            }
                        }
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
        postProcessEmitDefaultValue(parameter.vendorExtensions);
        super.postProcessParameter(parameter);
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

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        // To avoid unexpected behaviors when options are passed programmatically such as { "supportsAsync": "" }
        return super.processCompiler(compiler).emptyStringIsFalse(true);
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

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES)) {
            setOptionalEmitDefaultValuesFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, optionalEmitDefaultValuesFlag);
        }


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

        String framework = (String) additionalProperties.getOrDefault(CodegenConstants.DOTNET_FRAMEWORK, defaultFramework.name);
        FrameworkStrategy strategy = defaultFramework;
        for (FrameworkStrategy frameworkStrategy : frameworkStrategies) {
            if (framework.equals(frameworkStrategy.name)) {
                strategy = frameworkStrategy;
            }
        }

        strategy.configureAdditionalProperties(additionalProperties);

        setTargetFrameworkNuget(strategy.getNugetFrameworkIdentifier());
        setTargetFramework(strategy.name);
        setTestTargetFramework(strategy.testTargetFramework);

        if (strategy != FrameworkStrategy.NETSTANDARD_2_0) {
            LOGGER.warn("If using built-in templates-RestSharp only supports netstandard 2.0 or later.");
        }

        setSupportsAsync(Boolean.TRUE);
        setNetStandard(strategy.isNetStandard);

        if (!strategy.isNetStandard) {
            setNetCoreProjectFileFlag(true);
        }

        if (additionalProperties.containsKey(CodegenConstants.GENERATE_PROPERTY_CHANGED)) {
            LOGGER.warn(CodegenConstants.GENERATE_PROPERTY_CHANGED + " is not supported in the .NET Standard generator.");
            additionalProperties.remove(CodegenConstants.GENERATE_PROPERTY_CHANGED);
        }

        final AtomicReference<Boolean> excludeTests = new AtomicReference<>();
        syncBooleanProperty(additionalProperties, CodegenConstants.EXCLUDE_TESTS, excludeTests::set, false);

        syncStringProperty(additionalProperties, "clientPackage", (s) -> { }, clientPackage);

        syncStringProperty(additionalProperties, CodegenConstants.API_PACKAGE, this::setApiPackage, apiPackage);
        syncStringProperty(additionalProperties, CodegenConstants.MODEL_PACKAGE, this::setModelPackage, modelPackage);
        syncStringProperty(additionalProperties, CodegenConstants.OPTIONAL_PROJECT_GUID, this::setPackageGuid, packageGuid);
        syncStringProperty(additionalProperties, "targetFrameworkNuget", this::setTargetFrameworkNuget, this.targetFrameworkNuget);
        syncStringProperty(additionalProperties, "testTargetFramework", this::setTestTargetFramework, this.testTargetFramework);

        syncBooleanProperty(additionalProperties, "netStandard", this::setNetStandard, this.netStandard);

        syncBooleanProperty(additionalProperties, CodegenConstants.VALIDATABLE, this::setValidatable, this.validatable);
        syncBooleanProperty(additionalProperties, CodegenConstants.SUPPORTS_ASYNC, this::setSupportsAsync, this.supportsAsync);
        syncBooleanProperty(additionalProperties, CodegenConstants.OPTIONAL_METHOD_ARGUMENT, this::setOptionalMethodArgumentFlag, optionalMethodArgumentFlag);
        syncBooleanProperty(additionalProperties, CodegenConstants.NON_PUBLIC_API, this::setNonPublicApi, isNonPublicApi());

        final String testPackageName = testPackageName();
        String packageFolder = sourceFolder + File.separator + packageName;
        String clientPackageDir = packageFolder + File.separator + clientPackage;
        String testPackageFolder = testFolder + File.separator + testPackageName;

        additionalProperties.put("testPackageName", testPackageName);

        //Compute the relative path to the bin directory where the external assemblies live
        //This is necessary to properly generate the project file
        int packageDepth = packageFolder.length() - packageFolder.replace(java.io.File.separator, "").length();
        String binRelativePath = "..\\";
        for (int i = 0; i < packageDepth; i = i + 1) {
            binRelativePath += "..\\";
        }
        binRelativePath += "vendor";
        additionalProperties.put("binRelativePath", binRelativePath);

        supportingFiles.add(new SupportingFile("IApiAccessor.mustache", clientPackageDir, "IApiAccessor.cs"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache", clientPackageDir, "ApiResponse.cs"));
        supportingFiles.add(new SupportingFile("ExceptionFactory.mustache", clientPackageDir, "ExceptionFactory.cs"));
        supportingFiles.add(new SupportingFile("OpenAPIDateConverter.mustache", clientPackageDir, "OpenAPIDateConverter.cs"));
        supportingFiles.add(new SupportingFile("ClientUtils.mustache", clientPackageDir, "ClientUtils.cs"));
        supportingFiles.add(new SupportingFile("HttpMethod.mustache", clientPackageDir, "HttpMethod.cs"));
        supportingFiles.add(new SupportingFile("IAsynchronousClient.mustache", clientPackageDir, "IAsynchronousClient.cs"));
        supportingFiles.add(new SupportingFile("ISynchronousClient.mustache", clientPackageDir, "ISynchronousClient.cs"));
        supportingFiles.add(new SupportingFile("RequestOptions.mustache", clientPackageDir, "RequestOptions.cs"));
        supportingFiles.add(new SupportingFile("Multimap.mustache", clientPackageDir, "Multimap.cs"));

        supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache",
                clientPackageDir, "IReadableConfiguration.cs"));
        supportingFiles.add(new SupportingFile("GlobalConfiguration.mustache",
                clientPackageDir, "GlobalConfiguration.cs"));

        // Only write out test related files if excludeTests is unset or explicitly set to false (see start of this method)
        if (Boolean.FALSE.equals(excludeTests.get())) {
            modelTestTemplateFiles.put("model_test.mustache", ".cs");
            apiTestTemplateFiles.put("api_test.mustache", ".cs");
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));

        if (Boolean.FALSE.equals(excludeTests.get())) {
            supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    public void setNetStandard(Boolean netStandard) {
        this.netStandard = netStandard;
    }

    public void setOptionalAssemblyInfoFlag(boolean flag) {
        this.optionalAssemblyInfoFlag = flag;
    }

    public void setOptionalEmitDefaultValuesFlag(boolean flag){
        this.optionalEmitDefaultValuesFlag = flag;
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setSupportsAsync(Boolean supportsAsync) {
        this.supportsAsync = supportsAsync;
    }

    public void setTargetFramework(String dotnetFramework) {
        if (!frameworks.containsKey(dotnetFramework)) {
            LOGGER.warn("Invalid .NET framework version, defaulting to " + this.targetFramework);
        } else {
            this.targetFramework = dotnetFramework;
        }
        LOGGER.info("Generating code for .NET Framework " + this.targetFramework);
    }

    public void setTestTargetFramework(String testTargetFramework) {
        this.testTargetFramework = testTargetFramework;
    }

    public void setTargetFrameworkNuget(String targetFrameworkNuget) {
        this.targetFrameworkNuget = targetFrameworkNuget;
    }

    public void setValidatable(boolean validatable) {
        this.validatable = validatable;
    }

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    public void setLicenseId(String licenseId) {
        this.licenseId = licenseId;
    }

    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public void setPackageTags(String packageTags) {
        this.packageTags = packageTags;
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
    public String toModelDocFilename(String name) {
        return toModelFilename(name);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, do nothing
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
            for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
                // Look for enums
                if (parentModelCodegenPropery.isEnum) {
                    // Now that we have found an enum in the parent class,
                    // and search the child class for the same enum.
                    Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                    while (iterator.hasNext()) {
                        CodegenProperty codegenProperty = iterator.next();
                        if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
                            // We found an enum in the child class that is
                            // a duplicate of the one in the parent, so remove it.
                            iterator.remove();
                            removedChildEnum = true;
                        }
                    }
                }
            }

            if (removedChildEnum) {
                // If we removed an entry from this model's vars, we need to ensure hasMore is updated
                int count = 0, numVars = codegenProperties.size();
                for (CodegenProperty codegenProperty : codegenProperties) {
                    count += 1;
                    codegenProperty.hasMore = count < numVars;
                }
                codegenModel.vars = codegenProperties;
            }
        }

        return codegenModel;
    }

    private void syncBooleanProperty(final Map<String, Object> additionalProperties, final String key, final Consumer<Boolean> setter, final Boolean defaultValue) {
        if (additionalProperties.containsKey(key)) {
            setter.accept(convertPropertyToBooleanAndWriteBack(key));
        } else {
            additionalProperties.put(key, defaultValue);
            setter.accept(defaultValue);
        }
    }

    private void syncStringProperty(final Map<String, Object> additionalProperties, final String key, final Consumer<String> setter, final String defaultValue) {
        if (additionalProperties.containsKey(key)) {
            setter.accept((String) additionalProperties.get(key));
        } else {
            additionalProperties.put(key, defaultValue);
            setter.accept(defaultValue);
        }
    }

    // https://docs.microsoft.com/en-us/dotnet/standard/net-standard
    @SuppressWarnings("Duplicates")
    private static abstract class FrameworkStrategy {
        static FrameworkStrategy NETSTANDARD_1_3 = new FrameworkStrategy("netstandard1.3", ".NET Standard 1.3 compatible", "netcoreapp2.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_4 = new FrameworkStrategy("netstandard1.4", ".NET Standard 1.4 compatible", "netcoreapp2.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_5 = new FrameworkStrategy("netstandard1.5", ".NET Standard 1.5 compatible", "netcoreapp2.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_6 = new FrameworkStrategy("netstandard1.6", ".NET Standard 1.6 compatible", "netcoreapp2.0") {
        };
        static FrameworkStrategy NETSTANDARD_2_0 = new FrameworkStrategy("netstandard2.0", ".NET Standard 2.0 compatible", "netcoreapp2.0") {
        };
        static FrameworkStrategy NETSTANDARD_2_1 = new FrameworkStrategy("netstandard2.1", ".NET Standard 2.1 compatible", "netcoreapp3.0") {
        };
        static FrameworkStrategy NETCOREAPP_2_0 = new FrameworkStrategy("netcoreapp2.0", ".NET Core 2.0 compatible", "netcoreapp2.0", Boolean.FALSE) {
        };
        static FrameworkStrategy NETCOREAPP_2_1 = new FrameworkStrategy("netcoreapp2.1", ".NET Core 2.1 compatible", "netcoreapp2.1", Boolean.FALSE) {
        };
        static FrameworkStrategy NETCOREAPP_3_0 = new FrameworkStrategy("netcoreapp3.0", ".NET Core 3.0 compatible", "netcoreapp3.0", Boolean.FALSE) {
        };
        static FrameworkStrategy NETCOREAPP_3_1 = new FrameworkStrategy("netcoreapp3.1", ".NET Core 3.1 compatible", "netcoreapp3.1", Boolean.FALSE) {
        };
        protected String name;
        protected String description;
        protected String testTargetFramework;
        private Boolean isNetStandard = Boolean.TRUE;

        FrameworkStrategy(String name, String description, String testTargetFramework) {
            this.name = name;
            this.description = description;
            this.testTargetFramework = testTargetFramework;
        }

        FrameworkStrategy(String name, String description, String testTargetFramework, Boolean isNetStandard) {
            this.name = name;
            this.description = description;
            this.testTargetFramework = testTargetFramework;
            this.isNetStandard = isNetStandard;
        }

        protected void configureAdditionalProperties(final Map<String, Object> properties) {
            properties.putIfAbsent(CodegenConstants.DOTNET_FRAMEWORK, this.name);

            // not intended to be user-settable
            properties.put(TARGET_FRAMEWORK_IDENTIFIER, this.getTargetFrameworkIdentifier());
            properties.put(TARGET_FRAMEWORK_VERSION, this.getTargetFrameworkVersion());
            properties.putIfAbsent(MCS_NET_VERSION_KEY, "4.6-api");

            properties.put(NET_STANDARD, this.isNetStandard);
            if (properties.containsKey(SUPPORTS_UWP)) {
                LOGGER.warn(".NET " + this.name + " generator does not support the UWP option. Use the csharp generator instead.");
                properties.remove(SUPPORTS_UWP);
            }
        }

        protected String getNugetFrameworkIdentifier() {
            return this.name.toLowerCase(Locale.ROOT);
        }

        protected String getTargetFrameworkIdentifier() {
            if (this.isNetStandard) return ".NETStandard";
            else return ".NETCoreApp";
        }

        protected String getTargetFrameworkVersion() {
            if (this.isNetStandard) return "v" + this.name.replace("netstandard", "");
            else return "v" + this.name.replace("netcoreapp", "");
        }
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
}
