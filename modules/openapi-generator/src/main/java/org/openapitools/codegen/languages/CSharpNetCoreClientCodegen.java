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
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;
import static org.openapitools.codegen.CodegenDiscriminator.MappedModel;

@SuppressWarnings("Duplicates")
public class CSharpNetCoreClientCodegen extends AbstractCSharpCodegen {
    protected String apiName = "Api";

    // Defines the sdk option for targeted frameworks, which differs from targetFramework and targetFrameworkNuget
    protected static final String MCS_NET_VERSION_KEY = "x-mcs-sdk";
    protected static final String SUPPORTS_UWP = "supportsUWP";
    protected static final String SUPPORTS_RETRY = "supportsRetry";

    protected static final String NET_STANDARD = "netStandard";

    // HTTP libraries
    protected static final String RESTSHARP = "restsharp";
    protected static final String HTTPCLIENT = "httpclient";
    protected static final String GENERICHOST = "generichost";

    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_IDENTIFIER = "targetFrameworkIdentifier";
    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_VERSION = "targetFrameworkVersion";

    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final List<FrameworkStrategy> frameworkStrategies = Arrays.asList(
            FrameworkStrategy.NETSTANDARD_1_3,
            FrameworkStrategy.NETSTANDARD_1_4,
            FrameworkStrategy.NETSTANDARD_1_5,
            FrameworkStrategy.NETSTANDARD_1_6,
            FrameworkStrategy.NETSTANDARD_2_0,
            FrameworkStrategy.NETSTANDARD_2_1,
            FrameworkStrategy.NETCOREAPP_3_1,
            FrameworkStrategy.NETFRAMEWORK_4_7,
            FrameworkStrategy.NET_5_0,
            FrameworkStrategy.NET_6_0
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

    protected boolean supportsRetry = Boolean.TRUE;
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
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    protected boolean needsCustomHttpMethod = false;
    protected boolean needsUriBuilder = false;

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
        typeMapping.put("decimal", "decimal");
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

        addOption(CodegenConstants.API_NAME,
                "Must be a valid C# class name. Only used in Generic Host library. Default: " + this.apiName,
                this.apiName);

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

        CliOption disallowAdditionalPropertiesIfNotPresentOpt = CliOption.newBoolean(
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT,
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> disallowAdditionalPropertiesIfNotPresentOpts = new HashMap<>();
        disallowAdditionalPropertiesIfNotPresentOpts.put("false",
                "The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.");
        disallowAdditionalPropertiesIfNotPresentOpts.put("true",
                "Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.");
        disallowAdditionalPropertiesIfNotPresentOpt.setEnum(disallowAdditionalPropertiesIfNotPresentOpts);
        cliOptions.add(disallowAdditionalPropertiesIfNotPresentOpt);
        this.setDisallowAdditionalPropertiesIfNotPresent(true);

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
        addSwitch(CodegenConstants.NULLABLE_REFERENCE_TYPES,
                CodegenConstants.NULLABLE_REFERENCE_TYPES_DESC + " Starting in .NET 6.0 the default is true.",
                this.nullReferenceTypesFlag);

        addSwitch(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC,
                this.hideGenerationTimestamp);

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

        addSwitch(CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION,
                CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION_DESC,
                this.conditionalSerialization);

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

        addSwitch(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP,
                CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC,
                this.caseInsensitiveResponseHeaders);

        addSwitch(CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS,
                CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS_DESC,
                this.caseInsensitiveResponseHeaders);

        regexModifiers = new HashMap<>();
        regexModifiers.put('i', "IgnoreCase");
        regexModifiers.put('m', "Multiline");
        regexModifiers.put('s', "Singleline");
        regexModifiers.put('x', "IgnorePatternWhitespace");

        supportedLibraries.put(GENERICHOST, "HttpClient with Generic Host dependency injection (https://docs.microsoft.com/en-us/dotnet/core/extensions/generic-host) "
                + "(Experimental. Subject to breaking changes without notice.)");
        supportedLibraries.put(HTTPCLIENT, "HttpClient (https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient) "
                + "(Experimental. Subject to breaking changes without notice.)");
        supportedLibraries.put(RESTSHARP, "RestSharp (https://github.com/restsharp/RestSharp)");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "HTTP library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set RESTSHARP as the default
        libraryOption.setDefault(RESTSHARP);
        cliOptions.add(libraryOption);
        setLibrary(RESTSHARP);
    }

    @Override
    public String apiDocFileFolder() {
        if (GENERICHOST.equals(getLibrary())) {
            return (outputFolder + "/" + apiDocPath + File.separatorChar + "apis").replace('/', File.separatorChar);
        } else {
            return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
        }
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
                        LOGGER.debug("adding parent variable {}", property.name);
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

        // avoid breaking changes
        if (GENERICHOST.equals(getLibrary())) {
            Comparator<CodegenProperty> comparatorByDefaultValue = new Comparator<CodegenProperty>() {
                @Override
                public int compare(CodegenProperty one, CodegenProperty another) {
                    if (one.defaultValue == another.defaultValue)
                        return 0;
                    else if (Boolean.FALSE.equals(one.defaultValue))
                        return -1;
                    else
                        return 1;
                }
            };

            Comparator<CodegenProperty> comparatorByRequired = new Comparator<CodegenProperty>() {
                @Override
                public int compare(CodegenProperty one, CodegenProperty another) {
                    if (one.required == another.required)
                        return 0;
                    else if (Boolean.TRUE.equals(one.required))
                        return -1;
                    else
                        return 1;
                }
            };

            Collections.sort(codegenModel.vars, comparatorByDefaultValue);
            Collections.sort(codegenModel.vars, comparatorByRequired);
            Collections.sort(codegenModel.allVars, comparatorByDefaultValue);
            Collections.sort(codegenModel.allVars, comparatorByRequired);
            Collections.sort(codegenModel.readWriteVars, comparatorByDefaultValue);
            Collections.sort(codegenModel.readWriteVars, comparatorByRequired);
        }

        return codegenModel;
    }

    @Override
    public String getHelp() {
        return "Generates a C# client library (.NET Standard, .NET Core).";
    }

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
        if (GENERICHOST.equals(getLibrary())) {
            return (outputFolder + "/" + modelDocPath + File.separator + "models").replace('/', File.separatorChar);
        } else {
            return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
        }
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + modelPackage();
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
        postProcessEmitDefaultValue(property.vendorExtensions);

        if (GENERICHOST.equals(getLibrary())) {
            // all c# libraries should want this, but avoid breaking changes for now
            // a class cannot contain a property with the same name
            if (property.name.equals(model.classname)) {
                property.name = property.name + "Property";
            }
        }

        super.postProcessModelProperty(model, property);
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

            String regex = pattern.substring(1, i).replace("'", "\'").replace("\"", "\"\"");
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
        this.setLegacyDiscriminatorBehavior(false);

        super.processOpts();

        /*
         * NOTE: When supporting boolean additionalProperties, you should read the value and write it back as a boolean.
         * This avoids oddities where additionalProperties contains "false" rather than false, which will cause the
         * templating engine to behave unexpectedly.
         *
         * Use the pattern:
         *     if (additionalProperties.containsKey(prop)) convertPropertyToBooleanAndWriteBack(prop);
         */

        if (additionalProperties.containsKey(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT)) {
            this.setDisallowAdditionalPropertiesIfNotPresent(Boolean.parseBoolean(additionalProperties
                    .get(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES)) {
            setOptionalEmitDefaultValuesFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, optionalEmitDefaultValuesFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION)) {
            setConditionalSerialization(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION, conditionalSerialization);
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey((CodegenConstants.LICENSE_ID))) {
            setLicenseId((String) additionalProperties.get(CodegenConstants.LICENSE_ID));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_NAME)) {
            setApiName((String) additionalProperties.get(CodegenConstants.API_NAME));
        } else {
            additionalProperties.put(CodegenConstants.API_NAME, apiName);
        }

        if (isEmpty(apiPackage)) {
            setApiPackage("Api");
        }
        if (isEmpty(modelPackage)) {
            setModelPackage("Model");
        }

        clientPackage = "Client";

        if (GENERICHOST.equals(getLibrary())) {
            setLibrary(GENERICHOST);
            additionalProperties.put("useGenericHost", true);
            // all c# libraries should be doing this, but we only do it here to avoid breaking changes
            this.setSortModelPropertiesByRequiredFlag(true);
        } else if (RESTSHARP.equals(getLibrary())) {
            additionalProperties.put("useRestSharp", true);
            needsCustomHttpMethod = true;
        } else if (HTTPCLIENT.equals(getLibrary())) {
            setLibrary(HTTPCLIENT);
            additionalProperties.put("useHttpClient", true);
            needsUriBuilder = true;
        } else {
            throw new RuntimeException("Invalid HTTP library " + getLibrary() + ". Only restsharp, httpclient are supported.");
        }

        String inputFramework = (String) additionalProperties.getOrDefault(CodegenConstants.DOTNET_FRAMEWORK, defaultFramework.name);
        String[] frameworks;
        List<FrameworkStrategy> strategies = new ArrayList<>();

        if (inputFramework.contains(";")) {
            // multiple target framework
            frameworks = inputFramework.split(";");
            additionalProperties.put("multiTarget", true);
        } else {
            // just a single value
            frameworks = new String[]{inputFramework};
        }

        for (String framework : frameworks) {
            boolean strategyMatched = false;
            for (FrameworkStrategy frameworkStrategy : frameworkStrategies) {
                if (framework.equals(frameworkStrategy.name)) {
                    strategies.add(frameworkStrategy);
                    strategyMatched = true;
                }

                if (frameworkStrategy != FrameworkStrategy.NETSTANDARD_2_0 && "restsharp".equals(getLibrary())) {
                    LOGGER.warn("If using built-in templates, RestSharp only supports netstandard 2.0 or later.");
                }
            }

            if (!strategyMatched) {
                // throws exception if the input targetFramework is invalid
                throw new IllegalArgumentException("The input (" + inputFramework + ") contains Invalid .NET framework version: " +
                        framework + ". List of supported versions: " +
                        frameworkStrategies.stream()
                                .map(p -> p.name)
                                .collect(Collectors.joining(", ")));
            }
        }

        configureAdditionalPropertiesForFrameworks(additionalProperties, strategies);
        setTargetFrameworkNuget(strategies);
        setTargetFramework(strategies);
        setTestTargetFramework(strategies);

        setSupportsAsync(Boolean.TRUE);
        setNetStandard(strategies.stream().anyMatch(p -> Boolean.TRUE.equals(p.isNetStandard)));

        if (!netStandard) {
            setNetCoreProjectFileFlag(true);

            if (!additionalProperties.containsKey(CodegenConstants.NULLABLE_REFERENCE_TYPES) && !strategies.stream().anyMatch(s ->
                            s.equals(FrameworkStrategy.NETCOREAPP_3_1) ||
                            s.equals(FrameworkStrategy.NET_5_0) ||
                            s.equals(FrameworkStrategy.NETFRAMEWORK_4_7))) {
                // starting in .net 6.0, NRT is enabled by default. If not specified, lets enable NRT to match the framework's default
                setNullableReferenceTypes(true);
            }
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
        syncBooleanProperty(additionalProperties, SUPPORTS_RETRY, this::setSupportsRetry, this.supportsRetry);
        syncBooleanProperty(additionalProperties, CodegenConstants.OPTIONAL_METHOD_ARGUMENT, this::setOptionalMethodArgumentFlag, optionalMethodArgumentFlag);
        syncBooleanProperty(additionalProperties, CodegenConstants.NON_PUBLIC_API, this::setNonPublicApi, isNonPublicApi());
        syncBooleanProperty(additionalProperties, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, this::setUseOneOfDiscriminatorLookup, this.useOneOfDiscriminatorLookup);

        final String testPackageName = testPackageName();
        String packageFolder = sourceFolder + File.separator + packageName;
        String clientPackageDir = packageFolder + File.separator + clientPackage;
        String modelPackageDir = packageFolder + File.separator + modelPackage;
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

        // Only write out test related files if excludeTests is unset or explicitly set to false (see start of this method)
        if (Boolean.FALSE.equals(excludeTests.get())) {
            modelTestTemplateFiles.put("model_test.mustache", ".cs");
            apiTestTemplateFiles.put("api_test.mustache", ".cs");
        }

        if (HTTPCLIENT.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("FileParameter.mustache", clientPackageDir, "FileParameter.cs"));
            typeMapping.put("file", "FileParameter");
            addRestSharpSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir);
            additionalProperties.put("apiDocPath", apiDocPath);
            additionalProperties.put("modelDocPath", modelDocPath);
        } else if (GENERICHOST.equals(getLibrary())) {
            addGenericHostSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir);
            additionalProperties.put("apiDocPath", apiDocPath + File.separatorChar + "apis");
            additionalProperties.put("modelDocPath", modelDocPath + File.separatorChar + "models");
        } else {
            addRestSharpSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir);
            additionalProperties.put("apiDocPath", apiDocPath);
            additionalProperties.put("modelDocPath", modelDocPath);
        }

        addTestInstructions();
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (!GENERICHOST.equals(getLibrary())) {
            return op;
        }

        Collections.sort(op.allParams, new Comparator<CodegenParameter>() {
            @Override
            public int compare(CodegenParameter one, CodegenParameter another) {
                if (one.defaultValue == another.defaultValue)
                    return 0;
                else if (Boolean.FALSE.equals(one.defaultValue))
                    return -1;
                else
                    return 1;
            }
        });

        Collections.sort(op.allParams, new Comparator<CodegenParameter>() {
            @Override
            public int compare(CodegenParameter one, CodegenParameter another) {
                if (one.required == another.required)
                    return 0;
                else if (Boolean.TRUE.equals(one.required))
                    return -1;
                else
                    return 1;
            }
        });

        return op;
    }

    private void addTestInstructions() {
        if (GENERICHOST.equals(getLibrary())) {
            additionalProperties.put("testInstructions",
                    "/* *********************************************************************************" +
                            "\n*              Follow these manual steps to construct tests." +
                            "\n*              This file will not be overwritten." +
                            "\n*  *********************************************************************************" +
                            "\n* 1. Navigate to ApiTests.Base.cs and ensure any tokens are being created correctly." +
                            "\n*    Take care not to commit credentials to any repository." +
                            "\n*" +
                            "\n* 2. Mocking is coordinated by ApiTestsBase#AddApiHttpClients." +
                            "\n*    To mock the client, use the generic AddApiHttpClients." +
                            "\n*    To mock the server, change the client's BaseAddress." +
                            "\n*" +
                            "\n* 3. Locate the test you want below" +
                            "\n*      - remove the skip property from the Fact attribute" +
                            "\n*      - set the value of any variables if necessary" +
                            "\n*" +
                            "\n* 4. Run the tests and ensure they work." +
                            "\n*" +
                            "\n*/");
        }
    }

    public void addRestSharpSupportingFiles(final String clientPackageDir, final String packageFolder,
                                            final AtomicReference<Boolean> excludeTests, final String testPackageFolder, final String testPackageName, final String modelPackageDir) {
        supportingFiles.add(new SupportingFile("IApiAccessor.mustache", clientPackageDir, "IApiAccessor.cs"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache", clientPackageDir, "ApiResponse.cs"));
        supportingFiles.add(new SupportingFile("ExceptionFactory.mustache", clientPackageDir, "ExceptionFactory.cs"));
        supportingFiles.add(new SupportingFile("OpenAPIDateConverter.mustache", clientPackageDir, "OpenAPIDateConverter.cs"));
        supportingFiles.add(new SupportingFile("ClientUtils.mustache", clientPackageDir, "ClientUtils.cs"));
        if (needsCustomHttpMethod) {
            supportingFiles.add(new SupportingFile("HttpMethod.mustache", clientPackageDir, "HttpMethod.cs"));
        }
        if (needsUriBuilder) {
            supportingFiles.add(new SupportingFile("WebRequestPathBuilder.mustache", clientPackageDir, "WebRequestPathBuilder.cs"));
        }
        if (ProcessUtils.hasHttpSignatureMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("HttpSigningConfiguration.mustache", clientPackageDir, "HttpSigningConfiguration.cs"));
        }
        if (supportsAsync) {
            supportingFiles.add(new SupportingFile("IAsynchronousClient.mustache", clientPackageDir, "IAsynchronousClient.cs"));
        }
        supportingFiles.add(new SupportingFile("ISynchronousClient.mustache", clientPackageDir, "ISynchronousClient.cs"));
        supportingFiles.add(new SupportingFile("RequestOptions.mustache", clientPackageDir, "RequestOptions.cs"));
        supportingFiles.add(new SupportingFile("Multimap.mustache", clientPackageDir, "Multimap.cs"));

        if (supportsRetry) {
            supportingFiles.add(new SupportingFile("RetryConfiguration.mustache", clientPackageDir, "RetryConfiguration.cs"));
        }

        supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache",
                clientPackageDir, "IReadableConfiguration.cs"));
        supportingFiles.add(new SupportingFile("GlobalConfiguration.mustache",
                clientPackageDir, "GlobalConfiguration.cs"));

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));

        if (Boolean.FALSE.equals(excludeTests.get())) {
            supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
        }

        supportingFiles.add(new SupportingFile("appveyor.mustache", "", "appveyor.yml"));
        supportingFiles.add(new SupportingFile("AbstractOpenAPISchema.mustache", modelPackageDir, "AbstractOpenAPISchema.cs"));
    }

    public void addGenericHostSupportingFiles(final String clientPackageDir, final String packageFolder,
                                              final AtomicReference<Boolean> excludeTests, final String testPackageFolder, final String testPackageName, final String modelPackageDir) {
        supportingFiles.add(new SupportingFile("TokenProvider`1.mustache", clientPackageDir, "TokenProvider`1.cs"));
        supportingFiles.add(new SupportingFile("RateLimitProvider`1.mustache", clientPackageDir, "RateLimitProvider`1.cs"));
        supportingFiles.add(new SupportingFile("TokenContainer`1.mustache", clientPackageDir, "TokenContainer`1.cs"));
        supportingFiles.add(new SupportingFile("TokenBase.mustache", clientPackageDir, "TokenBase.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse`1.mustache", clientPackageDir, "ApiResponse`1.cs"));
        supportingFiles.add(new SupportingFile("ClientUtils.mustache", clientPackageDir, "ClientUtils.cs"));
        supportingFiles.add(new SupportingFile("HostConfiguration.mustache", clientPackageDir, "HostConfiguration.cs"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "docs" + File.separator + "scripts", "git_push.sh"));
        supportingFiles.add(new SupportingFile("git_push.ps1.mustache", "docs" + File.separator + "scripts", "git_push.ps1"));
        // TODO: supportingFiles.add(new SupportingFile("generate.ps1.mustache", "docs" + File.separator + "scripts", "generate.ps1"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));
        supportingFiles.add(new SupportingFile("appveyor.mustache", "", "appveyor.yml"));
        supportingFiles.add(new SupportingFile("OpenAPIDateConverter.mustache", clientPackageDir, "OpenAPIDateJsonConverter.cs"));
        supportingFiles.add(new SupportingFile("ApiResponseEventArgs.mustache", clientPackageDir, "ApiResponseEventArgs.cs"));
        supportingFiles.add(new SupportingFile("IApi.mustache", clientPackageDir, getInterfacePrefix() + "Api.cs"));

        String apiTestFolder = testFolder + File.separator + testPackageName() + File.separator + apiPackage();

        if (Boolean.FALSE.equals(excludeTests.get())) {
            supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
            supportingFiles.add(new SupportingFile("DependencyInjectionTests.mustache", apiTestFolder, "DependencyInjectionTests.cs"));

            // do not overwrite test file that already exists
            File apiTestsBaseFile = new File(apiTestFileFolder() + File.separator + "ApiTestsBase.cs");
            if (!apiTestsBaseFile.exists()) {
                supportingFiles.add(new SupportingFile("ApiTestsBase.mustache", apiTestFolder, "ApiTestsBase.cs"));
            }
        }

        if (ProcessUtils.hasHttpSignatureMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("HttpSigningConfiguration.mustache", clientPackageDir, "HttpSigningConfiguration.cs"));
            supportingFiles.add(new SupportingFile("HttpSigningToken.mustache", clientPackageDir, "HttpSigningToken.cs"));
        }

        if (ProcessUtils.hasHttpBasicMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("BasicToken.mustache", clientPackageDir, "BasicToken.cs"));
        }

        if (ProcessUtils.hasOAuthMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("OAuthToken.mustache", clientPackageDir, "OAuthToken.cs"));
        }

        if (ProcessUtils.hasHttpBearerMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("BearerToken.mustache", clientPackageDir, "BearerToken.cs"));
        }

        if (ProcessUtils.hasApiKeyMethods(openAPI)) {
            supportingFiles.add(new SupportingFile("ApiKeyToken.mustache", clientPackageDir, "ApiKeyToken.cs"));
        }
    }

    public void setNetStandard(Boolean netStandard) {
        this.netStandard = netStandard;
    }

    public void setOptionalAssemblyInfoFlag(boolean flag) {
        this.optionalAssemblyInfoFlag = flag;
    }

    public void setOptionalEmitDefaultValuesFlag(boolean flag) {
        this.optionalEmitDefaultValuesFlag = flag;
    }

    public void setConditionalSerialization(boolean flag) {
        this.conditionalSerialization = flag;
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    // TODO: this does the same as super
    @Override
    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    /**
     * Sets the api name. This value must be a valid class name.
     *
     * @param apiName The api name
     */
    public void setApiName(String apiName) {
        if (!"".equals(apiName) && (Boolean.FALSE.equals(apiName.matches("^[a-zA-Z0-9_]*$")) || Boolean.FALSE.equals(apiName.matches("^[a-zA-Z].*")))) {
            throw new RuntimeException("Invalid project name " + apiName + ". May only contain alphanumeric characters or underscore and start with a letter.");
        }
        this.apiName = apiName;
    }

    // TODO: this does the same as super
    @Override
    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setSupportsAsync(Boolean supportsAsync) {
        this.supportsAsync = supportsAsync;
    }

    public void setSupportsRetry(Boolean supportsRetry) {
        this.supportsRetry = supportsRetry;
    }

    public void setTargetFramework(String dotnetFramework) {
        if (!frameworks.containsKey(dotnetFramework)) {
            throw new IllegalArgumentException("Invalid .NET framework version: " +
                    dotnetFramework + ". List of supported versions: " +
                    frameworkStrategies.stream()
                            .map(p -> p.name)
                            .collect(Collectors.joining(", ")));
        } else {
            this.targetFramework = dotnetFramework;
        }
        LOGGER.info("Generating code for .NET Framework {}", this.targetFramework);
    }

    public void setTargetFramework(List<FrameworkStrategy> strategies) {
        for (FrameworkStrategy strategy : strategies) {
            if (!frameworks.containsKey(strategy.name)) {
                throw new IllegalArgumentException("Invalid .NET framework version: " +
                        strategy.name + ". List of supported versions: " +
                        frameworkStrategies.stream()
                                .map(p -> p.name)
                                .collect(Collectors.joining(", ")));
            }
        }
        this.targetFramework = strategies.stream().map(p -> p.name)
                .collect(Collectors.joining(";"));
        LOGGER.info("Generating code for .NET Framework {}", this.targetFramework);
    }

    public void setTestTargetFramework(String testTargetFramework) {
        this.testTargetFramework = testTargetFramework;
    }

    public void setTestTargetFramework(List<FrameworkStrategy> strategies) {
        this.testTargetFramework = strategies.stream().map(p -> p.testTargetFramework)
                .collect(Collectors.joining(";"));
    }

    public void setTargetFrameworkNuget(String targetFrameworkNuget) {
        this.targetFrameworkNuget = targetFrameworkNuget;
    }

    public void setTargetFrameworkNuget(List<FrameworkStrategy> strategies) {
        this.targetFrameworkNuget = strategies.stream().map(p -> p.getNugetFrameworkIdentifier())
                .collect(Collectors.joining(";"));
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

    @Override
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public void setPackageTags(String packageTags) {
        this.packageTags = packageTags;
    }

    public void setUseOneOfDiscriminatorLookup(boolean useOneOfDiscriminatorLookup) {
        this.useOneOfDiscriminatorLookup = useOneOfDiscriminatorLookup;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
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
        String var = value.replaceAll(" ", "_");
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

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        // for function names in the model, escape with the "Property" prefix
        if (propertySpecialKeywords.contains(name)) {
            return camelize("property_" + name);
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

        private final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);

        static FrameworkStrategy NETSTANDARD_1_3 = new FrameworkStrategy("netstandard1.3", ".NET Standard 1.3 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETSTANDARD_1_4 = new FrameworkStrategy("netstandard1.4", ".NET Standard 1.4 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETSTANDARD_1_5 = new FrameworkStrategy("netstandard1.5", ".NET Standard 1.5 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETSTANDARD_1_6 = new FrameworkStrategy("netstandard1.6", ".NET Standard 1.6 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETSTANDARD_2_0 = new FrameworkStrategy("netstandard2.0", ".NET Standard 2.0 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETSTANDARD_2_1 = new FrameworkStrategy("netstandard2.1", ".NET Standard 2.1 compatible", "netcoreapp3.1") {
        };
        static FrameworkStrategy NETCOREAPP_3_1 = new FrameworkStrategy("netcoreapp3.1", ".NET Core 3.1 compatible", "netcoreapp3.1", Boolean.FALSE) {
        };
        static FrameworkStrategy NETFRAMEWORK_4_7 = new FrameworkStrategy("net47", ".NET Framework 4.7 compatible", "net47", Boolean.FALSE) {
        };
        static FrameworkStrategy NET_5_0 = new FrameworkStrategy("net5.0", ".NET 5.0 compatible", "net5.0", Boolean.FALSE) {
        };
        static FrameworkStrategy NET_6_0 = new FrameworkStrategy("net6.0", ".NET 6.0 compatible", "net6.0", Boolean.FALSE) {
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
                LOGGER.warn(".NET {} generator does not support the UWP option. Use the csharp generator instead.",
                        this.name);
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

    protected void configureAdditionalPropertiesForFrameworks(final Map<String, Object> properties, List<FrameworkStrategy> strategies) {
        properties.putIfAbsent(CodegenConstants.DOTNET_FRAMEWORK, strategies.stream()
                .map(p -> p.name)
                .collect(Collectors.joining(";")));

        // not intended to be user-settable
        properties.put(TARGET_FRAMEWORK_IDENTIFIER, strategies.stream()
                .map(p -> p.getTargetFrameworkIdentifier())
                .collect(Collectors.joining(";")));
        properties.put(TARGET_FRAMEWORK_VERSION, strategies.stream()
                .map(p -> p.getTargetFrameworkVersion())
                .collect(Collectors.joining(";")));
        properties.putIfAbsent(MCS_NET_VERSION_KEY, "4.6-api");

        properties.put(NET_STANDARD, strategies.stream().anyMatch(p -> Boolean.TRUE.equals(p.isNetStandard)));
        for (FrameworkStrategy frameworkStrategy : frameworkStrategies) {
            properties.put(frameworkStrategy.name, strategies.stream().anyMatch(s -> s.name.equals(frameworkStrategy.name)));
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

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");

        // add implements for serializable/parcelable to all models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            if (cm.oneOf != null && !cm.oneOf.isEmpty() && cm.oneOf.contains("Null")) {
                // if oneOf contains "null" type
                cm.isNullable = true;
                cm.oneOf.remove("Null");
            }

            if (cm.anyOf != null && !cm.anyOf.isEmpty() && cm.anyOf.contains("Null")) {
                // if anyOf contains "null" type
                cm.isNullable = true;
                cm.anyOf.remove("Null");
            }

            for (CodegenProperty cp : cm.readWriteVars) {
                // ISSUE: https://github.com/OpenAPITools/openapi-generator/issues/11844
                // allVars may not have all properties
                // see modules\openapi-generator\src\test\resources\3_0\allOf.yaml
                // property boosterSeat will be in readWriteVars but not allVars
                // the property is present in the model but gets removed at CodegenModel#removeDuplicatedProperty
                if (Boolean.FALSE.equals(cm.allVars.stream().anyMatch(v -> v.baseName.equals(cp.baseName)))) {
                    LOGGER.debug("Property " + cp.baseName + " was found in readWriteVars but not in allVars. Adding it back to allVars");
                    cm.allVars.add(cp);
                }
            }

            for (CodegenProperty cp : cm.allVars) {
                // ISSUE: https://github.com/OpenAPITools/openapi-generator/issues/11845
                // some properties do not have isInherited set correctly
                // see modules\openapi-generator\src\test\resources\3_0\allOf.yaml
                // Child properties Type, LastName, FirstName will have isInherited set to false when it should be true
                if (cp.isInherited){
                    continue;
                }
                if (Boolean.TRUE.equals(cm.parentVars.stream().anyMatch(v -> v.baseName.equals(cp.baseName) && v.dataType.equals(cp.dataType)))) {
                    LOGGER.debug("Property " + cp.baseName + " was found in the parentVars but not marked as inherited.");
                    cp.isInherited = true;
                }
            }
        }

        return objs;
    }

    /**
    * ISSUE: https://github.com/OpenAPITools/openapi-generator/issues/11846
    * Ensures that a model has all inherited properties
    * Check modules\openapi-generator\src\test\resources\3_0\java\petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml
    * Without this method, property petType in GrandparentAnimal will not make it through ParentPet and into ChildCat
    */
    private void EnsureInheritedVariablesArePresent(CodegenModel derivedModel) {
        // every c# generator should definetly want this, or we should fix the issue
        // still, lets avoid breaking changes :(
        if (Boolean.FALSE.equals(GENERICHOST.equals(getLibrary()))){
            return;
        }

        if (derivedModel.parentModel == null){
            return;
        }

        for (CodegenProperty parentProperty : derivedModel.parentModel.allVars){
            if (Boolean.FALSE.equals(derivedModel.allVars.stream().anyMatch(v -> v.baseName.equals(parentProperty.baseName)))) {
                CodegenProperty clone = parentProperty.clone();
                clone.isInherited = true;
                LOGGER.debug("Inherited property " + clone.name + " from model" + derivedModel.parentModel.classname + " was not found in " + derivedModel.classname + ". Adding a clone now.");
                derivedModel.allVars.add(clone);
            }
        }

        EnsureInheritedVariablesArePresent(derivedModel.parentModel);
    }

    /**
     * Invoked by {@link DefaultGenerator} after all models have been post-processed, allowing for a last pass of codegen-specific model cleanup.
     *
     * @param objs Current state of codegen object model.
     * @return An in-place modified state of the codegen object model.
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);

        // other libraries probably want these fixes, but lets avoid breaking changes for now
        if (Boolean.FALSE.equals(GENERICHOST.equals(getLibrary()))){
            return objs;
        }

        ArrayList<CodegenModel> allModels = new ArrayList<CodegenModel>();
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), objs);
            allModels.add(model);
        }

        for (CodegenModel cm : allModels) {
            if (cm.parent != null){
                // remove the parent CodegenProperty from the model
                // we need it gone so we can use allOf/oneOf/anyOf in the constructor
                cm.allOf.removeIf(item -> item.equals(cm.parent));
                cm.oneOf.removeIf(item -> item.equals(cm.parent));
                cm.anyOf.removeIf(item -> item.equals(cm.parent));
            }

            cm.anyOf.forEach(anyOf -> removePropertiesDeclaredInComposedClass(anyOf, allModels, cm));
            cm.oneOf.forEach(oneOf -> removePropertiesDeclaredInComposedClass(oneOf, allModels, cm));
            cm.allOf.forEach(allOf -> removePropertiesDeclaredInComposedClass(allOf, allModels, cm));

            EnsureInheritedVariablesArePresent(cm);
        }

        return objs;
    }

    /**
     * Removes properties from a model which are also defined in a composed class.
     *
     * @param className The name which may be a composed model
     * @param allModels A collection of all CodegenModel
     * @param cm The CodegenModel to correct
     */
    private void removePropertiesDeclaredInComposedClass(String className, List<CodegenModel> allModels, CodegenModel cm) {
        CodegenModel otherModel = allModels.stream().filter(m -> m.classname.equals(className)).findFirst().orElse(null);
        if (otherModel == null){
            return;
        }

        otherModel.readWriteVars.stream().filter(v -> cm.readWriteVars.stream().anyMatch(cmV -> cmV.baseName.equals(v.baseName))).collect(Collectors.toList())
            .forEach(v -> {
                cm.readWriteVars.removeIf(item -> item.baseName.equals(v.baseName));
                cm.vars.removeIf(item -> item.baseName.equals(v.baseName));
                cm.readOnlyVars.removeIf(item -> item.baseName.equals(v.baseName));
                cm.requiredVars.removeIf(item -> item.baseName.equals(v.baseName));
                cm.allVars.removeIf(item -> item.baseName.equals(v.baseName));
            });
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator's contributed by Jim Schubert (https://github.com/jimschubert)#");
        System.out.println("# Please support his work directly via https://patreon.com/jimschubert \uD83D\uDE4F      #");
        System.out.println("################################################################################");
    }

    @Override
    protected void updateModelForObject(CodegenModel m, Schema schema) {
        /**
         * we have a custom version of this function so we only set isMap to true if
         * ModelUtils.isMapSchema
         * In other generators, isMap is true for all type object schemas
         */
        if (schema.getProperties() != null || schema.getRequired() != null && !(schema instanceof ComposedSchema)) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        if (ModelUtils.isMapSchema(schema)) {
            // an object or anyType composed schema that has additionalProperties set
            addAdditionPropertiesToCodeGenModel(m, schema);
        } else {
            m.setIsMap(false);
            if (ModelUtils.isFreeFormObject(openAPI, schema)) {
                // non-composed object type with no properties + additionalProperties
                // additionalProperties must be null, ObjectSchema, or empty Schema
                addAdditionPropertiesToCodeGenModel(m, schema);
            }
        }
        // process 'additionalProperties'
        setAddProps(schema, m);
    }
}
