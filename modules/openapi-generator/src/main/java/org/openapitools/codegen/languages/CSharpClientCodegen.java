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
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

@SuppressWarnings("Duplicates")
public class CSharpClientCodegen extends AbstractCSharpCodegen {
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
    protected static final String UNITY_WEB_REQUEST = "unityWebRequest";

    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_IDENTIFIER = "targetFrameworkIdentifier";
    // Project Variable, determined from target framework. Not intended to be user-settable.
    protected static final String TARGET_FRAMEWORK_VERSION = "targetFrameworkVersion";

    protected static final String NET_STANDARD_14_OR_LATER = "netstandard14OrLater";
    protected static final String NET_STANDARD_15_OR_LATER = "netstandard15OrLater";
    protected static final String NET_STANDARD_16_OR_LATER = "netstandard16OrLater";
    protected static final String NET_STANDARD_20_OR_LATER = "netstandard20OrLater";
    protected static final String NET_STANDARD_21_OR_LATER = "netstandard21OrLater";
    protected static final String NET_47_OR_LATER = "net47OrLater";
    protected static final String NET_48_OR_LATER = "net48OrLater";
    protected static final String NET_60_OR_LATER = "net60OrLater";
    protected static final String NET_70_OR_LATER = "net70OrLater";
    protected static final String NET_80_OR_LATER = "net80OrLater";
    protected static final String NET_90_OR_LATER = "net90OrLater";

    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final List<FrameworkStrategy> frameworkStrategies = Arrays.asList(
            FrameworkStrategy.NETSTANDARD_1_3,
            FrameworkStrategy.NETSTANDARD_1_4,
            FrameworkStrategy.NETSTANDARD_1_5,
            FrameworkStrategy.NETSTANDARD_1_6,
            FrameworkStrategy.NETSTANDARD_2_0,
            FrameworkStrategy.NETSTANDARD_2_1,
            FrameworkStrategy.NETFRAMEWORK_4_7,
            FrameworkStrategy.NETFRAMEWORK_4_8,
            FrameworkStrategy.NET_8_0,
            FrameworkStrategy.NET_9_0
    );
    private static FrameworkStrategy latestFramework = frameworkStrategies.get(frameworkStrategies.size() - 1);
    protected final Map<String, String> frameworks;
    @Setter protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
    @Setter protected String clientPackage = "Client";
    protected String authFolder = "Auth";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    // Defines TargetFrameworkVersion in csproj files
    protected String targetFramework = latestFramework.name;
    protected String testTargetFramework = latestFramework.testTargetFramework;

    // Defines nuget identifiers for target framework
    protected String targetFrameworkNuget = targetFramework;

    protected boolean supportsRetry = Boolean.TRUE;
    protected boolean supportsAsync = Boolean.TRUE;
    protected boolean useVirtualForHooks = Boolean.FALSE;
    protected boolean netStandard = Boolean.FALSE;
    protected boolean supportsFileParameters = Boolean.TRUE;
    protected boolean supportsDateOnly = Boolean.FALSE;
    protected boolean useIntForTimeout = Boolean.FALSE;

    @Setter protected boolean validatable = Boolean.TRUE;
    @Setter protected boolean equatable = Boolean.FALSE;
    // By default, generated code is considered public
    @Getter @Setter
    protected boolean nonPublicApi = Boolean.FALSE;

    private static final String OPERATION_PARAMETER_SORTING_KEY = "operationParameterSorting";
    private static final String MODEL_PROPERTY_SORTING_KEY = "modelPropertySorting";
    private static final String USE_INT_FOR_TIMEOUT = "useIntForTimeout";

    enum SortingMethod {
        DEFAULT,
        ALPHABETICAL,
        LEGACY
    }

    private SortingMethod operationParameterSorting = SortingMethod.DEFAULT;
    private SortingMethod modelPropertySorting = SortingMethod.DEFAULT;

    protected boolean caseInsensitiveResponseHeaders = Boolean.FALSE;
    protected String releaseNote = "Minor update";
    @Setter protected String licenseId;
    @Setter protected String packageTags;
    @Setter protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    protected boolean needsCustomHttpMethod = false;
    protected boolean needsUriBuilder = false;

    public CSharpClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.SignatureAuth
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

        setSupportNullable(Boolean.TRUE);
        hideGenerationTimestamp = Boolean.TRUE;
        supportsInheritance = true;
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "csharp";

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

        addOption(DATE_FORMAT,
                "The default Date format (only `generichost` library supports this option).",
                this.dateFormat);

        addOption(DATETIME_FORMAT,
                "The default DateTime format (only `generichost` library supports this option).",
                this.dateTimeFormat);

        addOption("zeroBasedEnums",
                "Enumerations with string values will start from 0 when true, 1 when false. If not set, enumerations with string values will start from 0 if the first value is 'unknown', case insensitive.",
                null);

        addOption(CSharpClientCodegen.OPERATION_PARAMETER_SORTING_KEY,
                "One of legacy, alphabetical, default.",
                this.operationParameterSorting.toString().toLowerCase(Locale.ROOT));

        addOption(CSharpClientCodegen.MODEL_PROPERTY_SORTING_KEY,
                "One of legacy, alphabetical, default.",
                this.modelPropertySorting.toString().toLowerCase(Locale.ROOT));

        addOption(CSharpClientCodegen.USE_INT_FOR_TIMEOUT,
                "Use int for Timeout (fall back to v7.9.0 templates). This option (for restsharp only) will be deprecated so please migrated to TimeSpan instead.",
                String.valueOf(this.useIntForTimeout));

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

        addSwitch(CodegenConstants.USE_DATETIME_FOR_DATE,
                CodegenConstants.USE_DATETIME_FOR_DATE_DESC,
                useDateTimeForDateFlag);

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
                this.useOneOfDiscriminatorLookup);

        addSwitch(CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS,
                CodegenConstants.CASE_INSENSITIVE_RESPONSE_HEADERS_DESC,
                this.caseInsensitiveResponseHeaders);

        addSwitch(CodegenConstants.EQUATABLE,
                CodegenConstants.EQUATABLE_DESC,
                this.equatable);

        addSwitch("useSourceGeneration",
                "Use source generation where available (only `generichost` library supports this option).",
                this.getUseSourceGeneration());

        addSwitch(CodegenConstants.USE_VIRTUAL_FOR_HOOKS,
                CodegenConstants.USE_VIRTUAL_FOR_HOOKS_DESC,
                this.useVirtualForHooks);

        supportedLibraries.put(GENERICHOST, "HttpClient, Generic Host integration, and System.Text.Json (https://docs.microsoft.com/en-us/dotnet/core/extensions/generic-host)");
        supportedLibraries.put(HTTPCLIENT, "HttpClient and Newtonsoft (https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient) "
                + "(Experimental. Subject to breaking changes without notice.)");
        supportedLibraries.put(UNITY_WEB_REQUEST, "UnityWebRequest (...) "
                + "(Experimental. Subject to breaking changes without notice.)");
        supportedLibraries.put(RESTSHARP, "RestSharp (https://github.com/restsharp/RestSharp)");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "HTTP library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set GENERICHOST as the default
        libraryOption.setDefault(GENERICHOST);
        cliOptions.add(libraryOption);
        setLibrary(GENERICHOST);
    }

    @Deprecated
    @Override
    protected Set<String> getNullableTypes() {
        return GENERICHOST.equals(getLibrary())
                ? super.getNullableTypes()
                : new HashSet<>(Arrays.asList("decimal", "bool", "int", "uint", "long", "ulong", "float", "double", "DateTime", "DateOnly", "DateTimeOffset", "Guid"));
    }

    @Override
    protected Set<String> getValueTypes() {
        return GENERICHOST.equals(getLibrary())
                ? super.getValueTypes()
                : new HashSet<>(Arrays.asList("decimal", "bool", "int", "uint", "long", "ulong", "float", "double"));
    }

    @Override
    protected boolean useNet60OrLater() {
        return additionalProperties.containsKey(NET_60_OR_LATER);
    }

    @Override
    protected void setTypeMapping() {
        super.setTypeMapping();

        if (HTTPCLIENT.equals(getLibrary())) {
            typeMapping.put("file", "FileParameter");
        }
    }

    @Override
    protected void updateCodegenParameterEnum(CodegenParameter parameter, CodegenModel model) {
        if (GENERICHOST.equals(getLibrary())) {
            super.updateCodegenParameterEnum(parameter, model);
            return;
        }

        super.updateCodegenParameterEnumLegacy(parameter, model);

        if (!parameter.required && parameter.vendorExtensions.get("x-csharp-value-type") != null) { //optional
            parameter.dataType = parameter.dataType + "?";
        }
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
        setEnumDiscriminatorDefaultValue(codegenModel);
        if (allDefinitions != null && codegenModel != null && codegenModel.parent != null) {
            final Schema<?> parentModel = allDefinitions.get(toModelName(codegenModel.parent));
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

                for (final CodegenProperty property : parentCodegenModel.vars) {
                    // helper list of parentVars simplifies templating
                    if (!propertyHash.containsKey(property.name)) {
                        final CodegenProperty parentVar = property.clone();
                        parentVar.isInherited = true;
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

        if (codegenModel != null) {
            if (this.modelPropertySorting == SortingMethod.ALPHABETICAL) {
                Collections.sort(codegenModel.vars, propertyComparatorByName);
                Collections.sort(codegenModel.allVars, propertyComparatorByName);
                Collections.sort(codegenModel.requiredVars, propertyComparatorByName);
                Collections.sort(codegenModel.optionalVars, propertyComparatorByName);
                Collections.sort(codegenModel.readOnlyVars, propertyComparatorByName);
                Collections.sort(codegenModel.readWriteVars, propertyComparatorByName);
                Collections.sort(codegenModel.parentVars, propertyComparatorByName);
            }

            if (GENERICHOST.equals(getLibrary())) {

                if (this.modelPropertySorting == SortingMethod.LEGACY) {
                    Collections.sort(codegenModel.vars, propertyComparatorByName);
                    Collections.sort(codegenModel.allVars, propertyComparatorByName);
                    Collections.sort(codegenModel.requiredVars, propertyComparatorByName);
                    Collections.sort(codegenModel.optionalVars, propertyComparatorByName);
                    Collections.sort(codegenModel.readOnlyVars, propertyComparatorByName);
                    Collections.sort(codegenModel.readWriteVars, propertyComparatorByName);
                    Collections.sort(codegenModel.parentVars, propertyComparatorByName);

                    Collections.sort(codegenModel.vars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.allVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.requiredVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.optionalVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.readOnlyVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.readWriteVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                    Collections.sort(codegenModel.parentVars, propertyComparatorByNotNullableRequiredNoDefaultLegacy);
                } else {
                    Collections.sort(codegenModel.vars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.allVars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.requiredVars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.optionalVars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.readOnlyVars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.readWriteVars, propertyComparatorByNotNullableRequiredNoDefault);
                    Collections.sort(codegenModel.parentVars, propertyComparatorByNotNullableRequiredNoDefault);
                }
            }
        } else {
            SortModelPropertiesByRequiredFlag(codegenModel);
        }

        return codegenModel;
    }

    public static Comparator<CodegenProperty> propertyComparatorByName = new Comparator<CodegenProperty>() {
        @Override
        public int compare(CodegenProperty one, CodegenProperty another) {
            return one.name.compareTo(another.name);
        }
    };

    public static Comparator<CodegenProperty> propertyComparatorByNotNullableRequiredNoDefaultLegacy = new Comparator<CodegenProperty>() {
        @Override
        public int compare(CodegenProperty one, CodegenProperty another) {
            if (one.isNullable == another.isNullable && one.required == another.required && (one.defaultValue == null) == (another.defaultValue == null))
                return 0;
            else if (!one.isNullable && one.required && one.defaultValue == null)
                return -1;
            else
                return 1;
        }
    };

    public static Comparator<CodegenProperty> propertyComparatorByNotNullableRequiredNoDefault =
            Comparator.comparing(p -> p.isNullable || !p.required || p.defaultValue != null);

    public static Comparator<CodegenParameter> parameterComparatorByDataType = new Comparator<CodegenParameter>() {
        @Override
        public int compare(CodegenParameter one, CodegenParameter another) {
            return one.dataType.compareTo(another.dataType);
        }
    };

    public static Comparator<CodegenParameter> parameterComparatorByName = new Comparator<CodegenParameter>() {
        @Override
        public int compare(CodegenParameter one, CodegenParameter another) {
            return one.paramName.compareTo(another.paramName);
        }
    };

    public static Comparator<CodegenParameter> parameterComparatorByNotNullableRequiredNoDefault =
            Comparator.comparing(p -> p.isNullable || !p.required || p.defaultValue != null);

    public static Comparator<CodegenParameter> parameterComparatorByDefaultValue = new Comparator<CodegenParameter>() {
        @Override
        public int compare(CodegenParameter one, CodegenParameter another) {
            if ((one.defaultValue == null) == (another.defaultValue == null))
                return 0;
            else if (one.defaultValue == null)
                return -1;
            else
                return 1;
        }
    };

    public static Comparator<CodegenParameter> parameterComparatorByRequired = new Comparator<CodegenParameter>() {
        @Override
        public int compare(CodegenParameter one, CodegenParameter another) {
            if (one.required == another.required)
                return 0;
            else if (Boolean.TRUE.equals(one.required))
                return -1;
            else
                return 1;
        }
    };

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
        return "csharp";
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, LOWERCASE_FIRST_LETTER);
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
        if (GENERICHOST.equals(getLibrary())) {
            return super.getNullableType(p, type);
        }

        if (languageSpecificPrimitives.contains(type)) {
            if (isSupportNullable() && ModelUtils.isNullable(p) && this.getNullableTypes().contains(type)) {
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

        super.postProcessModelProperty(model, property);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        postProcessEmitDefaultValue(parameter.vendorExtensions);

        if (!GENERICHOST.equals(getLibrary()) && !parameter.dataType.endsWith("?") && !parameter.required && (nullReferenceTypesFlag || this.getNullableTypes().contains(parameter.dataType))) {
            parameter.dataType = parameter.dataType + "?";
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

        final String library = getLibrary();

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

        if (additionalProperties.containsKey(CSharpClientCodegen.OPERATION_PARAMETER_SORTING_KEY)) {
            setOperationParameterSorting((String) additionalProperties.get(CSharpClientCodegen.OPERATION_PARAMETER_SORTING_KEY));
        }

        if (additionalProperties.containsKey(CSharpClientCodegen.MODEL_PROPERTY_SORTING_KEY)) {
            setModelPropertySorting((String) additionalProperties.get(CSharpClientCodegen.MODEL_PROPERTY_SORTING_KEY));
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

        final Map<String, Runnable> libraryActions = Map.of(
                GENERICHOST, () -> {
                    setLibrary(GENERICHOST);
                    additionalProperties.put("useGenericHost", true);
                },
                RESTSHARP, () -> {
                    additionalProperties.put("useRestSharp", true);
                    needsCustomHttpMethod = true;
                },
                HTTPCLIENT, () -> {
                    setLibrary(HTTPCLIENT);
                    additionalProperties.put("useHttpClient", true);
                    needsUriBuilder = true;
                },
                UNITY_WEB_REQUEST, () -> {
                    setLibrary(UNITY_WEB_REQUEST);
                    additionalProperties.put("useUnityWebRequest", true);
                    needsUriBuilder = true;
                }
        );
        final Runnable action = libraryActions.get(library);
        if (action != null) {
            action.run();
        } else {
            String supportedLibraries = String.join(", ", libraryActions.keySet());
            throw new RuntimeException("Invalid HTTP library " + library + ". Only " + supportedLibraries + " are supported.");
        }

        final String inputFramework = (String) additionalProperties.getOrDefault(CodegenConstants.DOTNET_FRAMEWORK, latestFramework.name);
        final String[] frameworks;
        final List<FrameworkStrategy> strategies = new ArrayList<>();

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

                if (frameworkStrategy != FrameworkStrategy.NETSTANDARD_2_0 && RESTSHARP.equals(library)) {
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
                    s.equals(FrameworkStrategy.NETFRAMEWORK_4_8) ||
                            s.equals(FrameworkStrategy.NETFRAMEWORK_4_7))) {
                // starting in .net 6.0, NRT is enabled by default. If not specified, lets enable NRT to match the framework's default
                setNullableReferenceTypes(true);
            }
        }

        final AtomicReference<Boolean> excludeTests = new AtomicReference<>();
        syncBooleanProperty(additionalProperties, CodegenConstants.EXCLUDE_TESTS, excludeTests::set, false);

        syncStringProperty(additionalProperties, "clientPackage", this::setClientPackage, clientPackage);

        syncStringProperty(additionalProperties, CodegenConstants.API_PACKAGE, this::setApiPackage, apiPackage);
        syncStringProperty(additionalProperties, CodegenConstants.MODEL_PACKAGE, this::setModelPackage, modelPackage);
        syncStringProperty(additionalProperties, CodegenConstants.OPTIONAL_PROJECT_GUID, this::setPackageGuid, packageGuid);
        syncStringProperty(additionalProperties, "targetFrameworkNuget", this::setTargetFrameworkNuget, this.targetFrameworkNuget);
        syncStringProperty(additionalProperties, "testTargetFramework", this::setTestTargetFramework, this.testTargetFramework);

        syncBooleanProperty(additionalProperties, "netStandard", this::setNetStandard, this.netStandard);

        syncBooleanProperty(additionalProperties, CodegenConstants.EQUATABLE, this::setEquatable, this.equatable);
        syncBooleanProperty(additionalProperties, CodegenConstants.VALIDATABLE, this::setValidatable, this.validatable);
        syncBooleanProperty(additionalProperties, CodegenConstants.SUPPORTS_ASYNC, this::setSupportsAsync, this.supportsAsync);
        syncBooleanProperty(additionalProperties, CodegenConstants.USE_VIRTUAL_FOR_HOOKS, this::setUseVirtualForHooks, this.useVirtualForHooks);
        syncBooleanProperty(additionalProperties, SUPPORTS_RETRY, this::setSupportsRetry, this.supportsRetry);
        syncBooleanProperty(additionalProperties, CodegenConstants.OPTIONAL_METHOD_ARGUMENT, this::setOptionalMethodArgumentFlag, optionalMethodArgumentFlag);
        syncBooleanProperty(additionalProperties, CodegenConstants.NON_PUBLIC_API, this::setNonPublicApi, isNonPublicApi());
        syncBooleanProperty(additionalProperties, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, this::setUseOneOfDiscriminatorLookup, this.useOneOfDiscriminatorLookup);
        syncBooleanProperty(additionalProperties, "supportsFileParameters", this::setSupportsFileParameters, this.supportsFileParameters);
        syncBooleanProperty(additionalProperties, "useSourceGeneration", this::setUseSourceGeneration, this.useSourceGeneration);
        syncBooleanProperty(additionalProperties, "supportsDateOnly", this::setSupportsDateOnly, this.supportsDateOnly);
        syncBooleanProperty(additionalProperties, "useIntForTimeout", this::setUseIntForTimeout, this.useIntForTimeout);

        final String testPackageName = testPackageName();
        String packageFolder = sourceFolder + File.separator + packageName;
        String clientPackageDir = packageFolder + File.separator + clientPackage;
        String modelPackageDir = packageFolder + File.separator + modelPackage;
        String authPackageDir = clientPackageDir + File.separator + authFolder;
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

        switch (library) {
            case RESTSHARP:
                addSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir, authPackageDir);
                additionalProperties.put("apiDocPath", apiDocPath);
                additionalProperties.put("modelDocPath", modelDocPath);

                if (ProcessUtils.hasOAuthMethods(openAPI)) {
                    supportingFiles.add(new SupportingFile("auth/OAuthAuthenticator.mustache", authPackageDir, "OAuthAuthenticator.cs"));
                    supportingFiles.add(new SupportingFile("auth/TokenResponse.mustache", authPackageDir, "TokenResponse.cs"));
                    supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authPackageDir, "OAuthFlow.cs"));
                }
                break;
            case HTTPCLIENT:
                supportingFiles.add(new SupportingFile("FileParameter.mustache", clientPackageDir, "FileParameter.cs"));
                addSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir, authPackageDir);
                additionalProperties.put("apiDocPath", apiDocPath);
                additionalProperties.put("modelDocPath", modelDocPath);
                break;
            case UNITY_WEB_REQUEST:
                additionalProperties.put(CodegenConstants.VALIDATABLE, false);
                setValidatable(false);
                setSupportsRetry(false);
                setSupportsAsync(true);
                // Some consoles and tvOS do not support either Application.persistentDataPath or will refuse to
                // compile/link if you even reference GetTempPath as well.
                additionalProperties.put("supportsFileParameters", false);
                setSupportsFileParameters(false);

                addSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir, authPackageDir);
                supportingFiles.add(new SupportingFile("ConnectionException.mustache", clientPackageDir, "ConnectionException.cs"));
                supportingFiles.add(new SupportingFile("UnexpectedResponseException.mustache", clientPackageDir, "UnexpectedResponseException.cs"));
                supportingFiles.add(new SupportingFile("UnityWebRequestAwaiterExtension.mustache", clientPackageDir, "UnityWebRequestAwaiterExtension.cs"));
                break;
            default: // generichost
                addGenericHostSupportingFiles(clientPackageDir, packageFolder, excludeTests, testPackageFolder, testPackageName, modelPackageDir);
                additionalProperties.put("apiDocPath", apiDocPath + File.separatorChar + "apis");
                additionalProperties.put("modelDocPath", modelDocPath + File.separatorChar + "models");
                break;
        }

        if (useDateOnly()) {
            setSupportsDateOnly(true);
            additionalProperties.put("supportsDateOnly", true);
        }
        // include the spec in the output
        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));

        this.setTypeMapping();
    }

    @Override
    public void setUseSourceGeneration(final Boolean useSourceGeneration) {
        if (useSourceGeneration && !this.additionalProperties.containsKey(NET_70_OR_LATER)) {
            throw new RuntimeException("Source generation is only compatible with .Net 7 or later.");
        }
        this.useSourceGeneration = useSourceGeneration;
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (this.operationParameterSorting == SortingMethod.ALPHABETICAL) {
            Collections.sort(op.allParams, parameterComparatorByName);
            Collections.sort(op.bodyParams, parameterComparatorByName);
            Collections.sort(op.pathParams, parameterComparatorByName);
            Collections.sort(op.queryParams, parameterComparatorByName);
            Collections.sort(op.headerParams, parameterComparatorByName);
            Collections.sort(op.implicitHeadersParams, parameterComparatorByName);
            Collections.sort(op.formParams, parameterComparatorByName);
            Collections.sort(op.cookieParams, parameterComparatorByName);
            Collections.sort(op.requiredParams, parameterComparatorByName);
            Collections.sort(op.optionalParams, parameterComparatorByName);
            Collections.sort(op.notNullableParams, parameterComparatorByName);
        }

        if (GENERICHOST.equals(getLibrary())) {
            if (this.operationParameterSorting == SortingMethod.LEGACY) {
                Collections.sort(op.allParams, parameterComparatorByDataType);
                Collections.sort(op.bodyParams, parameterComparatorByDataType);
                Collections.sort(op.pathParams, parameterComparatorByDataType);
                Collections.sort(op.queryParams, parameterComparatorByDataType);
                Collections.sort(op.headerParams, parameterComparatorByDataType);
                Collections.sort(op.implicitHeadersParams, parameterComparatorByDataType);
                Collections.sort(op.formParams, parameterComparatorByDataType);
                Collections.sort(op.cookieParams, parameterComparatorByDataType);
                Collections.sort(op.requiredParams, parameterComparatorByDataType);
                Collections.sort(op.optionalParams, parameterComparatorByDataType);
                Collections.sort(op.notNullableParams, parameterComparatorByDataType);

                Comparator<CodegenParameter> comparator = parameterComparatorByRequired.thenComparing(parameterComparatorByDefaultValue);
                Collections.sort(op.allParams, comparator);
                Collections.sort(op.bodyParams, comparator);
                Collections.sort(op.pathParams, comparator);
                Collections.sort(op.queryParams, comparator);
                Collections.sort(op.headerParams, comparator);
                Collections.sort(op.implicitHeadersParams, comparator);
                Collections.sort(op.formParams, comparator);
                Collections.sort(op.cookieParams, comparator);
                Collections.sort(op.requiredParams, comparator);
                Collections.sort(op.optionalParams, comparator);
                Collections.sort(op.notNullableParams, comparator);
            } else {
                Collections.sort(op.allParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.bodyParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.pathParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.queryParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.headerParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.implicitHeadersParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.formParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.cookieParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.requiredParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.optionalParams, parameterComparatorByNotNullableRequiredNoDefault);
                Collections.sort(op.notNullableParams, parameterComparatorByNotNullableRequiredNoDefault);
            }
        } else {
            SortParametersByRequiredFlag(op.allParams);
        }

        return op;
    }

    public void addSupportingFiles(final String clientPackageDir, final String packageFolder,
                                   final AtomicReference<Boolean> excludeTests, final String testPackageFolder, final String testPackageName, final String modelPackageDir, final String authPackageDir) {
        final String library = getLibrary();

        if (RESTSHARP.equals(library)) { // restsharp
            if (useIntForTimeout) { // option to fall back to int for Timeout using v7.9.0 template
                supportingFiles.add(new SupportingFile("ApiClient.v790.mustache", clientPackageDir, "ApiClient.cs"));
                supportingFiles.add(new SupportingFile("IReadableConfiguration.v790.mustache", clientPackageDir, "IReadableConfiguration.cs"));
                supportingFiles.add(new SupportingFile("Configuration.v790.mustache", clientPackageDir, "Configuration.cs"));
            } else {
                supportingFiles.add(new SupportingFile("ApiClient.mustache", clientPackageDir, "ApiClient.cs"));
                supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache", clientPackageDir, "IReadableConfiguration.cs"));
                supportingFiles.add(new SupportingFile("Configuration.mustache", clientPackageDir, "Configuration.cs"));
            }
        } else { // other libs, e.g. httpclient
            supportingFiles.add(new SupportingFile("ApiClient.mustache", clientPackageDir, "ApiClient.cs"));
            supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache", clientPackageDir, "IReadableConfiguration.cs"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", clientPackageDir, "Configuration.cs"));
        }
        supportingFiles.add(new SupportingFile("IApiAccessor.mustache", clientPackageDir, "IApiAccessor.cs"));
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

        supportingFiles.add(new SupportingFile("GlobalConfiguration.mustache",
                clientPackageDir, "GlobalConfiguration.cs"));

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (UNITY_WEB_REQUEST.equals(library)) {
            supportingFiles.add(new SupportingFile("asmdef.mustache", packageFolder, packageName + ".asmdef"));
        } else {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));
        }

        if (Boolean.FALSE.equals(excludeTests.get())) {
            if (UNITY_WEB_REQUEST.equals(library)) {
                supportingFiles.add(new SupportingFile("asmdef_test.mustache", testPackageFolder, testPackageName + ".asmdef"));
            } else {
                supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
            }
        }

        if (!UNITY_WEB_REQUEST.equals(library)) {
            supportingFiles.add(new SupportingFile("appveyor.mustache", "", "appveyor.yml"));
        }
        supportingFiles.add(new SupportingFile("AbstractOpenAPISchema.mustache", modelPackageDir, "AbstractOpenAPISchema.cs"));
    }

    public void addGenericHostSupportingFiles(final String clientPackageDir, final String packageDir,
                                              final AtomicReference<Boolean> excludeTests, final String testPackageDir, final String testPackageName, final String modelPackageDir) {
        supportingFiles.add(new SupportingFile("README.test.mustache", testPackageDir, "README.md"));

        supportingFiles.add(new SupportingFile("README.solution.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("appveyor.mustache", "", "appveyor.yml"));

        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "docs" + File.separator + "scripts", "git_push.sh"));
        supportingFiles.add(new SupportingFile("git_push.ps1.mustache", "docs" + File.separator + "scripts", "git_push.ps1"));
        // TODO: supportingFiles.add(new SupportingFile("generate.ps1.mustache", "docs" + File.separator + "scripts", "generate.ps1"));

        supportingFiles.add(new SupportingFile("netcore_project.mustache", packageDir, packageName + ".csproj"));
        supportingFiles.add(new SupportingFile("README.client.mustache", packageDir, "README.md"));

        // client directory
        supportingFiles.add(new SupportingFile("TokenProvider`1.mustache", clientPackageDir, "TokenProvider`1.cs"));
        supportingFiles.add(new SupportingFile("RateLimitProvider`1.mustache", clientPackageDir, "RateLimitProvider`1.cs"));
        supportingFiles.add(new SupportingFile("TokenContainer`1.mustache", clientPackageDir, "TokenContainer`1.cs"));
        supportingFiles.add(new SupportingFile("TokenBase.mustache", clientPackageDir, "TokenBase.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse`1.mustache", clientPackageDir, "ApiResponse`1.cs"));
        supportingFiles.add(new SupportingFile("ClientUtils.mustache", clientPackageDir, "ClientUtils.cs"));
        supportingFiles.add(new SupportingFile("HostConfiguration.mustache", clientPackageDir, "HostConfiguration.cs"));
        supportingFiles.add(new SupportingFile("ApiFactory.mustache", clientPackageDir, "ApiFactory.cs"));
        supportingFiles.add(new SupportingFile("DateTimeJsonConverter.mustache", clientPackageDir, "DateTimeJsonConverter.cs"));
        supportingFiles.add(new SupportingFile("DateTimeNullableJsonConverter.mustache", clientPackageDir, "DateTimeNullableJsonConverter.cs"));
        if (useDateOnly()) {
            supportingFiles.add(new SupportingFile("DateOnlyJsonConverter.mustache", clientPackageDir, "DateOnlyJsonConverter.cs"));
            supportingFiles.add(new SupportingFile("DateOnlyNullableJsonConverter.mustache", clientPackageDir, "DateOnlyNullableJsonConverter.cs"));
        }
        supportingFiles.add(new SupportingFile("ApiResponseEventArgs`1.mustache", clientPackageDir, "ApiResponseEventArgs.cs"));
        supportingFiles.add(new SupportingFile("ExceptionEventArgs.mustache", clientPackageDir, "ExceptionEventArgs.cs"));
        supportingFiles.add(new SupportingFile("JsonSerializerOptionsProvider.mustache", clientPackageDir, "JsonSerializerOptionsProvider.cs"));
        supportingFiles.add(new SupportingFile("CookieContainer.mustache", clientPackageDir, "CookieContainer.cs"));
        supportingFiles.add(new SupportingFile("Option.mustache", clientPackageDir, "Option.cs"));

        supportingFiles.add(new SupportingFile("IApi.mustache", sourceFolder + File.separator + packageName + File.separator + apiPackage(), getInterfacePrefix() + "Api.cs"));

        // extensions
        String extensionsFolder = sourceFolder + File.separator + packageName + File.separator + "Extensions";
        supportingFiles.add(new SupportingFile("IHttpClientBuilderExtensions.mustache", extensionsFolder, "IHttpClientBuilderExtensions.cs"));
        supportingFiles.add(new SupportingFile("IHostBuilderExtensions.mustache", extensionsFolder, "IHostBuilderExtensions.cs"));
        supportingFiles.add(new SupportingFile("IServiceCollectionExtensions.mustache", extensionsFolder, "IServiceCollectionExtensions.cs"));

        String apiTestFolder = testFolder + File.separator + testPackageName() + File.separator + apiPackage();
        if (Boolean.FALSE.equals(excludeTests.get())) {
            supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageDir, testPackageName + ".csproj"));
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

    public void setOperationParameterSorting(String operationParameterSorting) {
        if (operationParameterSorting == null) {
            operationParameterSorting = "DEFAULT";
        }

        this.operationParameterSorting = SortingMethod.valueOf(operationParameterSorting.toUpperCase(Locale.ROOT));
    }

    public void setModelPropertySorting(String modelPropertySorting) {
        if (modelPropertySorting == null) {
            modelPropertySorting = "DEFAULT";
        }

        this.modelPropertySorting = SortingMethod.valueOf(modelPropertySorting.toUpperCase(Locale.ROOT));
    }

    public void setSupportsAsync(Boolean supportsAsync) {
        this.supportsAsync = supportsAsync;
    }

    public void setUseVirtualForHooks(Boolean useVirtualForHooks) {
        this.useVirtualForHooks = useVirtualForHooks;
    }

    public void setSupportsFileParameters(Boolean supportsFileParameters) {
        this.supportsFileParameters = supportsFileParameters;
    }

    public void setSupportsDateOnly(Boolean supportsDateOnly) {
        this.supportsDateOnly = supportsDateOnly;
    }

    public void setUseIntForTimeout(Boolean useIntForTimeout) {
        this.useIntForTimeout = useIntForTimeout;
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

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    @Override
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (enumNameMapping.containsKey(value)) {
            return enumNameMapping.get(value);
        }

        if (value.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return camelize(getSymbolName(value));
        }

        // number
        if (datatype.startsWith("int") || datatype.startsWith("uint") ||
                datatype.startsWith("long") || datatype.startsWith("ulong") ||
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
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

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

        static FrameworkStrategy NETSTANDARD_1_3 = new FrameworkStrategy("netstandard1.3", ".NET Standard 1.3", "net7.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_4 = new FrameworkStrategy("netstandard1.4", ".NET Standard 1.4", "net7.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_5 = new FrameworkStrategy("netstandard1.5", ".NET Standard 1.5", "net7.0") {
        };
        static FrameworkStrategy NETSTANDARD_1_6 = new FrameworkStrategy("netstandard1.6", ".NET Standard 1.6", "net7.0") {
        };
        static FrameworkStrategy NETSTANDARD_2_0 = new FrameworkStrategy("netstandard2.0", ".NET Standard 2.0", "net7.0") {
        };
        static FrameworkStrategy NETSTANDARD_2_1 = new FrameworkStrategy("netstandard2.1", ".NET Standard 2.1", "net7.0") {
        };
        static FrameworkStrategy NETFRAMEWORK_4_7 = new FrameworkStrategy("net47", ".NET Framework 4.7", "net47", Boolean.FALSE) {
        };
        static FrameworkStrategy NETFRAMEWORK_4_8 = new FrameworkStrategy("net48", ".NET Framework 4.8", "net48", Boolean.FALSE) {
        };
        static FrameworkStrategy NET_8_0 = new FrameworkStrategy("net8.0", ".NET 8.0 (End of Support 10 November 2026)", "net8.0", Boolean.FALSE) {
        };
        static FrameworkStrategy NET_9_0 = new FrameworkStrategy("net9.0", ".NET 9.0 (End of Support 12 May 2026)", "net9.0", Boolean.FALSE) {
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

        if (strategies.stream().anyMatch(p -> !"netstandard1.3".equals(p.name))) {
            if (strategies.stream().anyMatch(p -> "netstandard1.4".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "netstandard1.5".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "netstandard1.6".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "netstandard2.0".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "netstandard2.1".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net47".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net48".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
                properties.put(NET_48_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net6.0".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
                properties.put(NET_48_OR_LATER, true);
                properties.put(NET_60_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net7.0".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
                properties.put(NET_48_OR_LATER, true);
                properties.put(NET_60_OR_LATER, true);
                properties.put(NET_70_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net8.0".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
                properties.put(NET_48_OR_LATER, true);
                properties.put(NET_60_OR_LATER, true);
                properties.put(NET_70_OR_LATER, true);
                properties.put(NET_80_OR_LATER, true);
            } else if (strategies.stream().anyMatch(p -> "net9.0".equals(p.name))) {
                properties.put(NET_STANDARD_14_OR_LATER, true);
                properties.put(NET_STANDARD_15_OR_LATER, true);
                properties.put(NET_STANDARD_16_OR_LATER, true);
                properties.put(NET_STANDARD_20_OR_LATER, true);
                properties.put(NET_STANDARD_21_OR_LATER, true);
                properties.put(NET_47_OR_LATER, true);
                properties.put(NET_48_OR_LATER, true);
                properties.put(NET_60_OR_LATER, true);
                properties.put(NET_70_OR_LATER, true);
                properties.put(NET_80_OR_LATER, true);
                properties.put(NET_90_OR_LATER, true);
            } else {
                throw new RuntimeException("Unhandled case");
            }
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
            Schema<?> additionalProperties = ModelUtils.getAdditionalProperties(schema);
            String inner = getSchemaType(additionalProperties);
            if (ModelUtils.isMapSchema(additionalProperties)) {
                inner = toInstantiationType(additionalProperties);
            }
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (ModelUtils.isArraySchema(schema)) {
            String inner = getSchemaType(ModelUtils.getSchemaItems(schema));
            return instantiationTypes.get("array") + "<" + inner + ">";
        } else {
            return null;
        }
    }

    @Override
    protected void patchPropertyIsInherited(CodegenModel model, CodegenProperty property) {
        if (GENERICHOST.equals(getLibrary())) {
            // the isInherited property is not always correct
            // fixing it here causes a breaking change in some generators
            // only do this in generators that are prepared for the improvement
            if (model.parentModel != null && model.parentModel.allVars.stream().anyMatch(v -> v.baseName.equals(property.baseName))) {
                property.isInherited = true;
            }
        }
    }

    @Override
    protected void patchProperty(Map<String, CodegenModel> enumRefs, CodegenModel model, CodegenProperty property) {
        super.patchProperty(enumRefs, model, property);

        if (!GENERICHOST.equals(getLibrary())) {
            if (!property.isContainer && (this.getNullableTypes().contains(property.dataType) || property.isEnum)) {
                property.vendorExtensions.put("x-csharp-value-type", true);
            }
        }
    }

    @Override
    protected void patchVendorExtensionNullableValueType(CodegenParameter parameter) {
        if (getLibrary().equals(GENERICHOST)) {
            super.patchVendorExtensionNullableValueType(parameter);
        } else {
            super.patchVendorExtensionNullableValueTypeLegacy(parameter);
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        // add implements for serializable/parcelable to all models
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();

            if (cm.oneOf != null && !cm.oneOf.isEmpty() && cm.oneOf.remove("Null")) {
                // if oneOf contains "null" type
                cm.isNullable = true;
            }

            if (cm.anyOf != null && !cm.anyOf.isEmpty() && cm.anyOf.remove("Null")) {
                // if anyOf contains "null" type
                cm.isNullable = true;
            }

            if (cm.getComposedSchemas() != null) {
                if (cm.getComposedSchemas().getOneOf() != null) {
                    cm.getComposedSchemas().getOneOf().removeIf(o -> "Null".equals(o.dataType));
                }
                if (cm.getComposedSchemas().getAnyOf() != null) {
                    cm.getComposedSchemas().getAnyOf().removeIf(o -> "Null".equals(o.dataType));
                }
            }

            for (CodegenProperty cp : cm.readWriteVars) {
                // ISSUE: https://github.com/OpenAPITools/openapi-generator/issues/11844
                // allVars may not have all properties
                // see modules\openapi-generator\src\test\resources\3_0\allOf.yaml
                // property boosterSeat will be in readWriteVars but not allVars
                // the property is present in the model but gets removed at CodegenModel#removeDuplicatedProperty
                if (cm.allVars.stream().noneMatch(v -> v.baseName.equals(cp.baseName))) {
                    LOGGER.debug("Property " + cp.baseName + " was found in readWriteVars but not in allVars. Adding it back to allVars");
                    cm.allVars.add(cp);
                }
            }
        }

        return objs;
    }


    // https://github.com/OpenAPITools/openapi-generator/issues/15867
    @Override
    protected void removePropertiesDeclaredInComposedTypes(Map<String, ModelsMap> objs, CodegenModel model, List<CodegenProperty> composedProperties) {
        if (!GENERICHOST.equals(getLibrary())) {
            return;
        }

        String discriminatorName = model.discriminator == null
                ? null
                : model.discriminator.getPropertyName();

        for (CodegenProperty oneOfProperty : composedProperties) {
            String ref = oneOfProperty.getRef();
            if (ref != null) {
                for (Map.Entry<String, ModelsMap> composedEntry : objs.entrySet()) {
                    CodegenModel composedModel = ModelUtils.getModelByName(composedEntry.getKey(), objs);
                    if (ref.endsWith("/" + composedModel.name)) {
                        for (CodegenProperty composedProperty : composedModel.allVars) {
                            if (discriminatorName != null && composedProperty.name.equals(discriminatorName)) {
                                continue;
                            }
                            model.vars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.allVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.readOnlyVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.nonNullableVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.optionalVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.parentRequiredVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.readWriteVars.removeIf(v -> v.name.equals(composedProperty.name));
                            model.requiredVars.removeIf(v -> v.name.equals(composedProperty.name));
                        }
                    }
                }
            }
        }
    }

    /**
     * Return true if the property being passed is a C# value type
     *
     * @param var property
     * @return true if property is a value type
     */
    @Override
    protected boolean isValueType(CodegenProperty var) {
        // this is temporary until x-csharp-value-type is removed
        return this.getLibrary().equals(GENERICHOST)
                ? super.isValueType(var)
                : this.getValueTypes().contains(var.dataType) || var.isEnum;
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
        if (schema.getProperties() != null || schema.getRequired() != null && !(ModelUtils.isComposedSchema(schema))) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        if (ModelUtils.isMapSchema(schema)) {
            // an object or anyType composed schema that has additionalProperties set
            addAdditionPropertiesToCodeGenModel(m, schema);
        } else {
            m.setIsMap(false);
            if (ModelUtils.isFreeFormObject(schema, openAPI)) {
                // non-composed object type with no properties + additionalProperties
                // additionalProperties must be null, ObjectSchema, or empty Schema
                addAdditionPropertiesToCodeGenModel(m, schema);
            }
        }
        // process 'additionalProperties'
        setAddProps(schema, m);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return objs;
    }
}
