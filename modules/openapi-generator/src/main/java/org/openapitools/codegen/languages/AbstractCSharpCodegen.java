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
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractCSharpCodegen extends DefaultCodegen implements CodegenConfig {

    protected boolean optionalAssemblyInfoFlag = true;
    protected boolean optionalEmitDefaultValuesFlag = false;
    protected boolean conditionalSerialization = false;
    protected boolean optionalProjectFileFlag = true;
    protected boolean optionalMethodArgumentFlag = true;
    protected boolean useDateTimeOffsetFlag = false;
    protected boolean useCollection = false;
    protected boolean returnICollection = false;
    protected boolean netCoreProjectFileFlag = false;
    protected boolean nullReferenceTypesFlag = false;
    protected boolean useSourceGeneration = false;

    protected String modelPropertyNaming = CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.PascalCase.name();

    protected String licenseUrl = "http://localhost";
    protected String licenseName = "NoLicense";

    protected String packageVersion = "1.0.0";
    protected String packageName = "Org.OpenAPITools";
    protected String packageTitle = "OpenAPI Library";
    protected String packageProductName = "OpenAPILibrary";
    protected String packageDescription = "A library generated from a OpenAPI doc";
    protected String packageCompany = "OpenAPI";
    protected String packageCopyright = "No Copyright";
    protected String packageAuthors = "OpenAPI";
    public static final String DATE_FORMAT = "dateFormat";
    protected String dateFormat = "yyyy'-'MM'-'dd";
    public static final String DATETIME_FORMAT = "dateTimeFormat";
    protected String dateTimeFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffffffK";

    protected String interfacePrefix = "I";
    protected String enumNameSuffix = "Enum";
    protected String enumValueSuffix = "Enum";

    protected String sourceFolder = "src";
    protected String invalidNamePrefix = "var";

    // TODO: Add option for test folder output location. Nice to allow e.g. ./test instead of ./src.
    //       This would require updating relative paths (e.g. path to main project file in test project file)
    protected String testFolder = sourceFolder;

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    // true if support nullable type
    protected boolean supportNullable = Boolean.FALSE;

    protected Boolean zeroBasedEnums = null;
    protected static final String zeroBasedEnumVendorExtension = "x-zero-based-enum";

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractCSharpCodegen.class);

    // special property keywords not allowed as these are the function names in the model files
    protected Set<String> propertySpecialKeywords = new HashSet<>(Arrays.asList("ToString", "ToJson", "GetHashCode", "Equals", "ShouldSerializeToString"));

    // A cache to efficiently lookup schema `toModelName()` based on the schema Key
    private Map<String, String> schemaKeyToModelNameCache = new HashMap<>();

    public AbstractCSharpCodegen() {
        super();

        supportsInheritance = true;

        // C# does not use import mapping
        importMapping.clear();

        outputFolder = "generated-code" + File.separator + this.getName();
        embeddedTemplateDir = templateDir = this.getName();

        collectionTypes = new HashSet<>(
                Arrays.asList(
                        "IList", "List",
                        "ICollection", "Collection",
                        "IEnumerable")
        );

        mapTypes = new HashSet<>(
                Arrays.asList("IDictionary", "Dictionary")
        );

        // NOTE: C# uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(
                Arrays.asList(
                        // set "client" as a reserved word to avoid conflicts with Org.OpenAPITools.Client
                        // this is a workaround and can be removed if c# api client is updated to use
                        // fully qualified name
                        "Client", "client", "parameter", "Configuration", "Version",
                        // local variable names in API methods (endpoints)
                        "localVarPath", "localVarPathParams", "localVarQueryParams", "localVarHeaderParams",
                        "localVarFormParams", "localVarFileParams", "localVarStatusCode", "localVarResponse",
                        "localVarPostBody", "localVarHttpHeaderAccepts", "localVarHttpHeaderAccept",
                        "localVarHttpContentTypes", "localVarHttpContentType",
                        "localVarStatusCode",
                        // C# reserved words
                        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked",
                        "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else",
                        "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for",
                        "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock",
                        "long", "namespace", "new", "null", "object", "operator", "out", "override", "params",
                        "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed",
                        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "system", "this", "throw",
                        "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
                        "virtual", "void", "volatile", "while")
        );

        // TODO: Either include fully qualified names here or handle in DefaultCodegen via lastIndexOf(".") search
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "String",
                        "string",
                        "bool?",
                        "bool",
                        "double?",
                        "double",
                        "decimal?",
                        "decimal",
                        "int?",
                        "int",
                        "uint",
                        "uint?",
                        "long?",
                        "long",
                        "ulong",
                        "ulong?",
                        "float?",
                        "float",
                        "byte[]",
                        "ICollection",
                        "Collection",
                        "List",
                        "Dictionary",
                        "DateTime?",
                        "DateTime",
                        "DateTimeOffset?",
                        "DateTimeOffset",
                        "Boolean",
                        "Double",
                        "Decimal",
                        "Int32",
                        "Int64",
                        "Float",
                        "Guid?",
                        "Guid",
                        "System.IO.Stream", // not really a primitive, we include it to avoid model import
                        "Object")
        );

        instantiationTypes.put("array", "List");
        instantiationTypes.put("list", "List");
        instantiationTypes.put("map", "Dictionary");

        this.setSortParamsByRequiredFlag(true);

        // do it only on newer libraries to avoid breaking changes
        // this.setSortModelPropertiesByRequiredFlag(true);
    }

    public void setReturnICollection(boolean returnICollection) {
        this.returnICollection = returnICollection;
    }

    public void setUseCollection(boolean useCollection) {
        this.useCollection = useCollection;
        if (useCollection) {
            instantiationTypes.put("array", "Collection");
            instantiationTypes.put("list", "Collection");
        }
        this.setTypeMapping();
    }

    public void setOptionalMethodArgumentFlag(boolean flag) {
        this.optionalMethodArgumentFlag = flag;
    }

    public void setNetCoreProjectFileFlag(boolean flag) {
        this.netCoreProjectFileFlag = flag;
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        this.setTypeMapping();
    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("CSHARP_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable CSHARP_POST_PROCESS_FILE not defined so the C# code may not be properly formatted by uncrustify (0.66 or later) or other code formatter. To define it, try `export CSHARP_POST_PROCESS_FILE=\"/usr/local/bin/uncrustify --no-backup\" && export UNCRUSTIFY_CONFIG=/path/to/uncrustify-rules.cfg` (Linux/Mac). Note: replace /path/to with the location of uncrustify-rules.cfg");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        // License info
        if (additionalProperties.containsKey(CodegenConstants.LICENSE_URL)) {
            setLicenseUrl((String) additionalProperties.get(CodegenConstants.LICENSE_URL));
        } else {
            additionalProperties.put(CodegenConstants.LICENSE_URL, this.licenseUrl);
        }

        if (additionalProperties.containsKey(CodegenConstants.LICENSE_NAME)) {
            setLicenseName((String) additionalProperties.get(CodegenConstants.LICENSE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.LICENSE_NAME, this.licenseName);
        }

        // {{packageVersion}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        // {{sourceFolder}}
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));

            // TODO: Move to its own option when a parameter for 'testFolder' is added.
            setTestFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, this.sourceFolder);
        }

        // {{packageName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            LOGGER.warn(String.format(Locale.ROOT, "%s is not used by C# generators. Please use %s",
                    CodegenConstants.INVOKER_PACKAGE, CodegenConstants.PACKAGE_NAME));
        }

        // {{packageTitle}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_TITLE)) {
            setPackageTitle((String) additionalProperties.get(CodegenConstants.PACKAGE_TITLE));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_TITLE, packageTitle);
        }

        // {{packageProductName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_PRODUCTNAME)) {
            setPackageProductName((String) additionalProperties.get(CodegenConstants.PACKAGE_PRODUCTNAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_PRODUCTNAME, packageProductName);
        }

        // {{dateFormat}}
        if (additionalProperties.containsKey(DATE_FORMAT)) {
            setDateFormat((String) additionalProperties.get(DATE_FORMAT));
        } else {
            additionalProperties.put(DATE_FORMAT, this.dateFormat);
        }

        // {{dateTimeFormat}}
        if (additionalProperties.containsKey(DATETIME_FORMAT)) {
            setDateTimeFormat((String) additionalProperties.get(DATETIME_FORMAT));
        } else {
            additionalProperties.put(DATETIME_FORMAT, this.dateTimeFormat);
        }

        // {{packageDescription}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_DESCRIPTION)) {
            setPackageDescription((String) additionalProperties.get(CodegenConstants.PACKAGE_DESCRIPTION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_DESCRIPTION, packageDescription);
        }

        // {{packageCompany}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COMPANY)) {
            setPackageCompany((String) additionalProperties.get(CodegenConstants.PACKAGE_COMPANY));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COMPANY, packageCompany);
        }

        // {{packageCopyright}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COPYRIGHT)) {
            setPackageCopyright((String) additionalProperties.get(CodegenConstants.PACKAGE_COPYRIGHT));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COPYRIGHT, packageCopyright);
        }

        // {{packageAuthors}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_AUTHORS)) {
            setPackageAuthors((String) additionalProperties.get(CodegenConstants.PACKAGE_AUTHORS));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_AUTHORS, packageAuthors);
        }

        // {{useDateTimeOffset}}
        if (additionalProperties.containsKey(CodegenConstants.USE_DATETIME_OFFSET)) {
            useDateTimeOffset(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_DATETIME_OFFSET));
        } else {
            additionalProperties.put(CodegenConstants.USE_DATETIME_OFFSET, useDateTimeOffsetFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_COLLECTION)) {
            setUseCollection(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_COLLECTION));
        } else {
            additionalProperties.put(CodegenConstants.USE_COLLECTION, useCollection);
        }

        if (additionalProperties.containsKey(CodegenConstants.RETURN_ICOLLECTION)) {
            setReturnICollection(convertPropertyToBooleanAndWriteBack(CodegenConstants.RETURN_ICOLLECTION));
        } else {
            additionalProperties.put(CodegenConstants.RETURN_ICOLLECTION, returnICollection);
        }

        if (additionalProperties.containsKey(CodegenConstants.NETCORE_PROJECT_FILE)) {
            setNetCoreProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.NETCORE_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.NETCORE_PROJECT_FILE, netCoreProjectFileFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.NULLABLE_REFERENCE_TYPES)) {
            setNullableReferenceTypes(convertPropertyToBooleanAndWriteBack(CodegenConstants.NULLABLE_REFERENCE_TYPES));
        }

        String zeroBasedEnums = "zeroBasedEnums";
        if (additionalProperties.containsKey(zeroBasedEnums)) {
            setZeroBasedEnums(convertPropertyToBooleanAndWriteBack(zeroBasedEnums));
        }

        if (additionalProperties.containsKey(CodegenConstants.INTERFACE_PREFIX)) {
            String useInterfacePrefix = additionalProperties.get(CodegenConstants.INTERFACE_PREFIX).toString();
            if ("false".equals(useInterfacePrefix.toLowerCase(Locale.ROOT))) {
                setInterfacePrefix("");
            } else if (!"true".equals(useInterfacePrefix.toLowerCase(Locale.ROOT))) {
                // NOTE: if user passes "true" explicitly, we use the default I- prefix. The other supported case here is a custom prefix.
                setInterfacePrefix(sanitizeName(useInterfacePrefix));
            }
        }

        if (additionalProperties().containsKey(CodegenConstants.ENUM_NAME_SUFFIX)) {
            setEnumNameSuffix(additionalProperties.get(CodegenConstants.ENUM_NAME_SUFFIX).toString());
        }

        if (additionalProperties().containsKey(CodegenConstants.ENUM_VALUE_SUFFIX)) {
            setEnumValueSuffix(additionalProperties.get(CodegenConstants.ENUM_VALUE_SUFFIX).toString());
        }

        // This either updates additionalProperties with the above fixes, or sets the default if the option was not specified.
        additionalProperties.put(CodegenConstants.INTERFACE_PREFIX, interfacePrefix);

        // add lambda for mustache templates
        additionalProperties.put("lambdaCref", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replace("<", "{");
                content = content.replace(">", "}");
                content = content.replace("{string}", "{String}");
                writer.write(content);
            }
        });

        this.setTypeMapping();
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("camelcase_param", new CamelCaseLambda().generator(this).escapeAsParamName(true))
                .put("required", new RequiredParameterLambda())
                .put("optional", new OptionalParameterLambda().generator(this))
                .put("joinWithComma", new JoinWithCommaLambda())
                .put("joinLinesWithComma", new JoinWithCommaLambda(false, "\n", ",\n"))
                .put("trimLineBreaks", new TrimLineBreaksLambda())
                .put("trimTrailingWithNewLine", new TrimTrailingWhiteSpaceLambda(true))
                .put("trimTrailing", new TrimTrailingWhiteSpaceLambda(false))
                .put("first", new FirstLambda("  "))
                .put("firstDot", new FirstLambda("\\."))
                .put("indent3", new IndentedLambda(12, " ", false))
                .put("indent4", new IndentedLambda(16, " ", false))
                .put("uniqueLinesWithNewLine", new UniqueLambda("\n", true));
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (property.isInnerEnum && property.items != null) {
            // format maps of inner enums to include the classname eg: Dictionary<string, MapTest.InnerEnum>
            property.datatypeWithEnum = property.datatypeWithEnum.replace(property.items.datatypeWithEnum, model.classname + "." + property.items.datatypeWithEnum);
            property.dataType = property.datatypeWithEnum;
        }

        if (property.isEnum && !property.vendorExtensions.containsKey(this.zeroBasedEnumVendorExtension)) {
            if (Boolean.TRUE.equals(this.zeroBasedEnums)) {
                property.vendorExtensions.put(this.zeroBasedEnumVendorExtension, true);
            } else if (!Boolean.FALSE.equals(this.zeroBasedEnums)) {
                if (property.allowableValues.containsKey("values")) {
                    final List<Object> allowableValues = (List<Object>) property.allowableValues.get("values");
                    boolean isZeroBased = String.valueOf(allowableValues.get(0)).toLowerCase(Locale.ROOT).equals("unknown");
                    property.vendorExtensions.put(this.zeroBasedEnumVendorExtension, isZeroBased);
                }
            }
        }

        if (property.isMap || property.isContainer) {
            // maps of enums will be marked both isMap and isEnum, correct that now
            property.isEnum = false;
            property.isInnerEnum = false;
            property.isString = false;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();

            if (cm.getComposedSchemas() != null) {
                List<CodegenProperty> oneOf = cm.getComposedSchemas().getOneOf();
                if (oneOf != null) {
                    Set<String> dataTypeSet = new HashSet<>();
                    for (CodegenProperty oneOfProperty : oneOf) {
                        if (dataTypeSet.contains(oneOfProperty.dataType)) {
                            // add "x-duplicated-data-type" to indicate if the dataType already occurs before
                            // in other sub-schemas of allOf/anyOf/oneOf
                            oneOfProperty.vendorExtensions.putIfAbsent("x-composed-data-type", true);
                        } else {
                            dataTypeSet.add(oneOfProperty.dataType);
                        }
                    }
                }

                List<CodegenProperty> anyOf = cm.getComposedSchemas().getAnyOf();
                if (anyOf != null) {
                    Set<String> dataTypeSet = new HashSet<>();
                    for (CodegenProperty anyOfProperty : anyOf) {
                        if (dataTypeSet.contains(anyOfProperty.dataType)) {
                            // add "x-duplicated-data-type" to indicate if the dataType already occurs before
                            // in other sub-schemas of allOf/anyOf/oneOf
                            anyOfProperty.vendorExtensions.putIfAbsent("x-composed-data-type", true);
                        } else {
                            dataTypeSet.add(anyOfProperty.dataType);
                        }
                    }
                }
            }

            if (cm.isEnum && !cm.vendorExtensions.containsKey(this.zeroBasedEnumVendorExtension)) {
                if (Boolean.TRUE.equals(this.zeroBasedEnums)) {
                    cm.vendorExtensions.put(this.zeroBasedEnumVendorExtension, true);
                } else if (!Boolean.FALSE.equals(this.zeroBasedEnums)) {
                    if (cm.allowableValues.containsKey("values")) {
                        final List<Object> allowableValues = (List<Object>) cm.allowableValues.get("values");
                        boolean isZeroBased = String.valueOf(allowableValues.get(0)).toLowerCase(Locale.ROOT).equals("unknown");
                        cm.vendorExtensions.put(this.zeroBasedEnumVendorExtension, isZeroBased);
                    }
                }
            }
        }
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    /**
     * Invoked by {@link DefaultGenerator} after all models have been post-processed, allowing for a last pass of codegen-specific model cleanup.
     *
     * @param objs Current state of codegen object model.
     * @return An in-place modified state of the codegen object model.
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        Map<String, CodegenModel> enumRefs = new HashMap<>();
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), processed);

            // if we don't call setHasDiscriminatorWithNonEmptyMapping then hasDiscriminatorWithNonEmptyMapping will be false, and we need it in the JsonConverter
            // the checks on oneOf and anyOf must be there or else hasDiscriminatorWithNonEmptyMapping will be true for GrandparentAnimal.
            // GrandparentAnimal has a discriminator, but no oneOf nor anyOf
            // modules\openapi-generator\src\test\resources\3_0\csharp\petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml
            model.setHasDiscriminatorWithNonEmptyMapping(
                    ((model.anyOf != null && model.anyOf.size() > 0) || (model.oneOf != null && model.oneOf.size() > 0)) &&
                            model.discriminator != null &&
                            model.discriminator.getMappedModels() != null &&
                            model.discriminator.getMappedModels().size() > 0);

            if (model.isEnum) {
                enumRefs.put(model.getClassname(), model);
            }
        }

        for (Map.Entry<String, ModelsMap> entry : objs.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), objs);
            model.vendorExtensions.put("x-model-is-mutatable", modelIsMutatable(model, null));

            CodegenComposedSchemas composedSchemas = model.getComposedSchemas();
            if (composedSchemas != null) {
                List<CodegenProperty> allOf = composedSchemas.getAllOf();
                if (allOf != null) {
                    for (CodegenProperty property : allOf) {
                        property.name = patchPropertyName(model, property.baseType);
                        patchPropertyVendorExtensions(property);
                    }
                }

                List<CodegenProperty> anyOf = composedSchemas.getAnyOf();
                if (anyOf != null) {
                    removePropertiesDeclaredInComposedTypes(objs, model, anyOf);
                    for (CodegenProperty property : anyOf) {
                        property.name = patchPropertyName(model, property.baseType);
                        property.isNullable = true;
                        patchPropertyVendorExtensions(property);
                    }
                }

                List<CodegenProperty> oneOf = composedSchemas.getOneOf();
                if (oneOf != null) {
                    removePropertiesDeclaredInComposedTypes(objs, model, oneOf);
                    for (CodegenProperty property : oneOf) {
                        property.name = patchPropertyName(model, property.baseType);
                        property.isNullable = true;
                        patchPropertyVendorExtensions(property);
                    }
                }
            }

            // https://github.com/OpenAPITools/openapi-generator/issues/12324
            // TODO: why do these collections contain different instances?
            // fixing allVars should suffice instead of patching every collection
            for (CodegenProperty property : model.allVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.vars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.readWriteVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.optionalVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.parentVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.requiredVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.readOnlyVars) {
                patchProperty(enumRefs, model, property);
            }
            for (CodegenProperty property : model.nonNullableVars) {
                patchProperty(enumRefs, model, property);
            }
        }
        return processed;
    }

    /**
     * Returns true if the model contains any properties with a public setter
     * If true, the model's constructor accessor should be made public to ensure end users
     * can instantiate the object. If false, then the model is only ever given
     * to us by the server, so we do not need a public constructor
     */
    private boolean modelIsMutatable(CodegenModel model, Set<String> processed) {
        if (processed == null) {
            processed = new HashSet<String>();
        }
        Boolean isMutatable = model.allVars.stream().anyMatch(v -> !v.isReadOnly);
        if (!isMutatable && !processed.contains(model.classname) && model.getDiscriminator() != null && model.getDiscriminator().getMappedModels() != null) {
            processed.add(model.classname);
            for (CodegenDiscriminator.MappedModel mappedModel : model.getDiscriminator().getMappedModels()) {
                isMutatable = modelIsMutatable(model, processed);
            }
        }

        return isMutatable;
    }

    protected void removePropertiesDeclaredInComposedTypes(Map<String, ModelsMap> objs, CodegenModel model, List<CodegenProperty> composedProperties) {
    }

    private String patchPropertyName(CodegenModel model, String value) {
        // the casing will be wrong if we just set the name to escapeReservedWord
        // if we try to fix it with camelize, underscores get stripped out
        // so test if the name was escaped and then replace var with Var
        String tmpPropertyName = escapeReservedWord(model, value);
        if (!value.equals(tmpPropertyName) || value.startsWith(this.invalidNamePrefix)) {
            value = tmpPropertyName;
            String firstCharacter = value.substring(0, 1);
            value = value.substring(1);
            value = firstCharacter.toUpperCase(Locale.ROOT) + value;
        }

        return value;
    }

    private void patchPropertyVendorExtensions(CodegenProperty property) {
        Boolean isValueType = isValueType(property);
        property.vendorExtensions.put("x-is-value-type", isValueType);
        property.vendorExtensions.put("x-is-reference-type", !isValueType);
        property.vendorExtensions.put("x-is-nullable-type", this.getNullableReferencesTypes() || isValueType);
    }

    protected void patchProperty(Map<String, CodegenModel> enumRefs, CodegenModel model, CodegenProperty property) {
        if (enumRefs.containsKey(property.dataType)) {
            // Handle any enum properties referred to by $ref.
            // This is different in C# than most other generators, because enums in C# are compiled to integral types,
            // while enums in many other languages are true objects.
            CodegenModel refModel = enumRefs.get(property.dataType);
            property.allowableValues = refModel.allowableValues;
            property.isEnum = true;

            // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
            property.isPrimitiveType = true;
        }

        patchPropertyVendorExtensions(property);

        String tmpPropertyName = escapeReservedWord(model, property.name);
        property.name = patchPropertyName(model, property.name);

        String[] nestedTypes = { "List", "Collection", "ICollection", "Dictionary" };

        Arrays.stream(nestedTypes).forEach(nestedType -> {
            // fix incorrect data types for maps of maps
            if (property.datatypeWithEnum.contains(", " + nestedType + ">") && property.items != null) {
                property.datatypeWithEnum = property.datatypeWithEnum.replace(", " + nestedType + ">", ", " + property.items.datatypeWithEnum + ">");
                property.dataType = property.datatypeWithEnum;
            }

            if (property.datatypeWithEnum.contains("<" + nestedType + ">") && property.items != null) {
                property.datatypeWithEnum = property.datatypeWithEnum.replace("<" + nestedType + ">", "<" + property.items.datatypeWithEnum + ">");
                property.dataType = property.datatypeWithEnum;
            }
        });

        // HOTFIX: https://github.com/OpenAPITools/openapi-generator/issues/14944
        if (property.datatypeWithEnum.equals("decimal")) {
            property.isDecimal = true;
        }
    }

    @Override
    protected List<Map<String, Object>> buildEnumVars(List<Object> values, String dataType) {
        List<Map<String, Object>> enumVars = super.buildEnumVars(values, dataType);

        // this is needed for enumRefs like OuterEnum marked as nullable and also have string values
        // keep isString true so that the index will be used as the enum value instead of a string
        // this is inline with C# enums with string values
        if ("string?".equals(dataType)) {
            enumVars.forEach((enumVar) -> {
                enumVar.put("isString", true);
            });
        }

        return enumVars;
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        if (var.vendorExtensions == null) {
            var.vendorExtensions = new HashMap<>();
        }

        super.updateCodegenPropertyEnum(var);

        // Because C# uses nullable primitives for datatype, and datatype is used in DefaultCodegen for determining enum-ness, guard against weirdness here.
        if (var.isEnum) {
            if ("byte".equals(var.dataFormat)) {// C# Actually supports byte and short enums.
                var.vendorExtensions.put("x-enum-byte", true);
                var.isString = false;
                var.isLong = false;
                var.isInteger = false;
            } else if ("int".equals(var.dataType) || "int32".equals(var.dataFormat)) {
                var.isInteger = true;
                var.isString = false;
                var.isLong = false;
            } else if ("int64".equals(var.dataFormat)) {
                var.isLong = true;
                var.isString = false;
                var.isInteger = false;
            } else {// C# doesn't support non-integral enums, so we need to treat everything else as strings (e.g. to not lose precision or data integrity)
                var.isString = true;
                var.isInteger = false;
                var.isLong = false;
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            OperationMap operations = objs.getOperations();
            if (operations != null) {
                List<CodegenOperation> ops = operations.getOperation();
                for (CodegenOperation operation : ops) {

                    // Check return types for collection
                    if (operation.returnType != null) {
                        int namespaceEnd = operation.returnType.lastIndexOf(".");
                        String typeMapping = namespaceEnd > 0
                                ? operation.returnType.substring(namespaceEnd)
                                : operation.returnType;

                        if (this.collectionTypes.contains(typeMapping)) {
                            operation.isArray = true;
                            operation.returnContainer = operation.returnType;
                            if (this.returnICollection && (
                                    typeMapping.startsWith("List") ||
                                            typeMapping.startsWith("Collection"))) {
                                // NOTE: ICollection works for both List<T> and Collection<T>
                                int genericStart = typeMapping.indexOf("<");
                                if (genericStart > 0) {
                                    operation.returnType = "ICollection" + typeMapping.substring(genericStart);
                                }
                            }
                        } else {
                            operation.returnContainer = operation.returnType;
                            operation.isMap = this.mapTypes.stream().anyMatch(t -> typeMapping.startsWith(t));
                        }
                    }

                    if (operation.examples != null) {
                        for (Map<String, String> example : operation.examples) {
                            for (Map.Entry<String, String> entry : example.entrySet()) {
                                // Replace " with \", \r, \n with \\r, \\n
                                String val = entry.getValue().replace("\"", "\\\"")
                                        .replace("\r", "\\r")
                                        .replace("\n", "\\n");
                                entry.setValue(val);
                            }
                        }
                    }

                    for (CodegenParameter parameter : operation.allParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.bodyParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.cookieParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.formParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.headerParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.implicitHeadersParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.optionalParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.pathParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.queryParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.notNullableParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    for (CodegenParameter parameter : operation.requiredParams) {
                        CodegenModel model = getModelFromParameter(allModels, parameter);
                        patchParameter(model, parameter);
                    }

                    List<CodegenParameter> referenceTypes = operation.allParams.stream().filter(p -> p.vendorExtensions.get("x-is-value-type") == null && !p.isNullable).collect(Collectors.toList());
                    operation.vendorExtensions.put("x-not-nullable-reference-types", referenceTypes);
                    operation.vendorExtensions.put("x-has-not-nullable-reference-types", referenceTypes.size() > 0);
                    processOperation(operation);
                }
            }
        }

        return objs;
    }

    protected void patchVendorExtensionNullableValueType(CodegenParameter parameter) {
        if (parameter.isNullable && !parameter.isContainer && (this.getValueTypes().contains(parameter.dataType) || parameter.isEnum)) {
            parameter.vendorExtensions.put("x-nullable-value-type", true);
        }
    }

    /**
     * Returns the model related to the given parameter
     */
    private CodegenModel getModelFromParameter(List<ModelMap> allModels, CodegenParameter parameter) {
        return parameter.isModel
                ? allModels.stream().map(m -> m.getModel()).filter(m -> m.getClassname().equals(parameter.dataType)).findFirst().orElse(null)
                : null;
    }

    /**
     * This is the same as patchVendorExtensionNullableValueType except it uses the deprecated getNullableTypes property
     */
    protected void patchVendorExtensionNullableValueTypeLegacy(CodegenParameter parameter) {
        if (parameter.isNullable && !parameter.isContainer && (this.getNullableTypes().contains(parameter.dataType) || parameter.isEnum)) {
            parameter.vendorExtensions.put("x-nullable-value-type", true);
        }
    }

    private void patchParameter(CodegenModel model, CodegenParameter parameter) {
        patchVendorExtensionNullableValueType(parameter);

        if (this.getNullableReferencesTypes() || (parameter.vendorExtensions.get("x-nullable-value-type") != null)) {
            parameter.vendorExtensions.put("x-nullable-type", true);
        }

        if (!isSupportNullable()) {
            if (model == null) {
                parameter.isNullable = true;
            } else {
                if (model.isEnum) {
                    parameter.isEnum = true;
                    parameter.allowableValues = model.allowableValues;
                    parameter.isPrimitiveType = true;
                    parameter.isNullable = false;
                } else {
                    parameter.isNullable = true;
                }
            }
        } else {
            updateCodegenParameterEnum(parameter, model);
        }
    }

    protected void processOperation(CodegenOperation operation) {
        String[] nestedTypes = { "List", "Collection", "ICollection", "Dictionary" };

        Arrays.stream(nestedTypes).forEach(nestedType -> {
            if (operation.returnProperty != null && operation.returnType.contains("<" + nestedType + ">") && operation.returnProperty.items != null) {
                String nestedReturnType = operation.returnProperty.items.dataType;
                operation.returnType = operation.returnType.replace("<" + nestedType + ">", "<" + nestedReturnType + ">");
                operation.returnProperty.dataType = operation.returnType;
                operation.returnProperty.datatypeWithEnum = operation.returnType;
            }

            if (operation.returnProperty != null && operation.returnType.contains(", " + nestedType + ">") && operation.returnProperty.items != null) {
                String nestedReturnType = operation.returnProperty.items.dataType;
                operation.returnType = operation.returnType.replace(", " + nestedType + ">", ", " + nestedReturnType + ">");
                operation.returnProperty.dataType = operation.returnType;
                operation.returnProperty.datatypeWithEnum = operation.returnType;
            }
        });
    }

    protected void updateCodegenParameterEnumLegacy(CodegenParameter parameter, CodegenModel model) {
        if (model != null) {
            if (model.isEnum) {
                parameter.isEnum = true;
                parameter.allowableValues = model.allowableValues;
                parameter.isPrimitiveType = true;
                parameter.vendorExtensions.put("x-csharp-value-type", true);
            }
        }

        if (!parameter.isContainer && this.getNullableTypes().contains(parameter.dataType)) {
            parameter.vendorExtensions.put("x-csharp-value-type", true);
        }
    }

    protected void updateCodegenParameterEnum(CodegenParameter parameter, CodegenModel model) {
        if (model != null) {
            if (model.isEnum) {
                parameter.isEnum = true;
                parameter.allowableValues = model.allowableValues;
                parameter.isPrimitiveType = true;
                parameter.vendorExtensions.put("x-is-value-type", true);
            }
        }

        if (!parameter.isContainer && this.getValueTypes().contains(parameter.dataType)) {
            parameter.vendorExtensions.put("x-is-value-type", true);
        }
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + apiPackage();
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + modelPackage();
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId));
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

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        if (propertySpecialKeywords.contains(name)) {
            return camelize("property_" + name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        return escapeReservedWord(name);
    }

    public String escapeReservedWord(CodegenModel model, String name) {
        name = this.escapeReservedWord(name);

        return name.equalsIgnoreCase(model.getClassname())
                ? this.invalidNamePrefix + camelize(name)
                : name;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (reservedWords().contains(name) ||
                reservedWords().contains(name.toLowerCase(Locale.ROOT)) ||
                reservedWords().contains(camelize(sanitizeName(name))) ||
                isReservedWord(name) ||
                name.matches("^\\d.*")) {
            name = this.invalidNamePrefix + camelize(name);
        }
        return name;
    }

    /**
     * Return the example value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the example value of the property
     */
    @Override
    public String toExampleValue(Schema p) {
        return p.getExample() == null
                ? null
                : p.getExample().toString();
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                if (ModelUtils.isFloatSchema(p)) { // float
                    return p.getDefault().toString() + "F";
                } else if (ModelUtils.isDoubleSchema(p)) { // double
                    return p.getDefault().toString() + "D";
                } else {    // decimal
                    return p.getDefault().toString() + "M";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String _default = String.valueOf(p.getDefault());
                if (p.getEnum() == null) {
                    return "\"" + _default + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return _default;
                }
            }
        }

        return null;
    }

    @Override
    protected boolean isReservedWord(String word) {
        // NOTE: This differs from super's implementation in that C# does _not_ want case insensitive matching.
        return reservedWords.contains(word);
    }

    public String getNullableType(Schema p, String type) {
        if (languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return null;
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;

        if (openAPIType == null) {
            LOGGER.error("OpenAPI Type for {} is null. Default to UNKNOWN_OPENAPI_TYPE instead.", p.getName());
            openAPIType = "UNKNOWN_OPENAPI_TYPE";
        }

        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            String languageType = getNullableType(p, type);
            if (languageType != null) {
                return languageType;
            }
        } else {
            type = openAPIType;
        }

        return toModelName(type);
    }

    /**
     * Provides C# strongly typed declaration for simple arrays of some type and arrays of arrays of some type.
     *
     * @param arr The input array property
     * @return The type declaration when the type is an array of arrays.
     */
    private String getArrayTypeDeclaration(ArraySchema arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Schema items = arr.getItems();
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        instantiationType.append("<").append(nestedType).append(">");
        return instantiationType.toString();
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        }
        return super.toInstantiationType(p);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        } else if (ModelUtils.isMapSchema(p)) {
            // Should we also support maps of maps?
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toModelName(String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return modelNameMapping.get(name);
        }

        // We need to check if schema-mapping has a different model for this class, so we use it
        // instead of the auto-generated one.
        if (schemaMapping.containsKey(name)) {
            return schemaMapping.get(name);
        }

        // memoization and lookup in the cache
        String origName = name;
        if (schemaKeyToModelNameCache.containsKey(origName)) {
            return schemaKeyToModelNameCache.get(origName);
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = camelize(sanitizeName(name));

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = camelize("model_" + name); // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    camelize("model_" + name));
            name = camelize("model_" + name); // e.g. 200Response => Model200Response (after camelize)
        }

        // store in cache
        schemaKeyToModelNameCache.put(origName, name);

        // camelize the model name
        // phone_number => PhoneNumber
        return name;
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + ".Test";
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + ".Test";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Tests";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Tests";
    }

    public void setLicenseUrl(String licenseUrl) {
        this.licenseUrl = licenseUrl;
    }

    public void setLicenseName(String licenseName) {
        this.licenseName = licenseName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setPackageTitle(String packageTitle) {
        this.packageTitle = packageTitle;
    }

    public void setPackageProductName(String packageProductName) {
        this.packageProductName = packageProductName;
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    public void setDateTimeFormat(String dateTimeFormat) {
        this.dateTimeFormat = dateTimeFormat;
    }

    public void setPackageDescription(String packageDescription) {
        this.packageDescription = packageDescription;
    }

    public void setPackageCompany(String packageCompany) {
        this.packageCompany = packageCompany;
    }

    public void setPackageCopyright(String packageCopyright) {
        this.packageCopyright = packageCopyright;
    }

    public void setPackageAuthors(String packageAuthors) {
        this.packageAuthors = packageAuthors;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }

    public String getInterfacePrefix() {
        return interfacePrefix;
    }

    public void setZeroBasedEnums(final Boolean zeroBasedEnums) {
        this.zeroBasedEnums = zeroBasedEnums;
    }

    public void setNullableReferenceTypes(final Boolean nullReferenceTypesFlag) {
        this.nullReferenceTypesFlag = nullReferenceTypesFlag;
        additionalProperties.put("nullableReferenceTypes", nullReferenceTypesFlag);
        additionalProperties.put("nrt", nullReferenceTypesFlag);

        if (nullReferenceTypesFlag) {
            additionalProperties.put("nrt?", "?");
            additionalProperties.put("nrt!", "!");
        } else {
            additionalProperties.remove("nrt?");
            additionalProperties.remove("nrt!");
        }
    }

    public boolean getNullableReferencesTypes() {
        return this.nullReferenceTypesFlag;
    }

    public void setUseSourceGeneration(final Boolean useSourceGeneration) {
        this.useSourceGeneration = useSourceGeneration;
    }

    public boolean getUseSourceGeneration() {
        return this.useSourceGeneration;
    }

    public void setInterfacePrefix(final String interfacePrefix) {
        this.interfacePrefix = interfacePrefix;
    }

    public void setEnumNameSuffix(final String enumNameSuffix) {
        this.enumNameSuffix = enumNameSuffix;
    }

    public void setEnumValueSuffix(final String enumValueSuffix) {
        this.enumValueSuffix = enumValueSuffix;
    }

    public boolean isSupportNullable() {
        return supportNullable;
    }

    public void setSupportNullable(final boolean supportNullable) {
        this.supportNullable = supportNullable;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // C# only supports enums as literals for int, int?, long, long?, byte, and byte?. All else must be treated as strings.
        // Per: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/enum
        // The approved types for an enum are byte, sbyte, short, ushort, int, uint, long, or ulong.
        // but we're not supporting unsigned integral types or shorts.
        if (datatype.startsWith("int") || datatype.startsWith("uint") ||
                datatype.startsWith("long") || datatype.startsWith("ulong") ||
                datatype.startsWith("byte")) {
            return value;
        }

        final String partiallyEscaped = value
                .replace("\n", "\\n")
                .replace("\t", "\\t")
                .replace("\r", "\\r")
                .replaceAll("(?<!\\\\)\"", "\\\\\"");

        return escapeUnsafeCharacters(partiallyEscaped);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(getSymbolName(name));
        }

        String enumName = sanitizeName(name);

        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = camelize(enumName) + this.enumValueSuffix;

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + this.enumNameSuffix;
    }

    public String testPackageName() {
        return this.packageName + ".Test";
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*").replace("--", "- -");
    }

    @Override
    public boolean isDataTypeString(String dataType) {
        // also treat double/decimal/float as "string" in enum so that the values (e.g. 2.8) get double-quoted
        return "String".equalsIgnoreCase(dataType) ||
                "double?".equals(dataType) || "decimal?".equals(dataType) || "float?".equals(dataType) ||
                "double".equals(dataType) || "decimal".equals(dataType) || "float".equals(dataType);
    }

    /**
     * Return true if the property being passed is a C# value type
     *
     * @param var property
     * @return true if property is a value type
     */

    protected boolean isValueType(CodegenProperty var) {
        return (this.getValueTypes().contains(var.dataType) || var.isEnum);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        boolean hasAllowableValues = p.allowableValues != null && !p.allowableValues.isEmpty();
        if (hasAllowableValues) {
            //support examples for inline enums
            final List<Object> values = (List<Object>) p.allowableValues.get("values");
            example = String.valueOf(values.get(0));
        } else if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if (p.isString) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (p.isInteger || p.isShort) {
            if (example == null) {
                example = "56";
            }
        } else if (p.isLong) {
            if (example == null) {
                example = "789";
            }
            example = StringUtils.appendIfMissingIgnoreCase(example, "L");
        } else if (p.isFloat) {
            if (example == null) {
                example = "3.4F";
            }
            example = StringUtils.appendIfMissingIgnoreCase(example, "F");
        } else if (p.isDouble) {
            if (example == null) {
                example = "1.2D";
            }
            example = StringUtils.appendIfMissingIgnoreCase(example, "D");
        } else if (p.isNumber) {
            if (example == null) {
                example = "8.14";
            }
            example = StringUtils.appendIfMissingIgnoreCase(example, "D");
        } else if (p.isBoolean) {
            if (example == null) {
                example = "true";
            }
        } else if (p.isBinary || p.isFile) {
            if (example == null) {
                example = "/path/to/file.txt";
            }
            example = "new System.IO.MemoryStream(System.IO.File.ReadAllBytes(\"" + escapeText(example) + "\"))";
        } else if (p.isByteArray) {
            if (example == null) {
                example = "BYTE_ARRAY_DATA_HERE";
            }
            example = "System.Text.Encoding.ASCII.GetBytes(\"" + escapeText(example) + "\")";
        } else if (p.isDate) {
            if (example == null) {
                example = "DateTime.Parse(\"2013-10-20\")";
            } else {
                example = "DateTime.Parse(\"" + example + "\")";
            }
        } else if (p.isDateTime) {
            if (example == null) {
                example = "DateTime.Parse(\"2013-10-20T19:20:30+01:00\")";
            } else {
                example = "DateTime.Parse(\"" + example + "\")";
            }
        } else if (p.isDecimal) {
            if (example == null) {
                example = "8.9M";
            }
            example = StringUtils.appendIfMissingIgnoreCase(example, "M");
        } else if (p.isUuid) {
            if (example == null) {
                example = "\"38400000-8cf0-11bd-b23e-10b96e4ef00d\"";
            } else {
                example = "\"" + example + "\"";
            }
        } else if (p.isUri) {
            if (example == null) {
                example = "new Uri(\"https://openapi-generator.tech\")";
            } else {
                example = "new Uri(\"" + example + "\")";
            }
        } else if (hasAllowableValues) {
            //parameter is enum defined as a schema component
            example = "(" + type + ") \"" + example + "\"";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + type + "()";
        }

        if (example == null) {
            example = "null";
        } else if (Boolean.TRUE.equals(p.isArray)) {
            if (p.items.defaultValue != null) {
                String innerExample;
                if ("String".equals(p.items.dataType)) {
                    innerExample = "\"" + p.items.defaultValue + "\"";
                } else {
                    innerExample = p.items.defaultValue;
                }
                example = "new List<" + p.items.dataType + ">({" + innerExample + "})";
            } else {
                example = "new List<" + p.items.dataType + ">()";
            }
        } else if (Boolean.TRUE.equals(p.isModel)) {
            example = "new " + p.dataType + "()";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            if (p.items != null) {
                example = "new Dictionary<String, " + p.items.dataType + ">";
            } else {
                // default to String if item is not defined
                example = "new Dictionary<String, String>";
            }
        }

        p.example = example;
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String csharpPostProcessFile = System.getenv("CSHARP_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(csharpPostProcessFile)) {
            return; // skip if CSHARP_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with .cs extension
        if ("cs".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = csharpPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
    }

    /*
     * See https://msdn.microsoft.com/en-us/library/yd1hzczs(v=vs.110).aspx for .NET options.
     */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            // check if the pattern has any modifiers
            // gmiyuvsd - ecma modifiers
            // l - legacy modifier provided by this library, provides a way to opt out of culture invariant
            // nx - c# modifiers https://learn.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-options
            Pattern hasModifiers = Pattern.compile(".*/[gmiyuvsdlnx]+$");

            int end = hasModifiers.matcher(pattern).find()
                ? pattern.lastIndexOf('/')
                : pattern.length() - 1;

            int start = pattern.startsWith("/")
                ? 1
                : 0;

            Map<Character, String> optionsMap = new HashMap();
            optionsMap.put('i', "IgnoreCase");
            optionsMap.put('m', "Multiline");
            optionsMap.put('s', "SingleLine");
            optionsMap.put('n', "ExplicitCapture");
            optionsMap.put('x', "IgnorePatternWhitespace");

            List<String> modifiers = new ArrayList<String>();
            modifiers.add("CultureInvariant");

            for (char c : pattern.substring(end).toCharArray()) {
                if (optionsMap.containsKey(c)) {
                    modifiers.add(optionsMap.get(c));
                } else if (c == 'l'){
                    modifiers.remove("CultureInvariant");
                } else {
                    vendorExtensions.put("x-modifier-" + c, c);
                }
            }

            String regex = pattern.substring(start, end).replace("'", "\'").replace("\"", "\"\"");
            vendorExtensions.put("x-regex", regex);
            vendorExtensions.put("x-modifiers", modifiers);
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.C_SHARP;
    }

    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

    @Override
    public String addRegularExpressionDelimiter(String pattern) {
        if (StringUtils.isEmpty(pattern)) {
            return pattern;
        }

        if (!pattern.matches("^/.*")) {
            return "/" + pattern + "/";
        }

        return pattern;
    }

    @Deprecated
    protected Set<String> getNullableTypes() {
        throw new RuntimeException("This method should no longer be used.");
    }

    protected Set<String> getValueTypes() {
        return new HashSet<>(Arrays.asList("decimal", "bool", "int", "uint", "long", "ulong", "float", "double", "DateTime", "DateTimeOffset", "Guid"));
    }

    protected void setTypeMapping() {
        typeMapping = new HashMap<>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("boolean", "bool");
        typeMapping.put("integer", "int");
        typeMapping.put("UnsignedInteger", "uint");
        typeMapping.put("UnsignedLong", "ulong");
        typeMapping.put("long", "long");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("number", "decimal");
        typeMapping.put("decimal", "decimal");
        typeMapping.put("BigDecimal", "decimal");
        typeMapping.put("DateTime", this.useDateTimeOffsetFlag ? "DateTimeOffset" : "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");
        typeMapping.put("UUID", "Guid");
        typeMapping.put("URI", "string");
        typeMapping.put("AnyType", "Object");

        if (this.useCollection) {
            typeMapping.put("array", "Collection");
            typeMapping.put("list", "Collection");
        }
    }
}
