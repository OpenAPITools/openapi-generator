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
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.model.WebhooksMap;
import org.openapitools.codegen.templating.mustache.*;
import org.openapitools.codegen.templating.mustache.CopyLambda.CopyContent;
import org.openapitools.codegen.templating.mustache.CopyLambda.WhiteSpaceStrategy;
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
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractCSharpCodegen extends DefaultCodegen {

    protected boolean optionalAssemblyInfoFlag = true;
    protected boolean optionalEmitDefaultValuesFlag = false;
    protected boolean conditionalSerialization = false;
    protected boolean optionalProjectFileFlag = true;
    @Setter protected boolean optionalMethodArgumentFlag = true;
    protected boolean useDateTimeOffsetFlag = false;
    protected boolean useDateTimeForDateFlag = false;
    protected boolean useCollection = false;
    @Setter protected boolean returnICollection = false;
    @Setter protected boolean netCoreProjectFileFlag = false;
    protected boolean nullReferenceTypesFlag = false;
    protected boolean useSourceGeneration = false;

    protected String modelPropertyNaming = CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.PascalCase.name();

    @Setter protected String licenseUrl = "http://localhost";
    @Setter protected String licenseName = "NoLicense";

    @Setter protected String packageVersion = "1.0.0";
    @Setter protected String packageName = "Org.OpenAPITools";
    @Setter protected String packageTitle = "OpenAPI Library";
    @Setter protected String packageProductName = "OpenAPILibrary";
    @Setter protected String packageDescription = "A library generated from a OpenAPI doc";
    @Setter protected String packageCompany = "OpenAPI";
    @Setter protected String packageCopyright = "No Copyright";
    @Setter protected String packageAuthors = "OpenAPI";
    public static final String DATE_FORMAT = "dateFormat";
    @Setter protected String dateFormat = "yyyy'-'MM'-'dd";
    public static final String DATETIME_FORMAT = "dateTimeFormat";
    @Setter protected String dateTimeFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffffffK";

    @Getter @Setter
    protected String interfacePrefix = "I";
    @Setter protected String enumNameSuffix = "Enum";
    @Setter protected String enumValueSuffix = "Enum";

    @Setter protected String sourceFolder = "src";
    protected static final String invalidParameterNamePrefix = "var";
    protected static final String invalidPropertyNamePrefix = "Var";
    @Getter protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase;

    // TODO: Add option for test folder output location. Nice to allow e.g. ./test instead of ./src.
    //       This would require updating relative paths (e.g. path to main project file in test project file)
    @Setter protected String testFolder = sourceFolder;

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    // true if support nullable type
    @Getter @Setter
    protected boolean supportNullable = Boolean.FALSE;

    @Setter protected Boolean zeroBasedEnums = null;
    protected static final String zeroBasedEnumVendorExtension = "x-zero-based-enum";

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractCSharpCodegen.class);

    // special property keywords not allowed as these are the function names in the model files
    protected Set<String> propertySpecialKeywords = new HashSet<>(Arrays.asList("ToString", "ToJson", "GetHashCode", "Equals", "ShouldSerializeToString"));

    // A cache to efficiently lookup schema `toModelName()` based on the schema Key
    private final Map<String, String> schemaKeyToModelNameCache = new HashMap<>();

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
                        "Client", "client", "parameter", "Configuration", "Version", "Environment",
                        "TimeZone", "OperatingSystem",
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
                        "DateOnly?",
                        "DateOnly",
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
    }

    public void setUseCollection(boolean useCollection) {
        this.useCollection = useCollection;
        if (useCollection) {
            instantiationTypes.put("array", "Collection");
            instantiationTypes.put("list", "Collection");
        }
        this.setTypeMapping();
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        this.setTypeMapping();
    }

    public void useDateTimeForDate(boolean flag) {
        this.useDateTimeForDateFlag = flag;
        this.setTypeMapping();
    }

    @Override
    protected void addParentFromContainer(CodegenModel model, Schema schema) {
        // we do not want to inherit simply because additionalProperties is true
        // do nothing here
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("CSHARP_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable CSHARP_POST_PROCESS_FILE not defined so the C# code may not be properly formatted by uncrustify (0.66 or later) or other code formatter. To define it, try `export CSHARP_POST_PROCESS_FILE=\"/usr/local/bin/uncrustify --no-backup\" && export UNCRUSTIFY_CONFIG=/path/to/uncrustify-rules.cfg` (Linux/Mac). Note: replace /path/to with the location of uncrustify-rules.cfg");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'CSHARP_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
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

        // {{useDateTimeForDate}}
        if (additionalProperties.containsKey(CodegenConstants.USE_DATETIME_FOR_DATE)) {
            useDateTimeForDate(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_DATETIME_FOR_DATE));
        } else {
            additionalProperties.put(CodegenConstants.USE_DATETIME_FOR_DATE, useDateTimeForDateFlag);
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

        if (additionalProperties.containsKey(CodegenConstants.ENUM_PROPERTY_NAMING)) {
            setEnumPropertyNaming((String) additionalProperties.get(CodegenConstants.ENUM_PROPERTY_NAMING));
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
        final CopyContent copyContent = new CopyContent();

        return super.addMustacheLambdas()
                .put("camelcase_sanitize_param", new CamelCaseAndSanitizeLambda().generator(this).escapeAsParamName(true))
                .put("required", new RequiredParameterLambda())
                .put("optional", new OptionalParameterLambda().generator(this))
                .put("joinWithComma", new JoinWithCommaLambda())
                .put("joinWithAmpersand", new JoinWithCommaLambda(true, "  ", " && "))
                .put("joinLinesWithComma", new JoinWithCommaLambda(false, "\n", ",\n"))
                .put("joinConditions", new JoinWithCommaLambda(true, "  ", " && "))
                .put("trimLineBreaks", new TrimLineBreaksLambda())
                .put("trimTrailingWithNewLine", new TrimTrailingWhiteSpaceLambda(true))
                .put("trimTrailing", new TrimTrailingWhiteSpaceLambda(false))
                .put("first", new FirstLambda("  "))
                .put("firstDot", new FirstLambda("\\."))
                .put("indent1", new IndentedLambda(4, " ", false, true))
                .put("indentAll1", new IndentedLambda(4, " ", true, true))
                .put("indent3", new IndentedLambda(12, " ", false, true))
                .put("indent4", new IndentedLambda(16, " ", false, true))
                .put("copy", new CopyLambda(copyContent, WhiteSpaceStrategy.None, WhiteSpaceStrategy.None))
                .put("copyText", new CopyLambda(copyContent, WhiteSpaceStrategy.Strip, WhiteSpaceStrategy.StripLineBreakIfPresent))
                .put("paste", new PasteLambda(copyContent, false))
                .put("pasteOnce", new PasteLambda(copyContent, true))
                .put("uniqueLines", new UniqueLambda("\n", false))
                .put("unique", new UniqueLambda("\n", true))
                .put("camel_case", new CamelCaseLambda())
                .put("escape_reserved_word", new EscapeKeywordLambda(this::escapeKeyword))
                .put("alphabet_or_underscore", new ReplaceAllLambda("[^A-Za-z]", "_"));
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (property.isInnerEnum && property.items != null) {
            // format maps of inner enums to include the classname eg: Dictionary<string, MapTest.InnerEnum>
            property.datatypeWithEnum = property.datatypeWithEnum.replace(property.items.datatypeWithEnum, model.classname + "." + property.items.datatypeWithEnum);
            property.dataType = property.datatypeWithEnum;
        }

        if (property.isEnum && !property.vendorExtensions.containsKey(AbstractCSharpCodegen.zeroBasedEnumVendorExtension)) {
            if (Boolean.TRUE.equals(this.zeroBasedEnums)) {
                property.vendorExtensions.put(AbstractCSharpCodegen.zeroBasedEnumVendorExtension, true);
            } else if (!Boolean.FALSE.equals(this.zeroBasedEnums)) {
                if (property.allowableValues.containsKey("values")) {
                    final List<?> allowableValues = (List<?>) property.allowableValues.get("values");
                    boolean isZeroBased = String.valueOf(allowableValues.get(0)).toLowerCase(Locale.ROOT).equals("unknown");
                    property.vendorExtensions.put(AbstractCSharpCodegen.zeroBasedEnumVendorExtension, isZeroBased);
                }
            }
        }

        if (property.isMap || property.isContainer) {
            // maps of enums will be marked both isMap and isEnum, correct that now
            property.isEnum = false;
            property.isInnerEnum = false;
            property.isString = false;
        }

        Double maximum = asDouble(property.maximum);
        if (property.dataType.equals("int") && maximum != null) {
            if ((!property.exclusiveMaximum && asInteger(property.maximum) == null) || (property.exclusiveMaximum && asInteger((maximum + 1) + "") == null)) {
                property.dataType = "long";
                property.datatypeWithEnum = "long";
            }
        }

        Double minimum = asDouble(property.minimum);
        if (property.dataType.equals("int") && minimum != null) {
            if ((!property.exclusiveMinimum && asInteger(property.minimum) == null) || (property.exclusiveMinimum && asInteger((minimum - 1) + "") == null)) {
                property.dataType = "long";
                property.datatypeWithEnum = "long";
            }
        }
    }

    /**
     * If the value can be parsed as a double, returns the value, otherwise returns null
     */
    public static Double asDouble(String strNum) {
        if (strNum == null) {
            return null;
        }
        try {
            return Double.parseDouble(strNum);
        } catch (NumberFormatException nfe) {
            return null;
        }
    }

    /**
     * If the value can be parsed as an integer, returns the value, otherwise returns null
     */
    public static Integer asInteger(String strNum) {
        if (strNum == null) {
            return null;
        }
        try {
            return Integer.parseInt(strNum);
        } catch (NumberFormatException nfe) {
            return null;
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

            if (cm.isEnum && !cm.vendorExtensions.containsKey(AbstractCSharpCodegen.zeroBasedEnumVendorExtension)) {
                if (Boolean.TRUE.equals(this.zeroBasedEnums)) {
                    cm.vendorExtensions.put(AbstractCSharpCodegen.zeroBasedEnumVendorExtension, true);
                } else if (!Boolean.FALSE.equals(this.zeroBasedEnums)) {
                    if (cm.allowableValues.containsKey("values")) {
                        final List<?> allowableValues = (List<?>) cm.allowableValues.get("values");
                        boolean isZeroBased = String.valueOf(allowableValues.get(0)).toLowerCase(Locale.ROOT).equals("unknown");
                        cm.vendorExtensions.put(AbstractCSharpCodegen.zeroBasedEnumVendorExtension, isZeroBased);
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
            if (model == null) {
                continue;
            }

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
            if (model == null) {
                continue;
            }

            model.vendorExtensions.put("x-model-is-mutable", modelIsMutable(model, null));

            CodegenComposedSchemas composedSchemas = model.getComposedSchemas();
            if (composedSchemas != null) {
                List<CodegenProperty> allOf = composedSchemas.getAllOf();
                if (allOf != null) {
                    for (CodegenProperty property : allOf) {
                        property.name = patchPropertyName(model, camelize(property.baseType));
                        patchPropertyVendorExtensions(property);
                    }
                }

                List<CodegenProperty> anyOf = composedSchemas.getAnyOf();
                if (anyOf != null) {
                    removePropertiesDeclaredInComposedTypes(objs, model, anyOf);
                    for (CodegenProperty property : anyOf) {
                        property.name = patchPropertyName(model, camelize(property.baseType));
                        property.isNullable = true;
                        patchPropertyVendorExtensions(property);
                        property.vendorExtensions.put("x-base-name", model.name.substring(model.name.lastIndexOf('_') + 1));
                    }
                }

                List<CodegenProperty> oneOf = composedSchemas.getOneOf();
                if (oneOf != null) {
                    removePropertiesDeclaredInComposedTypes(objs, model, oneOf);
                    for (CodegenProperty property : oneOf) {
                        property.name = patchPropertyName(model, camelize(property.baseType));
                        property.isNullable = true;
                        patchPropertyVendorExtensions(property);
                        property.vendorExtensions.put("x-base-name", model.name.substring(model.name.lastIndexOf('_') + 1));
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

            List<CodegenProperty> overriddenProperties = model.vars.stream().filter(v -> model.allVars.stream().anyMatch(a -> a.baseName.equals(v.baseName) && a.dataType != v.dataType)).collect(Collectors.toList());
            for (CodegenProperty overridden : overriddenProperties) {
                // if the current model overrides an allOf property, use the overridden property
                model.allVars.set(model.allVars.indexOf(model.allVars.stream().filter(a -> a.baseName.equals(overridden.baseName)).findFirst().get()), overridden);
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
    private boolean modelIsMutable(CodegenModel model, Set<String> processed) {
        if (processed == null) {
            processed = new HashSet<String>();
        }
        boolean isMutable = model.allVars.stream().anyMatch(v -> !v.isReadOnly);
        if (!isMutable && !processed.contains(model.classname) && model.getDiscriminator() != null && model.getDiscriminator().getMappedModels() != null) {
            processed.add(model.classname);
            isMutable = modelIsMutable(model, processed);
        }

        return isMutable;
    }

    protected void removePropertiesDeclaredInComposedTypes(Map<String, ModelsMap> objs, CodegenModel model, List<CodegenProperty> composedProperties) {
    }

    private String patchPropertyName(CodegenModel model, String value) {
        String name = escapeReservedWord(model, value);

        if (name.startsWith(AbstractCSharpCodegen.invalidParameterNamePrefix)) {
            name = AbstractCSharpCodegen.invalidPropertyNamePrefix + name.substring(AbstractCSharpCodegen.invalidParameterNamePrefix.length());
        }

        return name;
    }

    private void patchPropertyVendorExtensions(CodegenProperty property) {
        boolean isValueType = isValueType(property);
        property.vendorExtensions.put("x-is-value-type", isValueType);
        property.vendorExtensions.put("x-is-reference-type", !isValueType);
        property.vendorExtensions.put("x-is-nullable-type", this.getNullableReferencesTypes() || isValueType);
        property.vendorExtensions.put("x-is-base-or-new-discriminator", (property.isDiscriminator && !property.isInherited) || (property.isDiscriminator && property.isNew));
    }

    protected void patchPropertyIsInherited(CodegenModel model, CodegenProperty property) {
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

        this.patchPropertyIsInherited(model, property);

        patchPropertyVendorExtensions(property);

        property.name = patchPropertyName(model, property.name);

        String[] nestedTypes = {"List", "Collection", "ICollection", "Dictionary"};

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

        String[] numericTypes = {"double", "double?", "decimal", "decimal", "float", "float?", "int", "int?", "long", "long?", "ulong", "ulong?"};
        enumVars.forEach((enumVar) -> {
            enumVar.put("isNumeric", Arrays.asList(numericTypes).contains(dataType));
        });

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

    private void postProcessResponseCode(CodegenResponse response, String status, Set<String> httpStatusesWithReturn) {
        response.vendorExtensions.put("x-http-status", status);
        if (response.dataType != null) {
            httpStatusesWithReturn.add(status);
        }
    }

    @Override
    public WebhooksMap postProcessWebhooksWithModels(WebhooksMap objs, List<ModelMap> allModels) {
        super.postProcessWebhooksWithModels(objs, allModels);

        if (objs != null) {
            OperationMap operationMap = objs.getWebhooks();
            this.postProcessOperations(operationMap, allModels);
        }

        return objs;
    }

    private HashMap<String, String> duplicateOf = new HashMap<String, String>();

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);

        if (objs != null) {
            OperationMap operations = objs.getOperations();
            this.postProcessOperations(operations, allModels);
        }

        return objs;
    }

    private void postProcessOperations(OperationMap operations, List<ModelMap> allModels) {
        Set<String> httpStatusesWithReturn = additionalProperties.get("x-http-statuses-with-return") instanceof Set<?>
                ? (Set<String>) additionalProperties.get("x-http-statuses-with-return")
                : new HashSet<String>();

        additionalProperties.put("x-http-statuses-with-return", httpStatusesWithReturn);

        HashMap<String, CodegenModel> modelMaps = ModelMap.toCodegenModelMap(allModels);

        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                String duplicates = duplicateOf.get(operation.operationId);
                if (duplicates != null) {
                    operation.vendorExtensions.put("x-duplicates", duplicates);
                } else {
                    duplicateOf.put(operation.operationId, operations.getClassname());
                }
                if (operation.responses != null) {
                    for (CodegenResponse response : operation.responses) {

                        if (response.returnProperty != null) {
                            Boolean isValueType = isValueType(response.returnProperty);
                            response.vendorExtensions.put("x-is-value-type", isValueType);
                            response.vendorExtensions.put("x-is-reference-type", !isValueType);
                        }

                        if (response.headers != null && response.headers.stream().anyMatch(h -> h.baseName.equals("Set-Cookie"))) {
                            response.vendorExtensions.put("x-set-cookie", true);
                            operation.vendorExtensions.put("x-set-cookie", true);
                        }

                        String code = response.code.toLowerCase(Locale.ROOT);
                        switch (code) {
                            case "default":
                            case "0":
                                postProcessResponseCode(response, "Default", httpStatusesWithReturn);
                                response.vendorExtensions.put("x-http-status-is-default", true);
                                if ((long) operation.responses.size() == 1) {
                                    response.vendorExtensions.put("x-only-default", true);
                                }
                                break;
                            case "100":
                                postProcessResponseCode(response, "Continue", httpStatusesWithReturn);
                                break;
                            case "101":
                                postProcessResponseCode(response, "SwitchingProtocols", httpStatusesWithReturn);
                                break;
                            case "102":
                                postProcessResponseCode(response, "Processing", httpStatusesWithReturn);
                                break;
                            case "103":
                                postProcessResponseCode(response, "EarlyHints", httpStatusesWithReturn);
                                break;
                            case "200":
                                postProcessResponseCode(response, "Ok", httpStatusesWithReturn);
                                break;
                            case "201":
                                postProcessResponseCode(response, "Created", httpStatusesWithReturn);
                                break;
                            case "202":
                                postProcessResponseCode(response, "Accepted", httpStatusesWithReturn);
                                break;
                            case "203":
                                postProcessResponseCode(response, "NonAuthoritativeInformation", httpStatusesWithReturn);
                                break;
                            case "204":
                                postProcessResponseCode(response, "NoContent", httpStatusesWithReturn);
                                break;
                            case "205":
                                postProcessResponseCode(response, "ResetContent", httpStatusesWithReturn);
                                break;
                            case "206":
                                postProcessResponseCode(response, "PartialContent", httpStatusesWithReturn);
                                break;
                            case "207":
                                postProcessResponseCode(response, "MultiStatus", httpStatusesWithReturn);
                                break;
                            case "208":
                                postProcessResponseCode(response, "AlreadyImported", httpStatusesWithReturn);
                                break;
                            case "226":
                                postProcessResponseCode(response, "IMUsed", httpStatusesWithReturn);
                                break;
                            case "300":
                                postProcessResponseCode(response, "MultipleChoices", httpStatusesWithReturn);
                                break;
                            case "301":
                                postProcessResponseCode(response, "MovedPermanently", httpStatusesWithReturn);
                                break;
                            case "302":
                                postProcessResponseCode(response, "Found", httpStatusesWithReturn);
                                break;
                            case "303":
                                postProcessResponseCode(response, "SeeOther", httpStatusesWithReturn);
                                break;
                            case "304":
                                postProcessResponseCode(response, "NotModified", httpStatusesWithReturn);
                                break;
                            case "307":
                                postProcessResponseCode(response, "TemporaryRedirect", httpStatusesWithReturn);
                                break;
                            case "308":
                                postProcessResponseCode(response, "PermanentRedirect", httpStatusesWithReturn);
                                break;
                            case "400":
                                postProcessResponseCode(response, "BadRequest", httpStatusesWithReturn);
                                break;
                            case "401":
                                postProcessResponseCode(response, "Unauthorized", httpStatusesWithReturn);
                                break;
                            case "402":
                                postProcessResponseCode(response, "PaymentRequired", httpStatusesWithReturn);
                                break;
                            case "403":
                                postProcessResponseCode(response, "Forbidden", httpStatusesWithReturn);
                                break;
                            case "404":
                                postProcessResponseCode(response, "NotFound", httpStatusesWithReturn);
                                break;
                            case "405":
                                postProcessResponseCode(response, "MethodNotAllowed", httpStatusesWithReturn);
                                break;
                            case "406":
                                postProcessResponseCode(response, "NotAcceptable", httpStatusesWithReturn);
                                break;
                            case "407":
                                postProcessResponseCode(response, "ProxyAuthenticationRequired", httpStatusesWithReturn);
                                break;
                            case "408":
                                postProcessResponseCode(response, "RequestTimeout", httpStatusesWithReturn);
                                break;
                            case "409":
                                postProcessResponseCode(response, "Conflict", httpStatusesWithReturn);
                                break;
                            case "410":
                                postProcessResponseCode(response, "Gone", httpStatusesWithReturn);
                                break;
                            case "411":
                                postProcessResponseCode(response, "LengthRequired", httpStatusesWithReturn);
                                break;
                            case "412":
                                postProcessResponseCode(response, "PreconditionFailed", httpStatusesWithReturn);
                                break;
                            case "413":
                                postProcessResponseCode(response, "ContentTooLarge", httpStatusesWithReturn);
                                break;
                            case "414":
                                postProcessResponseCode(response, "URITooLong", httpStatusesWithReturn);
                                break;
                            case "415":
                                postProcessResponseCode(response, "UnsupportedMediaType", httpStatusesWithReturn);
                                break;
                            case "416":
                                postProcessResponseCode(response, "RangeNotSatisfiable", httpStatusesWithReturn);
                                break;
                            case "417":
                                postProcessResponseCode(response, "ExpectationFailed", httpStatusesWithReturn);
                                break;
                            case "421":
                                postProcessResponseCode(response, "MisdirectedRequest", httpStatusesWithReturn);
                                break;
                            case "422":
                                postProcessResponseCode(response, "UnprocessableContent", httpStatusesWithReturn);
                                break;
                            case "423":
                                postProcessResponseCode(response, "Locked", httpStatusesWithReturn);
                                break;
                            case "424":
                                postProcessResponseCode(response, "FailedDependency", httpStatusesWithReturn);
                                break;
                            case "425":
                                postProcessResponseCode(response, "TooEarly", httpStatusesWithReturn);
                                break;
                            case "426":
                                postProcessResponseCode(response, "UpgradeRequired", httpStatusesWithReturn);
                                break;
                            case "428":
                                postProcessResponseCode(response, "PreconditionRequired", httpStatusesWithReturn);
                                break;
                            case "429":
                                postProcessResponseCode(response, "TooManyRequests", httpStatusesWithReturn);
                                break;
                            case "431":
                                postProcessResponseCode(response, "RequestHeaderFieldsTooLong", httpStatusesWithReturn);
                                break;
                            case "451":
                                postProcessResponseCode(response, "UnavailableForLegalReasons", httpStatusesWithReturn);
                                break;
                            case "500":
                                postProcessResponseCode(response, "InternalServerError", httpStatusesWithReturn);
                                break;
                            case "501":
                                postProcessResponseCode(response, "NotImplemented", httpStatusesWithReturn);
                                break;
                            case "502":
                                postProcessResponseCode(response, "BadGateway", httpStatusesWithReturn);
                                break;
                            case "503":
                                postProcessResponseCode(response, "ServiceUnavailable", httpStatusesWithReturn);
                                break;
                            case "504":
                                postProcessResponseCode(response, "GatewayTimeout", httpStatusesWithReturn);
                                break;
                            case "505":
                                postProcessResponseCode(response, "HttpVersionNotSupported", httpStatusesWithReturn);
                                break;
                            case "506":
                                postProcessResponseCode(response, "VariantAlsoNegotiates", httpStatusesWithReturn);
                                break;
                            case "507":
                                postProcessResponseCode(response, "InsufficientStorage", httpStatusesWithReturn);
                                break;
                            case "508":
                                postProcessResponseCode(response, "LoopDetected", httpStatusesWithReturn);
                                break;
                            case "511":
                                postProcessResponseCode(response, "NetworkAuthenticationRequired", httpStatusesWithReturn);
                                break;
                            case "1xx":
                                response.vendorExtensions.put("x-http-status-range", 1);
                                postProcessResponseCode(response, "HttpStatusCode1XX", httpStatusesWithReturn);
                                break;
                            case "2xx":
                                response.vendorExtensions.put("x-http-status-range", 2);
                                postProcessResponseCode(response, "HttpStatusCode2XX", httpStatusesWithReturn);
                                break;
                            case "3xx":
                                response.vendorExtensions.put("x-http-status-range", 3);
                                postProcessResponseCode(response, "HttpStatusCode3XX", httpStatusesWithReturn);
                                break;
                            case "4xx":
                                response.vendorExtensions.put("x-http-status-range", 4);
                                postProcessResponseCode(response, "HttpStatusCode4XX", httpStatusesWithReturn);
                                break;
                            case "5xx":
                                response.vendorExtensions.put("x-http-status-range", 5);
                                postProcessResponseCode(response, "HttpStatusCode5XX", httpStatusesWithReturn);
                                break;
                            default:
                                postProcessResponseCode(response, "CustomHttpStatusCode" + code, httpStatusesWithReturn);
                        }
                    }
                }

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
                        operation.isMap = this.mapTypes.stream().anyMatch(typeMapping::startsWith);
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
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.bodyParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.cookieParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.formParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.headerParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.implicitHeadersParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.optionalParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.pathParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.queryParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.notNullableParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                for (CodegenParameter parameter : operation.requiredParams) {
                    CodegenModel model = getModelFromParameter(modelMaps, parameter);
                    patchParameter(model, parameter);
                }

                List<CodegenParameter> referenceTypes = operation.allParams.stream().filter(p -> p.vendorExtensions.get("x-is-value-type") == null && !p.isNullable).collect(Collectors.toList());
                operation.vendorExtensions.put("x-not-nullable-reference-types", referenceTypes);
                operation.vendorExtensions.put("x-has-not-nullable-reference-types", referenceTypes.size() > 0);
                processOperation(operation);

                // Remove constant params from allParams list and add to constantParams
                handleConstantParams(operation);
            }
        }
    }

    protected void patchVendorExtensionNullableValueType(CodegenParameter parameter) {
        if (parameter.isNullable && !parameter.isContainer && (this.getValueTypes().contains(parameter.dataType) || parameter.isEnum)) {
            parameter.vendorExtensions.put("x-nullable-value-type", true);
        }
    }

    /**
     * Returns the model related to the given parameter
     */
    private CodegenModel getModelFromParameter(HashMap<String, CodegenModel> allModels, CodegenParameter parameter) {
        return allModels.getOrDefault(parameter.dataType, null);
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
        String[] nestedTypes = {"List", "Collection", "ICollection", "Dictionary"};

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

        return name.equals(model.getClassname())
                ? AbstractCSharpCodegen.invalidParameterNamePrefix + camelize(name)
                : name;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (isReservedWord(name) ||
                name.matches("^\\d.*")) {
            name = AbstractCSharpCodegen.invalidParameterNamePrefix + camelize(name);
        }
        return name;
    }

    public String escapeKeyword(String value) {
        return isReservedWord(value) ? "@" + value : value;
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
    private String getArrayTypeDeclaration(Schema arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Schema<?> items = ModelUtils.getSchemaItems(arr);
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        instantiationType.append("<").append(nestedType).append(">");
        return instantiationType.toString();
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration(p);
        }
        return super.toInstantiationType(p);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration(p);
        } else if (ModelUtils.isMapSchema(p)) {
            // Should we also support maps of maps?
            Schema<?> inner = ModelUtils.getAdditionalProperties(p);
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

    public void setEnumPropertyNaming(final String enumPropertyNamingType) {
        try {
            this.enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.valueOf(enumPropertyNamingType);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumPropertyNamingType + " is an invalid enum property naming option. Please choose from:");
            for (CodegenConstants.ENUM_PROPERTY_NAMING_TYPE t : CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
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
        if (enumNameMapping.containsKey(name)) {
            return enumNameMapping.get(name);
        }

        if (name.length() == 0) {
            return adjustNamingStyle("Empty");
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return adjustNamingStyle(getSymbolName(name));
        }

        String enumName = sanitizeName(name);

        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = adjustNamingStyle(enumName) + this.enumValueSuffix;

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    /**
     * Adjust the naming style of a given name based on the enumPropertyNaming option.
     *
     * @param name The original name
     * @return The adjusted name
     */
    private String adjustNamingStyle(String name) {
        switch (getEnumPropertyNaming()) {
            case camelCase:
                // NOTE: Removes hyphens and underscores
                return camelize(name, LOWERCASE_FIRST_LETTER);
            case PascalCase:
                // NOTE: Removes hyphens and underscores
                return camelize(name);
            case snake_case:
                // NOTE: Removes hyphens
                return underscore(name);
            case UPPERCASE:
                return underscore(name).toUpperCase(Locale.ROOT);
            default:
                return name;
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

    protected boolean useNet60OrLater() {
        return false;
    }

    protected boolean useDateOnly() {
        return useNet60OrLater() && !useDateTimeForDateFlag;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        boolean hasAllowableValues = p.allowableValues != null && !p.allowableValues.isEmpty();
        if (hasAllowableValues) {
            //support examples for inline enums
            final List<?> values = (List<?>) p.allowableValues.get("values");
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
            String dateType = typeMapping.get("date");
            if (example == null) {
                example = dateType + ".Parse(\"2013-10-20\")";
            } else {
                example = dateType + ".Parse(\"" + example + "\")";
            }
        } else if (p.isDateTime) {
            String dateType = typeMapping.get("DateTime");
            if (example == null) {
                example = dateType + ".Parse(\"2013-10-20T19:20:30+01:00\")";
            } else {
                example = dateType + ".Parse(\"" + example + "\")";
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
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }

        String csharpPostProcessFile = System.getenv("CSHARP_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(csharpPostProcessFile)) {
            return; // skip if CSHARP_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with .cs extension
        if ("cs".equals(FilenameUtils.getExtension(file.toString()))) {
            this.executePostProcessor(new String[]{csharpPostProcessFile, file.toString()});
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

            Map<Character, String> optionsMap = new HashMap<Character, String>();
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
                } else if (c == 'l') {
                    modifiers.remove("CultureInvariant");
                } else {
                    vendorExtensions.put("x-modifier-" + c, c);
                }
            }

            String regex = pattern.substring(start, end).replace("\"", "\"\"");
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
        return new HashSet<>(Arrays.asList("decimal", "bool", "int", "uint", "long", "ulong", "float", "double", "DateTime", "DateOnly", "DateTimeOffset", "Guid"));
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
        typeMapping.put("date", this.useDateOnly() ? "DateOnly" : "DateTime");
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
