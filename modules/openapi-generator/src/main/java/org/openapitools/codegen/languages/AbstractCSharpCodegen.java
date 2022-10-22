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

    protected String interfacePrefix = "I";
    protected String enumNameSuffix = "Enum";
    protected String enumValueSuffix = "Enum";

    protected String sourceFolder = "src";

    // TODO: Add option for test folder output location. Nice to allow e.g. ./test instead of ./src.
    //       This would require updating relative paths (e.g. path to main project file in test project file)
    protected String testFolder = sourceFolder;

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    // true if support nullable type
    protected boolean supportNullable = Boolean.FALSE;

    // nullable type
    protected Set<String> nullableType = new HashSet<>();

    protected Set<String> valueTypes = new HashSet<>();

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
                        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw",
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
                        "long?",
                        "long",
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


        // Nullable types here assume C# 2 support is not part of base
        typeMapping = new HashMap<>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("boolean", "bool?");
        typeMapping.put("integer", "int?");
        typeMapping.put("float", "float?");
        typeMapping.put("long", "long?");
        typeMapping.put("double", "double?");
        typeMapping.put("number", "decimal?");
        typeMapping.put("BigDecimal", "decimal?");
        typeMapping.put("DateTime", "DateTime?");
        typeMapping.put("date", "DateTime?");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");
        typeMapping.put("UUID", "Guid?");
        typeMapping.put("URI", "string");
        typeMapping.put("AnyType", "Object");

        // nullable type
        nullableType = new HashSet<>(
                Arrays.asList("decimal", "bool", "int", "float", "long", "double", "DateTime", "DateTimeOffset", "Guid")
        );
        // value Types
        valueTypes = new HashSet<>(
                Arrays.asList("decimal", "bool", "int", "float", "long", "double")
        );

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
            typeMapping.put("array", "Collection");
            typeMapping.put("list", "Collection");

            instantiationTypes.put("array", "Collection");
            instantiationTypes.put("list", "Collection");
        }
    }

    public void setOptionalMethodArgumentFlag(boolean flag) {
        this.optionalMethodArgumentFlag = flag;
    }

    public void setNetCoreProjectFileFlag(boolean flag) {
        this.netCoreProjectFileFlag = flag;
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        if (flag) {
            typeMapping.put("DateTime", "DateTimeOffset");
        } else {
            typeMapping.put("DateTime", "DateTime");
        }
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
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("camelcase_param", new CamelCaseLambda().generator(this).escapeAsParamName(true))
                .put("required", new RequiredParameterLambda().generator(this))
                .put("optional", new OptionalParameterLambda().generator(this))
                .put("joinWithComma", new JoinWithCommaLambda());
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (property.isInnerEnum && property.items != null) {
            // format maps of inner enums to include the classname eg: Dictionary<string, MapTest.InnerEnum>
            property.datatypeWithEnum = property.datatypeWithEnum.replace(property.items.datatypeWithEnum, model.classname + "." + property.items.datatypeWithEnum);
            property.dataType = property.datatypeWithEnum;
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
            for (CodegenProperty var : cm.vars) {
                // check to see if model name is same as the property name
                // which will result in compilation error
                // if found, prepend with _ to workaround the limitation
                if (var.name.equalsIgnoreCase(cm.classname)) {
                    var.name = "_" + var.name;
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
        postProcessEnumRefs(processed);
        updateValueTypeProperty(processed);
        updateNullableTypeProperty(processed);

        for (Map.Entry<String, ModelsMap> entry : objs.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), objs);

            // https://github.com/OpenAPITools/openapi-generator/issues/12324
            // TODO: why do these collections contain different instances?
            // fixing allVars should suffice instead of patching every collection
            for (CodegenProperty property : model.allVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.vars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.readWriteVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.optionalVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.parentVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.requiredVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.readOnlyVars){
                patchProperty(model, property);
            }
            for (CodegenProperty property : model.nonNullableVars){
                patchProperty(model, property);
            }
        }
        return processed;
    }

    private void patchProperty(CodegenModel model, CodegenProperty property){
        /**
        * Hotfix for this issue
        * https://github.com/OpenAPITools/openapi-generator/issues/12155
        */
        if (property.dataType.equals("List") && property.getComposedSchemas() != null && property.getComposedSchemas().getAllOf() != null){
            List<CodegenProperty> composedSchemas = property.getComposedSchemas().getAllOf();
            if (composedSchemas.size() == 0) {
                return;
            }
            CodegenProperty composedProperty = composedSchemas.stream().findFirst().get();
            property.dataType = composedProperty.dataType;
            property.datatypeWithEnum = composedProperty.datatypeWithEnum;
            property.isMap = composedProperty.isMap;
            property.isContainer = composedProperty.isContainer;
        }

        // fix incorrect data types for maps of maps
        if (property.datatypeWithEnum.contains("List>") && property.items != null) {
            property.datatypeWithEnum = property.datatypeWithEnum.replace("List>", property.items.datatypeWithEnum + ">");
            property.dataType = property.datatypeWithEnum;
        }
        if (property.datatypeWithEnum.contains("Dictionary>") && property.items != null) {
            property.datatypeWithEnum = property.datatypeWithEnum.replace("Dictionary>", property.items.datatypeWithEnum + ">");
            property.dataType = property.datatypeWithEnum;
        }
    }

    @Override
    protected List<Map<String, Object>> buildEnumVars(List<Object> values, String dataType) {
        List<Map<String, Object>> enumVars = super.buildEnumVars(values, dataType);

        // this is needed for enumRefs like OuterEnum marked as nullable and also have string values
        // keep isString true so that the index will be used as the enum value instead of a string
        // this is inline with C# enums with string values
        if ("string?".equals(dataType)){
            enumVars.forEach((enumVar) -> {
                enumVar.put("isString", true);
            });
        }

        return enumVars;
    }

    /**
     * C# differs from other languages in that Enums are not _true_ objects; enums are compiled to integral types.
     * So, in C#, an enum is considers more like a user-defined primitive.
     * <p>
     * When working with enums, we can't always assume a RefModel is a nullable type (where default(YourType) == null),
     * so this post processing runs through all models to find RefModel'd enums. Then, it runs through all vars and modifies
     * those vars referencing RefModel'd enums to work the same as inlined enums rather than as objects.
     *
     * @param models processed models to be further processed for enum references
     */
    private void postProcessEnumRefs(final Map<String, ModelsMap> models) {
        Map<String, CodegenModel> enumRefs = new HashMap<>();
        for (Map.Entry<String, ModelsMap> entry : models.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), models);
            if (model.isEnum) {
                enumRefs.put(model.getClassname(), model);
            }
        }

        for (String openAPIName : models.keySet()) {
            CodegenModel model = ModelUtils.getModelByName(openAPIName, models);
            if (model != null) {
                for (CodegenProperty var : model.allVars) {
                    if (enumRefs.containsKey(var.dataType)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in C# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.dataType);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
                        var.isPrimitiveType = true;
                    }
                }
                for (CodegenProperty var : model.vars) {
                    if (enumRefs.containsKey(var.dataType)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in C# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.dataType);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
                        var.isPrimitiveType = true;
                    }
                }
                for (CodegenProperty var : model.readWriteVars) {
                    if (enumRefs.containsKey(var.dataType)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in C# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.dataType);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
                        var.isPrimitiveType = true;
                    }
                }
                for (CodegenProperty var : model.readOnlyVars) {
                    if (enumRefs.containsKey(var.dataType)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in C# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.dataType);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
                        var.isPrimitiveType = true;
                    }
                }

                /* Comment out the following as model.dataType is always the model name, eg. OuterIntegerEnum,
                 * and this will fix the integer enum via #9035.
                 * Only x-enum-byte is used in the template but it won't work due to the bug mentioned above.
                 * A better solution is to introduce isLong, isInteger, etc in the DefaultCodegen
                 * so that there is no need for each generator to post-process model enums.
                 *
                // We're looping all models here.
                if (model.isEnum) {
                    // We now need to make allowableValues.enumVars look like the context of CodegenProperty
                    Boolean isString = false;
                    Boolean isInteger = false;
                    Boolean isLong = false;
                    Boolean isByte = false;

                    if (model.dataType.startsWith("byte")) {
                        // C# Actually supports byte and short enums, swagger spec only supports byte.
                        isByte = true;
                        model.vendorExtensions.put("x-enum-byte", true);
                    } else if (model.dataType.startsWith("int32")) {
                        isInteger = true;
                        model.vendorExtensions.put("x-enum-integer", true);
                    } else if (model.dataType.startsWith("int64")) {
                        isLong = true;
                        model.vendorExtensions.put("x-enum-long", true);
                    } else {
                        // C# doesn't support non-integral enums, so we need to treat everything else as strings (e.g. to not lose precision or data integrity)
                        isString = true;
                        model.vendorExtensions.put("x-enum-string", true);
                    }

                    // Since we iterate enumVars for modelInnerEnum and enumClass templates, and CodegenModel is missing some of CodegenProperty's properties,
                    // we can take advantage of Mustache's contextual lookup to add the same "properties" to the model's enumVars scope rather than CodegenProperty's scope.
                    List<Map<String, String>> enumVars = (ArrayList<Map<String, String>>) model.allowableValues.get("enumVars");
                    List<Map<String, Object>> newEnumVars = new ArrayList<Map<String, Object>>();
                    for (Map<String, String> enumVar : enumVars) {
                        Map<String, Object> mixedVars = new HashMap<String, Object>();
                        mixedVars.putAll(enumVar);

                        mixedVars.put("isString", isString);
                        mixedVars.put("isLong", isLong);
                        mixedVars.put("isInteger", isInteger);
                        mixedVars.put("isByte", isByte);

                        newEnumVars.add(mixedVars);
                    }

                    if (!newEnumVars.isEmpty()) {
                        model.allowableValues.put("enumVars", newEnumVars);
                    }
                } */
            } else {
                LOGGER.warn("Expected to retrieve model %s by name, but no model was found. Check your -Dmodels inclusions.", openAPIName);
            }
        }
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

    /**
     * Update property if it is a C# value type
     *
     * @param models list of all models
     */
    protected void updateValueTypeProperty(Map<String, ModelsMap> models) {
        for (String openAPIName : models.keySet()) {
            CodegenModel model = ModelUtils.getModelByName(openAPIName, models);
            if (model != null) {
                for (CodegenProperty var : model.vars) {
                    var.vendorExtensions.put("x-is-value-type", isValueType(var));
                }
            }
        }
    }

    /**
     * Update property if it is a C# nullable type
     *
     * @param models list of all models
     */
    protected void updateNullableTypeProperty(Map<String, ModelsMap> models) {
        for (String openAPIName : models.keySet()) {
            CodegenModel model = ModelUtils.getModelByName(openAPIName, models);
            if (model != null) {
                for (CodegenProperty var : model.vars) {
                    if (!var.isContainer && (nullableType.contains(var.dataType) || var.isEnum)) {
                        var.vendorExtensions.put("x-csharp-value-type", true);
                    }
                }

                // https://github.com/OpenAPITools/openapi-generator/issues/12324
                // we should not need to iterate both vars and allVars
                // the collections dont have the same instance, so we have to do it again
                for (CodegenProperty var : model.allVars) {
                    if (!var.isContainer && (nullableType.contains(var.dataType) || var.isEnum)) {
                        var.vendorExtensions.put("x-csharp-value-type", true);
                    }
                }
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
                        String typeMapping;
                        int namespaceEnd = operation.returnType.lastIndexOf(".");
                        if (namespaceEnd > 0) {
                            typeMapping = operation.returnType.substring(namespaceEnd);
                        } else {
                            typeMapping = operation.returnType;
                        }

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


                    // check if the payload is json and set x-is-json accordingly
                    if (operation.consumes != null) {
                        for (Map<String, String> consume : operation.consumes) {
                            if (consume.containsKey("mediaType")) {
                                if (isJsonMimeType(consume.get("mediaType"))) {
                                    operation.vendorExtensions.put("x-is-json", true);
                                    break;
                                }
                            }
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

                    if (!isSupportNullable()) {
                        for (CodegenParameter parameter : operation.allParams) {
                            CodegenModel model = null;
                            for (ModelMap modelHashMap : allModels) {
                                CodegenModel codegenModel = modelHashMap.getModel();
                                if (codegenModel.getClassname().equals(parameter.dataType)) {
                                    model = codegenModel;
                                    break;
                                }
                            }

                            if (model == null) {
                                // Primitive data types all come already marked
                                parameter.isNullable = true;
                            } else {
                                // Effectively mark enum models as enums and non-nullable
                                if (model.isEnum) {
                                    parameter.isEnum = true;
                                    parameter.allowableValues = model.allowableValues;
                                    parameter.isPrimitiveType = true;
                                    parameter.isNullable = false;
                                } else {
                                    parameter.isNullable = true;
                                }
                            }
                        }
                    } else {
                        // Effectively mark enum models as enums
                        updateCodegenParametersEnum(operation.allParams, allModels);
                    }

                    processOperation(operation);
                }
            }
        }

        return objs;
    }

    protected void processOperation(CodegenOperation operation) {
        // default noop
    }

    private void updateCodegenParametersEnum(List<CodegenParameter> parameters, List<ModelMap> allModels) {
        for (CodegenParameter parameter : parameters) {
            CodegenModel model = null;
            for (ModelMap modelHashMap : allModels) {
                CodegenModel codegenModel = modelHashMap.getModel();
                if (codegenModel.getClassname().equals(parameter.dataType)) {
                    model = codegenModel;
                    break;
                }
            }

            if (model != null) {
                // Effectively mark enum models as enums and non-nullable
                if (model.isEnum) {
                    parameter.isEnum = true;
                    parameter.allowableValues = model.allowableValues;
                    parameter.isPrimitiveType = true;
                    parameter.vendorExtensions.put("x-csharp-value-type", true);
                }
            }

            if (!parameter.isContainer && nullableType.contains(parameter.dataType)) {
                parameter.vendorExtensions.put("x-csharp-value-type", true);
            }

            if (!parameter.required && parameter.vendorExtensions.get("x-csharp-value-type") != null) { //optional
                parameter.dataType = parameter.dataType + "?";
            }
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
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    /**
     * Return the example value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the example value of the property
     */
    @Override
    public String toExampleValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getExample() != null) {
                return "\"" + p.getExample().toString() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        }

        return null;
    }

    /**
     * Return the default value of the property
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
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toModelName(String name) {
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

    public void setLicenseUrl(String licenseUrl) {this.licenseUrl = licenseUrl;}

    public void setLicenseName(String licenseName) {this.licenseName = licenseName;}

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

    public void setNullableReferenceTypes(final Boolean nullReferenceTypesFlag){
        this.nullReferenceTypesFlag = nullReferenceTypesFlag;
        additionalProperties.put("nullableReferenceTypes", nullReferenceTypesFlag);
        additionalProperties.put("nrt", nullReferenceTypesFlag);

        if (nullReferenceTypesFlag){
            additionalProperties.put("nrt?", "?");
            additionalProperties.put("nrt!", "!");
        } else {
            additionalProperties.remove("nrt?");
            additionalProperties.remove("nrt!");
        }
    }

    public boolean getNullableReferencesTypes(){
        return this.nullReferenceTypesFlag;
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
        if (datatype.startsWith("int") || datatype.startsWith("long") || datatype.startsWith("byte")) {
            return value;
        }

        return escapeText(value);
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
        return (valueTypes.contains(var.dataType) || var.isEnum ) ;
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
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        // TODO: instead of appending the ?
        // use isNullable, OptionalParameterLambda, or RequiredParameterLambda
        if (!parameter.required && (nullReferenceTypesFlag || nullableType.contains(parameter.dataType))) {
            parameter.dataType = parameter.dataType.endsWith("?")
                ? parameter.dataType
                : parameter.dataType + "?";
        }
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
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.C_SHARP; }
}
