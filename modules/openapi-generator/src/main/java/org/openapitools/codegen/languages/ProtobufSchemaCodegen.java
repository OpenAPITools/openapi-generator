/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;

import com.google.common.base.CaseFormat;
import org.openapitools.codegen.*;
import org.openapitools.codegen.exceptions.ProtoBufIndexComputationException;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.option.CustomOptionDefinition;
import org.openapitools.codegen.option.CustomOption;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class ProtobufSchemaCodegen extends DefaultCodegen implements CodegenConfig {

    private static final String IMPORT = "import";

    private static final String IMPORTS = "imports";

    public static final String NUMBERED_FIELD_NUMBER_LIST = "numberedFieldNumberList";

    public static final String START_ENUMS_WITH_UNKNOWN = "startEnumsWithUnknown";

    public static final String START_ENUMS_WITH_UNSPECIFIED = "startEnumsWithUnspecified";
    
    public static final String FIELD_NAMES_IN_SNAKE_CASE = "fieldNamesInSnakeCase";

    private final Logger LOGGER = LoggerFactory.getLogger(ProtobufSchemaCodegen.class);

    protected String packageName = "openapitools";

    protected String protoVersion = "proto3";

    private boolean numberedFieldNumberList = false;

    private boolean startEnumsWithUnknown = false;

    private boolean startEnumsWithUnspecified = false;
    
    private boolean fieldNamesInSnakeCase = false;

    // store the available custom options
    // <option name as vendor extension, CustomOptionDefinition>
    protected Map<String, CustomOptionDefinition> customOptionsMapping = new HashMap<String, CustomOptionDefinition>();

    private String customOptionsFileName = "custom_options";

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    public String getName() {
        return "protobuf-schema";
    }

    public String getHelp() {
        return "Generates gRPC and protocol buffer 3 schema files (beta)";
    }

    public ProtobufSchemaCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeWireFormatFeatures(WireFormatFeature.PROTOBUF)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.PROTOBUF))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
        );

        outputFolder = "generated-code/protobuf-schema";
        customOptionsTemplateFiles.put("custom_options.mustache", ".proto");
        modelTemplateFiles.put("model.mustache", ".proto");
        apiTemplateFiles.put("api.mustache", ".proto");
        embeddedTemplateDir = templateDir = "protobuf-schema";
        hideGenerationTimestamp = Boolean.TRUE;
        modelPackage = "models";
        apiPackage = "services";
        customOptionsPackage = "custom_options";

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array",
                        "bool",
                        "bytes",
                        "string",
                        "int32",
                        "int64",
                        "uint32",
                        "uint64",
                        "sint32",
                        "sint64",
                        "fixed32",
                        "fixed64",
                        "sfixed32",
                        "sfixed64",
                        "float",
                        "double")
        );

        instantiationTypes.clear();
        instantiationTypes.put("array", "repeat");

        // ref: https://developers.google.com/protocol-buffers/docs/proto
        typeMapping.clear();
        typeMapping.put("array", "array");
        typeMapping.put("map", "map");
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("password", "string");
        // TODO fix file mapping
        typeMapping.put("file", "string");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "bytes");
        typeMapping.put("object", "google.protobuf.Any");
        typeMapping.put("AnyType", "google.protobuf.Any");

        importMapping.clear();
        importMapping.put("google.protobuf.Any", "google/protobuf/any");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();

        addSwitch(NUMBERED_FIELD_NUMBER_LIST, "Field numbers in order.", numberedFieldNumberList);
        addSwitch(START_ENUMS_WITH_UNKNOWN, "Introduces \"UNKNOWN\" as the first element of enumerations.", startEnumsWithUnknown);
        addSwitch(START_ENUMS_WITH_UNSPECIFIED, "Introduces \"UNSPECIFIED\" as the first element of enumerations.", startEnumsWithUnspecified);
        addSwitch(FIELD_NAMES_IN_SNAKE_CASE, "Field names in snake_case.", fieldNamesInSnakeCase);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        //apiTestTemplateFiles.put("api_test.mustache", ".proto");
        //modelTestTemplateFiles.put("model_test.mustache", ".proto");

        apiDocTemplateFiles.clear(); // TODO: add api doc template
        modelDocTemplateFiles.clear(); // TODO: add model doc template

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }

        if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        if (additionalProperties.containsKey(this.NUMBERED_FIELD_NUMBER_LIST)) {
            this.numberedFieldNumberList = convertPropertyToBooleanAndWriteBack(NUMBERED_FIELD_NUMBER_LIST);
        }

        if (additionalProperties.containsKey(this.START_ENUMS_WITH_UNKNOWN)) {
            this.startEnumsWithUnknown = convertPropertyToBooleanAndWriteBack(START_ENUMS_WITH_UNKNOWN);
        }

        if (additionalProperties.containsKey(this.START_ENUMS_WITH_UNSPECIFIED)) {
            this.startEnumsWithUnspecified = convertPropertyToBooleanAndWriteBack(START_ENUMS_WITH_UNSPECIFIED);
        }
        
        if (additionalProperties.containsKey(this.FIELD_NAMES_IN_SNAKE_CASE)) {
            this.fieldNamesInSnakeCase = convertPropertyToBooleanAndWriteBack(FIELD_NAMES_IN_SNAKE_CASE);
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
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

        return camelize(sanitizeName(operationId));
    }

    /**
     * Adds prefix to the enum allowable values
     * NOTE: Enum values use C++ scoping rules, meaning that enum values are siblings of their type, not children of it. Therefore, enum value must be unique
     *
     * @param allowableValues allowable values
     * @param prefix added prefix
     */
    public void addEnumValuesPrefix(Map<String, Object> allowableValues, String prefix, String dataType){
        if(allowableValues.containsKey("enumVars")) {
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");

            for(Map<String, Object> value : enumVars) {
                String name = (String)value.get("name");
                value.put("name", toEnumVarName(prefix + "_" + name, dataType));
                value.put("value", toEnumValue (prefix + "_" + name, dataType));
            }
        }

        // update values
        if(allowableValues.containsKey("values")) {
            List<String> values = (List<String>)allowableValues.get("values");
            for(int i = 0 ; i < values.size() ; i++) {
                if (!values.get(i).startsWith(prefix + "_")) {
                    // replace value by value with prefix
                    values.set(i, underscore(prefix + "_" + values.get(i)).toUpperCase());
                }
            }
        }
    }

    /**
     * Adds unknown value to the enum allowable values
     *
     * @param allowableValues allowable values
     */
    public void addUnknownToAllowableValues(Map<String, Object> allowableValues) {
        if(startEnumsWithUnspecified || startEnumsWithUnknown) {
            String value = startEnumsWithUnspecified ? "UNSPECIFIED" : "UNKNOWN";
            if(allowableValues.containsKey("enumVars")) {
                List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");

                // add unspecified only if not already present
                if (enumVars.size() > 0 && !value.equals(enumVars.get(0).get("name"))) {
                    HashMap<String, Object> unknown = new HashMap<String, Object>();
                    unknown.put("name", value);
                    unknown.put("isString", "false");
                    unknown.put("value", "\"" + value + "\"");

                    enumVars.add(0, unknown);
                }
            }

            // add unspecified only if not already present
            if(allowableValues.containsKey("values") && !((List<String>)allowableValues.get("values")).get(0).endsWith(value)) {
                List<String> values = (List<String>)allowableValues.get("values");           
                values.add(0, value);
            }
        }
    }

    //override default behaviour: value in SNAKE_CASE
    // e.g. PhoneNumber.mobile => PHONE_NUMBER_MOBILE
    @Override
    public String toEnumValue(String value, String datatype) {
        if ("number".equalsIgnoreCase(datatype) || "boolean".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            value = underscore(value).toUpperCase();
            return "\"" + escapeText(value) + "\"";
        }
    }

    //override default behaviour: put variable name for enum to SNAKE_CASE and sanitize it
    // e.g. PhoneNumber.mobile => PHONE_NUMBER_MOBILE
    @Override
    public String toEnumVarName(String value, String datatype) {       
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = underscore(value);

        var = var.replaceAll("\\W+", "_").toUpperCase(Locale.ROOT);
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    //override default behaviour: Return the Enum name (e.g. Status given 'status'), using x-ama-enum.name if defined
    @Override
    @SuppressWarnings("static-method")
    public String toEnumName(CodegenProperty property) {
        String name = "";
        Map<String, Object> extensions = property.getVendorExtensions();
        if (extensions.containsKey("x-ama-enum") && ((LinkedHashMap) extensions.get("x-ama-enum")).containsKey("name")) {
            name = (String) ((LinkedHashMap) extensions.get("x-ama-enum")).get("name");
        }
        else {
            name = property.name;
        }
        return StringUtils.capitalize(name);
    }

    /**
     * Iterates enum vars and puts index to them
     *
     * @param enumVars list of enum vars
     * @param vendorExtensions vendor extensions
     */
    public void addEnumIndexes(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions) throws ProtoBufIndexComputationException {
        //store used indexes to prevent duplicates
        Set<Integer> usedIndexes = new HashSet<Integer>();
        if (vendorExtensions != null) { 
            String extensionKey = "x-enum-protobuf-indexes";
            if (vendorExtensions.containsKey(extensionKey)) {
                List<Integer> indexes = (List<Integer>) vendorExtensions.get(extensionKey);
                // first value of x-enum-protobuf-indexes is associated to the second enum value if the first enum value is the 'unknown'
                int i = startEnumsWithUnknown || startEnumsWithUnspecified ? 1 : 0;
                int j = 0;
                while (i < enumVars.size() && j < indexes.size()) {
                    // ref : https://cloud.google.com/apis/design/proto3
                    if (i == 0 && indexes.get(j) != 0) {
                        LOGGER.error("First enum value '" + enumVars.get(i).get("name") + "' must have field number zero since enum definitions must start with enum value zero");
                        throw new RuntimeException("Enum definitions must start with enum value zero");
                    }
                    else if (indexes.get(j) == 0 && i != 0) {
                        LOGGER.error("Enum value '" + enumVars.get(i).get("name") + "' must NOT have field number zero since field number zero must be only used for first enum value '" + enumVars.get(0).get("name") + "'");
                        throw new RuntimeException("Field number zero reserved for first enum value");
                    }
                    else if (indexes.get(j) < 0) {
                        LOGGER.error("x-enum-protobuf-indexes contains negative value: " + indexes.get(j));
                        throw new RuntimeException("Negative enum field numbers are not allowed");
                    }
                    else {
                        if (!usedIndexes.contains(indexes.get(j))) {
                            //add protobuf-enum-index and update usedIndexes list
                            enumVars.get(i).put("protobuf-enum-index", indexes.get(j));
                            usedIndexes.add(indexes.get(j));
                            i++;
                            j++;      
                        }
                        else {
                            LOGGER.error("There are duplicates field numbers: " + indexes.get(j));
                            throw new RuntimeException("Enum indexes must be unique");
                        }
                    }
                }        
            }

            //amadeus enum extension handling
            extensionKey = "x-ama-enum";
            if (vendorExtensions.containsKey(extensionKey) && ((LinkedHashMap) vendorExtensions.get(extensionKey)).containsKey("values")) {
                //values specified in x-ama-enum
                List<Map<String, Object>> amaEnumValues = (List<Map<String, Object>>) ((LinkedHashMap) vendorExtensions.get(extensionKey)).get("values");
                
                // check duplicate values in x-ama-enum
                Set<String> amaUniqueEnumValues = new HashSet<String>();
                for(Map<String, Object> amaEnumValue : amaEnumValues) {
                    if (amaEnumValue.containsKey("value")) {
                        String value = (String) amaEnumValue.get("value");
                        if (amaUniqueEnumValues.contains(value)) {
                            LOGGER.error("Duplicate value in x-ama-enum.values: " + value);
                            throw new RuntimeException("Duplicate value in x-ama-enum.values");
                        }
                        else {
                            amaUniqueEnumValues.add(value);
                        }
                    } 
                }
                
                for (int i = 0; i < enumVars.size(); i++) {
                    //add protobuf-enum-index, unless already specified
                    if (!enumVars.get(i).containsKey("protobuf-enum-index") && enumVars.get(i).containsKey("value")) {
                        String value = (String) (enumVars.get(i).get("value"));
                        //Search matching specified value in x-ama-enum for enum value 
                        for(Map<String, Object> amaEnumValue : amaEnumValues) {
                            //enum values have additional "\"" at start and end
                            if (amaEnumValue.containsKey("value") && 
                            amaEnumValue.get("value").equals(value.replace("\"", "")) &&
                            amaEnumValue.containsKey("protobuf-enum-field-number")) {
                                int enumFieldNumber = (int) amaEnumValue.get("protobuf-enum-field-number");
                                if (enumFieldNumber < 0) {
                                    LOGGER.error("protobuf-enum-field-number is negative: " + enumFieldNumber);
                                    throw new RuntimeException("Negative enum field numbers are not allowed");
                                }
                                else {
                                    if (i == 0 && enumFieldNumber != 0) {
                                        LOGGER.error("First enum value '" + enumVars.get(i).get("name") + "' must have field number zero since enum definitions must start with enum value zero");
                                        throw new RuntimeException("Enum definitions must start with enum value zero");
                                    } 
                                    else if (enumFieldNumber == 0 && i != 0) {
                                        LOGGER.error("Enum value '" + enumVars.get(i).get("name") + "' must NOT have field number zero since field number zero must be only used for first enum value '" + enumVars.get(0).get("name") + "'");
                                        throw new RuntimeException("Field number zero reserved for first enum value");
                                    }
                                    else if (!usedIndexes.contains(enumFieldNumber)) {
                                        //add protobuf-enum-index and update usedIndexes list
                                        enumVars.get(i).put("protobuf-enum-index", enumFieldNumber);
                                        usedIndexes.add(enumFieldNumber);
                                    }
                                    else {
                                        LOGGER.error("There are duplicates field numbers: " + enumFieldNumber);
                                        throw new RuntimeException("Enum indexes must be unique");
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }

        //Add protobuf-enum-index generated automatically, unless already specified
        int enumIndex = 0;
        for (Map<String, Object> enumVar : enumVars) {
            if (!enumVar.containsKey("protobuf-enum-index")) {
                //prevent from using index already used
                while (usedIndexes.contains(enumIndex)) {
                    enumIndex++;
                }
                enumVar.put("protobuf-enum-index", enumIndex);
                usedIndexes.add(enumIndex);
            }
        }
    }

    @Override
    public void updateEnumVarsWithExtensions(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions, String dataType) {
        super.updateEnumVarsWithExtensions(enumVars, vendorExtensions, dataType);

        //check for duplicate enum names
        Set<String> uniqueNames = new HashSet<String>();
        for (int i = 0; i < enumVars.size(); i++) {
            if (enumVars.get(i).containsKey("name")) {
                String name = (String) enumVars.get(i).get("name");
                if (uniqueNames.contains(name)) {
                    LOGGER.error("Duplicate enum name: " + name);
                    throw new RuntimeException("Duplicate enum name");
                }
                else {
                    uniqueNames.add(name);
                }
            }
        }
    }

    @Override
    protected void updateEnumVarsWithExtensions(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions, String dataType, String extensionKey, String key) {
        if (vendorExtensions.containsKey(extensionKey)) {
            List<String> values = (List<String>) vendorExtensions.get(extensionKey);
            int size = Math.min(enumVars.size(), values.size());
            for (int i = 0; i < size; i++) {
                if ("name".equals(key)) {
                    enumVars.get(i).put(key, toEnumVarName(values.get(i), dataType));
                }
                else {
                    enumVars.get(i).put(key, values.get(i));
                }                
            }
        }
    }

    @Override
    public void postProcessAllCustomOptions(List<CodegenProperty> customOptions, String customOptionsFileName) {
        // file name used for import
        this.customOptionsFileName = customOptionsFileName;

        // range 50000-99999 is reserved for internal use within individual organizations
        // https://developers.google.com/protocol-buffers/docs/proto#customoptions
        int index = 55000;

        for (CodegenProperty customOption : customOptions) {
            // update codegen property enum with proper naming convention
            updateCodegenPropertyEnum(customOption);

            // add indexes
            if(customOption.vendorExtensions != null) {                
                customOption.vendorExtensions.put("x-protobuf-index", index);
            }
            index++;
        }

        // add types and names for each option
        processProtoTypesAndNames(customOptions);

        // overwrite type if it is a category (considered as object)
        for (CodegenProperty customOption : customOptions) {
            if (typeMapping.get("object").equals(customOption.vendorExtensions.get("x-protobuf-data-type"))) {
                customOption.vendorExtensions.put("x-protobuf-data-type", toModelName(customOption.name));
            }
        }
    }

    @Override
    public CodegenModel postProcessCustomOptionCategory(CodegenModel optionCategory) {
        int index = 1;

        for (CodegenProperty option : optionCategory.vars) {
            // update codegen property enum with proper naming convention
            updateCodegenPropertyEnum(option);

            // add indexes
            option.vendorExtensions.put("x-protobuf-index", index);
            index++;
        }

        // add types and names for each var
        processProtoTypesAndNames(optionCategory.vars);

        return optionCategory;
    }

    @Override
    public void updateCustomOptionsMapping(List<CodegenProperty> customOptions, Map<String, CodegenModel> categories) {
        for (CodegenProperty customOption : customOptions) {
            // if property is a custom option category
            if (categories.containsKey(customOption.baseName)) {
                CodegenModel category = categories.get(customOption.baseName);
                // add each option of the category
                for(CodegenProperty option : category.vars) {
                    addCustomOptionMapping(option, category);
                }
            }
            else {
                addCustomOptionMapping(customOption, null);
            }
        }   
    }

    private void addCustomOptionMapping(CodegenProperty option, CodegenModel category) {
        String name = option.getNameInCamelCase();
        String categoryClassName = "";
        String categoryGlobalName = "";
        String optionName = fieldNamesInSnakeCase ? underscore(name) : option.getName();
        if (category != null) {
            categoryClassName = category.getClassname();
            categoryGlobalName = customOptionsPackage + "." + category.getClassFilename();
        }
        else {
            optionName = customOptionsPackage + "." + optionName;
        }
        // extension name is built from option category and option name in kebab case prefixed with "x-"
        // e.g. category AmaRisk, option name personalDataField => x-ama-risk-personal-data-field
        String extensionName = "x-" + CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, categoryClassName + name);
        CustomOptionDefinition customOptionDef = new CustomOptionDefinition(optionName, categoryGlobalName, option.baseType);
        
        if (option.isEnum){
            // useful to check valid value
            customOptionDef.setEnum((List<String>) option.allowableValues.get("values"));
        }
        customOptionsMapping.put(extensionName, customOptionDef);
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = postProcessModelsEnum(objs);

        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();

            if(cm.isEnum) {
                Map<String, Object> allowableValues = cm.getAllowableValues();
                addUnknownToAllowableValues(allowableValues);
                if (allowableValues.containsKey("enumVars")) {
                    List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");
                    try {
                        addEnumIndexes(enumVars, cm.getVendorExtensions());
                    }
                    catch (ProtoBufIndexComputationException e) {
                        LOGGER.error("Exception when assigning an index to a protobuf enum field", e);
                        throw new RuntimeException("Exception when assigning an index to a protobuf enum field");
                    }
                }
                addEnumValuesPrefix(allowableValues, cm.getClassname(), cm.dataType);
            }

            // add types and names for each var
            processProtoTypesAndNames(cm.vars);

            //to keep track of the indexes used, prevent duplicate indexes
            Set<Integer> usedIndexes = new HashSet<Integer>();             
            for (CodegenProperty var : cm.vars) {
                //check x-protobuf-index
                if (var.vendorExtensions.containsKey("x-protobuf-index")) {
                    int protobufIndex = (int) var.vendorExtensions.get("x-protobuf-index");
                    checkIndex(protobufIndex, usedIndexes);                    
                }
                else if (var.vendorExtensions.containsKey("x-protobuf-field-number")) {
                    int protobufIndex = (int) var.vendorExtensions.get("x-protobuf-field-number");
                    checkIndex(protobufIndex, usedIndexes);
                    var.vendorExtensions.put("x-protobuf-index", protobufIndex);
                }

                // manage custom options
                for (Map.Entry<String, Object> extension : var.vendorExtensions.entrySet()) {
                    if (customOptionsMapping.containsKey(extension.getKey())) {
                        String customOptionsLocation = customOptionsPackage + "/" + this.customOptionsFileName;
                        //add import
                        if (!cm.getImports().contains(customOptionsLocation)) {
                            cm.getImports().add(customOptionsLocation);
                            Map<String, String> importItem = new HashMap<>();
                            importItem.put(IMPORT, customOptionsLocation);
                            ((List<Map<String, String>>) objs.get(IMPORTS)).add(importItem);
                        }

                        CustomOptionDefinition customOptionDef = customOptionsMapping.get(extension.getKey());                        
                        Object value = extension.getValue();

                        // check valid value
                        try {
                            switch(customOptionDef.getBaseType()) {
                                case "array": 
                                    value = (List<Object>) value;
                                    break;
                                case "int32":
                                    value = Integer.valueOf(value.toString());
                                    break;
                                case "int64":
                                    value = Long.valueOf(value.toString());
                                    break;
                                case "float":
                                    value = Float.valueOf(value.toString());
                                    break;
                                case "double":
                                    value = Double.valueOf(value.toString());
                                    break;
                                case "bool":
                                    value = (boolean) value;
                                    break;
                                case "bytes":
                                    value = Byte.valueOf(value.toString());
                                    break;
                                default:
                                    value = value.toString();
                                    break;
                            }
                        }
                        catch( Exception e) {
                            throw new RuntimeException("value \"" + value + "\" is of type " + value.getClass().getSimpleName() + ", expected " + customOptionDef.getBaseType());
                        }

                        if (customOptionDef.getIsEnum() && "String".equals(value.getClass().getSimpleName())) {
                            // add enum prefix
                            value = toEnumVarName(customOptionDef.getName() + "_" + value, "string");

                            //check valid value
                            if (!customOptionDef.getAllowedValues().contains(value)) {
                                throw new RuntimeException("value \"" + value + "\" is not part of allowed enum values " + customOptionDef.getAllowedValues().toString());
                            }
                        }
                        else if ("String".equals(value.getClass().getSimpleName())) {
                            // add quotes for string
                            value = "\"" + value + "\"";
                        }

                        // add custom option to field
                        if (value instanceof Collection) {
                            // each item of the list
                            for (Object item : (List<Object>) value) {
                                if ("String".equals(item.getClass().getSimpleName())) {
                                    // add quotes for string
                                    item = "\"" + item + "\"";
                                }

                                CustomOption option = new CustomOption(customOptionDef, item);
                                var.customOptions.add(option);
                            }
                        }
                        else {
                            CustomOption option = new CustomOption(customOptionDef, value);                        
                            var.customOptions.add(option);
                        }
                    }
                }
            }
            //automatic index generation when index not specified using extensions
            int index = 1;
            for (CodegenProperty var : cm.vars) {
                if (!var.vendorExtensions.containsKey("x-protobuf-index")) {
                    if (this.numberedFieldNumberList) {
                        //prevent from using index already used
                        while (usedIndexes.contains(index)) {
                            index++;
                        }
                        usedIndexes.add(index);
                        var.vendorExtensions.put("x-protobuf-index", index);
                    }
                    else {
                        try {
                            int protobufIndex = generateFieldNumberFromString(var.getName());
                            if (!usedIndexes.contains(protobufIndex)) {
                                //update usedIndexes list and add x-protobuf-index
                                usedIndexes.add(protobufIndex);
                                var.vendorExtensions.put("x-protobuf-index", protobufIndex);
                            }
                            else {
                                LOGGER.error("Field number " + protobufIndex + " already used");
                                throw new RuntimeException("A same field number is used multiple times");
                            }
                        } catch (ProtoBufIndexComputationException e) {
                            LOGGER.error("Exception when assigning a index to a protobuf field", e);
                            var.vendorExtensions.put("x-protobuf-index", "Generated field number is in reserved range (19000, 19999)");
                        }
                    }
                }
            }
        }
        return objs;
    }

    // return the field name to use
    private String getProtobufName(Map<String, Object> vendorExtensions, String defaultName) {
        String fieldName = "";
        if (vendorExtensions.containsKey("x-protobuf-name")) {
            fieldName = (String) vendorExtensions.get("x-protobuf-name");
        }
        else if (vendorExtensions.containsKey("x-protobuf-field-name")) {
            fieldName = (String) vendorExtensions.get("x-protobuf-field-name");
        }
        else {
            fieldName = defaultName;
        }

        if (fieldNamesInSnakeCase) {
            fieldName = underscore(fieldName);
        }

        return fieldName;
    }

    private void processProtoTypesAndNames(List<CodegenProperty> vars) {
        // store names used to prevent from duplicate names
        Set<String> usedNames = new HashSet<String>();
        for (CodegenProperty var : vars) {
            // add x-protobuf-type: repeated if it's an array
            if (Boolean.TRUE.equals(var.isArray)) {
                var.vendorExtensions.put("x-protobuf-type", "repeated");
            }
            else if (Boolean.TRUE.equals(var.isNullable &&  var.isPrimitiveType)) {
                var.vendorExtensions.put("x-protobuf-type", "optional");
            }

            // add x-protobuf-data-type
            // ref: https://developers.google.com/protocol-buffers/docs/proto3
            if (!var.vendorExtensions.containsKey("x-protobuf-data-type")) {
                if (var.isArray) {
                    var.vendorExtensions.put("x-protobuf-data-type", var.items.dataType);
                } else {
                    var.vendorExtensions.put("x-protobuf-data-type", var.dataType);
                }
            }
            
            // add x-protobuf-name
            String fieldName = getProtobufName(var.vendorExtensions, var.getName());                
            //check duplicate names
            checkName(fieldName, usedNames);
            var.vendorExtensions.put("x-protobuf-name", fieldName);             
            
            if (var.isEnum) {
                addUnknownToAllowableValues(var.allowableValues);
            
                //add enum indexes
                if(var.allowableValues.containsKey("enumVars")) {
                    List<Map<String, Object>> enumVars = (List<Map<String, Object>>) var.allowableValues.get("enumVars");
                    try {
                        addEnumIndexes(enumVars, var.getVendorExtensions());
                    }
                    catch (ProtoBufIndexComputationException e) {
                        LOGGER.error("Exception when assigning an index to a protobuf enum field", e);
                        throw new RuntimeException("Exception when assigning an index to a protobuf enum field");
                    }
                }
                addEnumValuesPrefix(var.allowableValues, var.getEnumName(), var.dataType);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        super.postProcessAllModels(objs);

        Map<String, CodegenModel> allModels = this.getAllModels(objs);

        for (CodegenModel cm : allModels.values()) {
            // Replicate all attributes from children to parents in case of allof, as there is no inheritance
            if (!cm.allOf.isEmpty() && cm.getParentModel() != null) {
                CodegenModel parentCM = cm.getParentModel();
                for (CodegenProperty var : cm.getVars()) {
                    if (!parentVarsContainsVar(parentCM.vars, var)) {
                        parentCM.vars.add(var);
                    }
                }
                // add all imports from child
                cm.getImports().stream()
                        // Filter self import && child import
                        .filter(importFromList -> !parentCM.getClassname().equalsIgnoreCase(importFromList) && !cm.getClassname().equalsIgnoreCase(importFromList))
                        .forEach(importFromList -> this.addImport(objs, parentCM, importFromList));
            }
        }
        return objs;
    }

    public void addImport(Map<String, ModelsMap> objs, CodegenModel cm, String importValue) {
        String mapping = importMapping().get(importValue);
        if (mapping == null) {
            mapping = toModelImport(importValue);
        }
        boolean skipImport = isImportAlreadyPresentInModel(objs, cm, mapping);
        if (!skipImport) {
            this.addImport(cm, importValue);
            Map<String, String> importItem = new HashMap<>();
            importItem.put(IMPORT, mapping);
            objs.get(cm.getName()).getImports().add(importItem);
        }
    }

    private boolean isImportAlreadyPresentInModel(Map<String, ModelsMap> objs, CodegenModel cm, String importValue) {
        boolean skipImport = false;
        List<Map<String, String>> cmImports = objs.get(cm.getName()).getImports();
        for (Map<String, String> cmImportItem : cmImports) {
            for (Entry<String, String> cmImportItemEntry : cmImportItem.entrySet()) {
                if (importValue.equals(cmImportItemEntry.getValue())) {
                    skipImport = true;
                    break;
                }
            }
        }
        return skipImport;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input;
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
                if (Boolean.valueOf(p.getDefault().toString()) == false)
                    return "false";
                else
                    return "true";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) p.getDefault()).find())
                    return "'''" + p.getDefault() + "'''";
                else
                    return "'" + p.getDefault() + "'";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }

        return null;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage;
    }

    @Override
    public String customOptionsFileFolder() {
        return outputFolder + File.separatorChar + customOptionsPackage;
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // e.g. PhoneNumber => phone_number
        return underscore(name) + "_service";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultService";
        }
        // e.g. phone_number => PhoneNumber
        return camelize(name) + "Service";
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_service";
        }
        return underscore(name) + "_service";
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(toModelName(name));
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        name = name.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = toModelName(schemaType);
        }
        return type;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();
        for (CodegenOperation op : operationList) {
            //to keep track of the indexes used, prevent duplicate indexes
            Set<Integer> usedIndexes = new HashSet<Integer>();
            // store names used to prevent from duplicate names
            Set<String> usedNames = new HashSet<String>();
            for (CodegenParameter p : op.allParams) {
                // add x-protobuf-type: repeated if it's an array
                
                if (Boolean.TRUE.equals(p.isArray)) {
                    p.vendorExtensions.put("x-protobuf-type", "repeated");
                }
                else if (Boolean.TRUE.equals(p.isNullable &&  p.isPrimitiveType)) {
                    p.vendorExtensions.put("x-protobuf-type", "optional");
                }
                else if (Boolean.TRUE.equals(p.isMap)) {
                    LOGGER.warn("Map parameter (name: {}, operation ID: {}) not yet supported", p.paramName, op.operationId);
                }

                // add x-protobuf-data-type
                // ref: https://developers.google.com/protocol-buffers/docs/proto3
                if (!p.vendorExtensions.containsKey("x-protobuf-data-type")) {
                    if (Boolean.TRUE.equals(p.isArray)) {
                        p.vendorExtensions.put("x-protobuf-data-type", p.items.dataType);
                    } else {
                        p.vendorExtensions.put("x-protobuf-data-type", p.dataType);
                    }
                }

                // add x-protobuf-name
                String fieldName = getProtobufName(p.vendorExtensions, p.paramName);
                //check duplicate names
                checkName(fieldName, usedNames);
                p.vendorExtensions.put("x-protobuf-name", fieldName);

                //check x-protobuf-index
                if (p.vendorExtensions.containsKey("x-protobuf-index")) {
                    int protobufIndex = (int) p.vendorExtensions.get("x-protobuf-index");
                    checkIndex(protobufIndex, usedIndexes);
                }
                else if (p.vendorExtensions.containsKey("x-protobuf-field-number")) {
                    int protobufIndex = (int) p.vendorExtensions.get("x-protobuf-field-number");
                    checkIndex(protobufIndex, usedIndexes);
                    p.vendorExtensions.put("x-protobuf-index", protobufIndex);
                }
            }
            //automatic index generation when index not specified using extensions
            int index = 1;
            for (CodegenParameter p : op.allParams) {
                if (!p.vendorExtensions.containsKey("x-protobuf-index")) {
                    //prevent from using index already used
                    while (usedIndexes.contains(index)) {
                        index++;
                    }
                    usedIndexes.add(index);
                    p.vendorExtensions.put("x-protobuf-index", index);
                }                
            }

            if (StringUtils.isEmpty(op.returnType)) {
                op.vendorExtensions.put("x-grpc-response", "google.protobuf.Empty");
            } else {
                if (Boolean.FALSE.equals(op.returnTypeIsPrimitive) && StringUtils.isEmpty(op.returnContainer)) {
                    op.vendorExtensions.put("x-grpc-response", op.returnType);
                } else {
                    if ("map".equals(op.returnContainer)) {
                        LOGGER.warn("Map response (operation ID: {}) not yet supported", op.operationId);
                        op.vendorExtensions.put("x-grpc-response-type", op.returnBaseType);
                    } else if ("array".equals(op.returnContainer)) {
                        op.vendorExtensions.put("x-grpc-response-type", "repeated " + op.returnBaseType);
                    } else { // primitive type
                        op.vendorExtensions.put("x-grpc-response-type", op.returnBaseType);
                    }
                }
            }
        }

        return objs;
    }

    private void checkIndex(int protobufIndex, Set<Integer> usedIndexes) {
        if (protobufIndex > 0) {
            if (!usedIndexes.contains(protobufIndex)) {
                //update usedIndexes list
                usedIndexes.add(protobufIndex);
            }
            else {
                LOGGER.error("Field number " + protobufIndex + " already used");
                throw new RuntimeException("A same field number is used multiple times");
            }
        }
        else {
            LOGGER.error("Field number " + protobufIndex + " not strictly positive");
            throw new RuntimeException("Only strictly positive field numbers are allowed");
        }
    }

    private void checkName(String name, Set<String> usedNames) {
        if (!usedNames.contains(name)) {
            //update usedNames set            
            usedNames.add(name);
        }
        else {
            LOGGER.error("Field name '" + name + "' already used");
            throw new RuntimeException("A same field name is used multiple times");
        }   
    }

    @Override
    public String toModelImport(String name) {
        return modelPackage() + "/" + underscore(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    private int generateFieldNumberFromString(String name) throws ProtoBufIndexComputationException {
        // Max value from developers.google.com/protocol-buffers/docs/proto3#assigning_field_numbers
        int fieldNumber = Math.abs(name.hashCode() % 536870911);
        if (19000 <= fieldNumber && fieldNumber <= 19999) {
            LOGGER.error("Generated field number is in reserved range (19000, 19999) for %s, %d", name, fieldNumber);
            throw new ProtoBufIndexComputationException("Generated field number is in reserved range (19000, 19999).");
        }
        return fieldNumber;
    }

    /**
     * Checks if the var provided is already in the list of the parent's vars, matching the type and the name
     *
     * @param parentVars list of parent's vars
     * @param var        var to compare
     * @return true if the var is already in the parent's list, false otherwise
     */
    private boolean parentVarsContainsVar(List<CodegenProperty> parentVars, CodegenProperty var) {
        boolean containsVar = false;
        for (CodegenProperty parentVar : parentVars) {
            if (var.getDataType().equals(parentVar.getDataType())
                    && var.getName().equals(parentVar.getName())) {
                containsVar = true;
                break;
            }
        }
        return containsVar;
    }

    private String getProtoVersion() {
        return this.protoVersion;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.PROTOBUF; }
}
