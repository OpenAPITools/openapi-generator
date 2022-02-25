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

import org.openapitools.codegen.*;
import org.openapitools.codegen.exceptions.ProtoBufIndexComputationException;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.utils.ModelUtils;

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

    private final Logger LOGGER = LoggerFactory.getLogger(ProtobufSchemaCodegen.class);

    protected String packageName = "openapitools";

    private boolean numberedFieldNumberList = false;

    private boolean startEnumsWithUnknown = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    public String getName() {
        return "protobuf-schema";
    }

    public String getHelp() {
        return "Generates gRPC and protocol buffer schema files (beta)";
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
        modelTemplateFiles.put("model.mustache", ".proto");
        apiTemplateFiles.put("api.mustache", ".proto");
        embeddedTemplateDir = templateDir = "protobuf-schema";
        hideGenerationTimestamp = Boolean.TRUE;
        modelPackage = "models";
        apiPackage = "services";

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
        typeMapping.put("object", "TODO_OBJECT_MAPPING");

        importMapping.clear();

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();

        addSwitch(NUMBERED_FIELD_NUMBER_LIST, "Field numbers in order.", numberedFieldNumberList);
        addSwitch(START_ENUMS_WITH_UNKNOWN, "Introduces \"UNKNOWN\" as the first element of enumerations.", startEnumsWithUnknown);
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
    public void addEnumValuesPrefix(Map<String, Object> allowableValues, String prefix){
        if(allowableValues.containsKey("enumVars")) {
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");

            for(Map<String, Object> value : enumVars) {
                String name = (String)value.get("name");
                value.put("name", prefix + "_" + name);
                value.put("value", "\"" + prefix + "_" + name + "\"");
            }
        }

        if(allowableValues.containsKey("values")) {
            List<String> values = (List<String>)allowableValues.get("values");
            for(String value : values) {
                value = prefix + "_" + value;
            }
        }
    }

    /**
     * Adds unknown value to the enum allowable values
     *
     * @param allowableValues allowable values
     */
    public void addUnknownToAllowableValues(Map<String, Object> allowableValues) {
        if(startEnumsWithUnknown) {
            if(allowableValues.containsKey("enumVars")) {
                List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");

                HashMap<String, Object> unknown = new HashMap<String, Object>();
                unknown.put("name", "UNKNOWN");
                unknown.put("isString", "false");
                unknown.put("value", "\"UNKNOWN\"");

                enumVars.add(0, unknown);
            }

            if(allowableValues.containsKey("values")) {
                List<String> values = (List<String>)allowableValues.get("values");           
                values.add(0, "UNKNOWN");
            }
        }
    }

    /**
     * Iterates enum vars and puts index to them
     *
     * @param enumVars list of enum vars
     */
    public void addEnumIndexes(List<Map<String, Object>> enumVars) {
        int enumIndex = 0;
        for (Map<String, Object> enumVar : enumVars) {
            enumVar.put("protobuf-enum-index", enumIndex);
            enumIndex++;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = postProcessModelsEnum(objs);
        List<Object> models = (List<Object>) objs.get("models");

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            if(cm.isEnum) {
                Map<String, Object> allowableValues = cm.getAllowableValues();
                addUnknownToAllowableValues(allowableValues);
                addEnumValuesPrefix(allowableValues, cm.getClassname());
                if (allowableValues.containsKey("enumVars")) {
                    List<Map<String, Object>> enumVars = (List<Map<String, Object>>)allowableValues.get("enumVars");
                    addEnumIndexes(enumVars);
                }
            }

            int index = 1;
            for (CodegenProperty var : cm.vars) {
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

                if (var.isEnum) {
                    addUnknownToAllowableValues(var.allowableValues);
                    addEnumValuesPrefix(var.allowableValues, var.getEnumName());
                
                    if(var.allowableValues.containsKey("enumVars")) {
                        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) var.allowableValues.get("enumVars");
                        addEnumIndexes(enumVars);
                    }
                }

                // Add x-protobuf-index, unless already specified
                if(this.numberedFieldNumberList) {
                    var.vendorExtensions.putIfAbsent("x-protobuf-index", index);
                    index++;
                }
                else {
                    try {
                        var.vendorExtensions.putIfAbsent("x-protobuf-index", generateFieldNumberFromString(var.getName()));
                    } catch (ProtoBufIndexComputationException e) {
                        LOGGER.error("Exception when assigning a index to a protobuf field", e);
                        var.vendorExtensions.putIfAbsent("x-protobuf-index", "Generated field number is in reserved range (19000, 19999)");
                    }
                }
            }
        }
        return objs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
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

    public void addImport(Map<String, Object> objs, CodegenModel cm, String importValue) {
        String modelFileName = this.toModelFilename(importValue);
        boolean skipImport = isImportAlreadyPresentInModel(objs, cm, modelFileName);
        if (!skipImport) {
            this.addImport(cm, importValue);
            Map<String, Object> importItem = new HashMap<>();
            importItem.put(IMPORT, modelFileName);
            ((List<Map<String, Object>>) ((Map<String, Object>) objs.get(cm.getName())).get(IMPORTS)).add(importItem);
        }
    }

    private boolean isImportAlreadyPresentInModel(Map<String, Object> objs, CodegenModel cm, String importValue) {
        boolean skipImport = false;
        List<Map<String, Object>> cmImports = ((List<Map<String, Object>>) ((Map<String, Object>) objs.get(cm.getName())).get(IMPORTS));
        for (Map<String, Object> cmImportItem : cmImports) {
            for (Entry<String, Object> cmImportItemEntry : cmImportItem.entrySet()) {
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            int index = 1;
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

                p.vendorExtensions.putIfAbsent("x-protobuf-index", index);
                index++;
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

    @Override
    public String toModelImport(String name) {
        return underscore(name);
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

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.PROTOBUF; }
}
