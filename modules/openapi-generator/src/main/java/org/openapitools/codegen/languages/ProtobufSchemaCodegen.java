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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.base.CaseFormat;

import static org.openapitools.codegen.utils.StringUtils.*;

public class ProtobufSchemaCodegen extends DefaultCodegen implements CodegenConfig {

    private static final String IMPORT = "import";

    private static final String IMPORTS = "imports";

    private static final String ARRAY_SUFFIX = "Array";

    private static final String MAP_SUFFIX = "Map";

    public static final String NUMBERED_FIELD_NUMBER_LIST = "numberedFieldNumberList";

    public static final String START_ENUMS_WITH_UNSPECIFIED = "startEnumsWithUnspecified";

    public static final String ADD_JSON_NAME_ANNOTATION = "addJsonNameAnnotation";

    public static final String WRAP_COMPLEX_TYPE = "wrapComplexType";

    public static final String USE_SIMPLIFIED_ENUM_NAMES = "useSimplifiedEnumNames";

    public static final String AGGREGATE_MODELS_NAME = "aggregateModelsName";

    public static final String CUSTOM_OPTIONS_API = "customOptionsApi";

    public static final String CUSTOM_OPTIONS_MODEL = "customOptionsModel";

    public static final String SUPPORT_MULTIPLE_RESPONSES = "supportMultipleResponses";

    private final Logger LOGGER = LoggerFactory.getLogger(ProtobufSchemaCodegen.class);

    @Setter protected String packageName = "openapitools";

    @Setter protected String aggregateModelsName = null;

    @SuppressWarnings("unused")
    @Setter protected String customOptionsApi = null;

    @SuppressWarnings("unused")
    @Setter protected String customOptionsModel = null;

    private boolean numberedFieldNumberList = false;

    private boolean startEnumsWithUnspecified = false;

    private boolean addJsonNameAnnotation = false;

    private boolean wrapComplexType = true;

    private boolean useSimplifiedEnumNames = false;

    private boolean supportMultipleResponses = true;

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return StringUtils.capitalize(property.name);
    }

    @Override
    public String getName() {
        return "protobuf-schema";
    }

    @Override
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
                        "set",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "map",
                        "set",
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
        instantiationTypes.put("set", "repeat");


        // ref: https://developers.google.com/protocol-buffers/docs/proto
        typeMapping.clear();
        typeMapping.put("set", "array");
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
        addSwitch(START_ENUMS_WITH_UNSPECIFIED, "Introduces \"UNSPECIFIED\" as the first element of enumerations.", startEnumsWithUnspecified);
        addSwitch(ADD_JSON_NAME_ANNOTATION, "Append \"json_name\" annotation to message field when the specification name differs from the protobuf field name", addJsonNameAnnotation);
        addSwitch(WRAP_COMPLEX_TYPE, "Generate Additional message for complex type", wrapComplexType);
        addSwitch(USE_SIMPLIFIED_ENUM_NAMES, "Use a simple name for enums", useSimplifiedEnumNames);
        addSwitch(SUPPORT_MULTIPLE_RESPONSES, "Support multiple responses", supportMultipleResponses);
        addOption(AGGREGATE_MODELS_NAME, "Aggregated model filename. If set, all generated models will be combined into this single file.", null);
        addOption(CUSTOM_OPTIONS_API, "Custom options for the api files.", null);
        addOption(CUSTOM_OPTIONS_MODEL, "Custom options for the model files.", null);
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
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }

        if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        if (additionalProperties.containsKey(NUMBERED_FIELD_NUMBER_LIST)) {
            this.numberedFieldNumberList = convertPropertyToBooleanAndWriteBack(NUMBERED_FIELD_NUMBER_LIST);
        }

        if (additionalProperties.containsKey(START_ENUMS_WITH_UNSPECIFIED)) {
            this.startEnumsWithUnspecified = convertPropertyToBooleanAndWriteBack(START_ENUMS_WITH_UNSPECIFIED);
        }

        if (additionalProperties.containsKey(ADD_JSON_NAME_ANNOTATION)) {
            this.addJsonNameAnnotation = convertPropertyToBooleanAndWriteBack(ADD_JSON_NAME_ANNOTATION);
        }

        if (additionalProperties.containsKey(WRAP_COMPLEX_TYPE)) {
            this.wrapComplexType = convertPropertyToBooleanAndWriteBack(WRAP_COMPLEX_TYPE);
        }

        if (additionalProperties.containsKey(USE_SIMPLIFIED_ENUM_NAMES)) {
            this.useSimplifiedEnumNames = convertPropertyToBooleanAndWriteBack(USE_SIMPLIFIED_ENUM_NAMES);
        }

        if (additionalProperties.containsKey(AGGREGATE_MODELS_NAME)) {
            this.setAggregateModelsName((String) additionalProperties.get(AGGREGATE_MODELS_NAME));
        }

        if (additionalProperties.containsKey(CUSTOM_OPTIONS_API)) {
            this.setCustomOptionsApi((String) additionalProperties.get(CUSTOM_OPTIONS_API));
        }

        if (additionalProperties.containsKey(CUSTOM_OPTIONS_MODEL)) {
            this.setCustomOptionsModel((String) additionalProperties.get(CUSTOM_OPTIONS_MODEL));
        }
          
        if (additionalProperties.containsKey(this.SUPPORT_MULTIPLE_RESPONSES)) {
            this.supportMultipleResponses = convertPropertyToBooleanAndWriteBack(SUPPORT_MULTIPLE_RESPONSES);
        } else {
            additionalProperties.put(this.SUPPORT_MULTIPLE_RESPONSES, this.supportMultipleResponses);
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
     * Creates an array schema from the provided object schema.
     *
     *  @param objectSchema the schema of the object to be wrapped in an array schema
     *  @return the created array schema
     */
    private Schema createArraySchema(Schema objectSchema) {
        ArraySchema arraySchema = new ArraySchema();
        arraySchema.items(objectSchema);
        return arraySchema;
    }


    /**
     * Creates a map schema from the provided object schema.
     *
     *  @param objectSchema the schema of the object to be wrapped in a map schema
     *  @return the created map schema
     */
    private Schema createMapSchema(Schema objectSchema) {
        MapSchema mapSchema = new MapSchema();
        mapSchema.additionalProperties(objectSchema);
        return mapSchema;
    }

    /**
     * Adds a new schema to the OpenAPI components.
     *
     * @param schema the schema to be added
     * @param schemaName the name of the schema
     * @param visitedSchema a set of schemas that have already been visited
     * @return the reference schema
     */
    private Schema addSchemas(Schema schema, String schemaName, Set<Schema> visitedSchema) {
        LOGGER.info("Generating new model: {}", schemaName);

        ObjectSchema model = new ObjectSchema();
        model.setName(schemaName);

        Map<String, Schema> properties = new HashMap<>();
        properties.put(toVarName(schemaName), schema);
        model.setProperties(properties);

        Schema refSchema = new Schema();
        refSchema.set$ref("#/components/schemas/" + schemaName);
        refSchema.setName(schemaName);

        visitedSchema.add(refSchema);

        openAPI.getComponents().addSchemas(schemaName, model);

        return refSchema;
    }

    /**
     * Derive name from schema primitive type
     *
     *  @param schema the schema to derive the name from
     *  @return the derived name
     */
    private String getNameFromSchemaPrimitiveType(Schema schema) {
        if (!ModelUtils.isPrimitiveType(schema)) return "";
        if(ModelUtils.isNumberSchema(schema)) {
            if(schema.getFormat() != null) {
                return schema.getFormat();
            } else if (typeMapping.get(schema.getType()) != null) {
                return typeMapping.get(schema.getType());
            }
        }
        return ModelUtils.getType(schema);
    }

    /**
     * Recursively generates schemas for nested maps and arrays.
     * @param schema the schema to be processed
     * @param visitedSchemas a set of schemas that have already been visited
     * @return the processed schema
     */
    private Schema generateNestedSchema(Schema schema, Set<Schema> visitedSchemas) {
        if (visitedSchemas.contains(schema)) {
            LOGGER.warn("Skipping recursive schema");
            return schema;
        }

        if(ModelUtils.isArraySchema(schema)) {
            Schema itemsSchema = ModelUtils.getSchemaItems(schema);
            itemsSchema = ModelUtils.getReferencedSchema(openAPI, itemsSchema);
            if(ModelUtils.isModel(itemsSchema)) {
                String newSchemaName = ModelUtils.getSimpleRef(ModelUtils.getSchemaItems(schema).get$ref()) + ARRAY_SUFFIX;
                return addSchemas(schema, newSchemaName, visitedSchemas);
            }else if (ModelUtils.isPrimitiveType(itemsSchema)){
                String newSchemaName = getNameFromSchemaPrimitiveType(itemsSchema) + ARRAY_SUFFIX;
                return addSchemas(schema, newSchemaName, visitedSchemas);
            } else {
                Schema childSchema = generateNestedSchema(itemsSchema, visitedSchemas);
                String newSchemaName = childSchema.getName() + ARRAY_SUFFIX;
                Schema arrayModel = createArraySchema(childSchema);
                return addSchemas(arrayModel, newSchemaName, visitedSchemas);
            }
        } else if(ModelUtils.isMapSchema(schema)) {
            Schema mapValueSchema = ModelUtils.getAdditionalProperties(schema);
            mapValueSchema = ModelUtils.getReferencedSchema(openAPI, mapValueSchema);
            if(ModelUtils.isModel(mapValueSchema) ) {
                String newSchemaName = ModelUtils.getSimpleRef(ModelUtils.getAdditionalProperties(schema).get$ref()) + MAP_SUFFIX;
                return addSchemas(schema, newSchemaName, visitedSchemas);
            }else if (ModelUtils.isPrimitiveType(mapValueSchema)){
                String newSchemaName = getNameFromSchemaPrimitiveType(mapValueSchema) + MAP_SUFFIX;
                return addSchemas(schema, newSchemaName, visitedSchemas);
            } else {
                Schema innerSchema = generateNestedSchema(mapValueSchema, visitedSchemas);
                String newSchemaName = innerSchema.getName() + MAP_SUFFIX;
                Schema mapModel = createMapSchema(innerSchema);
                return addSchemas(mapModel, newSchemaName, visitedSchemas);
            }
        }
        return schema;
    }

    /**
     * Processes nested schemas for complex type(map, array, oneOf)
     *
     *  @param schema the schema to be processed
     *  @param visitedSchemas a set of schemas that have already been visited
     */
    private void processNestedSchemas(Schema schema, Set<Schema> visitedSchemas) {
        if (ModelUtils.isMapSchema(schema) && ModelUtils.getAdditionalProperties(schema) != null) {
            Schema mapValueSchema = ModelUtils.getAdditionalProperties(schema);
            mapValueSchema = ModelUtils.getReferencedSchema(openAPI, mapValueSchema);
            if (ModelUtils.isArraySchema(mapValueSchema) || (ModelUtils.isMapSchema(mapValueSchema) && !ModelUtils.isModel(mapValueSchema))) {
                Schema innerSchema = generateNestedSchema(mapValueSchema, visitedSchemas);
                schema.setAdditionalProperties(innerSchema);

            }
        } else if (ModelUtils.isArraySchema(schema) && ModelUtils.getSchemaItems(schema) != null) {
            Schema arrayItemSchema = ModelUtils.getSchemaItems(schema);
            arrayItemSchema = ModelUtils.getReferencedSchema(openAPI, arrayItemSchema);
            if ((ModelUtils.isMapSchema(arrayItemSchema) && !ModelUtils.isModel(arrayItemSchema)) || ModelUtils.isArraySchema(arrayItemSchema)) {
                Schema innerSchema = generateNestedSchema(arrayItemSchema, visitedSchemas);
                schema.setItems(innerSchema);
            }
        } else if (ModelUtils.isOneOf(schema) && schema.getOneOf() != null) {
            List<Schema> oneOfs = schema.getOneOf();
            List<Schema> newOneOfs = new ArrayList<>();
            for (Schema oneOf : oneOfs) {
                Schema oneOfSchema = ModelUtils.getReferencedSchema(openAPI, oneOf);
                if (ModelUtils.isArraySchema(oneOfSchema)) {
                    Schema innerSchema = generateNestedSchema(oneOfSchema, visitedSchemas);
                    innerSchema.setTitle(oneOf.getTitle());
                    newOneOfs.add(innerSchema);
                } else if (ModelUtils.isMapSchema(oneOfSchema) && !ModelUtils.isModel(oneOfSchema)) {
                    Schema innerSchema = generateNestedSchema(oneOfSchema, visitedSchemas);
                    innerSchema.setTitle(oneOf.getTitle());
                    newOneOfs.add(innerSchema);
                } else {
                    newOneOfs.add(oneOf);
                }
            }
            schema.setOneOf(newOneOfs);
        }
    }

    /**
     * Traverses models and properties to wrap nested schemas.
     */
    private void wrapModels() {
        Map<String, Schema> models = openAPI.getComponents().getSchemas();
        Set<Schema> visitedSchema = new HashSet<>();
        List<String> modelNames = new ArrayList<String>(models.keySet());
        for (String modelName: modelNames) {
            Schema schema = models.get(modelName);
            processNestedSchemas(schema, visitedSchema);
            if (ModelUtils.isModel(schema) && schema.getProperties() != null) {
                Map<String, Schema> properties = schema.getProperties();
                for (Map.Entry<String, Schema> propertyEntry : properties.entrySet()) {
                    Schema propertySchema = propertyEntry.getValue();
                    processNestedSchemas(propertySchema, visitedSchema);
                }
            }  else if (ModelUtils.isAllOf(schema)) {
                wrapComposedChildren(schema.getAllOf(), visitedSchema);
            } else if (ModelUtils.isOneOf(schema)) {
                wrapComposedChildren(schema.getOneOf(), visitedSchema);
            } else if (ModelUtils.isAnyOf(schema)) {
                wrapComposedChildren(schema.getAnyOf(), visitedSchema);
            }
        }
    }

    /**
     * Traverses a composed schema and its properties to wrap nested schemas.
     *
     * @param children the list of child schemas to be processed
     * @param visitedSchema a set of schemas that have already been visited
     */
    private void wrapComposedChildren(List<Schema> children, Set<Schema> visitedSchema) {
        if (children == null || children.isEmpty()) {
            return;
        }
        for(Schema child: children) {
            child = ModelUtils.getReferencedSchema(openAPI, child);
            Map<String, Schema> properties = child.getProperties();
            if(properties == null || properties.isEmpty()) continue;
            for(Map.Entry<String, Schema> propertyEntry : properties.entrySet()) {
                Schema propertySchema = propertyEntry.getValue();
                processNestedSchemas(propertySchema, visitedSchema);
            }
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        if (wrapComplexType) {
            wrapModels();
        }
    }

    /**
     * Adds prefix to the enum allowable values
     * NOTE: Enum values use C++ scoping rules, meaning that enum values are siblings of their type, not children of it. Therefore, enum value must be unique
     *
     * @param allowableValues allowable values
     * @param prefix          added prefix
     */
    public void addEnumValuesPrefix(Map<String, Object> allowableValues, String prefix) {
        if (allowableValues.containsKey("enumVars")) {
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");
            prefix = CaseFormat.LOWER_CAMEL.to(CaseFormat.UPPER_UNDERSCORE, prefix);
            for (Map<String, Object> value : enumVars) {
                String name = (String) value.get("name");
                value.put("name", useSimplifiedEnumNames ? name : prefix + "_" + name);
                value.put("value", useSimplifiedEnumNames ? name : "\"" + prefix + "_" + name + "\"");
            }
        }

        if (allowableValues.containsKey("values")) {
            List<Object> values = (List<Object>) allowableValues.get("values");
            for (Object value : values) {
                value = useSimplifiedEnumNames ? value : prefix + "_" + value;
            }
        }
    }

    /**
     * Adds unknown value to the enum allowable values
     *
     * @param allowableValues allowable values
     */
    public void addUnspecifiedToAllowableValues(Map<String, Object> allowableValues) {
        if (startEnumsWithUnspecified) {
            if (allowableValues.containsKey("enumVars")) {
                List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");

                HashMap<String, Object> unspecified = new HashMap<String, Object>();
                unspecified.put("name", "UNSPECIFIED");
                unspecified.put("isString", "false");
                unspecified.put("value", "\"UNSPECIFIED\"");
                enumVars.add(0, unspecified);
            }

            if (allowableValues.containsKey("values")) {
                List<String> values = (List<String>) allowableValues.get("values");
                List<String> modifiableValues = new ArrayList<>(values);
                modifiableValues.add(0, "UNSPECIFIED");
                allowableValues.put("values", modifiableValues);
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

    public List<CodegenProperty> processOneOfAnyOfItems(List<CodegenProperty> composedSchemasProperty) {
        for(CodegenProperty cd: composedSchemasProperty) {
            cd.name = resolveVarName(cd);
            cd.baseName = resolveVarName(cd);
        }
        return composedSchemasProperty;
    }


    private String resolveVarName(CodegenProperty property) {
        if(property.getTitle() != null) {
            return toVarName(property.getTitle());
        } else {
            return getNameFromDataType(property);
        }
    }

    public String getNameFromDataType(CodegenProperty property) {
        if (Boolean.TRUE.equals(property.getIsArray())){
            return toVarName(property.mostInnerItems.dataType + ARRAY_SUFFIX);
        } else if (Boolean.TRUE.equals(property.getIsMap())) {
            return toVarName(property.mostInnerItems.dataType + MAP_SUFFIX);
        } else {
            return toVarName(property.dataType);
        }
    }


    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = postProcessModelsEnum(objs);

        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();

            if (cm.isEnum) {
                Map<String, Object> allowableValues = cm.getAllowableValues();
                addUnspecifiedToAllowableValues(allowableValues);
                addEnumValuesPrefix(allowableValues, cm.getClassname());
                if (allowableValues.containsKey("enumVars")) {
                    List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");
                    addEnumIndexes(enumVars);
                }
            }

            if(cm.oneOf != null && !cm.oneOf.isEmpty()){
                cm.vars = processOneOfAnyOfItems(cm.getComposedSchemas().getOneOf());
            } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) {
                cm.vars = processOneOfAnyOfItems(cm.getComposedSchemas().getAnyOf());
            }
            int index = 1;
            for (CodegenProperty var : cm.vars) {
                // add x-protobuf-type: repeated if it's an array
                if (Boolean.TRUE.equals(var.isArray)) {
                    var.vendorExtensions.put("x-protobuf-type", "repeated");
                } else if (Boolean.TRUE.equals(var.isNullable && var.isPrimitiveType)) {
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
                    addUnspecifiedToAllowableValues(var.allowableValues);
                    addEnumValuesPrefix(var.allowableValues, var.getEnumName());

                    if (var.allowableValues.containsKey("enumVars")) {
                        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) var.allowableValues.get("enumVars");
                        addEnumIndexes(enumVars);
                    }
                }

                // Add x-protobuf-index, unless already specified
                if (this.numberedFieldNumberList) {
                    var.vendorExtensions.putIfAbsent("x-protobuf-index", index);
                    index++;
                } else {
                    try {
                        var.vendorExtensions.putIfAbsent("x-protobuf-index", generateFieldNumberFromString(var.getName()));
                    } catch (ProtoBufIndexComputationException e) {
                        LOGGER.error("Exception when assigning a index to a protobuf field", e);
                        var.vendorExtensions.putIfAbsent("x-protobuf-index", "Generated field number is in reserved range (19000, 19999)");
                    }
                }

                if (addJsonNameAnnotation && !var.baseName.equals(var.name)) {
                    var.vendorExtensions.put("x-protobuf-json-name", var.baseName);
                }
            }
        }
        return objs;
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
        return aggregateModelsName == null ? objs : aggregateModels(objs);
    }

    /**
     * Aggregates all individual model definitions into a single entry.
     *
     * @param objs the original map of model names to their respective entries
     * @return a new {@link Map} containing a single entry keyed by {@code aggregateModelsName} with
     *         combined models and imports from all provided entries
     */
    public Map<String, ModelsMap> aggregateModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> objects = new HashMap<>();
        ModelsMap aggregateObj = objs.values().stream()
                .findFirst()
                .orElse(new ModelsMap());

        List<ModelMap> models = objs.values().stream()
                .flatMap(modelsMap -> modelsMap.getModels().stream())
                .collect(Collectors.toList());

        Set<Map<String, String>> imports = objs.values().stream()
                .flatMap(modelsMap -> modelsMap.getImports().stream())
                .filter(importMap -> !importMap.get("import").startsWith("models/"))
                .collect(Collectors.toSet());

        aggregateObj.setModels(models);
        aggregateObj.setImports(new ArrayList<>(imports));
        objects.put(this.aggregateModelsName, aggregateObj);
        return objects;
    }

    public void addImport(Map<String, ModelsMap> objs, CodegenModel cm, String importValue) {
        String modelFileName = this.toModelFilename(importValue);
        boolean skipImport = isImportAlreadyPresentInModel(objs, cm, modelFileName);
        if (!skipImport) {
            this.addImport(cm, importValue);
            Map<String, String> importItem = new HashMap<>();
            importItem.put(IMPORT, modelFileName);
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
                if (Pattern.compile("\r\n|\r|\n").matcher(String.valueOf(p.getDefault())).find())
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

    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(toModelName(name));
    }

    @Override
    public String toVarName(String name) {
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all upper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        // underscore the variable name
        // petId => pet_id
        name = underscore(name);

        // remove leading underscore
        name = name.replaceAll("^_*", "");

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        name = name.replace("$", "");

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
            int index = 1;
            for (CodegenParameter p : op.allParams) {
                // add x-protobuf-type: repeated if it's an array

                if (Boolean.TRUE.equals(p.isArray)) {
                    p.vendorExtensions.put("x-protobuf-type", "repeated");
                } else if (Boolean.TRUE.equals(p.isNullable && p.isPrimitiveType)) {
                    p.vendorExtensions.put("x-protobuf-type", "optional");
                } else if (Boolean.TRUE.equals(p.isMap)) {
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

                if (addJsonNameAnnotation && !p.baseName.equals(p.paramName) && !p.isBodyParam) {
                    p.vendorExtensions.put("x-protobuf-json-name", p.baseName);
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

            if(this.supportMultipleResponses) {
                int responseIdx = 1;
                op.vendorExtensions.put("x-grpc-response", op.operationId+"Response");
                for (CodegenResponse r : op.responses) {
                    if (r.returnProperty == null) {
                        r.vendorExtensions.put("x-oneOf-response-type", "google.protobuf.Empty");
                        r.vendorExtensions.put("x-oneOf-response-name", "empty");
                    } else if (r.isMap && r.additionalProperties != null) {
                        r.vendorExtensions.put("x-oneOf-response-type", r.returnProperty.additionalProperties.dataType);
                        r.vendorExtensions.put("x-oneOf-response-name", resolveVarName(r.returnProperty.additionalProperties));
                        LOGGER.warn("Mapping responses for operations with supportMultipleResponses flag (operation ID: {}) is not currently supported.", op.operationId);
                    } else if (r.isArray && r.items != null) {
                        r.vendorExtensions.put("x-oneOf-response-type", r.returnProperty.items.dataType);
                        r.vendorExtensions.put("x-oneOf-response-name", resolveVarName(r.returnProperty.items));
                        LOGGER.warn("Array responses for operations with supportMultipleResponses flag (operation ID: {}) is not currently supported.", op.operationId);
                    }
                    else {
                        r.vendorExtensions.put("x-oneOf-response-type", r.returnProperty.dataType);
                        r.vendorExtensions.put("x-oneOf-response-name", resolveVarName(r.returnProperty));
                    }
                    r.vendorExtensions.put("x-oneOf-response-index", responseIdx++);
                }
            }
        }

        if (this.aggregateModelsName != null) {
            List<Map<String, String>> imports = objs.getImports().stream()
                    .filter(importMap -> !importMap.get("import").startsWith("models/"))
                    .collect(Collectors.toList());

            List<Map<String, String>> aggregate_imports = Collections.singletonList(Collections
                    .singletonMap(IMPORT, toModelImport(this.aggregateModelsName)));
            imports.addAll(aggregate_imports);
            objs.setImports(imports);
        }
        return objs;
    }

    @Override
    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "/" + underscore(name);
        }
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
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
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.PROTOBUF;
    }


/**
 * Handles additionalProperties defined in composed schemas (e.g., allOf) by injecting into the model's properties.
 * Example:
 *  components:
 *    schemas:
 *      Dog:
 *        allOf:
 *          - $ref: '#/components/schemas/DogBase'
 *          - type: object
 *            additionalProperties:
 *              title: pet
 *              $ref: '#/components/schemas/Pet'
 * In this case, the second allOf that defines a map with string keys and Pet values will be part of model's property.
 */
    @Override
    protected void addProperties(Map<String, Schema> properties, List<String> required, Schema schema, Set<Schema> visitedSchemas){
        super.addProperties(properties, required, schema, visitedSchemas);
        if(schema.getAdditionalProperties() != null) {
            String addtionalPropertiesName = "default_map";
            if(schema.getTitle() != null) {
                addtionalPropertiesName = schema.getTitle();
            } else {
                Schema additionalProperties = ModelUtils.getAdditionalProperties(schema);
                if (additionalProperties.getTitle() != null) {
                    addtionalPropertiesName = additionalProperties.getTitle();
                } else if (additionalProperties.get$ref() != null) {
                    String ref = ModelUtils.getSimpleRef(additionalProperties.get$ref());
                    addtionalPropertiesName = toVarName(toModelName(ref));
                }
            }
            properties.put(addtionalPropertiesName, schema);
        }
    }
}
