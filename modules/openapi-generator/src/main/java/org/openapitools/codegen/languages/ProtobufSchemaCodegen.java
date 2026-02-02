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
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
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

    public static final String EXTRACT_ENUMS_TO_SEPARATE_FILES = "extractEnumsToSeparateFiles";

    /**
     * The inner enum name used when wrapping extracted enums in message containers.
     * This prevents enum value name collisions in the Protocol Buffers global namespace.
     */
    public static final String ENUM_WRAPPER_INNER_NAME = "Enum";

    /**
     * Vendor extension key indicating that an enum property has been extracted to a separate file.
     */
    private static final String VENDOR_EXT_ENUM_EXTRACTED = "x-protobuf-enum-extracted-to-file";

    /**
     * Vendor extension key for the wrapper message name of an extracted enum.
     */
    private static final String VENDOR_EXT_ENUM_WRAPPER_MESSAGE = "x-protobuf-enum-wrapper-message";

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

    private boolean extractEnumsToSeparateFiles = false;

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
        super.setModelPackage("models");
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
        addSwitch(EXTRACT_ENUMS_TO_SEPARATE_FILES, "Extract enums to separate protobuf files and import them in models", extractEnumsToSeparateFiles);
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

        if (additionalProperties.containsKey(EXTRACT_ENUMS_TO_SEPARATE_FILES)) {
            this.extractEnumsToSeparateFiles = convertPropertyToBooleanAndWriteBack(EXTRACT_ENUMS_TO_SEPARATE_FILES);
        } else {
            additionalProperties.put(EXTRACT_ENUMS_TO_SEPARATE_FILES, this.extractEnumsToSeparateFiles);
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
            if(ModelUtils.isModel(itemsSchema) || (itemsSchema != null && ModelUtils.isEnumSchema(itemsSchema))) {
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
            if(ModelUtils.isModel(mapValueSchema) || (mapValueSchema != null && ModelUtils.isEnumSchema(mapValueSchema))) {
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
                Schema oneOfSchema = oneOf;
                if (ModelUtils.isAllOf(oneOf) && oneOf.getAllOf() != null && oneOf.getAllOf().size() == 1) {
                    Object allOfObj = oneOf.getAllOf().get(0);
                    if (allOfObj instanceof Schema) {
                        Schema allOfItem = (Schema) allOfObj;
                        if (StringUtils.isNotEmpty(allOfItem.get$ref())) {
                            oneOfSchema = ModelUtils.getReferencedSchema(openAPI, allOfItem);
                        }
                    }
                } else {
                    oneOfSchema = ModelUtils.getReferencedSchema(openAPI, oneOf);
                }

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

        final String UNSPECIFIED = "UNSPECIFIED";

        if (startEnumsWithUnspecified) {
            if (allowableValues.containsKey("enumVars")) {
                List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");
                boolean unspecifiedPresent = enumVars.stream()
                        .anyMatch(e -> {
                            return UNSPECIFIED.equals(e.get("name"));
                        });
                if (!unspecifiedPresent) {
                    HashMap<String, Object> unspecifiedEnum = new HashMap<String, Object>();
                    unspecifiedEnum.put("name", UNSPECIFIED);
                    unspecifiedEnum.put("isString", "false");
                    unspecifiedEnum.put("value", "\"" + UNSPECIFIED + "\"");
                    enumVars.add(0, unspecifiedEnum);
                }
            }

            if (allowableValues.containsKey("values")) {
                List<String> values = (List<String>) allowableValues.get("values");
                if (!values.contains(UNSPECIFIED)) {
                    List<String> modifiableValues = new ArrayList<>(values);
                    modifiableValues.add(0, UNSPECIFIED);
                    allowableValues.put("values", modifiableValues);
                }
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

    /**
     * Post-processes CodegenModel objects to apply protobuf-specific transformations.
     * 
     * <p>This method performs several critical operations:
     * <ol>
     *   <li>Processes enum definitions and optionally extracts them to separate files</li>
     *   <li>Sets vendor extensions for protobuf field types and data types</li>
     *   <li>Assigns field numbers based on configuration</li>
     *   <li>Handles oneOf/anyOf composition schemas</li>
     * </ol>
     * 
     * <p><b>Enum Extraction Behavior</b>: When {@code extractEnumsToSeparateFiles} is enabled,
     * inline enum properties are extracted to separate .proto files wrapped in message containers.
     * This prevents enum value name collisions in the protobuf global namespace. The resulting
     * format is:
     * <pre>
     * message EnumName {
     *   enum Enum {
     *     VALUE1 = 0;
     *     VALUE2 = 1;
     *   }
     * }
     * </pre>
     * 
     * References to these enums use the qualified name {@code EnumName.Enum}.
     * 
     * @param objs the models map containing all CodegenModel objects
     * @return the modified models map with protobuf-specific transformations applied
     * @see #extractEnumsToSeparateFiles(ModelsMap)
     * @see #extractEnums(ModelsMap)
     */
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
            for (CodegenProperty property : cm.vars) {
                // add x-protobuf-type: repeated if it's an array
                if (Boolean.TRUE.equals(property.isArray)) {
                    property.vendorExtensions.put("x-protobuf-type", "repeated");
                } else if (Boolean.TRUE.equals(property.isNullable && property.isPrimitiveType)) {
                    property.vendorExtensions.put("x-protobuf-type", "optional");
                }

                // add x-protobuf-data-type
                // ref: https://developers.google.com/protocol-buffers/docs/proto3
                if (!property.vendorExtensions.containsKey("x-protobuf-data-type")) {
                    if (property.isArray) {
                        property.vendorExtensions.put("x-protobuf-data-type", property.items.dataType);
                    } else {
                        property.vendorExtensions.put("x-protobuf-data-type", property.dataType);
                    }
                }

                // Check if this property is an enum or an array of enums.
                // 
                // This handles two distinct cases:
                // 1. Direct enum property: property.isEnum=true
                //    Example OpenAPI: myStatus: {$ref: '#/components/schemas/Status'}
                //    Generated Protobuf: Status.Enum my_status = N;
                //
                // 2. Array of enums: property.isArray=true && property.items.isEnum=true
                //    Example OpenAPI: tags: {type: array, items: {$ref: '#/components/schemas/Tag'}}
                //    Generated Protobuf: repeated Tag.Enum tags = N;
                //
                // Both cases require the same enum extraction and wrapper processing when
                // EXTRACT_ENUMS_TO_SEPARATE_FILES is enabled. This fix ensures arrays of enums
                // are handled consistently with direct enum references (previously arrays were
                // not being wrapped with the .Enum suffix).
                boolean isDirectEnum = property.isEnum;
                boolean isArrayOfEnums = property.isArray && property.items != null && property.items.isEnum;
                
                if (isDirectEnum || isArrayOfEnums) {
                    // For arrays of enums, extract enum values from items property;
                    // for direct enums, extract from the property itself.
                    CodegenProperty enumProperty = isArrayOfEnums ? property.items : property;
                    
                    addUnspecifiedToAllowableValues(enumProperty.allowableValues);
                    addEnumValuesPrefix(enumProperty.allowableValues, enumProperty.getEnumName());

                    if (enumProperty.allowableValues.containsKey("enumVars")) {
                        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.allowableValues.get("enumVars");
                        addEnumIndexes(enumVars);
                    }
                    
                    // Process enum extraction when EXTRACT_ENUMS_TO_SEPARATE_FILES is enabled.
                    // This prevents naming collisions when multiple models have inline enums with the same field name.
                    // 
                    // Naming Convention:
                    //   Wrapper message name format: ParentModelName_FieldName
                    //   File name format: snake_case(wrapper_message_name).proto
                    //   Enum reference in model: ParentModelName_FieldName.Enum
                    // 
                    // Example:
                    //   Model A has field "inlineEnumProperty" -> ModelA_InlineEnumProperty.proto
                    //   Model B has field "inlineEnumProperty" -> ModelB_InlineEnumProperty.proto
                    //   (Without parent prefix, both would create InlineEnumProperty.proto - causing collision)
                    // 
                    // Vendor Extensions Set:
                    //   x-protobuf-enum-extracted-to-file: Boolean flag for extraction
                    //   x-protobuf-enum-wrapper-message: The wrapper message name (for later use in extractEnums())
                    //   x-protobuf-enum-reference-import: Prevents inline rendering in template
                    //   x-protobuf-data-type: Full reference with .Enum suffix for property type
                    if (this.extractEnumsToSeparateFiles) {
                        property.vendorExtensions.put("x-protobuf-enum-extracted-to-file", true);
                        property.vendorExtensions.put("x-protobuf-enum-reference-import", true);
                        
                        // Compute the wrapper message name: ParentModelName_FieldName
                        // This naming scheme ensures uniqueness across models
                        String enumTypeName = cm.getClassname() + "_" + toModelName(toEnumName(enumProperty));
                        if (StringUtils.isBlank(enumTypeName)) {
                            LOGGER.warn("Unable to determine enum type name for property: {}", property.name);
                            continue;
                        }
                        
                        // Store wrapper message name for use in extractEnums() method
                        property.vendorExtensions.put("x-protobuf-enum-wrapper-message", enumTypeName);
                        
                        // Set property data type to reference the extracted enum wrapper's inner Enum
                        // The template will use this to reference: ParentModelName_FieldName.Enum
                        property.vendorExtensions.put("x-protobuf-data-type", enumTypeName + "." + ENUM_WRAPPER_INNER_NAME);
                    }
                }

                // Add x-protobuf-index, unless already specified
                if (this.numberedFieldNumberList) {
                    property.vendorExtensions.putIfAbsent("x-protobuf-index", index);
                    index++;
                } else {
                    try {
                        property.vendorExtensions.putIfAbsent("x-protobuf-index", generateFieldNumberFromString(property.getName()));
                    } catch (ProtoBufIndexComputationException e) {
                        LOGGER.error("Exception when assigning a index to a protobuf field", e);
                        property.vendorExtensions.putIfAbsent("x-protobuf-index", "Generated field number is in reserved range (19000, 19999)");
                    }
                }

                if (addJsonNameAnnotation && !property.baseName.equals(property.name)) {
                    property.vendorExtensions.put("x-protobuf-json-name", property.baseName);
                }
            }
        }
        
        // Extract enums to separate files if the option is enabled
        if (this.extractEnumsToSeparateFiles) {
            objs = extractEnumsToSeparateFiles(objs);
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

        // Ensure models in discriminator mappings import the discriminator parent
        this.addDiscriminatorParentImports(objs, allModels);

        // Manage children properties for inheritance (must be done before enum extraction)
        this.manageChildrenProperties(objs, allModels);

        // Extract enum properties to separate files if enabled
        if (this.extractEnumsToSeparateFiles) {
            // Recompute allModels after managing children properties
            // This ensures inherited properties are included in the vars
            allModels = this.getAllModels(objs);
            
            // First, update property data types for referenced enum models
            this.updateReferencedEnumPropertyDataTypes(objs, allModels);
            
            Map<String, ModelsMap> extractedEnums = new HashMap<>();
            
            for (Entry<String, ModelsMap> entry : objs.entrySet()) {
                ModelsMap modelsMap = entry.getValue();

                Map<String, CodegenModel> extractedCodegenModelEnums = this.extractEnums(modelsMap);
                for (String enumName : extractedCodegenModelEnums.keySet()) {
                  CodegenModel enumModel = extractedCodegenModelEnums.get(enumName);

                  ModelsMap enumModelsMap = new ModelsMap();
                  ModelMap enumModelMap = new ModelMap();
                  enumModelMap.setModel(enumModel);
                  enumModelsMap.setModels(Arrays.asList(enumModelMap));
                  enumModelsMap.setImports(new ArrayList<>());
                  
                  enumModelsMap.putAll(additionalProperties);
                  extractedEnums.put(enumName, enumModelsMap);
                }
            }
            
            // Add all extracted enums to the objs map so they get generated
            objs.putAll(extractedEnums);
        }

        return aggregateModelsName == null ? objs : aggregateModels(objs);
    }

    /**
     * Updates property data types for properties that reference enum models.
     * When extractEnumsToSeparateFiles is enabled, referenced enum properties
     * need to have their data types updated to include the .Enum suffix
     * to reference the inner enum within the message wrapper.
     * 
     * This method handles:
     * - Direct references to enum models (e.g., property: {$ref: 'MyEnum'})
     * - Arrays of enums (e.g., items with enum dataType)
     * - Maps with enum values
     *
     * @param objs the complete models map
     * @param allModels map of all CodegenModels by name
     */
    private void updateReferencedEnumPropertyDataTypes(Map<String, ModelsMap> objs, Map<String, CodegenModel> allModels) {
        for (CodegenModel model : allModels.values()) {
            if (model.vars == null || model.vars.isEmpty()) {
                continue;
            }
            
            for (CodegenProperty property : model.vars) {
                // Skip properties that are already marked as inline extracted enums
                if (property.vendorExtensions.containsKey(VENDOR_EXT_ENUM_EXTRACTED)) {
                    continue;
                }
                
                // For array/map types, check the inner item's dataType
                String dataTypeToCheck = null;
                CodegenProperty itemToCheck = property;
                
                // Determine what data type to check
                if (property.isArray && property.items != null) {
                    dataTypeToCheck = property.items.dataType;
                    itemToCheck = property.items;
                } else if (property.isMap && property.items != null) {
                    dataTypeToCheck = property.items.dataType;
                    itemToCheck = property.items;
                } else {
                    dataTypeToCheck = property.dataType;
                    itemToCheck = property;
                }
                
                if (StringUtils.isBlank(dataTypeToCheck)) {
                    continue;
                }
                
                // Look for an enum model that matches this dataType
                // Note: Need to apply toModelName to ensure correct key lookup since getAllModels() uses this
                String modelNameKey = toModelName(dataTypeToCheck);
                
                // If the dataType already ends with .Enum, it's already been wrapped
                // Strip the .Enum suffix to find the underlying enum model
                if (dataTypeToCheck.endsWith("." + ENUM_WRAPPER_INNER_NAME)) {
                    // Strip the .Enum suffix for lookup
                    modelNameKey = toModelName(dataTypeToCheck.substring(0, dataTypeToCheck.length() - ("." + ENUM_WRAPPER_INNER_NAME).length()));
                }
                
                CodegenModel referencedModel = allModels.get(modelNameKey);
                if (referencedModel != null && referencedModel.isEnum) {
                    
                    // Use the actual classname from the enum model for consistency with how it's used in templates
                    String enumWrapperName = referencedModel.getClassname();
                    
                    // For referenced enums, the wrapper name is just the enum model's classname
                    // (not a composite like "ModelName_PropertyName" which is for inline enums)
                    
                    // Check if already properly wrapped with .Enum suffix
                    String expectedWrappedType = enumWrapperName + "." + ENUM_WRAPPER_INNER_NAME;
                    
                    // This property references an enum model, update its data type to include .Enum suffix
                    // Also mark it as a referenced extracted enum and add import
                    property.vendorExtensions.put(VENDOR_EXT_ENUM_EXTRACTED, true);
                    property.vendorExtensions.put("x-protobuf-enum-reference-import", true);
                    property.vendorExtensions.put(VENDOR_EXT_ENUM_WRAPPER_MESSAGE, enumWrapperName);
                    
                    // Update the data type of the item to include .Enum suffix
                    itemToCheck.dataType = expectedWrappedType;
                    
                    // Update the vendor extension for protobuf rendering
                    // This is critical for arrays, as the template uses x-protobuf-data-type
                    // For arrays: x-protobuf-data-type must reference the wrapped enum type
                    // For direct properties: the dataType in the template is used
                    if (property.isArray) {
                        // For array properties, the template uses x-protobuf-data-type which comes from items.dataType
                        // Make sure it's updated there
                        property.vendorExtensions.put("x-protobuf-data-type", expectedWrappedType);
                    } else {
                        // For non-array properties, also update vendor extension for consistency
                        property.vendorExtensions.put("x-protobuf-data-type", expectedWrappedType);
                    }
                    
                    // Add import for the referenced enum to the current model
                    this.addImport(objs, model, enumWrapperName);
                }
            }
        }
    }

    /**
     * Ensures that all models in discriminator mappings import the discriminator parent.
     * In protobuf, a child model needs to import the discriminator parent (root of the hierarchy),
     * not just its immediate allOf parent.
     * 
     * This method only adds imports FROM children TO the discriminator parent,
     * not the other way around.
     * 
     * @param objs the complete models map for import tracking
     * @param allModels map of all CodegenModels by name
     */
    private void addDiscriminatorParentImports(Map<String, ModelsMap> objs, Map<String, CodegenModel> allModels) {
        for (CodegenModel model : allModels.values()) {
            if (isDiscriminatorParent(model)) {
                // For each child in the discriminator mapping
                for (MappedModel mappedModel : model.discriminator.getMappedModels()) {
                    String childName = mappedModel.getModelName();
                    CodegenModel childModel = allModels.get(childName);
                    
                    if (childModel == null) {
                        LOGGER.warn("Discriminator mapping references model '{}' which was not found", childName);
                        continue;
                    }
                    
                    // Add import FROM child TO discriminator parent
                    // This ensures D imports A (not just C) in multi-level inheritance
                    // DO NOT add imports from parent to child (that would create circular references)
                    this.addImport(objs, childModel, model.getClassname());
                }
            }
        }
    }

    /**
     * Manages property and import propagation from child models to parent models.
     * In protobuf, inheritance doesn't exist, so all descendant properties must be
     * copied to parent models to achieve pseudo-inheritance.
     * 
     * This method uses a two-phase approach:
     * 1. Bottom-up propagation: Each model copies properties to all its ancestors
     * 2. Discriminator aggregation: Discriminator parents recursively collect from ALL descendants
     * 
     * @param objs the complete models map for import tracking
     * @param allModels map of all CodegenModels by name
     */
    public void manageChildrenProperties(Map<String, ModelsMap> objs, Map<String, CodegenModel> allModels) {
        // Phase 1: Bottom-up property propagation
        // Each child copies its properties to all ancestors in the chain
        for (CodegenModel model : allModels.values()) {
            if (!model.allOf.isEmpty() && model.getParentModel() != null) {
                // Walk up the entire parent chain
                CodegenModel currentAncestor = model.getParentModel();
                
                while (currentAncestor != null) {
                    // Copy properties from this model to the current ancestor
                    copyPropertiesToParent(objs, model, currentAncestor);
                    
                    // Copy imports from this model to the current ancestor
                    copyImportsToParent(objs, model, currentAncestor);
                    
                    // Move to the next ancestor in the chain
                    currentAncestor = currentAncestor.getParentModel();
                }
            }
        }
        
        // Phase 2: Discriminator parent aggregation
        // For models with discriminators, recursively collect properties from ALL descendants
        // This handles multi-level inheritance (grandchildren, great-grandchildren, etc.)
        for (CodegenModel model : allModels.values()) {
            if (isDiscriminatorParent(model)) {
                Set<String> visited = new HashSet<>();
                collectAllDescendantProperties(objs, model, allModels, visited);
            }
        }
        
        // Phase 3: Clean up parent imports for models NOT in discriminator mappings
        // In protobuf, we copy properties instead of using inheritance, so models shouldn't import their parents
        // UNLESS they're explicitly in a discriminator mapping (handled by addDiscriminatorParentImports)
        for (CodegenModel model : allModels.values()) {
            if (model.getParentModel() != null) {
                // Check if this model is in any discriminator mapping
                boolean isInDiscriminatorMapping = false;
                for (CodegenModel potentialParent : allModels.values()) {
                    if (isDiscriminatorParent(potentialParent)) {
                        for (MappedModel mappedModel : potentialParent.discriminator.getMappedModels()) {
                            if (mappedModel.getModelName().equals(model.getClassname())) {
                                isInDiscriminatorMapping = true;
                                break;
                            }
                        }
                    }
                    if (isInDiscriminatorMapping) break;
                }
                
                // If NOT in discriminator mapping, remove parent imports
                // (These were added by base OpenAPI processing but aren't needed in protobuf)
                if (!isInDiscriminatorMapping) {
                    CodegenModel parent = model.getParentModel();
                    while (parent != null) {
                        // Capture parent's classname for use in lambda
                        final String parentClassname = parent.getClassname();
                        
                        // Remove imports matching the parent's name from both model.imports and ModelsMap
                        model.getImports().removeIf(importName -> {
                            String modelNameFromImport = importName;
                            if (importName.contains("/")) {
                                modelNameFromImport = importName.substring(importName.lastIndexOf('/') + 1);
                            }
                            return parentClassname.equalsIgnoreCase(modelNameFromImport);
                        });
                        
                        // Also remove from ModelsMap
                        ModelsMap modelsMap = objs.get(model.getClassname());
                        if (modelsMap != null && modelsMap.getImports() != null) {
                            modelsMap.getImports().removeIf(importDef -> {
                                String modelNameFromImport = importDef.get("import");
                                if (modelNameFromImport != null && modelNameFromImport.contains("/")) {
                                    modelNameFromImport = modelNameFromImport.substring(modelNameFromImport.lastIndexOf('/') + 1);
                                }
                                return parentClassname.equalsIgnoreCase(modelNameFromImport);
                            });
                        }
                        
                        parent = parent.getParentModel();
                    }
                }
            }
        }
        
        // Phase 4: Remove self-imports from ALL models
        // A model should never import itself (circular reference)
        for (CodegenModel model : allModels.values()) {
            final String modelClassname = model.getClassname();
            
            // Remove self-imports from model.imports
            model.getImports().removeIf(importName -> {
                String modelNameFromImport = importName;
                if (importName.contains("/")) {
                    modelNameFromImport = importName.substring(importName.lastIndexOf('/') + 1);
                }
                // Normalize for comparison (convert snake_case to PascalCase)
                String normalizedImportName = org.openapitools.codegen.utils.StringUtils.camelize(modelNameFromImport);
                return modelClassname.equalsIgnoreCase(normalizedImportName);
            });
            
            // Also remove from ModelsMap
            ModelsMap modelsMap = objs.get(model.getClassname());
            if (modelsMap != null && modelsMap.getImports() != null) {
                modelsMap.getImports().removeIf(importDef -> {
                    String modelNameFromImport = importDef.get("import");
                    if (modelNameFromImport != null && modelNameFromImport.contains("/")) {
                        modelNameFromImport = modelNameFromImport.substring(modelNameFromImport.lastIndexOf('/') + 1);
                    }
                    // Normalize for comparison (convert snake_case to PascalCase)
                    String normalizedImportName = org.openapitools.codegen.utils.StringUtils.camelize(modelNameFromImport);
                    return modelClassname.equalsIgnoreCase(normalizedImportName);
                });
            }
        }
    }
    
    /**
     * Checks if a model is a discriminator parent (has a discriminator with mapped models).
     * 
     * @param model the model to check
     * @return true if the model has a discriminator with mapped models
     */
    private boolean isDiscriminatorParent(CodegenModel model) {
        return model != null
                && model.discriminator != null 
                && model.discriminator.getMappedModels() != null 
                && !model.discriminator.getMappedModels().isEmpty();
    }
    
    /**
     * Normalizes a model name for case-insensitive comparison.
     * 
     * In protobuf generation, import paths often use snake_case format
     *  while classnames use PascalCase. This method
     * normalizes both formats to PascalCase for accurate comparison
     * 
     * @param modelName the model name or import path to normalize
     * @return normalized PascalCase model name, or empty string if input is null/empty
     */
    private String normalizeModelNameForComparison(String modelName) {
        if (modelName == null || modelName.isEmpty()) {
            return "";
        }
        
        // Extract model name from path if present
        String extractedName = modelName;
        if (modelName.contains("/")) {
            extractedName = modelName.substring(modelName.lastIndexOf('/') + 1);
        } else if (modelName.contains("\\")) {  // Windows path support
            extractedName = modelName.substring(modelName.lastIndexOf('\\') + 1);
        }
        
        // Convert to PascalCase (handles snake_case, kebab-case, etc.)
        return org.openapitools.codegen.utils.StringUtils.camelize(extractedName);
    }
    
    /**
     * Recursively collects properties and imports from all descendants of a discriminator parent.
     * This ensures that grandchildren, great-grandchildren, etc. all have their properties
     * included in the discriminator parent.
     * 
     * @param objs the complete models map for import tracking
     * @param discriminatorParent the discriminator parent model
     * @param allModels map of all CodegenModels by name
     * @param visited set of already processed model names to prevent infinite loops
     */
    private void collectAllDescendantProperties(Map<String, ModelsMap> objs, 
                                               CodegenModel discriminatorParent,
                                               Map<String, CodegenModel> allModels,
                                               Set<String> visited) {
        if (objs == null || discriminatorParent == null || allModels == null || visited == null) {
            LOGGER.warn("Skipping descendant property collection due to null parameter");
            return;
        }
        
        // Mark this discriminator parent as visited to prevent infinite recursion
        if (!visited.add(discriminatorParent.getClassname())) {
            return; // Already processed
        }
        
        // Get all direct children from discriminator mappings
        if (discriminatorParent.discriminator == null || 
            discriminatorParent.discriminator.getMappedModels() == null) {
            return;
        }
        
        for (MappedModel mappedModel : discriminatorParent.discriminator.getMappedModels()) {
            String childName = mappedModel.getModelName();
            CodegenModel childModel = allModels.get(childName);
            
            if (childModel == null) {
                LOGGER.warn("Discriminator references model '{}' which was not found in allModels", childName);
                continue;
            }
            
            // Recursively collect from this child and all its descendants
            collectDescendantPropertiesRecursive(objs, discriminatorParent, childModel, allModels, visited);
        }
    }
    
    /**
     * Recursively collects properties from a descendant and all its children.
     * 
     * @param objs the complete models map for import tracking
     * @param discriminatorParent the root discriminator parent receiving all properties
     * @param descendant the current descendant being processed
     * @param allModels map of all CodegenModels by name
     * @param visited set of already processed model names
     */
    private void collectDescendantPropertiesRecursive(Map<String, ModelsMap> objs,
                                                     CodegenModel discriminatorParent,
                                                     CodegenModel descendant,
                                                     Map<String, CodegenModel> allModels,
                                                     Set<String> visited) {
        // Prevent infinite loops
        if (visited.contains(descendant.getClassname())) {
            return;
        }
        
        visited.add(descendant.getClassname());
        
        // Copy this descendant's properties to the discriminator parent
        copyPropertiesToParent(objs, descendant, discriminatorParent);
        
        // Copy imports from descendant to discriminator parent
        // Filter out models that are in the discriminator mapping (they're siblings, not dependencies)
        copyImportsToDiscriminatorParent(objs, descendant, discriminatorParent);
        
        // Recursively process this descendant's children
        if (descendant.getChildren() != null) {
            for (CodegenModel grandchild : descendant.getChildren()) {
                collectDescendantPropertiesRecursive(objs, discriminatorParent, grandchild, allModels, visited);
            }
        }
    }
    
    /**
     * Copies imports from a descendant to a discriminator parent.
     * Filters out self-references and models that are in the discriminator's mapping
     * (those are siblings in the polymorphic hierarchy, not dependencies).
     * Also filters out the discriminator parent itself and any intermediate parents.
     * 
     * @param objs the complete models map for import tracking
     * @param descendant the descendant model whose imports are being copied
     * @param discriminatorParent the discriminator parent receiving the imports
     */
    private void copyImportsToDiscriminatorParent(Map<String, ModelsMap> objs,
                                                  CodegenModel descendant,
                                                  CodegenModel discriminatorParent) {
        if (objs == null || descendant == null || discriminatorParent == null) {
            LOGGER.warn("Skipping import copy due to null parameter");
            return;
        }
        
        // Additional safety: check if descendant has imports
        if (descendant.getImports() == null || descendant.getImports().isEmpty()) {
            LOGGER.debug("Descendant {} has no imports to copy", descendant.getClassname());
            return;
        }
        
        // Build set of model names that are in the discriminator mapping
        Set<String> modelsInDiscriminatorMapping = new HashSet<>();
        if (discriminatorParent.discriminator != null && 
            discriminatorParent.discriminator.getMappedModels() != null) {
            for (MappedModel mappedModel : discriminatorParent.discriminator.getMappedModels()) {
                modelsInDiscriminatorMapping.add(mappedModel.getModelName());
            }
        }
        
        // Build set of all parents in the chain from descendant to discriminator parent
        Set<String> parentsInChain = new HashSet<>();
        parentsInChain.add(discriminatorParent.getClassname());
        CodegenModel currentParent = descendant.getParentModel();
        while (currentParent != null) {
            parentsInChain.add(currentParent.getClassname());
            if (currentParent == discriminatorParent) {
                break;
            }
            currentParent = currentParent.getParentModel();
        }
        
        // Copy imports, filtering out:
        // - self-references (descendant's own name)
        // - discriminator siblings (other models in the discriminator mapping)
        // - any parent in the inheritance chain (to avoid circular references)
        descendant.getImports().stream()
                .filter(importName -> {
                    // Normalize the import name for comparison using the helper method
                    String normalizedImportName = normalizeModelNameForComparison(importName);
                    
                    // Filter out self-references
                    if (descendant.getClassname().equalsIgnoreCase(normalizedImportName)) {
                        return false;
                    }
                    // Filter out discriminator siblings
                    for (String siblingName : modelsInDiscriminatorMapping) {
                        if (siblingName.equalsIgnoreCase(normalizedImportName)) {
                            return false;
                        }
                    }
                    // Filter out any parent in the chain
                    for (String parentName : parentsInChain) {
                        if (parentName.equalsIgnoreCase(normalizedImportName)) {
                            return false;
                        }
                    }
                    return true;
                })
                .forEach(importName -> this.addImport(objs, discriminatorParent, importName));
    }

    /**
     * Copies properties from a child model to a parent model if they don't already exist.
     * When a property is an extracted enum (identified by the x-protobuf-enum-extracted-to-file vendor extension),
     * also adds the corresponding enum import to the parent model.
     * 
     * @param objs the complete models map for import tracking
     * @param child the child model whose properties are being copied
     * @param parent the parent model receiving the properties
     */
    private void copyPropertiesToParent(Map<String, ModelsMap> objs, CodegenModel child, CodegenModel parent) {
        if (child == null || parent == null) {
            LOGGER.warn("Skipping property copy due to null parameter");
            return;
        }
        
        // Add null safety for collections
        if (child.getVars() == null || parent.vars == null) {
            LOGGER.warn("Skipping property copy due to null vars: child.vars={}, parent.vars={}", 
                child.getVars(), parent.vars);
            return;
        }
        
        for (CodegenProperty property : child.getVars()) {
            if (property == null) {
                LOGGER.warn("Skipping null property in child model {}", child.getClassname());
                continue;
            }
            
            if (!parentVarsContainsVar(parent.vars, property)) {
                parent.vars.add(property);
                
                // Guard against null vendorExtensions
                if (this.extractEnumsToSeparateFiles && 
                    property.vendorExtensions != null &&
                    property.vendorExtensions.containsKey(VENDOR_EXT_ENUM_EXTRACTED) &&
                    Boolean.TRUE.equals(property.vendorExtensions.get(VENDOR_EXT_ENUM_EXTRACTED))) {
                    
                    // Get the enum wrapper message name from vendor extensions
                    String enumWrapperMessage = (String) property.vendorExtensions.get(VENDOR_EXT_ENUM_WRAPPER_MESSAGE);
                    if (StringUtils.isNotBlank(enumWrapperMessage)) {
                        // Add import for the extracted enum file
                        addImport(objs, parent, enumWrapperMessage);
                    } else {
                        LOGGER.warn("Property {} in model {} is marked as extracted enum but has no wrapper message name", 
                            property.name, child.getClassname());
                    }
                }
            }
        }
    }

    /**
     * Copies imports from a child model to a parent model.
     * Filters out self-references, circular imports, and intermediate parents in the inheritance chain.
     * 
     * @param objs the complete models map for import tracking
     * @param child the child model whose imports are being copied
     * @param parent the parent model receiving the imports
     */
    private void copyImportsToParent(Map<String, ModelsMap> objs, CodegenModel child, CodegenModel parent) {
        if (objs == null || child == null || parent == null) {
            LOGGER.warn("Skipping import copy due to null parameter");
            return;
        }
        
        // Build set of model names to filter out:
        // 1. Child's own name
        // 2. Parent's name  
        // 3. All models in the inheritance chain from child to parent (intermediate parents)
        // 4. All ancestors of parent (grandparents, great-grandparents, etc.)
        Set<String> modelsToFilter = new HashSet<>();
        modelsToFilter.add(child.getClassname());
        modelsToFilter.add(parent.getClassname());
        
        // Add all models in the chain from child up to (but not including) parent
        CodegenModel current = child.getParentModel();
        while (current != null && current != parent) {
            modelsToFilter.add(current.getClassname());
            current = current.getParentModel();
        }
        
        // Add all ancestors of the parent to the filter set
        CodegenModel currentAncestor = parent.getParentModel();
        while (currentAncestor != null) {
            modelsToFilter.add(currentAncestor.getClassname());
            currentAncestor = currentAncestor.getParentModel();
        }
        
        // Copy imports, filtering out the models in our filter set
        child.getImports().stream()
                .filter(importName -> {
                    // Normalize the import name for comparison using the helper method
                    String normalizedImportName = normalizeModelNameForComparison(importName);
                    
                    // Filter out any model in our filter set
                    for (String filterName : modelsToFilter) {
                        if (filterName.equalsIgnoreCase(normalizedImportName)) {
                            return false;
                        }
                    }
                    return true;
                })
                .forEach(importName -> this.addImport(objs, parent, importName));
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

    /**
     * Adds an import to a CodegenModel, preventing duplicate imports.
     * 
     * @param objs the complete models map for import tracking
     * @param cm the CodegenModel to add the import to
     * @param importValue the import path (e.g., "models/pet" or "Pet")
     * 
     * @implNote This method uses toModelImport() to normalize the import path,
     *           preventing duplicate model package prefixes that can occur when
     *           imports are propagated through discriminator inheritance chains.
     */
    public void addImport(Map<String, ModelsMap> objs, CodegenModel cm, String importValue) {
        // Use toModelImport to get the correct import path with model package prefix
        if (importValue == null || importValue.trim().isEmpty()) {
          LOGGER.warn("Attempted to add null or empty import to model: {}", cm.getName());
          return;
        }

        String processedImport = this.toModelImport(importValue);
        boolean skipImport = isImportAlreadyPresentInModel(objs, cm, processedImport);
        if (!skipImport) {
            this.addImport(cm, processedImport);
            Map<String, String> importItem = new HashMap<>();
            importItem.put(IMPORT, processedImport);
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
            // Check if the name already starts with the model package path to avoid duplication
            String modelPath = modelPackage() + "/";
            if (name.startsWith(modelPath)) {
                return name;
            }
            // Also check if it's already a full path (contains "/")
            if (name.contains("/")) {
                // Extract just the model name from the path
                String[] parts = name.split("/");
                String modelName = parts[parts.length - 1];
                return modelPackage() + "/" + underscore(modelName);
            }
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
     * Extracts enum properties from models and creates separate enum model files.
     * Also adds imports to the parent models for the extracted enums.
     *
     * @param objs the models map containing all models
     * @return the modified models map with extracted enum models added
     */
    private ModelsMap extractEnumsToSeparateFiles(ModelsMap objs) {
        List<Map<String, String>> enumImports = new ArrayList<>();

        Map<String, CodegenModel> extractedEnums = this.extractEnums(objs);
        for (String enumName : extractedEnums.keySet()) {
          // Add an import for this enum to the parent model
          String enumImportPath = toModelImport(toModelName(enumName));
          Map<String, String> importItem = new HashMap<>();
          importItem.put(IMPORT, enumImportPath);
          
          // Add to the list if not already present
          boolean alreadyImported = enumImports.stream()
                  .anyMatch(imp -> imp.get(IMPORT).equals(enumImportPath));
          
          if (!alreadyImported) {
              enumImports.add(importItem);
          }
        }
          
        // Add all the enum imports to the model's imports
        if (!enumImports.isEmpty()) {
            List<Map<String, String>> existingImports = objs.getImports();
            if (existingImports == null) {
                existingImports = new ArrayList<>();
                objs.setImports(existingImports);
            }
            
            for (Map<String, String> enumImport : enumImports) {
                boolean alreadyExists = existingImports.stream()
                        .anyMatch(imp -> imp.get(IMPORT).equals(enumImport.get(IMPORT)));
                if (!alreadyExists) {
                    existingImports.add(enumImport);
                }
            }
        }
        
        return objs;
    }

    /**
     * Extracts enum properties from models to be used in other process.
     * For inline enums, uses the naming scheme ParentModelName_FieldName to avoid collisions.
     *
     * @param objs the models map containing all models
     * @return the models map with extracted enum models
     */
    private Map<String, CodegenModel> extractEnums(ModelsMap objs) {
        Map<String, CodegenModel> extractedEnums = new HashMap<>();

        for (ModelMap mo : objs.getModels()) {
          CodegenModel cm = mo.getModel();

          // Skip if this model itself is an enum (standalone enums are already separate files)
          if (cm.isEnum) {
              continue; 
          }

          // Find all enum properties in this model
          for (CodegenProperty property : cm.vars) {
            if (property.isEnum && property.vendorExtensions.containsKey("x-protobuf-enum-extracted-to-file")) {
              // Create a new CodegenModel for the extracted enum
              CodegenModel enumModel = new CodegenModel();
              // Use ParentModelName_FieldName for inline enums to avoid collisions
              String enumKey = (String) property.vendorExtensions.get("x-protobuf-enum-wrapper-message");
              if (enumKey == null || enumKey.isEmpty()) {
                LOGGER.warn("Enum property {} has no wrapper message name, skipping extraction", property.getName());
                continue;
              }
              
              if (property.allowableValues == null || property.allowableValues.isEmpty()) {
                LOGGER.warn("Enum {} has no allowable values, skipping extraction", enumKey);
                continue;
              }

              if (!extractedEnums.containsKey(enumKey)) {
                enumModel.setName(enumKey);
                enumModel.setClassname(enumKey);
                enumModel.setIsEnum(true);
                enumModel.setAllowableValues(property.allowableValues);
                // Set the base data type for the enum (string, int32, etc.)
                enumModel.setDataType(property.baseType != null ? property.baseType : property.dataType);
                
                extractedEnums.put(enumKey, enumModel);
              }
            }
          }
        }
        return extractedEnums;
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
                if(additionalProperties == null) {
                   return;
                } else if (additionalProperties.getTitle() != null) {
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
