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

package org.openapitools.codegen.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.util.ClasspathHelper;
import io.swagger.v3.parser.ObjectMapperFactory;
import io.swagger.v3.parser.util.RemoteUrl;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.IJsonSchemaValidationProperties;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.commons.io.FileUtils;

import java.math.BigDecimal;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class ModelUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(ModelUtils.class);

    private static final String URI_FORMAT = "uri";

    private static final String generateAliasAsModelKey = "generateAliasAsModel";

    // A vendor extension to track the value of the 'swagger' field in a 2.0 doc, if applicable.
    private static final String openapiDocVersion = "x-original-swagger-version";

    // A vendor extension to track the value of the 'disallowAdditionalPropertiesIfNotPresent' CLI
    private static final String disallowAdditionalPropertiesIfNotPresent = "x-disallow-additional-properties-if-not-present";

    private static ObjectMapper JSON_MAPPER, YAML_MAPPER;

    static {
        JSON_MAPPER = ObjectMapperFactory.createJson();
        YAML_MAPPER = ObjectMapperFactory.createYaml();
    }
    
    public static void setDisallowAdditionalPropertiesIfNotPresent(boolean value) {
        GlobalSettings.setProperty(disallowAdditionalPropertiesIfNotPresent, Boolean.toString(value));
    }

    public static boolean isDisallowAdditionalPropertiesIfNotPresent() {
        return Boolean.parseBoolean(GlobalSettings.getProperty(disallowAdditionalPropertiesIfNotPresent, "true"));
    }

    public static void setGenerateAliasAsModel(boolean value) {
        GlobalSettings.setProperty(generateAliasAsModelKey, Boolean.toString(value));
    }

    public static boolean isGenerateAliasAsModel() {
        return Boolean.parseBoolean(GlobalSettings.getProperty(generateAliasAsModelKey, "false"));
    }

    /**
     * Searches for the model by name in the map of models and returns it
     *
     * @param name   Name of the model
     * @param models Map of models
     * @return model
     */
    public static CodegenModel getModelByName(final String name, final Map<String, Object> models) {
        final Object data = models.get(name);
        if (data instanceof Map) {
            final Map<?, ?> dataMap = (Map<?, ?>) data;
            final Object dataModels = dataMap.get("models");
            if (dataModels instanceof List) {
                final List<?> dataModelsList = (List<?>) dataModels;
                for (final Object entry : dataModelsList) {
                    if (entry instanceof Map) {
                        final Map<?, ?> entryMap = (Map<?, ?>) entry;
                        final Object model = entryMap.get("model");
                        if (model instanceof CodegenModel) {
                            return (CodegenModel) model;
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Return the list of all schemas in the 'components/schemas' section used in the openAPI specification
     *
     * @param openAPI specification
     * @return schemas a list of used schemas
     */
    public static List<String> getAllUsedSchemas(OpenAPI openAPI) {
        Map<String, List<String>> childrenMap = getChildrenMap(openAPI);
        List<String> allUsedSchemas = new ArrayList<String>();
        visitOpenAPI(openAPI, (s, t) -> {
            if (s.get$ref() != null) {
                String ref = getSimpleRef(s.get$ref());
                if (!allUsedSchemas.contains(ref)) {
                    allUsedSchemas.add(ref);
                }
                if (childrenMap.containsKey(ref)) {
                    for (String child : childrenMap.get(ref)) {
                        if (!allUsedSchemas.contains(child)) {
                            allUsedSchemas.add(child);
                        }
                    }
                }
            }
        });
        return allUsedSchemas;
    }

    /**
     * Return the list of unused schemas in the 'components/schemas' section of an openAPI specification
     *
     * @param openAPI specification
     * @return schemas a list of unused schemas
     */
    public static List<String> getUnusedSchemas(OpenAPI openAPI) {
        final Map<String, List<String>> childrenMap;
        Map<String, List<String>> tmpChildrenMap;
        try {
            tmpChildrenMap = getChildrenMap(openAPI);
        } catch (NullPointerException npe) {
            // in rare cases, such as a spec document with only one top-level oneOf schema and multiple referenced schemas,
            // the stream used in getChildrenMap will raise an NPE. Rather than modify getChildrenMap which is used by getAllUsedSchemas,
            // we'll catch here as a workaround for this edge case.
            tmpChildrenMap = new HashMap<>();
        }

        childrenMap = tmpChildrenMap;
        List<String> unusedSchemas = new ArrayList<String>();

        Map<String, Schema> schemas = getSchemas(openAPI);
        unusedSchemas.addAll(schemas.keySet());

        visitOpenAPI(openAPI, (s, t) -> {
            if (s.get$ref() != null) {
                String ref = getSimpleRef(s.get$ref());
                unusedSchemas.remove(ref);
                if (childrenMap.containsKey(ref)) {
                    unusedSchemas.removeAll(childrenMap.get(ref));
                }
            }
        });
        return unusedSchemas;
    }

    /**
     * Return the list of schemas in the 'components/schemas' used only in a 'application/x-www-form-urlencoded' or 'multipart/form-data' mime time
     *
     * @param openAPI specification
     * @return schemas a list of schemas
     */
    public static List<String> getSchemasUsedOnlyInFormParam(OpenAPI openAPI) {
        List<String> schemasUsedInFormParam = new ArrayList<String>();
        List<String> schemasUsedInOtherCases = new ArrayList<String>();

        visitOpenAPI(openAPI, (s, t) -> {
            if (s.get$ref() != null) {
                String ref = getSimpleRef(s.get$ref());
                if ("application/x-www-form-urlencoded".equalsIgnoreCase(t) ||
                        "multipart/form-data".equalsIgnoreCase(t)) {
                    schemasUsedInFormParam.add(ref);
                } else {
                    schemasUsedInOtherCases.add(ref);
                }
            }
        });
        return schemasUsedInFormParam.stream().filter(n -> !schemasUsedInOtherCases.contains(n)).collect(Collectors.toList());
    }

    /**
     * Private method used by several methods ({@link #getAllUsedSchemas(OpenAPI)},
     * {@link #getUnusedSchemas(OpenAPI)},
     * {@link #getSchemasUsedOnlyInFormParam(OpenAPI)}, ...) to traverse all paths of an
     * OpenAPI instance and call the visitor functional interface when a schema is found.
     *
     * @param openAPI specification
     * @param visitor functional interface (can be defined as a lambda) called each time a schema is found.
     */
    private static void visitOpenAPI(OpenAPI openAPI, OpenAPISchemaVisitor visitor) {
        Map<String, PathItem> paths = openAPI.getPaths();
        List<String> visitedSchemas = new ArrayList<>();

        if (paths != null) {
            for (PathItem path : paths.values()) {
                visitPathItem(path, openAPI, visitor, visitedSchemas);
            }
        }
    }

    private static void visitPathItem(PathItem pathItem, OpenAPI openAPI, OpenAPISchemaVisitor visitor, List<String> visitedSchemas) {
        List<Operation> allOperations = pathItem.readOperations();
        if (allOperations != null) {
            for (Operation operation : allOperations) {
                //Params:
                visitParameters(openAPI, operation.getParameters(), visitor, visitedSchemas);

                //RequestBody:
                RequestBody requestBody = getReferencedRequestBody(openAPI, operation.getRequestBody());
                if (requestBody != null) {
                    visitContent(openAPI, requestBody.getContent(), visitor, visitedSchemas);
                }

                //Responses:
                if (operation.getResponses() != null) {
                    for (ApiResponse r : operation.getResponses().values()) {
                        ApiResponse apiResponse = getReferencedApiResponse(openAPI, r);
                        if (apiResponse != null) {
                            visitContent(openAPI, apiResponse.getContent(), visitor, visitedSchemas);
                            if (apiResponse.getHeaders() != null) {
                                for (Entry<String, Header> e : apiResponse.getHeaders().entrySet()) {
                                    Header header = getReferencedHeader(openAPI, e.getValue());
                                    if (header.getSchema() != null) {
                                        visitSchema(openAPI, header.getSchema(), e.getKey(), visitedSchemas, visitor);
                                    }
                                    visitContent(openAPI, header.getContent(), visitor, visitedSchemas);
                                }
                            }
                        }
                    }
                }

                //Callbacks:
                if (operation.getCallbacks() != null) {
                    for (Callback c : operation.getCallbacks().values()) {
                        Callback callback = getReferencedCallback(openAPI, c);
                        if (callback != null) {
                            for (PathItem p : callback.values()) {
                                visitPathItem(p, openAPI, visitor, visitedSchemas);
                            }
                        }
                    }
                }
            }
        }
        //Params:
        visitParameters(openAPI, pathItem.getParameters(), visitor, visitedSchemas);
    }

    private static void visitParameters(OpenAPI openAPI, List<Parameter> parameters, OpenAPISchemaVisitor visitor,
                                        List<String> visitedSchemas) {
        if (parameters != null) {
            for (Parameter p : parameters) {
                Parameter parameter = getReferencedParameter(openAPI, p);
                if (parameter != null) {
                    if (parameter.getSchema() != null) {
                        visitSchema(openAPI, parameter.getSchema(), null, visitedSchemas, visitor);
                    }
                    visitContent(openAPI, parameter.getContent(), visitor, visitedSchemas);
                } else {
                    once(LOGGER).warn("Unreferenced parameter(s) found.");
                }
            }
        }
    }

    private static void visitContent(OpenAPI openAPI, Content content, OpenAPISchemaVisitor visitor, List<String> visitedSchemas) {
        if (content != null) {
            for (Entry<String, MediaType> e : content.entrySet()) {
                if (e.getValue().getSchema() != null) {
                    visitSchema(openAPI, e.getValue().getSchema(), e.getKey(), visitedSchemas, visitor);
                }
            }
        }
    }

    /**
     * Invoke the specified visitor function for every schema that matches mimeType in the OpenAPI document.
     *
     * To avoid infinite recursion, referenced schemas are visited only once. When a referenced schema is visited,
     * it is added to visitedSchemas.
     *
     * @param openAPI the OpenAPI document that contains schema objects.
     * @param schema the root schema object to be visited.
     * @param mimeType the mime type. TODO: does not seem to be used in a meaningful way.
     * @param visitedSchemas the list of referenced schemas that have been visited.
     * @param visitor the visitor function which is invoked for every visited schema.
     */
    private static void visitSchema(OpenAPI openAPI, Schema schema, String mimeType, List<String> visitedSchemas, OpenAPISchemaVisitor visitor) {
        visitor.visit(schema, mimeType);
        if (schema.get$ref() != null) {
            String ref = getSimpleRef(schema.get$ref());
            if (!visitedSchemas.contains(ref)) {
                visitedSchemas.add(ref);
                Schema referencedSchema = getSchemas(openAPI).get(ref);
                if (referencedSchema != null) {
                    visitSchema(openAPI, referencedSchema, mimeType, visitedSchemas, visitor);
                }
            }
        }
        if (schema instanceof ComposedSchema) {
            List<Schema> oneOf = ((ComposedSchema) schema).getOneOf();
            if (oneOf != null) {
                for (Schema s : oneOf) {
                    visitSchema(openAPI, s, mimeType, visitedSchemas, visitor);
                }
            }
            List<Schema> allOf = ((ComposedSchema) schema).getAllOf();
            if (allOf != null) {
                for (Schema s : allOf) {
                    visitSchema(openAPI, s, mimeType, visitedSchemas, visitor);
                }
            }
            List<Schema> anyOf = ((ComposedSchema) schema).getAnyOf();
            if (anyOf != null) {
                for (Schema s : anyOf) {
                    visitSchema(openAPI, s, mimeType, visitedSchemas, visitor);
                }
            }
        } else if (schema instanceof ArraySchema) {
            Schema itemsSchema = ((ArraySchema) schema).getItems();
            if (itemsSchema != null) {
                visitSchema(openAPI, itemsSchema, mimeType, visitedSchemas, visitor);
            }
        } else if (isMapSchema(schema)) {
            Object additionalProperties = schema.getAdditionalProperties();
            if (additionalProperties instanceof Schema) {
                visitSchema(openAPI, (Schema) additionalProperties, mimeType, visitedSchemas, visitor);
            }
        }
        if (schema.getNot() != null) {
            visitSchema(openAPI, schema.getNot(), mimeType, visitedSchemas, visitor);
        }
        Map<String, Schema> properties = schema.getProperties();
        if (properties != null) {
            for (Schema property : properties.values()) {
                visitSchema(openAPI, property, mimeType, visitedSchemas, visitor);
            }
        }
    }

    @FunctionalInterface
    private static interface OpenAPISchemaVisitor {

        public void visit(Schema schema, String mimeType);
    }

    public static String getSimpleRef(String ref) {
        if (ref.startsWith("#/components/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        } else if (ref.startsWith("#/definitions/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        } else {
            once(LOGGER).warn("Failed to get the schema name: {}", ref);
            //throw new RuntimeException("Failed to get the schema: " + ref);
            return null;

        }

        return ref;
    }

    /**
     * Return true if the specified schema is an object with a fixed number of properties.
     *
     * A ObjectSchema differs from an MapSchema in the following way:
     * - An ObjectSchema is not extensible, i.e. it has a fixed number of properties.
     * - A MapSchema is an object that can be extended with an arbitrary set of properties.
     *   The payload may include dynamic properties.
     *
     * For example, an OpenAPI schema is considered an ObjectSchema in the following scenarios:
     *
     *   type: object
     *   additionalProperties: false
     *   properties:
     *     name:
     *       type: string
     *     address:
     *       type: string
     *
     * @param schema the OAS schema
     * @return true if the specified schema is an Object schema.
     */
    public static boolean isObjectSchema(Schema schema) {
        if (schema instanceof ObjectSchema) {
            return true;
        }

        // must not be a map
        if (SchemaTypeUtil.OBJECT_TYPE.equals(schema.getType()) && !(schema instanceof MapSchema)) {
            return true;
        }

        // must have at least one property
        if (schema.getType() == null && schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            return true;
        }
        return false;
    }

    /**
     * Return true if the specified schema is composed, i.e. if it uses
     * 'oneOf', 'anyOf' or 'allOf'.
     *
     * @param schema the OAS schema
     * @return true if the specified schema is a Composed schema.
     */
    public static boolean isComposedSchema(Schema schema) {
        if (schema instanceof ComposedSchema) {
            return true;
        }
        return false;
    }

    /**
     * Return true if the specified 'schema' is an object that can be extended with additional properties.
     * Additional properties means a Schema should support all explicitly defined properties plus any
     * undeclared properties.
     *
     * A MapSchema differs from an ObjectSchema in the following way:
     * - An ObjectSchema is not extensible, i.e. it has a fixed number of properties.
     * - A MapSchema is an object that can be extended with an arbitrary set of properties.
     *   The payload may include dynamic properties.
     *
     * Note that isMapSchema returns true for a composed schema (allOf, anyOf, oneOf) that also defines
     * additionalproperties.
     *
     * For example, an OpenAPI schema is considered a MapSchema in the following scenarios:
     *
     *   type: object
     *   additionalProperties: true
     *
     *   type: object
     *   additionalProperties:
     *     type: object
     *     properties:
     *       code:
     *         type: integer
     *
     *   allOf:
     *     - $ref: '#/components/schemas/Class1'
     *     - $ref: '#/components/schemas/Class2'
     *   additionalProperties: true
     *
     * @param schema the OAS schema
     * @return true if the specified schema is a Map schema.
     */
    public static boolean isMapSchema(Schema schema) {
        if (schema instanceof MapSchema) {
            return true;
        }

        if (schema == null) {
            return false;
        }

        if (schema.getAdditionalProperties() instanceof Schema) {
            return true;
        }

        if (schema.getAdditionalProperties() instanceof Boolean && (Boolean) schema.getAdditionalProperties()) {
            return true;
        }

        return false;
    }

    /**
     * Return true if the specified schema is an array of items.
     *
     * @param schema the OAS schema
     * @return true if the specified schema is an Array schema.
     */
    public static boolean isArraySchema(Schema schema) {
        return (schema instanceof ArraySchema);
    }

    public static boolean isSet(Schema schema) {
        return ModelUtils.isArraySchema(schema) && Boolean.TRUE.equals(schema.getUniqueItems());
    }

    public static boolean isStringSchema(Schema schema) {
        if (schema instanceof StringSchema || SchemaTypeUtil.STRING_TYPE.equals(schema.getType())) {
            return true;
        }
        return false;
    }

    public static boolean isIntegerSchema(Schema schema) {
        if (schema instanceof IntegerSchema) {
            return true;
        }
        if (SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType())) {
            return true;
        }
        return false;
    }

    public static boolean isShortSchema(Schema schema) {
        if (SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) // type: integer
                && SchemaTypeUtil.INTEGER32_FORMAT.equals(schema.getFormat())) { // format: short (int32)
            return true;
        }
        return false;
    }

    public static boolean isLongSchema(Schema schema) {
        if (SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) // type: integer
                && SchemaTypeUtil.INTEGER64_FORMAT.equals(schema.getFormat())) { // format: long (int64)
            return true;
        }
        return false;
    }

    public static boolean isBooleanSchema(Schema schema) {
        if (schema instanceof BooleanSchema) {
            return true;
        }
        if (SchemaTypeUtil.BOOLEAN_TYPE.equals(schema.getType())) {
            return true;
        }
        return false;
    }

    public static boolean isNumberSchema(Schema schema) {
        if (schema instanceof NumberSchema) {
            return true;
        }
        if (SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType())) {
            return true;
        }
        return false;
    }

    public static boolean isFloatSchema(Schema schema) {
        if (SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType())
                && SchemaTypeUtil.FLOAT_FORMAT.equals(schema.getFormat())) { // format: float
            return true;
        }
        return false;
    }

    public static boolean isDoubleSchema(Schema schema) {
        if (SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType())
                && SchemaTypeUtil.DOUBLE_FORMAT.equals(schema.getFormat())) { // format: double
            return true;
        }
        return false;
    }

    public static boolean isDateSchema(Schema schema) {
        if (schema instanceof DateSchema) {
            return true;
        }

        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.DATE_FORMAT.equals(schema.getFormat())) { // format: date
            return true;
        }
        return false;
    }

    public static boolean isDateTimeSchema(Schema schema) {
        if (schema instanceof DateTimeSchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.DATE_TIME_FORMAT.equals(schema.getFormat())) { // format: date-time
            return true;
        }
        return false;
    }

    public static boolean isPasswordSchema(Schema schema) {
        if (schema instanceof PasswordSchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.PASSWORD_FORMAT.equals(schema.getFormat())) { // double
            return true;
        }
        return false;
    }

    public static boolean isByteArraySchema(Schema schema) {
        if (schema instanceof ByteArraySchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.BYTE_FORMAT.equals(schema.getFormat())) { // format: byte
            return true;
        }
        return false;
    }

    public static boolean isBinarySchema(Schema schema) {
        if (schema instanceof BinarySchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.BINARY_FORMAT.equals(schema.getFormat())) { // format: binary
            return true;
        }
        return false;
    }

    public static boolean isFileSchema(Schema schema) {
        if (schema instanceof FileSchema) {
            return true;
        }
        // file type in oas2 mapped to binary in oas3
        return isBinarySchema(schema);
    }

    public static boolean isUUIDSchema(Schema schema) {
        if (schema instanceof UUIDSchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.UUID_FORMAT.equals(schema.getFormat())) { // format: uuid
            return true;
        }
        return false;
    }

    public static boolean isURISchema(Schema schema) {
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && URI_FORMAT.equals(schema.getFormat())) { // format: uri
            return true;
        }
        return false;
    }

    public static boolean isEmailSchema(Schema schema) {
        if (schema instanceof EmailSchema) {
            return true;
        }
        if (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && SchemaTypeUtil.EMAIL_FORMAT.equals(schema.getFormat())) { // format: email
            return true;
        }
        return false;
    }

    /**
     * Check to see if the schema is a model with at least one property.
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a model with at least one properties
     */
    public static boolean isModel(Schema schema) {
        if (schema == null) {
            // TODO: Is this message necessary? A null schema is not a model, so the result is correct.
            once(LOGGER).error("Schema cannot be null in isModel check");
            return false;
        }

        // has at least one property
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            return true;
        }

        // composed schema is a model
        return schema instanceof ComposedSchema;
    }

    /**
     * Check to see if the schema is a free form object.
     *
     * A free form object is an object (i.e. 'type: object' in a OAS document) that:
     * 1) Does not define properties, and
     * 2) Is not a composed schema (no anyOf, oneOf, allOf), and
     * 3) additionalproperties is not defined, or additionalproperties: true, or additionalproperties: {}.
     *
     * Examples:
     *
     * components:
     *   schemas:
     *     arbitraryObject:
     *       type: object
     *       description: This is a free-form object.
     *         The value must be a map of strings to values. The value cannot be 'null'.
     *         It cannot be array, string, integer, number.
     *     arbitraryNullableObject:
     *       type: object
     *       description: This is a free-form object.
     *         The value must be a map of strings to values. The value can be 'null',
     *         It cannot be array, string, integer, number.
     *       nullable: true
     *     arbitraryTypeValue:
     *       description: This is NOT a free-form object.
     *         The value can be any type except the 'null' value.
     *
     * @param openAPI the object that encapsulates the OAS document.
     * @param schema potentially containing a '$ref'
     * @return true if it's a free-form object
     */
    public static boolean isFreeFormObject(OpenAPI openAPI, Schema schema) {
        if (schema == null) {
            // TODO: Is this message necessary? A null schema is not a free-form object, so the result is correct.
            once(LOGGER).error("Schema cannot be null in isFreeFormObject check");
            return false;
        }

        // not free-form if allOf, anyOf, oneOf is not empty
        if (schema instanceof ComposedSchema) {
            ComposedSchema cs = (ComposedSchema) schema;
            List<Schema> interfaces = ModelUtils.getInterfaces(cs);
            if (interfaces != null && !interfaces.isEmpty()) {
                return false;
            }
        }

        // has at least one property
        if ("object".equals(schema.getType())) {
            // no properties
            if ((schema.getProperties() == null || schema.getProperties().isEmpty())) {
                Schema addlProps = getAdditionalProperties(openAPI, schema);
                // additionalProperties not defined
                if (addlProps == null) {
                    return true;
                } else {
                    if (addlProps instanceof ObjectSchema) {
                        ObjectSchema objSchema = (ObjectSchema) addlProps;
                        // additionalProperties defined as {}
                        if (objSchema.getProperties() == null || objSchema.getProperties().isEmpty()) {
                            return true;
                        }
                    } else if (addlProps instanceof Schema) {
                        // additionalProperties defined as {}
                        if (addlProps.getType() == null && (addlProps.getProperties() == null || addlProps.getProperties().isEmpty())) {
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

    /**
     * If a Schema contains a reference to another Schema with '$ref', returns the referenced Schema if it is found or the actual Schema in the other cases.
     *
     * @param openAPI specification being checked
     * @param schema  potentially containing a '$ref'
     * @return schema without '$ref'
     */
    public static Schema getReferencedSchema(OpenAPI openAPI, Schema schema) {
        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            String name = getSimpleRef(schema.get$ref());
            Schema referencedSchema = getSchema(openAPI, name);
            if (referencedSchema != null) {
                return referencedSchema;
            }
        }
        return schema;
    }

    public static Schema getSchema(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        return getSchemas(openAPI).get(name);
    }

    /**
     * Return a Map of the schemas defined under /components/schemas in the OAS document.
     * The returned Map only includes the direct children of /components/schemas in the OAS document; the Map
     * does not include inlined schemas.
     *
     * @param openAPI the OpenAPI document.
     * @return a map of schemas in the OAS document.
     */
    public static Map<String, Schema> getSchemas(OpenAPI openAPI) {
        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getSchemas() != null) {
            return openAPI.getComponents().getSchemas();
        }
        return Collections.emptyMap();
    }

    /**
     * Return the list of all schemas in the 'components/schemas' section of an openAPI specification,
     * including inlined schemas and children of composed schemas.
     *
     * @param openAPI OpenAPI document
     * @return a list of schemas
     */
    public static List<Schema> getAllSchemas(OpenAPI openAPI) {
        List<Schema> allSchemas = new ArrayList<Schema>();
        List<String> refSchemas = new ArrayList<String>();
        getSchemas(openAPI).forEach((key, schema) -> {
            // Invoke visitSchema to recursively visit all schema objects, included inlined and composed schemas.
            // Use the OpenAPISchemaVisitor visitor function
            visitSchema(openAPI, schema, null, refSchemas, (s, mimetype) -> {
                allSchemas.add(s);
            });
        });
        return allSchemas;
    }

    /**
     * If a RequestBody contains a reference to an other RequestBody with '$ref', returns the referenced RequestBody if it is found or the actual RequestBody in the other cases.
     *
     * @param openAPI     specification being checked
     * @param requestBody potentially containing a '$ref'
     * @return requestBody without '$ref'
     */
    public static RequestBody getReferencedRequestBody(OpenAPI openAPI, RequestBody requestBody) {
        if (requestBody != null && StringUtils.isNotEmpty(requestBody.get$ref())) {
            String name = getSimpleRef(requestBody.get$ref());
            RequestBody referencedRequestBody = getRequestBody(openAPI, name);
            if (referencedRequestBody != null) {
                return referencedRequestBody;
            }
        }
        return requestBody;
    }

    public static RequestBody getRequestBody(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getRequestBodies() != null) {
            return openAPI.getComponents().getRequestBodies().get(name);
        }
        return null;
    }

    /**
     * If a ApiResponse contains a reference to an other ApiResponse with '$ref', returns the referenced ApiResponse if it is found or the actual ApiResponse in the other cases.
     *
     * @param openAPI     specification being checked
     * @param apiResponse potentially containing a '$ref'
     * @return apiResponse without '$ref'
     */
    public static ApiResponse getReferencedApiResponse(OpenAPI openAPI, ApiResponse apiResponse) {
        if (apiResponse != null && StringUtils.isNotEmpty(apiResponse.get$ref())) {
            String name = getSimpleRef(apiResponse.get$ref());
            ApiResponse referencedApiResponse = getApiResponse(openAPI, name);
            if (referencedApiResponse != null) {
                return referencedApiResponse;
            }
        }
        return apiResponse;
    }

    public static ApiResponse getApiResponse(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getResponses() != null) {
            return openAPI.getComponents().getResponses().get(name);
        }
        return null;
    }

    /**
     * If a Parameter contains a reference to an other Parameter with '$ref', returns the referenced Parameter if it is found or the actual Parameter in the other cases.
     *
     * @param openAPI   specification being checked
     * @param parameter potentially containing a '$ref'
     * @return parameter without '$ref'
     */
    public static Parameter getReferencedParameter(OpenAPI openAPI, Parameter parameter) {
        if (parameter != null && StringUtils.isNotEmpty(parameter.get$ref())) {
            String name = getSimpleRef(parameter.get$ref());
            Parameter referencedParameter = getParameter(openAPI, name);
            if (referencedParameter != null) {
                return referencedParameter;
            }
        }
        return parameter;
    }

    public static Parameter getParameter(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getParameters() != null) {
            return openAPI.getComponents().getParameters().get(name);
        }
        return null;
    }

    /**
     * If a Callback contains a reference to an other Callback with '$ref', returns the referenced Callback if it is found or the actual Callback in the other cases.
     *
     * @param openAPI  specification being checked
     * @param callback potentially containing a '$ref'
     * @return callback without '$ref'
     */
    public static Callback getReferencedCallback(OpenAPI openAPI, Callback callback) {
        if (callback != null && StringUtils.isNotEmpty(callback.get$ref())) {
            String name = getSimpleRef(callback.get$ref());
            Callback referencedCallback = getCallback(openAPI, name);
            if (referencedCallback != null) {
                return referencedCallback;
            }
        }
        return callback;
    }

    public static Callback getCallback(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getCallbacks() != null) {
            return openAPI.getComponents().getCallbacks().get(name);
        }
        return null;
    }

    /**
     * Return the first defined Schema for a RequestBody
     *
     * @param requestBody request body of the operation
     * @return firstSchema
     */
    public static Schema getSchemaFromRequestBody(RequestBody requestBody) {
        return getSchemaFromContent(requestBody.getContent());
    }

    /**
     * Return the first defined Schema for a ApiResponse
     *
     * @param response api response of the operation
     * @return firstSchema
     */
    public static Schema getSchemaFromResponse(ApiResponse response) {
        return getSchemaFromContent(response.getContent());
    }

    /**
     * Return the first Schema from a specified OAS 'content' section.
     *
     * For example, given the following OAS, this method returns the schema
     * for the 'application/json' content type because it is listed first in the OAS.
     *
     * responses:
     *   '200':
     *     content:
     *       application/json:
     *         schema:
     *           $ref: '#/components/schemas/XYZ'
     *       application/xml:
     *          ...
     *
     * @param content a 'content' section in the OAS specification.
     * @return the Schema.
     */
    private static Schema getSchemaFromContent(Content content) {
        if (content == null || content.isEmpty()) {
            return null;
        }
        Map.Entry<String, MediaType> entry = content.entrySet().iterator().next();
        if (content.size() > 1) {
            // Other content types are currently ignored by codegen. If you see this warning,
            // reorder the OAS spec to put the desired content type first.
            once(LOGGER).warn("Multiple schemas found in the OAS 'content' section, returning only the first one ({})",
                    entry.getKey());
        }
        return entry.getValue().getSchema();
    }

    /**
     * Get the actual schema from aliases. If the provided schema is not an alias, the schema itself will be returned.
     *
     * @param openAPI specification being checked
     * @param schema  schema (alias or direct reference)
     * @return actual schema
     */
    public static Schema unaliasSchema(OpenAPI openAPI,
                                       Schema schema) {
        return unaliasSchema(openAPI, schema, Collections.<String, String>emptyMap());
    }

    /**
     * Get the actual schema from aliases. If the provided schema is not an alias, the schema itself will be returned.
     *
     * @param openAPI        specification being checked
     * @param schema         schema (alias or direct reference)
     * @param importMappings mappings of external types to be omitted by unaliasing
     * @return actual schema
     */
    public static Schema unaliasSchema(OpenAPI openAPI,
                                       Schema schema,
                                       Map<String, String> importMappings) {
        Map<String, Schema> allSchemas = getSchemas(openAPI);
        if (allSchemas == null || allSchemas.isEmpty()) {
            // skip the warning as the spec can have no model defined
            //LOGGER.warn("allSchemas cannot be null/empty in unaliasSchema. Returned 'schema'");
            return schema;
        }

        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            String simpleRef = ModelUtils.getSimpleRef(schema.get$ref());
            if (importMappings.containsKey(simpleRef)) {
                LOGGER.info("Schema unaliasing of {} omitted because aliased class is to be mapped to {}", simpleRef, importMappings.get(simpleRef));
                return schema;
            }
            Schema ref = allSchemas.get(simpleRef);
            if (ref == null) {
                once(LOGGER).warn("{} is not defined", schema.get$ref());
                return schema;
            } else if (ref.getEnum() != null && !ref.getEnum().isEmpty()) {
                // top-level enum class
                return schema;
            } else if (isArraySchema(ref)) {
                if (isGenerateAliasAsModel()) {
                    return schema; // generate a model extending array
                } else {
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            importMappings);
                }
            } else if (isComposedSchema(ref)) {
                return schema;
            } else if (isMapSchema(ref)) {
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) // has at least one property
                    return schema; // treat it as model
                else {
                    if (isGenerateAliasAsModel()) {
                        return schema; // generate a model extending map
                    } else {
                        // treat it as a typical map
                        return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                                importMappings);
                    }
                }
            } else if (isObjectSchema(ref)) { // model
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) { // has at least one property
                    return schema;
                } else { // free form object (type: object)
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            importMappings);
                }
            } else {
                return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())), importMappings);
            }
        }
        return schema;
    }

    /**
     * Returns the additionalProperties Schema for the specified input schema.
     * 
     * The additionalProperties keyword is used to control the handling of additional, undeclared
     * properties, that is, properties whose names are not listed in the properties keyword.
     * The additionalProperties keyword may be either a boolean or an object.
     * If additionalProperties is a boolean and set to false, no additional properties are allowed.
     * By default when the additionalProperties keyword is not specified in the input schema,
     * any additional properties are allowed. This is equivalent to setting additionalProperties
     * to the boolean value True or setting additionalProperties: {}
     * 
     * @param openAPI the object that encapsulates the OAS document.
     * @param schema the input schema that may or may not have the additionalProperties keyword.
     * @return the Schema of the additionalProperties. The null value is returned if no additional
     *         properties are allowed.
     */
    public static Schema getAdditionalProperties(OpenAPI openAPI, Schema schema) {
        Object addProps = schema.getAdditionalProperties();
        if (addProps instanceof Schema) {
            return (Schema) addProps;
        }
        if (addProps == null) {
            // When reaching this code path, this should indicate the 'additionalProperties' keyword is
            // not present in the OAS schema. This is true for OAS 3.0 documents.
            // However, the parsing logic is broken for OAS 2.0 documents because of the
            // https://github.com/swagger-api/swagger-parser/issues/1369 issue.
            // When OAS 2.0 documents are parsed, the swagger-v2-converter ignores the 'additionalProperties'
            // keyword if the value is boolean. That means codegen is unable to determine whether
            // additional properties are allowed or not.
            //
            // The original behavior was to assume additionalProperties had been set to false.
            if (isDisallowAdditionalPropertiesIfNotPresent()) {
                // If the 'additionalProperties' keyword is not present in a OAS schema,
                // interpret as if the 'additionalProperties' keyword had been set to false.
                // This is NOT compliant with the JSON schema specification. It is the original
                // 'openapi-generator' behavior.
                return null;
            }
            /*
            // The disallowAdditionalPropertiesIfNotPresent CLI option has been set to true,
            // but for now that only works with OAS 3.0 documents.
            // The new behavior does not work with OAS 2.0 documents.
            if (extensions == null || !extensions.containsKey(EXTENSION_OPENAPI_DOC_VERSION)) {
                // Fallback to the legacy behavior.
                return null;
            }
            // Get original swagger version from OAS extension.
            // Note openAPI.getOpenapi() is always set to 3.x even when the document
            // is converted from a OAS/Swagger 2.0 document.
            // https://github.com/swagger-api/swagger-parser/pull/1374
            SemVer version = new SemVer((String)extensions.get(EXTENSION_OPENAPI_DOC_VERSION));
            if (version.major != 3) {
                return null;
            }
            */
        }
        if (addProps == null || (addProps instanceof Boolean && (Boolean) addProps)) {
            // Return ObjectSchema to specify any object (map) value is allowed.
            // Set nullable to specify the value of additional properties may be
            // the null value.
            // Free-form additionalProperties don't need to have an inner
            // additional properties, the type is already free-form.
            return new ObjectSchema().additionalProperties(Boolean.FALSE).nullable(Boolean.TRUE);
        }
        return null;
    }
    
    public static Header getReferencedHeader(OpenAPI openAPI, Header header) {
        if (header != null && StringUtils.isNotEmpty(header.get$ref())) {
            String name = getSimpleRef(header.get$ref());
            Header referencedheader = getHeader(openAPI, name);
            if (referencedheader != null) {
                return referencedheader;
            }
        }
        return header;
    }

    public static Header getHeader(OpenAPI openAPI, String name) {
        if (name == null) {
            return null;
        }

        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getHeaders() != null) {
            return openAPI.getComponents().getHeaders().get(name);
        }
        return null;
    }

    public static Map<String, List<String>> getChildrenMap(OpenAPI openAPI) {
        Map<String, Schema> allSchemas = getSchemas(openAPI);

        Map<String, List<Entry<String, Schema>>> groupedByParent = allSchemas.entrySet().stream()
                .filter(entry -> isComposedSchema(entry.getValue()))
                .filter(entry -> getParentName((ComposedSchema) entry.getValue(), allSchemas)!=null)
                .collect(Collectors.groupingBy(entry -> getParentName((ComposedSchema) entry.getValue(), allSchemas)));

        return groupedByParent.entrySet().stream()
                .collect(Collectors.toMap(entry -> entry.getKey(), entry -> entry.getValue().stream().map(e -> e.getKey()).collect(Collectors.toList())));
    }


    /**
     * Get the interfaces from the schema (composed)
     *
     * @param composed schema (alias or direct reference)
     * @return a list of schema defined in allOf, anyOf or oneOf
     */
    public static List<Schema> getInterfaces(ComposedSchema composed) {
        if (composed.getAllOf() != null && !composed.getAllOf().isEmpty()) {
            return composed.getAllOf();
        } else if (composed.getAnyOf() != null && !composed.getAnyOf().isEmpty()) {
            return composed.getAnyOf();
        } else if (composed.getOneOf() != null && !composed.getOneOf().isEmpty()) {
            return composed.getOneOf();
        } else {
            return Collections.<Schema>emptyList();
        }
    }

    /**
     * Get the parent model name from the composed schema (allOf, anyOf, oneOf).
     * It traverses the OAS model (possibly resolving $ref) to determine schemas
     * that specify a determinator.
     * If there are multiple elements in the composed schema and it is not clear
     * which one should be the parent, return null.
     *
     * For example, given the following OAS spec, the parent of 'Dog' is Animal
     * because 'Animal' specifies a discriminator.
     *
     * animal:
     *   type: object
     *   discriminator:
     *     propertyName: type
     *   properties:
     *     type: string
     *
     * dog:
     *   allOf:
     *      - $ref: '#/components/schemas/animal'
     *      - type: object
     *        properties:
     *          breed: string
     *
     * @param composedSchema schema (alias or direct reference)
     * @param allSchemas     all schemas
     * @return the name of the parent model
     */
    public static String getParentName(ComposedSchema composedSchema, Map<String, Schema> allSchemas) {
        List<Schema> interfaces = getInterfaces(composedSchema);
        int nullSchemaChildrenCount = 0;
        boolean hasAmbiguousParents = false;
        List<String> refedWithoutDiscriminator = new ArrayList<>();

        if (interfaces != null && !interfaces.isEmpty()) {
            for (Schema schema : interfaces) {
                // get the actual schema
                if (StringUtils.isNotEmpty(schema.get$ref())) {
                    String parentName = getSimpleRef(schema.get$ref());
                    Schema s = allSchemas.get(parentName);
                    if (s == null) {
                        LOGGER.error("Failed to obtain schema from {}", parentName);
                        return "UNKNOWN_PARENT_NAME";
                    } else if (hasOrInheritsDiscriminator(s, allSchemas)) {
                        // discriminator.propertyName is used
                        return parentName;
                    } else {
                        // not a parent since discriminator.propertyName is not set
                        hasAmbiguousParents = true;
                        refedWithoutDiscriminator.add(parentName);
                    }
                } else {
                    // not a ref, doing nothing, except counting the number of times the 'null' type
                    // is listed as composed element.
                    if (ModelUtils.isNullType(schema)) {
                        // If there are two interfaces, and one of them is the 'null' type,
                        // then the parent is obvious and there is no need to warn about specifying
                        // a determinator.
                        nullSchemaChildrenCount++;
                    }
                }
            }
            if (refedWithoutDiscriminator.size() == 1 && nullSchemaChildrenCount == 1) {
                // One schema is a $ref and the other is the 'null' type, so the parent is obvious.
                // In this particular case there is no need to specify a discriminator.
                hasAmbiguousParents = false;
            }
        }

        // parent name only makes sense when there is a single obvious parent
        if (refedWithoutDiscriminator.size() == 1) {
            if (hasAmbiguousParents) {
                LOGGER.warn("[deprecated] inheritance without use of 'discriminator.propertyName' is deprecated " +
                                "and will be removed in a future release. Generating model for composed schema name: {}. Title: {}",
                        composedSchema.getName(), composedSchema.getTitle());
            }
            return refedWithoutDiscriminator.get(0);
        }

        return null;
    }

    /**
     * Get the list of parent model names from the schemas (allOf, anyOf, oneOf).
     *
     * @param composedSchema   schema (alias or direct reference)
     * @param allSchemas       all schemas
     * @param includeAncestors if true, include the indirect ancestors in the return value. If false, return the direct parents.
     * @return the name of the parent model
     */
    public static List<String> getAllParentsName(ComposedSchema composedSchema, Map<String, Schema> allSchemas, boolean includeAncestors) {
        List<Schema> interfaces = getInterfaces(composedSchema);
        List<String> names = new ArrayList<String>();

        if (interfaces != null && !interfaces.isEmpty()) {
            for (Schema schema : interfaces) {
                // get the actual schema
                if (StringUtils.isNotEmpty(schema.get$ref())) {
                    String parentName = getSimpleRef(schema.get$ref());
                    Schema s = allSchemas.get(parentName);
                    if (s == null) {
                        LOGGER.error("Failed to obtain schema from {}", parentName);
                        names.add("UNKNOWN_PARENT_NAME");
                    } else if (hasOrInheritsDiscriminator(s, allSchemas)) {
                        // discriminator.propertyName is used
                        names.add(parentName);
                        if (includeAncestors && s instanceof ComposedSchema) {
                            names.addAll(getAllParentsName((ComposedSchema) s, allSchemas, true));
                        }
                    } else {
                        // not a parent since discriminator.propertyName is not set
                    }
                } else {
                    // not a ref, doing nothing
                }
            }
        }

        // ensure `allParents` always includes `parent`
        // this is more robust than keeping logic in getParentName() and getAllParentsName() in sync
        String parentName = getParentName(composedSchema, allSchemas);
        if (parentName != null && !names.contains(parentName)) {
            names.add(parentName);
        }

        return names;
    }

    private static boolean hasOrInheritsDiscriminator(Schema schema, Map<String, Schema> allSchemas) {
        if (schema.getDiscriminator() != null && StringUtils.isNotEmpty(schema.getDiscriminator().getPropertyName())) {
            return true;
        }
        else if (StringUtils.isNotEmpty(schema.get$ref())) {
            String parentName = getSimpleRef(schema.get$ref());
            Schema s = allSchemas.get(parentName);
            if (s != null) {
                return hasOrInheritsDiscriminator(s, allSchemas);
            } else {
                LOGGER.error("Failed to obtain schema from {}", parentName);
            }
        } else if (schema instanceof ComposedSchema) {
            final ComposedSchema composed = (ComposedSchema) schema;
            final List<Schema> interfaces = getInterfaces(composed);
            for (Schema i : interfaces) {
                if (hasOrInheritsDiscriminator(i, allSchemas)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Return true if the 'nullable' attribute is set to true in the schema, i.e. if the value
     * of the property can be the null value.
     *
     * In addition, if the OAS document is 3.1 or above, isNullable returns true if the input
     * schema is a 'oneOf' composed document with at most two children, and one of the children
     * is the 'null' type.
     *
     * The caller is responsible for resolving schema references before invoking isNullable.
     * If the input schema is a $ref and the referenced schema has 'nullable: true', this method
     * returns false (because the nullable attribute is defined in the referenced schema).
     *
     * The 'nullable' attribute was introduced in OAS 3.0.
     * The 'nullable' attribute is deprecated in OAS 3.1. In a OAS 3.1 document, the preferred way
     * to specify nullable properties is to use the 'null' type.
     *
     * @param schema the OAS schema.
     * @return true if the schema is nullable.
     */
    public static boolean isNullable(Schema schema) {
        if (schema == null) {
            return false;
        }

        if (Boolean.TRUE.equals(schema.getNullable())) {
            return true;
        }

        if (schema.getExtensions() != null && schema.getExtensions().get("x-nullable") != null) {
            return Boolean.valueOf(schema.getExtensions().get("x-nullable").toString());
        }
        // In OAS 3.1, the recommended way to define a nullable property or object is to use oneOf.
        if (schema instanceof ComposedSchema) {
            return isNullableComposedSchema(((ComposedSchema) schema));
        }
        return false;
    }

    /**
     * Return true if the specified composed schema is 'oneOf', contains one or two elements,
     * and at least one of the elements is the 'null' type.
     *
     * The 'null' type is supported in OAS 3.1 and above.
     * In the example below, the 'OptionalOrder' can have the null value because the 'null'
     * type is one of the elements under 'oneOf'.
     *
     * OptionalOrder:
     *   oneOf:
     *     - type: 'null'
     *     - $ref: '#/components/schemas/Order'
     *
     * @param schema the OAS composed schema.
     * @return true if the composed schema is nullable.
     */
    public static boolean isNullableComposedSchema(ComposedSchema schema) {
        List<Schema> oneOf = schema.getOneOf();
        if (oneOf != null && oneOf.size() <= 2) {
            for (Schema s : oneOf) {
                if (isNullType(s)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * isNullType returns true if the input schema is the 'null' type.
     *
     * The 'null' type is supported in OAS 3.1 and above. It is not supported
     * in OAS 2.0 and OAS 3.0.x.
     *
     * For example, the "null" type could be used to specify that a value must
     * either be null or a specified type:
     *
     * OptionalOrder:
     *   oneOf:
     *     - type: 'null'
     *     - $ref: '#/components/schemas/Order'
     *
     * @param schema the OpenAPI schema
     * @return true if the schema is the 'null' type
     */
    public static boolean isNullType(Schema schema) {
        if ("null".equals(schema.getType())) {
            return true;
        }
        return false;
    }

    public static void syncValidationProperties(Schema schema, IJsonSchemaValidationProperties target) {
        if (schema != null && target != null) {
            target.setPattern(schema.getPattern());
            BigDecimal minimum = schema.getMinimum();
            BigDecimal maximum = schema.getMaximum();
            Boolean exclusiveMinimum = schema.getExclusiveMinimum();
            Boolean exclusiveMaximum = schema.getExclusiveMaximum();
            Integer minLength = schema.getMinLength();
            Integer maxLength = schema.getMaxLength();
            Integer minItems = schema.getMinItems();
            Integer maxItems = schema.getMaxItems();
            Boolean uniqueItems = schema.getUniqueItems();
            Integer minProperties = schema.getMinProperties();
            Integer maxProperties = schema.getMaxProperties();

            if (minimum != null) target.setMinimum(String.valueOf(minimum));
            if (maximum != null) target.setMaximum(String.valueOf(maximum));
            if (exclusiveMinimum != null) target.setExclusiveMinimum(exclusiveMinimum);
            if (exclusiveMaximum != null) target.setExclusiveMaximum(exclusiveMaximum);
            if (minLength != null) target.setMinLength(minLength);
            if (maxLength != null) target.setMaxLength(maxLength);
            if (minItems != null) target.setMinItems(minItems);
            if (maxItems != null) target.setMaxItems(maxItems);
            if (uniqueItems != null) target.setUniqueItems(uniqueItems);
            if (minProperties != null) target.setMinProperties(minProperties);
            if (maxProperties != null) target.setMaxProperties(maxProperties);
        }
    }

    private static ObjectMapper getRightMapper(String data) {
        ObjectMapper mapper;
        if(data.trim().startsWith("{")) {
            mapper = JSON_MAPPER;
        } else {
            mapper = YAML_MAPPER;
        }
        return mapper;
    }

    /**
     * Parse and return a JsonNode representation of the input OAS document.
     * 
     * @param location the URL of the OAS document.
     * @param auths the list of authorization values to access the remote URL.
     * 
     * @throws java.lang.Exception if an error occurs while retrieving the OpenAPI document.
     * 
     * @return A JsonNode representation of the input OAS document.
     */
    public static JsonNode readWithInfo(String location, List<AuthorizationValue> auths) throws Exception {
        String data;
        location = location.replaceAll("\\\\","/");
        if (location.toLowerCase(Locale.ROOT).startsWith("http")) {
            data = RemoteUrl.urlToString(location, auths);
        } else {
            final String fileScheme = "file:";
            Path path;
            if (location.toLowerCase(Locale.ROOT).startsWith(fileScheme)) {
                path = Paths.get(URI.create(location));
            } else {
                path = Paths.get(location);
            }
            if (Files.exists(path)) {
                data = FileUtils.readFileToString(path.toFile(), "UTF-8");
            } else {
                data = ClasspathHelper.loadFileFromClasspath(location);
            }
        }
        return getRightMapper(data).readTree(data);
    }

    /**
     * Parse the OAS document at the specified location, get the swagger or openapi version
     * as specified in the source document, and return the version.
     * 
     * For OAS 2.0 documents, return the value of the 'swagger' attribute.
     * For OAS 3.x documents, return the value of the 'openapi' attribute.
     * 
     * @param openAPI the object that encapsulates the OAS document.
     * @param location the URL of the OAS document.
     * @param auths the list of authorization values to access the remote URL.
     * 
     * @return the version of the OpenAPI document.
     */
    public static SemVer getOpenApiVersion(OpenAPI openAPI, String location, List<AuthorizationValue> auths) {
        String version;
        try {
            JsonNode document = readWithInfo(location, auths);
            JsonNode value = document.findValue("swagger");
            if (value == null) {
                // This is not a OAS 2.0 document.
                // Note: we cannot simply return the value of the "openapi" attribute
                // because the 2.0 to 3.0 converter always sets the value to '3.0'.
                value = document.findValue("openapi");
            }
            version = value.asText();
        } catch (Exception ex) {
            // Fallback to using the 'openapi' attribute.
            LOGGER.warn("Unable to read swagger/openapi attribute");
            version = openAPI.getOpenapi();
        }
        // Cache the OAS version in global settings so it can be looked up in the helper functions.
        //GlobalSettings.setProperty(openapiDocVersion, version);

        return new SemVer(version);
    }
}
