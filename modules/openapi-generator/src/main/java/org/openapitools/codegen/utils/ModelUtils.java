/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;


public class ModelUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(ModelUtils.class);
    private static boolean generateAliasAsModel = false;

    public static void setGenerateAliasAsModel(boolean value) {
        generateAliasAsModel = value;
    }

    public static boolean isGenerateAliasAsModel() {
        return generateAliasAsModel;
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
        List<String> allUsedSchemas = new ArrayList<String>();
        visitOpenAPI(openAPI, (s, t) -> {
            if (s.get$ref() != null) {
                String ref = getSimpleRef(s.get$ref());
                if (!allUsedSchemas.contains(ref)) {
                    allUsedSchemas.add(ref);
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
        List<String> unusedSchemas = new ArrayList<String>();

        Map<String, Schema> schemas = getSchemas(openAPI);
        unusedSchemas.addAll(schemas.keySet());

        visitOpenAPI(openAPI, (s, t) -> {
            if (s.get$ref() != null) {
                unusedSchemas.remove(getSimpleRef(s.get$ref()));
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
                if (operation.getParameters() != null) {
                    for (Parameter p : operation.getParameters()) {
                        Parameter parameter = getReferencedParameter(openAPI, p);
                        if (parameter.getSchema() != null) {
                            visitSchema(openAPI, parameter.getSchema(), null, visitedSchemas, visitor);
                        }
                    }
                }

                //RequestBody:
                RequestBody requestBody = getReferencedRequestBody(openAPI, operation.getRequestBody());
                if (requestBody != null && requestBody.getContent() != null) {
                    for (Entry<String, MediaType> e : requestBody.getContent().entrySet()) {
                        if (e.getValue().getSchema() != null) {
                            visitSchema(openAPI, e.getValue().getSchema(), e.getKey(), visitedSchemas, visitor);
                        }
                    }
                }

                //Responses:
                if (operation.getResponses() != null) {
                    for (ApiResponse r : operation.getResponses().values()) {
                        ApiResponse apiResponse = getReferencedApiResponse(openAPI, r);
                        if (apiResponse != null) {
                            if (apiResponse.getContent() != null) {
                                for (Entry<String, MediaType> e : apiResponse.getContent().entrySet()) {
                                    if (e.getValue().getSchema() != null) {
                                        visitSchema(openAPI, e.getValue().getSchema(), e.getKey(), visitedSchemas, visitor);
                                    }
                                }
                            }
                            if (apiResponse.getHeaders() != null) {
                                for (Entry<String, Header> e : apiResponse.getHeaders().entrySet()) {
                                    Header header = getReferencedHeader(openAPI, e.getValue());
                                    if (header.getSchema() != null) {
                                        visitSchema(openAPI, header.getSchema(), e.getKey(), visitedSchemas, visitor);
                                    }
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
    }

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
            LOGGER.warn("Failed to get the schema name: {}", ref);
            //throw new RuntimeException("Failed to get the schema: " + ref);
            return null;

        }

        return ref;
    }

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

    public static boolean isComposedSchema(Schema schema) {
        if (schema instanceof ComposedSchema) {
            return true;
        }
        return false;
    }

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

    public static boolean isArraySchema(Schema schema) {
        if (schema instanceof ArraySchema) {
            return true;
        }
        // assume it's an array if maxItems, minItems is set
        if (schema != null && (schema.getMaxItems() != null || schema.getMinItems() != null)) {
            return true;
        }
        return false;
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
     * Check to see if the schema is a model with at least one properties
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a model with at least one properties
     */
    public static boolean isModel(Schema schema) {
        if (schema == null) {
            LOGGER.error("Schema cannot be null in isModel check");
            return false;
        }

        // has at least one property
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            return true;
        }

        // composed schema is a model
        if (schema instanceof ComposedSchema) {
            return true;
        }

        return false;
    }

    /**
     * Check to see if the schema is a free form object
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a free-form object
     */
    public static boolean isFreeFormObject(Schema schema) {
        if (schema == null) {
            LOGGER.error("Schema cannot be null in isFreeFormObject check");
            return false;
        }

        // not free-form if allOf, anyOf, oneOf is not empty
        if (schema instanceof ComposedSchema) {
            ComposedSchema cs = (ComposedSchema) schema;
            List<Schema> interfaces = getInterfaces(cs);
            if (interfaces != null && !interfaces.isEmpty()) {
                return false;
            }
        }

        // has at least one property
        if ("object".equals(schema.getType())) {
            // no properties
            if ((schema.getProperties() == null || schema.getProperties().isEmpty())) {
                Schema addlProps = getAdditionalProperties(schema);
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

    public static Map<String, Schema> getSchemas(OpenAPI openAPI) {
        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getSchemas() != null) {
            return openAPI.getComponents().getSchemas();
        }
        return Collections.emptyMap();
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

    private static Schema getSchemaFromContent(Content content) {
        if (content == null || content.isEmpty()) {
            return null;
        }
        if (content.size() > 1) {
            LOGGER.warn("Multiple schemas found in content, returning only the first one");
        }
        MediaType mediaType = content.values().iterator().next();
        return mediaType.getSchema();
    }

    /**
     * Get the actual schema from aliases. If the provided schema is not an alias, the schema itself will be returned.
     *
     * @param openAPI specification being checked
     * @param schema  schema (alias or direct reference)
     * @return actual schema
     */
    public static Schema unaliasSchema(OpenAPI openAPI, Schema schema) {
        Map<String, Schema> allSchemas = getSchemas(openAPI);
        if (allSchemas == null || allSchemas.isEmpty()) {
            // skip the warning as the spec can have no model defined
            //LOGGER.warn("allSchemas cannot be null/empty in unaliasSchema. Returned 'schema'");
            return schema;
        }

        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            Schema ref = allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref()));
            if (ref == null) {
                LOGGER.warn("{} is not defined", schema.get$ref());
                return schema;
            } else if (ref.getEnum() != null && !ref.getEnum().isEmpty()) {
                // top-level enum class
                return schema;
            } else if (isArraySchema(ref)) {
                if (generateAliasAsModel) {
                    return schema; // generate a model extending array
                } else {
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())));
                }
            } else if (isComposedSchema(ref)) {
                return schema;
            } else if (isMapSchema(ref)) {
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) // has at least one property
                    return schema; // treat it as model
                else {
                    if (generateAliasAsModel) {
                        return schema; // generate a model extending map
                    } else {
                        // treat it as a typical map
                        return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())));
                    }
                }
            } else if (isObjectSchema(ref)) { // model
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) { // has at least one property
                    return schema;
                } else { // free form object (type: object)
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())));
                }
            } else {
                return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())));
            }
        }
        return schema;
    }

    public static Schema getAdditionalProperties(Schema schema) {
        if (schema.getAdditionalProperties() instanceof Schema) {
            return (Schema) schema.getAdditionalProperties();
        }
        if (schema.getAdditionalProperties() instanceof Boolean && (Boolean) schema.getAdditionalProperties()) {
            return new ObjectSchema();
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
     * Get the the parent model name from the schemas (allOf, anyOf, oneOf)
     *
     * @param composedSchema schema (alias or direct reference)
     * @param allSchemas     all schemas
     * @return the name of the parent model
     */
    public static String getParentName(ComposedSchema composedSchema, Map<String, Schema> allSchemas) {
        List<Schema> interfaces = getInterfaces(composedSchema);

        if (interfaces != null && !interfaces.isEmpty()) {
            for (Schema schema : interfaces) {
                // get the actual schema
                if (StringUtils.isNotEmpty(schema.get$ref())) {
                    String parentName = getSimpleRef(schema.get$ref());
                    Schema s = allSchemas.get(parentName);
                    if (s == null) {
                        LOGGER.error("Failed to obtain schema from {}", parentName);
                        return "UNKNOWN_PARENT_NAME";
                    } else if (s.getDiscriminator() != null && StringUtils.isNotEmpty(s.getDiscriminator().getPropertyName())) {
                        // discriminator.propertyName is used
                        return parentName;
                    } else {
                        LOGGER.debug("Not a parent since discriminator.propertyName is not set {}", s.get$ref());
                        // not a parent since discriminator.propertyName is not set
                    }
                } else {
                    // not a ref, doing nothing
                }
            }
        }

        return null;
    }

    public static List<String> getAllParentsName(ComposedSchema composedSchema, Map<String, Schema> allSchemas) {
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
                    } else if (s.getDiscriminator() != null && StringUtils.isNotEmpty(s.getDiscriminator().getPropertyName())) {
                        // discriminator.propertyName is used
                        names.add(parentName);
                    } else {
                        LOGGER.debug("Not a parent since discriminator.propertyName is not set {}", s.get$ref());
                        // not a parent since discriminator.propertyName is not set
                    }
                } else {
                    // not a ref, doing nothing
                }
            }
        }

        return names;
    }

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

        return false;
    }
}
