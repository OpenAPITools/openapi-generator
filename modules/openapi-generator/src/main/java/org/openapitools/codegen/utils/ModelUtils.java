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
import io.swagger.v3.parser.ObjectMapperFactory;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.util.ClasspathHelper;
import io.swagger.v3.parser.util.RemoteUrl;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.IJsonSchemaValidationProperties;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.URI;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class ModelUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(ModelUtils.class);

    private static final String URI_FORMAT = "uri";

    private static final String generateAliasAsModelKey = "generateAliasAsModel";

    // A vendor extension to track the value of the 'swagger' field in a 2.0 doc, if applicable.
    private static final String openapiDocVersion = "x-original-swagger-version";

    // A vendor extension to track the value of the 'disallowAdditionalPropertiesIfNotPresent' CLI
    private static final String disallowAdditionalPropertiesIfNotPresent = "x-disallow-additional-properties-if-not-present";

    private static final String freeFormExplicit = "x-is-free-form";

    private static final ObjectMapper JSON_MAPPER;
    private static final ObjectMapper YAML_MAPPER;

    static {
        JSON_MAPPER = ObjectMapperFactory.createJson();
        YAML_MAPPER = ObjectMapperFactory.createYaml();
    }

    public static boolean isDisallowAdditionalPropertiesIfNotPresent() {
        return Boolean.parseBoolean(GlobalSettings.getProperty(disallowAdditionalPropertiesIfNotPresent, "true"));
    }

    public static void setDisallowAdditionalPropertiesIfNotPresent(boolean value) {
        GlobalSettings.setProperty(disallowAdditionalPropertiesIfNotPresent, Boolean.toString(value));
    }

    public static boolean isGenerateAliasAsModel() {
        return Boolean.parseBoolean(GlobalSettings.getProperty(generateAliasAsModelKey, "false"));
    }

    public static void setGenerateAliasAsModel(boolean value) {
        GlobalSettings.setProperty(generateAliasAsModelKey, Boolean.toString(value));
    }

    public static boolean isGenerateAliasAsModel(Schema schema) {
        return isGenerateAliasAsModel() || (schema.getExtensions() != null && schema.getExtensions().getOrDefault("x-generate-alias-as-model", false).equals(true));
    }

    /**
     * Searches for the model by name in the map of models and returns it
     *
     * @param name   Name of the model
     * @param models Map of models
     * @return model
     */
    public static CodegenModel getModelByName(final String name, final Map<String, ModelsMap> models) {
        final ModelsMap data = models.get(name);
        if (data != null) {
            final List<ModelMap> dataModelsList = data.getModels();
            if (dataModelsList != null) {
                for (final ModelMap entryMap : dataModelsList) {
                    final CodegenModel model = entryMap.getModel();
                    if (model != null) {
                        return model;
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

        if (openAPI != null) {
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
        }
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
     * <p>
     * To avoid infinite recursion, referenced schemas are visited only once. When a referenced schema is visited,
     * it is added to visitedSchemas.
     *
     * @param openAPI        the OpenAPI document that contains schema objects.
     * @param schema         the root schema object to be visited.
     * @param mimeType       the mime type. TODO: does not seem to be used in a meaningful way.
     * @param visitedSchemas the list of referenced schemas that have been visited.
     * @param visitor        the visitor function which is invoked for every visited schema.
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
        if (isComposedSchema(schema)) {
            List<Schema> oneOf = schema.getOneOf();
            if (oneOf != null) {
                for (Schema s : oneOf) {
                    visitSchema(openAPI, s, mimeType, visitedSchemas, visitor);
                }
            }
            List<Schema> allOf = schema.getAllOf();
            if (allOf != null) {
                for (Schema s : allOf) {
                    visitSchema(openAPI, s, mimeType, visitedSchemas, visitor);
                }
            }
            List<Schema> anyOf = schema.getAnyOf();
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
                visitSchema(openAPI, property, null, visitedSchemas, visitor);
            }
        }
    }

    public static String getSimpleRef(String ref) {
        if (ref == null) {
            once(LOGGER).warn("Failed to get the schema name: null");
            //throw new RuntimeException("Failed to get the schema: null");
            return null;
        } else if (ref.startsWith("#/components/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        } else if (ref.startsWith("#/definitions/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        } else {
            once(LOGGER).warn("Failed to get the schema name: {}", ref);
            //throw new RuntimeException("Failed to get the schema: " + ref);
            return null;
        }

        try {
            ref = URLDecoder.decode(ref, "UTF-8");
        } catch (UnsupportedEncodingException ignored) {
            once(LOGGER).warn("Found UnsupportedEncodingException: {}", ref);
        }

        // see https://tools.ietf.org/html/rfc6901#section-3
        // Because the characters '~' (%x7E) and '/' (%x2F) have special meanings in
        // JSON Pointer, '~' needs to be encoded as '~0' and '/' needs to be encoded
        // as '~1' when these characters appear in a reference token.
        // This reverses that encoding.
        ref = ref.replace("~1", "/").replace("~0", "~");

        return ref;
    }

    /**
     * Return true if the specified schema is type object
     * We can't use isObjectSchema because it requires properties to exist which is not required
     * We can't use isMap because it is true for AnyType use cases
     *
     * @param schema the OAS schema
     * @return true if the specified schema is an Object schema.
     */
    public static boolean isTypeObjectSchema(Schema schema) {
        return SchemaTypeUtil.OBJECT_TYPE.equals(schema.getType());
    }

    /**
     * Return true if the specified schema is an object with a fixed number of properties.
     * <p>
     * A ObjectSchema differs from a MapSchema in the following way:
     * - An ObjectSchema is not extensible, i.e. it has a fixed number of properties.
     * - A MapSchema is an object that can be extended with an arbitrary set of properties.
     *   The payload may include dynamic properties.
     * <p>
     * For example, an OpenAPI schema is considered an ObjectSchema in the following scenarios:
     * <p>
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
        if (schema == null) {
            return false;
        }

        return (schema instanceof ObjectSchema) ||
                // must not be a map
                (SchemaTypeUtil.OBJECT_TYPE.equals(schema.getType()) && !(schema instanceof MapSchema)) ||
                // must have at least one property
                (schema.getType() == null && schema.getProperties() != null && !schema.getProperties().isEmpty());
    }

    /**
     * Return true if the specified schema is composed, i.e. if it uses
     * 'oneOf', 'anyOf' or 'allOf'.
     *
     * @param schema the OAS schema
     * @return true if the specified schema is a Composed schema.
     */
    public static boolean isComposedSchema(Schema schema) {
        if (schema == null) {
            return false;
        }

        // in 3.0, ComposeSchema is used for anyOf/oneOf/allOf
        // in 3.1, it's not the case so we need more checks below
        if (schema instanceof ComposedSchema) {
            return true;
        }

        // has oneOf
        if (schema.getOneOf() != null) {
            return true;
        }

        // has anyOf
        if (schema.getAnyOf() != null) {
            return true;
        }

        // has allOf
        if (schema.getAllOf() != null) {
            return true;
        }

        return false;
    }

    /**
     * Return true if the specified schema is composed with more than one of the following:
     * 'oneOf', 'anyOf' or 'allOf'.
     *
     * @param schema the OAS schema
     * @return true if the specified schema is a Composed schema.
     */
    public static boolean isComplexComposedSchema(Schema schema) {
        if (!(schema instanceof ComposedSchema)) {
            return false;
        }

        int count = 0;

        if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
            count++;
        }

        if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
            count++;
        }

        if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
            count++;
        }

        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            count++;
        }

        return count > 1;
    }

    /**
     * Return true if the specified 'schema' is an object that can be extended with additional properties.
     * Additional properties means a Schema should support all explicitly defined properties plus any
     * undeclared properties.
     * <p>
     * A MapSchema differs from an ObjectSchema in the following way:
     * - An ObjectSchema is not extensible, i.e. it has a fixed number of properties.
     * - A MapSchema is an object that can be extended with an arbitrary set of properties.
     * The payload may include dynamic properties.
     * <p>
     * Note that isMapSchema returns true for a composed schema (allOf, anyOf, oneOf) that also defines
     * additionalproperties.
     * <p>
     * For example, an OpenAPI schema is considered a MapSchema in the following scenarios:
     * <p>
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
        if (schema == null) {
            return false;
        }

        // additionalProperties explicitly set to false
        if (schema.getAdditionalProperties() instanceof Boolean && Boolean.FALSE.equals(schema.getAdditionalProperties())) {
            return false;
        }

        return (schema instanceof MapSchema) ||
                (schema.getAdditionalProperties() instanceof Schema) ||
                (schema.getAdditionalProperties() instanceof Boolean && (Boolean) schema.getAdditionalProperties());
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
        return schema instanceof StringSchema || SchemaTypeUtil.STRING_TYPE.equals(schema.getType());
    }

    public static boolean isIntegerSchema(Schema schema) {
        return schema instanceof IntegerSchema || SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType());
    }

    public static boolean isShortSchema(Schema schema) {
        // format: short (int32)
        return SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) // type: integer
                && SchemaTypeUtil.INTEGER32_FORMAT.equals(schema.getFormat());
    }

    public static boolean isUnsignedIntegerSchema(Schema schema) {
        return SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) && // type: integer
                ("int32".equals(schema.getFormat()) || schema.getFormat() == null) && // format: int32
                (schema.getExtensions() != null && (Boolean) schema.getExtensions().getOrDefault("x-unsigned", Boolean.FALSE));
    }

    public static boolean isLongSchema(Schema schema) {
        // format: long (int64)
        return SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) // type: integer
                && SchemaTypeUtil.INTEGER64_FORMAT.equals(schema.getFormat());
    }

    public static boolean isUnsignedLongSchema(Schema schema) {
        return SchemaTypeUtil.INTEGER_TYPE.equals(schema.getType()) && // type: integer
                "int64".equals(schema.getFormat()) && // format: int64
                (schema.getExtensions() != null && (Boolean) schema.getExtensions().getOrDefault("x-unsigned", Boolean.FALSE));
    }

    public static boolean isBooleanSchema(Schema schema) {
        return schema instanceof BooleanSchema || SchemaTypeUtil.BOOLEAN_TYPE.equals(schema.getType());
    }

    public static boolean isNumberSchema(Schema schema) {
        return schema instanceof NumberSchema || SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType());
    }

    public static boolean isFloatSchema(Schema schema) {
        // format: float
        return SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType())
                && SchemaTypeUtil.FLOAT_FORMAT.equals(schema.getFormat());
    }

    public static boolean isDoubleSchema(Schema schema) {
        // format: double
        return SchemaTypeUtil.NUMBER_TYPE.equals(schema.getType())
                && SchemaTypeUtil.DOUBLE_FORMAT.equals(schema.getFormat());
    }

    public static boolean isDateSchema(Schema schema) {
        return (schema instanceof DateSchema) ||
                // format: date
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.DATE_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isDateTimeSchema(Schema schema) {
        return (schema instanceof DateTimeSchema) ||
                // format: date-time
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.DATE_TIME_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isPasswordSchema(Schema schema) {
        return (schema instanceof PasswordSchema) ||
                // double
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.PASSWORD_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isByteArraySchema(Schema schema) {
        return (schema instanceof ByteArraySchema) ||
                // format: byte
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.BYTE_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isBinarySchema(Schema schema) {
        return (schema instanceof BinarySchema) ||
                // format: binary
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.BINARY_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isFileSchema(Schema schema) {
        return (schema instanceof FileSchema) ||
                // file type in oas2 mapped to binary in oas3
                isBinarySchema(schema);
    }

    public static boolean isUUIDSchema(Schema schema) {
        return (schema instanceof UUIDSchema) ||
                // format: uuid
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.UUID_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isURISchema(Schema schema) {
        // format: uri
        return SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                && URI_FORMAT.equals(schema.getFormat());
    }

    public static boolean isEmailSchema(Schema schema) {
        return (schema instanceof EmailSchema) ||
                // format: email
                (SchemaTypeUtil.STRING_TYPE.equals(schema.getType())
                        && SchemaTypeUtil.EMAIL_FORMAT.equals(schema.getFormat()));
    }

    public static boolean isDecimalSchema(Schema schema) {
        // format: number
        return SchemaTypeUtil.STRING_TYPE.equals(schema.getType()) // type: string
                && "number".equals(schema.getFormat());
    }

    /**
     * Check to see if the schema is a model
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a model with at least one properties
     */
    public static boolean isModel(Schema schema) {
        return (schema != null) &&
                // has properties
                ((null != schema.getProperties() && !schema.getProperties().isEmpty())
                // composed schema is a model, consider very simple ObjectSchema a model
                        || isComposedSchema(schema)
                        || schema instanceof ObjectSchema);
    }

    /**
     * Check to see if the schema is a model with properties only (non-composed model)
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a model with at least one properties
     */
    public static boolean isModelWithPropertiesOnly(Schema schema) {
        return (schema != null) &&
                // has properties
                (null != schema.getProperties() && !schema.getProperties().isEmpty()) &&
                // no additionalProperties is set
                (schema.getAdditionalProperties() == null ||
                        (schema.getAdditionalProperties() instanceof Boolean && !(Boolean) schema.getAdditionalProperties()));
    }

    public static boolean hasValidation(Schema sc) {
        return (
                sc.getMaxItems() != null ||
                        sc.getMinProperties() != null ||
                        sc.getMaxProperties() != null ||
                        sc.getMinLength() != null ||
                        sc.getMinItems() != null ||
                        sc.getMultipleOf() != null ||
                        sc.getPattern() != null ||
                        sc.getMaxLength() != null ||
                        sc.getMinimum() != null ||
                        sc.getMaximum() != null ||
                        sc.getExclusiveMaximum() != null ||
                        sc.getExclusiveMinimum() != null ||
                        sc.getUniqueItems() != null
        );
    }

    /**
     * Check to see if the schema is a free form object.
     * <p>
     * A free form object is an object (i.e. 'type: object' in a OAS document) that:
     * 1) Does not define properties, and
     * 2) Is not a composed schema (no anyOf, oneOf, allOf), and
     * 3) additionalproperties is not defined, or additionalproperties: true, or additionalproperties: {}.
     * <p>
     * Examples:
     * <p>
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
     * @param schema  potentially containing a '$ref'
     * @return true if it's a free-form object
     */
    public static boolean isFreeFormObject(Schema schema) {
        if (schema == null) {
            // TODO: Is this message necessary? A null schema is not a free-form object, so the result is correct.
            once(LOGGER).error("Schema cannot be null in isFreeFormObject check");
            return false;
        }

        // not free-form if allOf, anyOf, oneOf is not empty
        if (isComposedSchema(schema)) {
            List<Schema> interfaces = ModelUtils.getInterfaces(schema);
            if (interfaces != null && !interfaces.isEmpty()) {
                return false;
            }
        }

        // has at least one property
        if ("object".equals(schema.getType())) {
            // no properties
            if ((schema.getProperties() == null || schema.getProperties().isEmpty())) {
                Schema addlProps = ModelUtils.getAdditionalProperties(schema);

                if (schema.getExtensions() != null && schema.getExtensions().containsKey(freeFormExplicit)) {
                    // User has hard-coded vendor extension to handle free-form evaluation.
                    boolean isFreeFormExplicit = Boolean.parseBoolean(String.valueOf(schema.getExtensions().get(freeFormExplicit)));
                    if (!isFreeFormExplicit && addlProps != null && addlProps.getProperties() != null && !addlProps.getProperties().isEmpty()) {
                        once(LOGGER).error(String.format(Locale.ROOT, "Potentially confusing usage of %s within model which defines additional properties", freeFormExplicit));
                    }
                    return isFreeFormExplicit;
                }

                // additionalProperties not defined
                if (addlProps == null) {
                    return true;
                } else {
                    if (addlProps instanceof ObjectSchema) {
                        ObjectSchema objSchema = (ObjectSchema) addlProps;
                        // additionalProperties defined as {}
                        return objSchema.getProperties() == null || objSchema.getProperties().isEmpty();
                    } else if (addlProps instanceof Schema) {
                        // additionalProperties defined as {}
                        return addlProps.getType() == null && addlProps.get$ref() == null && (addlProps.getProperties() == null || addlProps.getProperties().isEmpty());
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
     * If a RequestBody contains a reference to another RequestBody with '$ref', returns the referenced RequestBody if it is found or the actual RequestBody in the other cases.
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
     * If a ApiResponse contains a reference to another ApiResponse with '$ref', returns the referenced ApiResponse if it is found or the actual ApiResponse in the other cases.
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
        if (name != null && openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getResponses() != null) {
            return openAPI.getComponents().getResponses().get(name);
        }
        return null;
    }

    /**
     * If a Parameter contains a reference to another Parameter with '$ref', returns the referenced Parameter if it is found or the actual Parameter in the other cases.
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
        if (name != null && openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getParameters() != null) {
            return openAPI.getComponents().getParameters().get(name);
        }
        return null;
    }

    /**
     * If a Callback contains a reference to another Callback with '$ref', returns the referenced Callback if it is found or the actual Callback in the other cases.
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
        if (name != null && openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getCallbacks() != null) {
            return openAPI.getComponents().getCallbacks().get(name);
        }
        return null;
    }

    /**
     * Return the first defined Schema for a RequestBody
     *
     * @param requestBody request body of the operation
     * @return first schema
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
     * <p>
     * For example, given the following OAS, this method returns the schema
     * for the 'application/json' content type because it is listed first in the OAS.
     * <p>
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
            once(LOGGER).debug("Multiple schemas found in the OAS 'content' section, returning only the first one ({})",
                    entry.getKey());
        }
        return entry.getValue().getSchema();
    }

    /**
     * Has self reference?
     *
     * @param openAPI OpenAPI spec.
     * @param schema  Schema
     * @return boolean true if it has at least one self reference
     */
    public static boolean hasSelfReference(OpenAPI openAPI,
                                           Schema schema) {
        return hasSelfReference(openAPI, schema, null);
    }

    /**
     * Has self reference?
     *
     * @param openAPI            OpenAPI spec.
     * @param schema             Schema
     * @param visitedSchemaNames A set of visited schema names
     * @return boolean true if it has at least one self reference
     */
    public static boolean hasSelfReference(OpenAPI openAPI,
                                           Schema schema,
                                           Set<String> visitedSchemaNames) {
        if (visitedSchemaNames == null) {
            visitedSchemaNames = new HashSet<String>();
        }

        if (schema.get$ref() != null) {
            String ref = getSimpleRef(schema.get$ref());
            if (!visitedSchemaNames.contains(ref)) {
                visitedSchemaNames.add(ref);
                Schema referencedSchema = getSchemas(openAPI).get(ref);
                if (referencedSchema != null) {
                    return hasSelfReference(openAPI, referencedSchema, visitedSchemaNames);
                } else {
                    LOGGER.error("Failed to obtain schema from `{}` in self reference check", ref);
                    return false;
                }
            } else {
                return true;
            }
        }
        if (isComposedSchema(schema)) {
            List<Schema> oneOf = schema.getOneOf();
            if (oneOf != null) {
                for (Schema s : oneOf) {
                    if (hasSelfReference(openAPI, s, visitedSchemaNames)) {
                        return true;
                    }
                }
            }
            List<Schema> allOf = schema.getAllOf();
            if (allOf != null) {
                for (Schema s : allOf) {
                    if (hasSelfReference(openAPI, s, visitedSchemaNames)) {
                        return true;
                    }
                }
            }
            List<Schema> anyOf = schema.getAnyOf();
            if (anyOf != null) {
                for (Schema s : anyOf) {
                    if (hasSelfReference(openAPI, s, visitedSchemaNames)) {
                        return true;
                    }
                }
            }
        } else if (isArraySchema(schema)) {
            Schema itemsSchema = ((ArraySchema) schema).getItems();
            if (itemsSchema != null) {
                return hasSelfReference(openAPI, itemsSchema, visitedSchemaNames);
            }
        } else if (isMapSchema(schema)) {
            Object additionalProperties = schema.getAdditionalProperties();
            if (additionalProperties instanceof Schema) {
                return hasSelfReference(openAPI, (Schema) additionalProperties, visitedSchemaNames);
            }
        } else if (schema.getNot() != null) {
            return hasSelfReference(openAPI, schema.getNot(), visitedSchemaNames);
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            // go through properties to see if there's any self-reference
            for (Schema property : ((Map<String, Schema>) schema.getProperties()).values()) {
                if (hasSelfReference(openAPI, property, visitedSchemaNames)) {
                    return true;
                }
            }
        }
        return false;
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
        return unaliasSchema(openAPI, schema, Collections.emptyMap());
    }

    /**
     * Get the actual schema from aliases. If the provided schema is not an alias, the schema itself will be returned.
     *
     * @param openAPI        OpenAPI document containing the schemas.
     * @param schema         schema (alias or direct reference)
     * @param schemaMappings mappings of external types to be omitted by unaliasing
     * @return actual schema
     */
    public static Schema unaliasSchema(OpenAPI openAPI,
                                       Schema schema,
                                       Map<String, String> schemaMappings) {
        Map<String, Schema> allSchemas = getSchemas(openAPI);
        if (allSchemas == null || allSchemas.isEmpty()) {
            // skip the warning as the spec can have no model defined
            //LOGGER.warn("allSchemas cannot be null/empty in unaliasSchema. Returned 'schema'");
            return schema;
        }

        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            String simpleRef = ModelUtils.getSimpleRef(schema.get$ref());
            if (schemaMappings.containsKey(simpleRef)) {
                LOGGER.debug("Schema unaliasing of {} omitted because aliased class is to be mapped to {}", simpleRef, schemaMappings.get(simpleRef));
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
                if (isGenerateAliasAsModel(ref)) {
                    return schema; // generate a model extending array
                } else {
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            schemaMappings);
                }
            } else if (isComposedSchema(ref)) {
                return schema;
            } else if (isMapSchema(ref)) {
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) // has at least one property
                    return schema; // treat it as model
                else {
                    if (isGenerateAliasAsModel(ref)) {
                        return schema; // generate a model extending map
                    } else {
                        // treat it as a typical map
                        return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                                schemaMappings);
                    }
                }
            } else if (isObjectSchema(ref)) { // model
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) { // has at least one property
                    // TODO we may need to check `hasSelfReference(openAPI, ref)` as a special/edge case:
                    // TODO we may also need to revise below to return `ref` instead of schema
                    // which is the last reference to the actual model/object
                    return schema;
                } else { // free form object (type: object)
                    return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            schemaMappings);
                }
            } else {
                return unaliasSchema(openAPI, allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())), schemaMappings);
            }
        }
        return schema;
    }

    /**
     * Returns the additionalProperties Schema for the specified input schema.
     * <p>
     * The additionalProperties keyword is used to control the handling of additional, undeclared
     * properties, that is, properties whose names are not listed in the properties keyword.
     * The additionalProperties keyword may be either a boolean or an object.
     * If additionalProperties is a boolean and set to false, no additional properties are allowed.
     * By default when the additionalProperties keyword is not specified in the input schema,
     * any additional properties are allowed. This is equivalent to setting additionalProperties
     * to the boolean value True or setting additionalProperties: {}
     *
     * @param schema  the input schema that may or may not have the additionalProperties keyword.
     * @return the Schema of the additionalProperties. The null value is returned if no additional
     * properties are allowed.
     */
    public static Schema getAdditionalProperties(Schema schema) {
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
            // Return an empty schema as the properties can take on any type per
            // the spec. See
            // https://github.com/OpenAPITools/openapi-generator/issues/9282 for
            // more details.
            return new Schema();
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
        if (name != null && openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getHeaders() != null) {
            return openAPI.getComponents().getHeaders().get(name);
        }
        return null;
    }

    public static Map<String, List<String>> getChildrenMap(OpenAPI openAPI) {
        Map<String, Schema> allSchemas = getSchemas(openAPI);

        Map<String, List<Entry<String, Schema>>> groupedByParent = allSchemas.entrySet().stream()
                .filter(entry -> isComposedSchema(entry.getValue()))
                .filter(entry -> getParentName((Schema) entry.getValue(), allSchemas) != null)
                .collect(Collectors.groupingBy(entry -> getParentName((Schema) entry.getValue(), allSchemas)));

        return groupedByParent.entrySet().stream()
                .collect(Collectors.toMap(entry -> entry.getKey(), entry -> entry.getValue().stream().map(e -> e.getKey()).collect(Collectors.toList())));
    }

    /**
     * Get the interfaces from the schema (composed)
     *
     * @param composed schema (alias or direct reference)
     * @return a list of schema defined in allOf, anyOf or oneOf
     */
    public static List<Schema> getInterfaces(Schema composed) {
        if (composed.getAllOf() != null && !composed.getAllOf().isEmpty()) {
            return composed.getAllOf();
        } else if (composed.getAnyOf() != null && !composed.getAnyOf().isEmpty()) {
            return composed.getAnyOf();
        } else if (composed.getOneOf() != null && !composed.getOneOf().isEmpty()) {
            return composed.getOneOf();
        } else {
            return Collections.emptyList();
        }
    }

    /**
     * Get the parent model name from the composed schema (allOf, anyOf, oneOf).
     * It traverses the OAS model (possibly resolving $ref) to determine schemas
     * that specify a determinator.
     * If there are multiple elements in the composed schema and it is not clear
     * which one should be the parent, return null.
     * <p>
     * For example, given the following OAS spec, the parent of 'Dog' is Animal
     * because 'Animal' specifies a discriminator.
     * <p>
     * animal:
     *   type: object
     *   discriminator:
     *     propertyName: type
     *   properties:
     *     type: string
     *
     * <p>
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
    public static String getParentName(Schema composedSchema, Map<String, Schema> allSchemas) {
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
                    } else if (hasOrInheritsDiscriminator(s, allSchemas, new ArrayList<Schema>())) {
                        // discriminator.propertyName is used or x-parent is used
                        return parentName;
                    } else {
                        // not a parent since discriminator.propertyName or x-parent is not set
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
    public static List<String> getAllParentsName(Schema composedSchema, Map<String, Schema> allSchemas, boolean includeAncestors) {
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
                    } else if (hasOrInheritsDiscriminator(s, allSchemas, new ArrayList<Schema>())) {
                        // discriminator.propertyName is used or x-parent is used
                        names.add(parentName);
                        if (includeAncestors && isComposedSchema(s)) {
                            names.addAll(getAllParentsName(s, allSchemas, true));
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

    private static boolean hasOrInheritsDiscriminator(Schema schema, Map<String, Schema> allSchemas, ArrayList<Schema> visitedSchemas) {
        for (Schema s : visitedSchemas) {
            if (s == schema) {
                return false;
            }
        }
        visitedSchemas.add(schema);

        if ((schema.getDiscriminator() != null && StringUtils.isNotEmpty(schema.getDiscriminator().getPropertyName()))
                || (isExtensionParent(schema))) { // x-parent is used
            return true;
        } else if (StringUtils.isNotEmpty(schema.get$ref())) {
            String parentName = getSimpleRef(schema.get$ref());
            Schema s = allSchemas.get(parentName);
            if (s != null) {
                return hasOrInheritsDiscriminator(s, allSchemas, visitedSchemas);
            } else {
                LOGGER.error("Failed to obtain schema from {}", parentName);
            }
        } else if (isComposedSchema(schema)) {
            final List<Schema> interfaces = getInterfaces(schema);
            for (Schema i : interfaces) {
                if (hasOrInheritsDiscriminator(i, allSchemas, visitedSchemas)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * If it's a boolean, returns the value of the extension `x-parent`.
     * If it's string, return true if it's non-empty.
     * If the return value is `true`, the schema is a parent.
     *
     * @param schema    Schema
     * @return boolean
     */
    public static boolean isExtensionParent(Schema schema) {
        if (schema == null || schema.getExtensions() == null) {
            return false;
        }

        Object xParent = schema.getExtensions().get("x-parent");
        if (xParent == null) {
            return false;
        } else if (xParent instanceof Boolean) {
            return (Boolean) xParent;
        } else if (xParent instanceof String) {
            return StringUtils.isNotEmpty((String) xParent);
        } else {
            return false;
        }
    }

    /**
     * Return true if the 'nullable' attribute is set to true in the schema, i.e. if the value
     * of the property can be the null value.
     * <p>
     * In addition, if the OAS document is 3.1 or above, isNullable returns true if the input
     * schema is a 'oneOf' composed document with at most two children, and one of the children
     * is the 'null' type.
     * <p>
     * The caller is responsible for resolving schema references before invoking isNullable.
     * If the input schema is a $ref and the referenced schema has 'nullable: true', this method
     * returns false (because the nullable attribute is defined in the referenced schema).
     * <p>
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
            return Boolean.parseBoolean(schema.getExtensions().get("x-nullable").toString());
        }
        // In OAS 3.1, the recommended way to define a nullable property or object is to use oneOf.
        if (isComposedSchema(schema)) {
            return isNullableComposedSchema(schema);
        }
        return false;
    }

    /**
     * Return true if the specified composed schema is 'oneOf', contains one or two elements,
     * and at least one of the elements is the 'null' type.
     * <p>
     * The 'null' type is supported in OAS 3.1 and above.
     * In the example below, the 'OptionalOrder' can have the null value because the 'null'
     * type is one of the elements under 'oneOf'.
     * <p>
     * OptionalOrder:
     *   oneOf:
     *     - type: 'null'
     *     - $ref: '#/components/schemas/Order'
     *
     * @param schema the OAS composed schema.
     * @return true if the composed schema is nullable.
     */
    public static boolean isNullableComposedSchema(Schema schema) {
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
     * <p>
     * The 'null' type is supported in OAS 3.1 and above. It is not supported
     * in OAS 2.0 and OAS 3.0.x.
     * <p>
     * For example, the "null" type could be used to specify that a value must
     * either be null or a specified type:
     * <p>
     * OptionalOrder:
     * oneOf:
     * - type: 'null'
     * - $ref: '#/components/schemas/Order'
     *
     * @param schema the OpenAPI schema
     * @return true if the schema is the 'null' type
     */
    public static boolean isNullType(Schema schema) {
        return "null".equals(schema.getType());
    }

    /**
     * For when a type is not defined on a schema
     * Note: properties, additionalProperties, enums, validations, items, and composed schemas (oneOf/anyOf/allOf)
     * can be defined or omitted on these any type schemas
     *
     * @param schema the schema that we are checking
     * @return boolean
     */
    public static boolean isAnyType(Schema schema) {
        // $ref is not a type, it is a keyword
        // TODO remove the ref check here, or pass in the spec version
        // openapi 3.1.0 specs allow ref to be adjacent to any keyword
        // openapi 3.0.3 and earlier do not allow adjacent keywords to refs
        return (schema.get$ref() == null && schema.getType() == null);
    }

    public static void syncValidationProperties(Schema schema, IJsonSchemaValidationProperties target) {
        // TODO move this method to IJsonSchemaValidationProperties
        if (schema == null ||
                target == null ||
                schema.get$ref() != null)
            return;
        SchemaValidations.ValidationSetBuilder vSB = new SchemaValidations.ValidationSetBuilder();

        Integer minItems = schema.getMinItems();
        if (minItems != null) vSB.withMinItems();

        Integer maxItems = schema.getMaxItems();
        if (maxItems != null) vSB.withMaxItems();

        Boolean uniqueItems = schema.getUniqueItems();
        if (uniqueItems != null) vSB.withUniqueItems();

        Integer minProperties = schema.getMinProperties();
        if (minProperties != null) vSB.withMinProperties();

        Integer maxProperties = schema.getMaxProperties();
        if (maxProperties != null) vSB.withMaxProperties();

        Integer minLength = schema.getMinLength();
        if (minLength != null) vSB.withMinLength();

        Integer maxLength = schema.getMaxLength();
        if (maxLength != null) vSB.withMaxLength();

        String pattern = schema.getPattern();
        if (pattern != null) vSB.withPattern();

        BigDecimal multipleOf = schema.getMultipleOf();
        if (multipleOf != null) vSB.withMultipleOf();

        BigDecimal minimum = schema.getMinimum();
        if (minimum != null) vSB.withMinimum();

        BigDecimal maximum = schema.getMaximum();
        if (maximum != null) vSB.withMaximum();

        Boolean exclusiveMinimum = schema.getExclusiveMinimum();
        if (exclusiveMinimum != null) vSB.withExclusiveMinimum();

        Boolean exclusiveMaximum = schema.getExclusiveMaximum();
        if (exclusiveMaximum != null) vSB.withExclusiveMaximum();

        LinkedHashSet<String> setValidations = vSB.build();

        if (isBooleanSchema(schema) || isNullType(schema)) {
            logWarnMessagesForIneffectiveValidations(setValidations, schema, new HashSet<>());
        } else if (isArraySchema(schema)) {
            if (minItems != null || maxItems != null || uniqueItems != null)
                setArrayValidations(minItems, maxItems, uniqueItems, target);
            logWarnMessagesForIneffectiveValidations(new LinkedHashSet(setValidations), schema, SchemaValidations.ARRAY_VALIDATIONS);
        } else if (isTypeObjectSchema(schema)) {
            if (minProperties != null || maxProperties != null)
                setObjectValidations(minProperties, maxProperties, target);
            logWarnMessagesForIneffectiveValidations(new LinkedHashSet(setValidations), schema, SchemaValidations.OBJECT_VALIDATIONS);
        } else if (isStringSchema(schema)) {
            if (minLength != null || maxLength != null || pattern != null)
                setStringValidations(minLength, maxLength, pattern, target);
            if (isDecimalSchema(schema)) {
                if (multipleOf != null || minimum != null || maximum != null || exclusiveMinimum != null || exclusiveMaximum != null)
                    setNumericValidations(schema, multipleOf, minimum, maximum, exclusiveMinimum, exclusiveMaximum, target);

                Set<String> stringAndNumericValidations = new HashSet<>(SchemaValidations.STRING_VALIDATIONS);
                stringAndNumericValidations.addAll(SchemaValidations.NUMERIC_VALIDATIONS);
                logWarnMessagesForIneffectiveValidations(new LinkedHashSet(setValidations), schema, stringAndNumericValidations);
            } else
                logWarnMessagesForIneffectiveValidations(new LinkedHashSet(setValidations), schema, SchemaValidations.STRING_VALIDATIONS);

        } else if (isNumberSchema(schema) || isIntegerSchema(schema)) {
            if (multipleOf != null || minimum != null || maximum != null || exclusiveMinimum != null || exclusiveMaximum != null)
                setNumericValidations(schema, multipleOf, minimum, maximum, exclusiveMinimum, exclusiveMaximum, target);
            logWarnMessagesForIneffectiveValidations(new LinkedHashSet(setValidations), schema, SchemaValidations.NUMERIC_VALIDATIONS);
        } else if (isAnyType(schema)) {
            // anyType can have any validations set on it
            setArrayValidations(minItems, maxItems, uniqueItems, target);
            setObjectValidations(minProperties, maxProperties, target);
            setStringValidations(minLength, maxLength, pattern, target);
            setNumericValidations(schema, multipleOf, minimum, maximum, exclusiveMinimum, exclusiveMaximum, target);
        }

        if (!setValidations.isEmpty())
            target.setHasValidation(true);
    }

    private static void setArrayValidations(Integer minItems, Integer maxItems, Boolean uniqueItems, IJsonSchemaValidationProperties target) {
        if (minItems != null) target.setMinItems(minItems);
        if (maxItems != null) target.setMaxItems(maxItems);
        if (uniqueItems != null) target.setUniqueItems(uniqueItems);
        if (uniqueItems != null) target.setUniqueItemsBoolean(uniqueItems);
    }

    private static void setObjectValidations(Integer minProperties, Integer maxProperties, IJsonSchemaValidationProperties target) {
        if (minProperties != null) target.setMinProperties(minProperties);
        if (maxProperties != null) target.setMaxProperties(maxProperties);
    }

    private static void setStringValidations(Integer minLength, Integer maxLength, String pattern, IJsonSchemaValidationProperties target) {
        if (minLength != null) target.setMinLength(minLength);
        if (maxLength != null) target.setMaxLength(maxLength);
        if (pattern != null) target.setPattern(pattern);
    }

    private static void setNumericValidations(Schema schema, BigDecimal multipleOf, BigDecimal minimum, BigDecimal maximum, Boolean exclusiveMinimum, Boolean exclusiveMaximum, IJsonSchemaValidationProperties target) {
        if (multipleOf != null) target.setMultipleOf(multipleOf);
        if (minimum != null) {
            if (isIntegerSchema(schema)) {
                target.setMinimum(String.valueOf(minimum.longValue()));
            } else {
                target.setMinimum(String.valueOf(minimum));
            }
            if (exclusiveMinimum != null) target.setExclusiveMinimum(exclusiveMinimum);
        }
        if (maximum != null) {
            if (isIntegerSchema(schema)) {
                target.setMaximum(String.valueOf(maximum.longValue()));
            } else {
                target.setMaximum(String.valueOf(maximum));
            }
            if (exclusiveMaximum != null) target.setExclusiveMaximum(exclusiveMaximum);
        }
    }

    private static void logWarnMessagesForIneffectiveValidations(Set<String> setValidations, Schema schema, Set<String> effectiveValidations) {
        setValidations.removeAll(effectiveValidations);
        setValidations.stream().forEach(validation -> {
            LOGGER.warn("Validation '" + validation + "' has no effect on schema '"+ schema.getType() +"'. Ignoring!");
        });
    }

    private static ObjectMapper getRightMapper(String data) {
        ObjectMapper mapper;
        if (data.trim().startsWith("{")) {
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
     * @param auths    the list of authorization values to access the remote URL.
     * @return A JsonNode representation of the input OAS document.
     * @throws java.lang.Exception if an error occurs while retrieving the OpenAPI document.
     */
    public static JsonNode readWithInfo(String location, List<AuthorizationValue> auths) throws Exception {
        String data;
        location = location.replaceAll("\\\\", "/");
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
     * <p>
     * For OAS 2.0 documents, return the value of the 'swagger' attribute.
     * For OAS 3.x documents, return the value of the 'openapi' attribute.
     *
     * @param openAPI  the object that encapsulates the OAS document.
     * @param location the URL of the OAS document.
     * @param auths    the list of authorization values to access the remote URL.
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

    /**
     * Returns true if the schema contains allOf but
     * no properties/oneOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if the schema contains allOf but no properties/oneOf/anyOf defined.
     */
    public static boolean isAllOf(Schema schema) {
        if (hasAllOf(schema) && (schema.getProperties() == null || schema.getProperties().isEmpty()) &&
                (schema.getOneOf() == null || schema.getOneOf().isEmpty()) &&
                (schema.getAnyOf() == null || schema.getAnyOf().isEmpty())) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema contains allOf and may or may not have
     * properties/oneOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if allOf is not empty
     */
    public static boolean hasAllOf(Schema schema) {
        if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema contains allOf and properties,
     * and no oneOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if the schema contains allOf but no properties/oneOf/anyOf defined.
     */
    public static boolean isAllOfWithProperties(Schema schema) {
        return hasAllOf(schema) && (schema.getProperties() != null && !schema.getProperties().isEmpty()) &&
                (schema.getOneOf() == null || schema.getOneOf().isEmpty()) &&
                (schema.getAnyOf() == null || schema.getAnyOf().isEmpty());
    }

    /**
     * Returns true if the schema contains oneOf but
     * no properties/allOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if the schema contains oneOf but no properties/allOf/anyOf defined.
     */
    public static boolean isOneOf(Schema schema) {
        if (schema == null) {
            return false;
        }

        if (hasOneOf(schema) && (schema.getProperties() == null || schema.getProperties().isEmpty()) &&
                (schema.getAllOf() == null || schema.getAllOf().isEmpty()) &&
                (schema.getAnyOf() == null || schema.getAnyOf().isEmpty())) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema contains oneOf and may or may not have
     * properties/allOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if allOf is not empty
     */
    public static boolean hasOneOf(Schema schema) {
        if (schema != null && schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema contains anyOf but
     * no properties/allOf/anyOf defined.
     *
     * @param schema the schema
     * @return true if the schema contains oneOf but no properties/allOf/anyOf defined.
     */
    public static boolean isAnyOf(Schema schema) {
        if (schema == null) {
            return false;
        }

        if (hasAnyOf(schema) && (schema.getProperties() == null || schema.getProperties().isEmpty()) &&
                (schema.getAllOf() == null || schema.getAllOf().isEmpty()) &&
                (schema.getOneOf() == null || schema.getOneOf().isEmpty())) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema contains anyOf and may or may not have
     * properties/allOf/oneOf defined.
     *
     * @param schema the schema
     * @return true if anyOf is not empty
     */
    public static boolean hasAnyOf(Schema schema) {
        if (schema != null && schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if any of the common attributes of the schema (e.g. readOnly, default, maximum, etc) is defined.
     *
     * @param schema the schema
     * @return true if allOf is not empty
     */
    public static boolean hasCommonAttributesDefined(Schema schema) {
        if (schema.getNullable() != null || schema.getDefault() != null ||
                schema.getMinimum() != null || schema.getMaximum() != null ||
                schema.getExclusiveMaximum() != null || schema.getExclusiveMinimum() != null ||
                schema.getMinLength() != null || schema.getMaxLength() != null ||
                schema.getMinItems() != null || schema.getMaxItems() != null ||
                schema.getReadOnly() != null || schema.getWriteOnly() != null ||
                schema.getPattern() != null) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if the schema is a parent (with discriminator).
     *
     * @param schema the schema.
     *
     * @return true if the schema is a parent.
     */
    public static boolean isParent(Schema schema) {
        if (schema != null && schema.getDiscriminator() != null) {
            return true;
        }

        // if x-parent is set
        if (isExtensionParent(schema)) {
            return true;
        }

        return false;
    }

    @FunctionalInterface
    private interface OpenAPISchemaVisitor {

        void visit(Schema schema, String mimeType);
    }

    private static final class SchemaValidations {

        public static Set<String> ARRAY_VALIDATIONS = new ValidationSetBuilder()
                .withMinItems()
                .withMaxItems()
                .withUniqueItems()
                .build();
        public static Set<String> OBJECT_VALIDATIONS = new ValidationSetBuilder()
                .withMinProperties()
                .withMaxProperties()
                .build();
        public static Set<String> STRING_VALIDATIONS = new ValidationSetBuilder()
                .withMinLength()
                .withMaxLength()
                .withPattern()
                .build();
        public static Set<String> NUMERIC_VALIDATIONS = new ValidationSetBuilder()
                .withMultipleOf()
                .withMinimum()
                .withMaximum()
                .withExclusiveMinimum()
                .withExclusiveMaximum()
                .build();

        public static Set<String> ALL_VALIDATIONS;

        static {
            ALL_VALIDATIONS = new HashSet<>(ARRAY_VALIDATIONS);
            ALL_VALIDATIONS.addAll(OBJECT_VALIDATIONS);
            ALL_VALIDATIONS.addAll(STRING_VALIDATIONS);
            ALL_VALIDATIONS.addAll(NUMERIC_VALIDATIONS);
        }

        SchemaValidations() {
        }


        public static class ValidationSetBuilder {
            LinkedHashSet<String> validationSet;

            ValidationSetBuilder() {
                this.validationSet = new LinkedHashSet<String>();
            }

            public ValidationSetBuilder withMinItems() {
                this.validationSet.add("minItems");
                return this;
            }

            public ValidationSetBuilder withMaxItems() {
                this.validationSet.add("maxItems");
                return this;
            }

            public ValidationSetBuilder withUniqueItems() {
                this.validationSet.add("uniqueItems");
                return this;
            }

            public ValidationSetBuilder withMinProperties() {
                this.validationSet.add("minProperties");
                return this;
            }

            public ValidationSetBuilder withMaxProperties() {
                this.validationSet.add("maxProperties");
                return this;
            }

            public ValidationSetBuilder withMinLength() {
                this.validationSet.add("minLength");
                return this;
            }

            public ValidationSetBuilder withMaxLength() {
                this.validationSet.add("maxLength");
                return this;
            }

            public ValidationSetBuilder withPattern() {
                this.validationSet.add("pattern");
                return this;
            }

            public ValidationSetBuilder withMultipleOf() {
                this.validationSet.add("multipleOf");
                return this;
            }

            public ValidationSetBuilder withMinimum() {
                this.validationSet.add("minimum");
                return this;
            }

            public ValidationSetBuilder withMaximum() {
                this.validationSet.add("maximum");
                return this;
            }

            public ValidationSetBuilder withExclusiveMinimum() {
                this.validationSet.add("exclusiveMinimum");
                return this;
            }

            public ValidationSetBuilder withExclusiveMaximum() {
                this.validationSet.add("exclusiveMaximum");
                return this;
            }

            public LinkedHashSet<String> build() {
                return this.validationSet;
            }
        }
    }
}
