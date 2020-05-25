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

package org.openapitools.codegen;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

public class InlineModelResolver {
    private OpenAPI openapi;
    private Map<String, Schema> addedModels = new HashMap<String, Schema>();
    private Map<String, String> generatedSignature = new HashMap<String, String>();
    public boolean resolveInlineEnums = false;
    static Logger LOGGER = LoggerFactory.getLogger(InlineModelResolver.class);

    void flatten(OpenAPI openapi) {
        this.openapi = openapi;

        if (openapi.getComponents() == null) {
            openapi.setComponents(new Components());
        }

        if (openapi.getComponents().getSchemas() == null) {
            openapi.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        flattenPaths(openapi);
        flattenComponents(openapi);
    }

    /**
     * Flatten inline models in Paths
     *
     * @param openAPI target spec
     */
    private void flattenPaths(OpenAPI openAPI) {
        Paths paths = openAPI.getPaths();
        if (paths == null) {
            return;
        }

        for (String pathname : paths.keySet()) {
            PathItem path = paths.get(pathname);
            List<Operation> operations = new ArrayList<>(path.readOperations());

            // Include callback operation as well
            for (Operation operation : path.readOperations()) {
                Map<String, Callback> callbacks = operation.getCallbacks();
                if (callbacks != null) {
                    operations.addAll(callbacks.values().stream()
                            .flatMap(callback -> callback.values().stream())
                            .flatMap(pathItem -> pathItem.readOperations().stream())
                            .collect(Collectors.toList()));
                }
            }

            for (Operation operation : operations) {
                flattenRequestBody(openAPI, pathname, operation);
                flattenParameters(openAPI, pathname, operation);
                flattenResponses(openAPI, pathname, operation);
            }
        }
    }

    /**
     * Return false if model can be represented by primitives e.g. string, object 
     * without properties, array or map of other model (model contanier), etc.
     *
     * Return true if a model should be generated e.g. object with properties,
     * enum, oneOf, allOf, anyOf, etc.
     *
     * @param schema target schema
     */
    private boolean isModelNeeded(Schema schema) {
        if (resolveInlineEnums && schema.getEnum() != null && schema.getEnum().size() > 0) {
            return true;
        }
        if (schema.getType() == null || "object".equals(schema.getType())) {
            // object or undeclared type with properties
            if (schema.getProperties() != null && schema.getProperties().size() > 0) {
                return true;
            }
        }
        if (schema instanceof ComposedSchema) {
            // allOf, anyOf, oneOf
            ComposedSchema m = (ComposedSchema) schema;
            if (m.getAllOf() != null && !m.getAllOf().isEmpty()) {
                return true;
            }
            if (m.getAnyOf() != null && !m.getAnyOf().isEmpty()) {
                return true;
            }
            if (m.getOneOf() != null && !m.getOneOf().isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Recursively gather inline models that need to be generated and
     * replace inline schemas with $ref to schema to-be-generated.
     *
     * @param schema target schema
     */
    private void gatherInlineModels(Schema schema, String modelPrefix) {
        if (schema.get$ref() != null) {
            // if ref already, no inline schemas should be present but check for
            // any to catch OpenAPI violations
            if (isModelNeeded(schema) || "object".equals(schema.getType()) ||
                    schema.getProperties() != null || schema.getAdditionalProperties() != null ||
                    schema instanceof ComposedSchema) {
                LOGGER.error("Illegal schema found with $ref combined with other properties," +
                        " no properties should be defined alongside a $ref:\n " + schema.toString());
            }
            return;
        }
        if (schema.getType() == null || "object".equals(schema.getType())) {
            // Check properties and recurse, each property could be its own inline model
            Map<String, Schema> props = schema.getProperties();
            if (props != null) {
                for (String propName : props.keySet()) {
                    Schema prop = props.get(propName);
                    // Recurse to create $refs for inner models
                    gatherInlineModels(prop, modelPrefix + StringUtils.camelize(propName));
                    if (isModelNeeded(prop)) {
                        // If this schema should be split into its own model, do so
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, StringUtils.camelize(propName), prop);
                        props.put(propName, refSchema);
                    }
                }
            }
            // Check additionalProperties for inline models
            if (schema.getAdditionalProperties() != null) {
                if (schema.getAdditionalProperties() instanceof Schema) {
                    Schema inner = (Schema) schema.getAdditionalProperties();
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "AddlProps");
                    if (isModelNeeded(inner)) {
                        // If this schema should be split into its own model, do so
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "AddlProps", inner);
                        schema.setAdditionalProperties(refSchema);
                    }
                }
            }
        } else if (schema.getProperties() != null) {
            // If non-object type is specified but also properties
            LOGGER.error("Illegal schema found with non-object type combined with properties," +
                    " no properties should be defined:\n " + schema.toString());
            return;
        } else if (schema.getAdditionalProperties() != null) {
            // If non-object type is specified but also additionalProperties
            LOGGER.error("Illegal schema found with non-object type combined with" +
                    " additionalProperties, no additionalProperties should be defined:\n " +
                    schema.toString());
            return;
        }
        // Check array items
        if (schema instanceof ArraySchema) {
            ArraySchema array = (ArraySchema) schema;
            Schema items = array.getItems();
            if (items == null) {
                LOGGER.error("Illegal schema found with array type but no items," +
                        " items must be defined for array schemas:\n " + schema.toString());
                return;
            }
            // Recurse to create $refs for inner models
            gatherInlineModels(items, modelPrefix + "Items");
            if (isModelNeeded(items)) {
                // If this schema should be split into its own model, do so
                Schema refSchema = this.makeSchemaResolve(modelPrefix, "Items", items);
                array.setItems(refSchema);
            }
        }
        // Check allOf, anyOf, oneOf for inline models
        if (schema instanceof ComposedSchema) {
            ComposedSchema m = (ComposedSchema) schema;
            if (m.getAllOf() != null) {
                List<Schema> newAllOf = new ArrayList<Schema>();
                for (Schema inner : m.getAllOf()) {
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "AllOf");
                    if (isModelNeeded(inner)) {
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_allOf", inner);
                        newAllOf.add(refSchema); // replace with ref
                    } else {
                        newAllOf.add(inner);
                    }
                }
                m.setAllOf(newAllOf);
            }
            if (m.getAnyOf() != null) {
                List<Schema> newAnyOf = new ArrayList<Schema>();
                for (Schema inner : m.getAnyOf()) {
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "AnyOf");
                    if (isModelNeeded(inner)) {
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_anyOf", inner);
                        newAnyOf.add(refSchema); // replace with ref
                    } else {
                        newAnyOf.add(inner);
                    }
                }
                m.setAnyOf(newAnyOf);
            }
            if (m.getOneOf() != null) {
                List<Schema> newOneOf = new ArrayList<Schema>();
                for (Schema inner : m.getOneOf()) {
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "OneOf");
                    if (isModelNeeded(inner)) {
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_oneOf", inner);
                        newOneOf.add(refSchema); // replace with ref
                    } else {
                        newOneOf.add(inner);
                    }
                }
                m.setOneOf(newOneOf);
            }
        }
        // Check not schema
        if (schema.getNot() != null) {
            Schema not = schema.getNot();
            // Recurse to create $refs for inner models
            gatherInlineModels(not, modelPrefix + "Not");
            if (isModelNeeded(not)) {
                Schema refSchema = this.makeSchemaResolve(modelPrefix, "Not", not);
                schema.setNot(refSchema);
            }
        }
    }

    /**
     * Flatten inline models in content
     *
     * @param content target content
     * @param name backup name if no title is found
     */
    private void flattenContent(Content content, String name) {
        if (content == null || content.isEmpty()) {
            return;
        }

        for (String contentType : content.keySet()) {
            MediaType mediaType = content.get(contentType);
            if (mediaType == null) {
                continue;
            }
            Schema schema = mediaType.getSchema();
            if (schema == null) {
                continue;
            }
            String schemaName = resolveModelName(schema.getTitle(), name);
            // Recursively gather/make inline models within this schema if any
            gatherInlineModels(schema, schemaName);
            if (isModelNeeded(schema)) {
                // If this schema should be split into its own model, do so
                Schema refSchema = this.makeSchema(schemaName, schema);
                mediaType.setSchema(refSchema);
            }
        }
    }

    /**
     * Flatten inline models in RequestBody
     *
     * @param openAPI target spec
     * @param pathname target pathname
     * @param operation target operation
     */
    private void flattenRequestBody(OpenAPI openAPI, String pathname, Operation operation) {
        RequestBody requestBody = operation.getRequestBody();
        if (requestBody == null) {
            return;
        }
        String name = operation.getOperationId() == null ? "InlineBody" : operation.getOperationId() + "Body";
        flattenContent(requestBody.getContent(), name);
    }

    /**
     * Flatten inline models in parameters
     *
     * @param openAPI target spec
     * @param pathname target pathname
     * @param operation target operation
     */
    private void flattenParameters(OpenAPI openAPI, String pathname, Operation operation) {
        List<Parameter> parameters = operation.getParameters();
        if (parameters == null) {
            return;
        }

        for (Parameter parameter : parameters) {
            if (parameter.getSchema() == null) {
                continue;
            }

            Schema schema = parameter.getSchema();
            if (schema == null) {
                continue;
            }
            String schemaName = resolveModelName(schema.getTitle(), parameter.getName());
            // Recursively gather/make inline models within this schema if any
            gatherInlineModels(schema, schemaName);
            if (isModelNeeded(schema)) {
                // If this schema should be split into its own model, do so
                Schema refSchema = this.makeSchema(schemaName, schema);
                parameter.setSchema(refSchema);
            }
        }
    }

    /**
     * Flatten inline models in ApiResponses
     *
     * @param openAPI target spec
     * @param pathname target pathname
     * @param operation target operation
     */
    private void flattenResponses(OpenAPI openAPI, String pathname, Operation operation) {
        ApiResponses responses = operation.getResponses();
        if (responses == null) {
            return;
        }

        for (String key : responses.keySet()) {
            ApiResponse response = responses.get(key);
            String prefix = operation.getOperationId() == null ? "Inline" : operation.getOperationId();
            String name;
            if ("200".equals(key)) {
                name = prefix + "Response";
            } else {
                name = prefix + "Response" + StringUtils.camelize(key);
            }
            flattenContent(response.getContent(), name);
        }
    }

    /**
     * Flatten inline models in components
     *
     * @param openAPI target spec
     */
    private void flattenComponents(OpenAPI openAPI) {
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        if (schemas == null) {
            return;
        }

        List<String> schemaNames = new ArrayList<String>(schemas.keySet());
        for (String schemaName : schemaNames) {
            Schema schema = schemas.get(schemaName);
            // Recursively gather/make inline models within this schema if any
            gatherInlineModels(schema, schemaName);
        }
    }

    /**
     * Generates a unique model name. Non-alphanumeric characters will be replaced
     * with underscores
     *
     * e.g. io.schema.User_name => io_schema_User_name
     *
     * @param title String title field in the schema if present
     * @param key String model name
     *
     * @return if provided the sanitized {@code title}, else the sanitized {@code key}
     */
    private String resolveModelName(String title, String key) {
        if (title == null) {
            if (key == null) {
                LOGGER.warn("Found an inline schema without the `title` attribute. Default the model name to InlineObject instead. To have better control of the model naming, define the model separately so that it can be reused throughout the spec.");
                return uniqueName("InlineObject");
            }
            return uniqueName(sanitizeName(key));
        } else {
            return uniqueName(sanitizeName(title));
        }
    }

    private String matchGenerated(Schema model) {
        String json = Json.pretty(model);
        if (generatedSignature.containsKey(json)) {
            return generatedSignature.get(json);
        }
        return null;
    }

    private void addGenerated(String name, Schema model) {
        generatedSignature.put(Json.pretty(model), name);
    }

    /**
     * Sanitizes the input so that it's valid name for a class or interface
     *
     * e.g. 12.schema.User name => _2_schema_User_name
     */
    private String sanitizeName(final String name) {
        return name
                .replaceAll("^[0-9]", "_") // e.g. 12object => _2object
                .replaceAll("[^A-Za-z0-9]", "_"); // e.g. io.schema.User name => io_schema_User_name
    }

    private String uniqueName(final String name) {
        if (openapi.getComponents().getSchemas() == null) {
            return name;
        }

        String uniqueName = name;
        int count = 0;
        while (true) {
            if (!openapi.getComponents().getSchemas().containsKey(uniqueName)) {
                return uniqueName;
            }
            uniqueName = name + "_" + ++count;
        }
        // TODO it would probably be a good idea to check against a list of used uniqueNames to make sure there are no collisions
    }

    /**
     * Resolve namespace conflicts using:
     * title (if title exists) or
     * prefix + suffix (if title not specified)
     * @param prefix used to form name if no title found in schema
     * @param suffix used to form name if no title found in schema
     * @param schema title property used to form name if exists and schema definition used
     *   to create new schema if doesn't exist
     * @return a new schema or $ref to an existing one if it was already created
     */
    private Schema makeSchemaResolve(String prefix, String suffix, Schema schema) {
        if (schema.getTitle() == null) {
            return makeSchema(uniqueName(sanitizeName(prefix + suffix)), schema);
        }
        return makeSchema(uniqueName(sanitizeName(schema.getTitle())), schema);
    }

    /**
     * Move schema to components (if new) and return $ref to schema or
     * existing schema.
     *
     * @param name   new schema name
     * @param schema schema to move to components or find existing ref
     * @return {@link Schema} $ref schema to new or existing schema
     */
    private Schema makeSchema(String name, Schema schema) {
        String existing = matchGenerated(schema);
        Schema refSchema;
        if (existing != null) {
            refSchema = new Schema().$ref(existing);
        } else {
            if (resolveInlineEnums && schema.getEnum() != null && schema.getEnum().size() > 0) {
                LOGGER.warn("Model " + name + " promoted to its own schema due to resolveInlineEnums=true");
            }
            refSchema = new Schema().$ref(name);
            addGenerated(name, schema);
            openapi.getComponents().addSchemas(name, schema);
        }
        this.copyVendorExtensions(schema, refSchema);
        return refSchema;
    }

    /**
     * Copy vendor extensions from Model to another Model
     *
     * @param source source property
     * @param target target property
     */

    private void copyVendorExtensions(Schema source, Schema target) {
        Map<String, Object> vendorExtensions = source.getExtensions();
        if (vendorExtensions == null) {
            return;
        }
        for (String extName : vendorExtensions.keySet()) {
            target.addExtension(extName, vendorExtensions.get(extName));
        }
    }
}