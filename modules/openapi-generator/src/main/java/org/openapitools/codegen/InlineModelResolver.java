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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

public class InlineModelResolver {
    private OpenAPI openapi;
    private Map<String, Schema> addedModels = new HashMap<String, Schema>();
    private Map<String, String> generatedSignature = new HashMap<String, String>();

    // structure mapper sorts properties alphabetically on write to ensure models are
    // serialized consistently for lookup of existing models
    private static ObjectMapper structureMapper;

    static {
        structureMapper = Json.mapper().copy();
        structureMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
        structureMapper.writer(new DefaultPrettyPrinter());
    }

     final Logger LOGGER = LoggerFactory.getLogger(InlineModelResolver.class);

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

        for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
            String pathname = pathsEntry.getKey();
            PathItem path = pathsEntry.getValue();
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

        Schema model = ModelUtils.getSchemaFromRequestBody(requestBody);
        if (model instanceof ObjectSchema) {
            Schema obj = model;
            if (obj.getType() == null || "object".equals(obj.getType())) {
                if (obj.getProperties() != null && obj.getProperties().size() > 0) {
                    flattenProperties(openAPI, obj.getProperties(), pathname);
                    // for model name, use "title" if defined, otherwise default to 'inline_object'
                    String modelName = resolveModelName(obj.getTitle(), "inline_object");
                    addGenerated(modelName, model);
                    openAPI.getComponents().addSchemas(modelName, model);

                    // create request body
                    RequestBody rb = new RequestBody();
                    rb.setRequired(requestBody.getRequired());
                    Content content = new Content();
                    MediaType mt = new MediaType();
                    Schema schema = new Schema();
                    schema.set$ref(modelName);
                    mt.setSchema(schema);

                    // get "consumes", e.g. application/xml, application/json
                    Set<String> consumes;
                    if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
                        consumes = new HashSet<>();
                        consumes.add("application/json"); // default to application/json
                        LOGGER.info("Default to application/json for inline body schema");
                    } else {
                        consumes = requestBody.getContent().keySet();
                    }

                    for (String consume : consumes) {
                        content.addMediaType(consume, mt);
                    }

                    rb.setContent(content);

                    // add to openapi "components"
                    if (openAPI.getComponents().getRequestBodies() == null) {
                        Map<String, RequestBody> requestBodies = new HashMap<String, RequestBody>();
                        requestBodies.put(modelName, rb);
                        openAPI.getComponents().setRequestBodies(requestBodies);
                    } else {
                        openAPI.getComponents().getRequestBodies().put(modelName, rb);
                    }

                    // update requestBody to use $ref instead of inline def
                    requestBody.set$ref(modelName);

                }
            }
        } else if (model instanceof ArraySchema) {
            ArraySchema am = (ArraySchema) model;
            Schema inner = am.getItems();
            if (inner instanceof ObjectSchema) {
                ObjectSchema op = (ObjectSchema) inner;
                if (op.getProperties() != null && op.getProperties().size() > 0) {
                    flattenProperties(openAPI, op.getProperties(), pathname);
                    // Generate a unique model name based on the title.
                    String modelName = resolveModelName(op.getTitle(), null);
                    Schema innerModel = modelFromProperty(openAPI, op, modelName);
                    String existing = matchGenerated(innerModel);
                    if (existing != null) {
                        Schema schema = new Schema().$ref(existing);
                        schema.setRequired(op.getRequired());
                        am.setItems(schema);
                    } else {
                        Schema schema = new Schema().$ref(modelName);
                        schema.setRequired(op.getRequired());
                        am.setItems(schema);
                        addGenerated(modelName, innerModel);
                        openAPI.getComponents().addSchemas(modelName, innerModel);
                    }
                }
            }
        }
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

            Schema model = parameter.getSchema();
            if (model instanceof ObjectSchema) {
                Schema obj = model;
                if (obj.getType() == null || "object".equals(obj.getType())) {
                    if (obj.getProperties() != null && obj.getProperties().size() > 0) {
                        flattenProperties(openAPI, obj.getProperties(), pathname);
                        String modelName = resolveModelName(obj.getTitle(), parameter.getName());

                        parameter.$ref(modelName);
                        addGenerated(modelName, model);
                        openAPI.getComponents().addSchemas(modelName, model);
                    }
                }
            } else if (model instanceof ArraySchema) {
                ArraySchema am = (ArraySchema) model;
                Schema inner = am.getItems();
                if (inner instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) inner;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        flattenProperties(openAPI, op.getProperties(), pathname);
                        String modelName = resolveModelName(op.getTitle(), parameter.getName());
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            am.setItems(schema);
                        } else {
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            am.setItems(schema);
                            addGenerated(modelName, innerModel);
                            openAPI.getComponents().addSchemas(modelName, innerModel);
                        }
                    }
                }
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

        for (Map.Entry<String, ApiResponse> responsesEntry : responses.entrySet()) {
            String key = responsesEntry.getKey();
            ApiResponse response = responsesEntry.getValue();
            if (ModelUtils.getSchemaFromResponse(response) == null) {
                continue;
            }

            Schema property = ModelUtils.getSchemaFromResponse(response);
            if (property instanceof ObjectSchema) {
                ObjectSchema op = (ObjectSchema) property;
                if (op.getProperties() != null && op.getProperties().size() > 0) {
                    String modelName = resolveModelName(op.getTitle(), "inline_response_" + key);
                    Schema model = modelFromProperty(openAPI, op, modelName);
                    String existing = matchGenerated(model);
                    Content content = response.getContent();
                    for (MediaType mediaType : content.values()) {
                        if (existing != null) {
                            Schema schema = this.makeSchema(existing, property);
                            schema.setRequired(op.getRequired());
                            mediaType.setSchema(schema);
                        } else {
                            Schema schema = this.makeSchema(modelName, property);
                            schema.setRequired(op.getRequired());
                            mediaType.setSchema(schema);
                            addGenerated(modelName, model);
                            openAPI.getComponents().addSchemas(modelName, model);
                        }
                    }
                }
            } else if (property instanceof ArraySchema) {
                ArraySchema ap = (ArraySchema) property;
                Schema inner = ap.getItems();
                if (inner instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) inner;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        flattenProperties(openAPI, op.getProperties(), pathname);
                        String modelName = resolveModelName(op.getTitle(),
                                "inline_response_" + key);
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = this.makeSchema(existing, op);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
                        } else {
                            Schema schema = this.makeSchema(modelName, op);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
                            addGenerated(modelName, innerModel);
                            openAPI.getComponents().addSchemas(modelName, innerModel);
                        }
                    }
                }
            } else if (property instanceof MapSchema) {
                MapSchema mp = (MapSchema) property;
                Schema innerProperty = ModelUtils.getAdditionalProperties(openAPI, mp);
                if (innerProperty instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) innerProperty;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        flattenProperties(openAPI, op.getProperties(), pathname);
                        String modelName = resolveModelName(op.getTitle(),
                                "inline_response_" + key);
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            mp.setAdditionalProperties(schema);
                        } else {
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            mp.setAdditionalProperties(schema);
                            addGenerated(modelName, innerModel);
                            openAPI.getComponents().addSchemas(modelName, innerModel);
                        }
                    }
                }
            }
        }
    }

    /**
     * Flattens properties of inline object schemas that belong to a composed schema into a
     * single flat list of properties. This is useful to generate a single or multiple
     * inheritance model.
     *
     * In the example below, codegen may generate a 'Dog' class that extends from the
     * generated 'Animal' class. 'Dog' has additional properties 'name', 'age' and 'breed' that
     * are flattened as a single list of properties.
     *
     * Dog:
     *   allOf:
     *     - $ref: '#/components/schemas/Animal'
     *     - type: object
     *       properties:
     *         name:
     *           type: string
     *         age:
     *           type: string
     *     - type: object
     *       properties:
     *         breed:
     *           type: string
     *
     * @param openAPI the OpenAPI document
     * @param key a unique name ofr the composed schema.
     * @param children the list of nested schemas within a composed schema (allOf, anyOf, oneOf).
     */
    private void flattenComposedChildren(OpenAPI openAPI, String key, List<Schema> children) {
        if (children == null || children.isEmpty()) {
            return;
        }
        ListIterator<Schema> listIterator = children.listIterator();
        while (listIterator.hasNext()) {
            Schema component = listIterator.next();
            if ((component != null) &&
                (component.get$ref() == null) &&
                ((component.getProperties() != null && !component.getProperties().isEmpty()) ||
                 (component.getEnum() != null && !component.getEnum().isEmpty()))) {
                // If a `title` attribute is defined in the inline schema, codegen uses it to name the
                // inline schema. Otherwise, we'll use the default naming such as InlineObject1, etc.
                // We know that this is not the best way to name the model.
                //
                // Such naming strategy may result in issues. If the value of the 'title' attribute
                // happens to match a schema defined elsewhere in the specification, 'innerModelName'
                // will be the same as that other schema.
                //
                // To have complete control of the model naming, one can define the model separately
                // instead of inline.
                String innerModelName = resolveModelName(component.getTitle(), key);
                Schema innerModel = modelFromProperty(openAPI, component, innerModelName);
                String existing = matchGenerated(innerModel);
                if (existing == null) {
                    openAPI.getComponents().addSchemas(innerModelName, innerModel);
                    addGenerated(innerModelName, innerModel);
                    Schema schema = new Schema().$ref(innerModelName);
                    schema.setRequired(component.getRequired());
                    listIterator.set(schema);
                } else {
                    Schema schema = new Schema().$ref(existing);
                    schema.setRequired(component.getRequired());
                    listIterator.set(schema);
                }
            }
        }
    }

    /**
     * Flatten inline models in components
     *
     * @param openAPI target spec
     */
    private void flattenComponents(OpenAPI openAPI) {
        Map<String, Schema> models = openAPI.getComponents().getSchemas();
        if (models == null) {
            return;
        }

        List<String> modelNames = new ArrayList<String>(models.keySet());
        for (String modelName : modelNames) {
            Schema model = models.get(modelName);
            if (ModelUtils.isComposedSchema(model)) {
                ComposedSchema m = (ComposedSchema) model;
                // inline child schemas
                flattenComposedChildren(openAPI, modelName + "_allOf", m.getAllOf());
                flattenComposedChildren(openAPI, modelName + "_anyOf", m.getAnyOf());
                flattenComposedChildren(openAPI, modelName + "_oneOf", m.getOneOf());
            } else if (model instanceof Schema) {
                Schema m = model;
                Map<String, Schema> properties = m.getProperties();
                flattenProperties(openAPI, properties, modelName);
                fixStringModel(m);
            } else if (ModelUtils.isArraySchema(model)) {
                ArraySchema m = (ArraySchema) model;
                Schema inner = m.getItems();
                if (inner instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) inner;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        String innerModelName = resolveModelName(op.getTitle(), modelName + "_inner");
                        Schema innerModel = modelFromProperty(openAPI, op, innerModelName);
                        String existing = matchGenerated(innerModel);
                        if (existing == null) {
                            openAPI.getComponents().addSchemas(innerModelName, innerModel);
                            addGenerated(innerModelName, innerModel);
                            Schema schema = new Schema().$ref(innerModelName);
                            schema.setRequired(op.getRequired());
                            m.setItems(schema);
                        } else {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            m.setItems(schema);
                        }
                    }
                }
            }
        }
    }

    /**
     * This function fix models that are string (mostly enum). Before this fix, the
     * example would look something like that in the doc: "\"example from def\""
     *
     * @param m Schema implementation
     */
    private void fixStringModel(Schema m) {
        if (schemaIsOfType(m, "string") && schemaContainsExample(m)) {
            String example = m.getExample().toString();
            if (example.startsWith("\"") && example.endsWith("\"")) {
                m.setExample(example.substring(1, example.length() - 1));
            }
        }
    }

    private boolean schemaIsOfType(Schema m, String type) {
        return m.getType() != null && m.getType().equals(type);
    }

    private boolean schemaContainsExample(Schema m) {
        return m.getExample() != null && m.getExample() != "";
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
        try {
            String json = structureMapper.writeValueAsString(model);
            if (generatedSignature.containsKey(json)) {
                return generatedSignature.get(json);
            }
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        return null;
    }

    private void addGenerated(String name, Schema model) {
        try {
            String json = structureMapper.writeValueAsString(model);
            generatedSignature.put(json, name);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
    }

    /**
     * Sanitizes the input so that it's valid name for a class or interface
     *
     * e.g. 12.schema.User name => _2_schema_User_name
     */
    private String sanitizeName(final String name) {
        return name
            .replaceAll("^[0-9]", "_$0") // e.g. 12object => _12object
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

    private void flattenProperties(OpenAPI openAPI, Map<String, Schema> properties, String path) {
        if (properties == null) {
            return;
        }
        Map<String, Schema> propsToUpdate = new HashMap<String, Schema>();
        Map<String, Schema> modelsToAdd = new HashMap<String, Schema>();
        for (Map.Entry<String, Schema> propertiesEntry : properties.entrySet()) {
            String key = propertiesEntry.getKey();
            Schema property = propertiesEntry.getValue();
            if (property instanceof ObjectSchema && ((ObjectSchema) property).getProperties() != null
                    && ((ObjectSchema) property).getProperties().size() > 0) {
                ObjectSchema op = (ObjectSchema) property;
                String modelName = resolveModelName(op.getTitle(), path + "_" + key);
                Schema model = modelFromProperty(openAPI, op, modelName);
                String existing = matchGenerated(model);
                if (existing != null) {
                    Schema schema = new Schema().$ref(existing);
                    schema.setRequired(op.getRequired());
                    propsToUpdate.put(key, schema);
                } else {
                    Schema schema = new Schema().$ref(modelName);
                    schema.setRequired(op.getRequired());
                    propsToUpdate.put(key, schema);
                    modelsToAdd.put(modelName, model);
                    addGenerated(modelName, model);
                    openapi.getComponents().addSchemas(modelName, model);
                }
            } else if (property instanceof ArraySchema) {
                ArraySchema ap = (ArraySchema) property;
                Schema inner = ap.getItems();
                if (inner instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) inner;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        flattenProperties(openAPI, op.getProperties(), path);
                        String modelName = resolveModelName(op.getTitle(), path + "_" + key);
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
                        } else {
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
                            addGenerated(modelName, innerModel);
                            openapi.getComponents().addSchemas(modelName, innerModel);
                        }
                    }
                }
            }
            if (ModelUtils.isMapSchema(property)) {
                Schema inner = ModelUtils.getAdditionalProperties(openAPI, property);
                if (inner instanceof ObjectSchema) {
                    ObjectSchema op = (ObjectSchema) inner;
                    if (op.getProperties() != null && op.getProperties().size() > 0) {
                        flattenProperties(openAPI, op.getProperties(), path);
                        String modelName = resolveModelName(op.getTitle(), path + "_" + key);
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            property.setAdditionalProperties(schema);
                        } else {
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            property.setAdditionalProperties(schema);
                            addGenerated(modelName, innerModel);
                            openapi.getComponents().addSchemas(modelName, innerModel);
                        }
                    }
                }
            }
        }
        if (propsToUpdate.size() > 0) {
            for (String key : propsToUpdate.keySet()) {
                properties.put(key, propsToUpdate.get(key));
            }
        }
        for (String key : modelsToAdd.keySet()) {
            openapi.getComponents().addSchemas(key, modelsToAdd.get(key));
            this.addedModels.put(key, modelsToAdd.get(key));
        }
    }

    private Schema modelFromProperty(OpenAPI openAPI, Schema object, String path) {
        String description = object.getDescription();
        String example = null;
        Object obj = object.getExample();
        if (obj != null) {
            example = obj.toString();
        }
        XML xml = object.getXml();
        Map<String, Schema> properties = object.getProperties();

        // NOTE:
        // No need to null check setters below. All defaults in the new'd Schema are null, so setting to null would just be a noop.
        Schema model = new Schema();
        model.setType(object.getType());

        // Even though the `format` keyword typically applies to primitive types only,
        // the JSON schema specification states `format` can be used for any model type instance
        // including object types.
        model.setFormat(object.getFormat());

        model.setDescription(description);
        model.setExample(example);
        model.setName(object.getName());
        model.setXml(xml);
        model.setRequired(object.getRequired());
        model.setNullable(object.getNullable());
        model.setEnum(object.getEnum());
        model.setType(object.getType());
        model.setDiscriminator(object.getDiscriminator());
        model.setWriteOnly(object.getWriteOnly());
        model.setUniqueItems(object.getUniqueItems());
        model.setTitle(object.getTitle());
        model.setReadOnly(object.getReadOnly());
        model.setPattern(object.getPattern());
        model.setNot(object.getNot());
        model.setMinProperties(object.getMinProperties());
        model.setMinLength(object.getMinLength());
        model.setMinItems(object.getMinItems());
        model.setMinimum(object.getMinimum());
        model.setMaxProperties(object.getMaxProperties());
        model.setMaxLength(object.getMaxLength());
        model.setMaxItems(object.getMaxItems());
        model.setMaximum(object.getMaximum());
        model.setExternalDocs(object.getExternalDocs());
        model.setExtensions(object.getExtensions());
        model.setExclusiveMinimum(object.getExclusiveMinimum());
        model.setExclusiveMaximum(object.getExclusiveMaximum());
        model.setExample(object.getExample());
        model.setDeprecated(object.getDeprecated());

        if (properties != null) {
            flattenProperties(openAPI, properties, path);
            model.setProperties(properties);
        }
        return model;
    }

    /**
     * Make a Schema
     *
     * @param ref      new property name
     * @param property Schema
     * @return {@link Schema} A constructed OpenAPI property
     */
    private Schema makeSchema(String ref, Schema property) {
        Schema newProperty = new Schema().$ref(ref);
        this.copyVendorExtensions(property, newProperty);
        return newProperty;
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
