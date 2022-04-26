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
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

public class InlineModelResolver {
    private OpenAPI openAPI;
    private Map<String, Schema> addedModels = new HashMap<>();
    private Map<String, String> generatedSignature = new HashMap<>();
    private Map<String, String> inlineSchemaNameMapping = new HashMap<>();
    private Set<String> inlineSchemaNameMappingValues = new HashSet<>();
    public boolean resolveInlineEnums = false;

    // structure mapper sorts properties alphabetically on write to ensure models are
    // serialized consistently for lookup of existing models
    private static ObjectMapper structureMapper;

    // a set to keep track of names generated for inline schemas
    private Set<String> uniqueNames = new HashSet<>();

    static {
        structureMapper = Json.mapper().copy();
        structureMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
        structureMapper.writer(new DefaultPrettyPrinter());
    }

    final Logger LOGGER = LoggerFactory.getLogger(InlineModelResolver.class);

    public void setInlineSchemaNameMapping(Map inlineSchemaNameMapping) {
        this.inlineSchemaNameMapping = inlineSchemaNameMapping;
        this.inlineSchemaNameMappingValues = new HashSet<>(inlineSchemaNameMapping.values());
    }

    void flatten(OpenAPI openAPI) {
        this.openAPI = openAPI;

        if (this.openAPI.getComponents() == null) {
            this.openAPI.setComponents(new Components());
        }

        if (this.openAPI.getComponents().getSchemas() == null) {
            this.openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        flattenPaths();
        flattenComponents();
    }

    /**
     * Flatten inline models in Paths
     */
    private void flattenPaths() {
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
                flattenRequestBody(pathname, operation);
                flattenParameters(pathname, operation);
                flattenResponses(pathname, operation);
            }
        }
    }

    /**
     * Return false if model can be represented by primitives e.g. string, object
     * without properties, array or map of other model (model contanier), etc.
     * <p>
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
                // check to ensure at least of the allOf item is model
                for (Schema inner : m.getAllOf()) {
                    if (isModelNeeded(inner)) {
                        return true;
                    }
                }
                // allOf items are all non-model (e.g. type: string) only
                return false;
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
        // Check object models / any type models / composed models for properties,
        // if the schema has a type defined that is not "object" it should not define
        // any properties
        if (schema.getType() == null || "object".equals(schema.getType())) {
            // Check properties and recurse, each property could be its own inline model
            Map<String, Schema> props = schema.getProperties();
            if (props != null) {
                for (String propName : props.keySet()) {
                    Schema prop = props.get(propName);
                    // Recurse to create $refs for inner models
                    //gatherInlineModels(prop, modelPrefix + StringUtils.camelize(propName));
                    gatherInlineModels(prop, modelPrefix + "_" + propName);
                    if (isModelNeeded(prop)) {
                        // If this schema should be split into its own model, do so
                        //Schema refSchema = this.makeSchemaResolve(modelPrefix, StringUtils.camelize(propName), prop);
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_" + propName, prop);
                        props.put(propName, refSchema);
                    } else if (prop instanceof ComposedSchema) {
                        ComposedSchema m = (ComposedSchema) prop;
                        if (m.getAllOf() != null && m.getAllOf().size() == 1 &&
                                !(m.getAllOf().get(0).getType() == null || "object".equals(m.getAllOf().get(0).getType()))) {
                            // allOf with only 1 type (non-model)
                            LOGGER.info("allOf schema used by the property `{}` replaced by its only item (a type)", propName);
                            props.put(propName, m.getAllOf().get(0));
                        }
                    }
                }
            }
            // Check additionalProperties for inline models
            if (schema.getAdditionalProperties() != null) {
                if (schema.getAdditionalProperties() instanceof Schema) {
                    Schema inner = (Schema) schema.getAdditionalProperties();
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "_addl_props");
                    if (isModelNeeded(inner)) {
                        // If this schema should be split into its own model, do so
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_addl_props", inner);
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
                Schema refSchema = this.makeSchemaResolve(modelPrefix, "_inner", items);
                array.setItems(refSchema);
            }
        }
        // Check allOf, anyOf, oneOf for inline models
        if (schema instanceof ComposedSchema) {
            ComposedSchema m = (ComposedSchema) schema;
            if (m.getAllOf() != null) {
                List<Schema> newAllOf = new ArrayList<Schema>();
                boolean atLeastOneModel = false;
                for (Schema inner : m.getAllOf()) {
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "_allOf");
                    if (isModelNeeded(inner)) {
                        Schema refSchema = this.makeSchemaResolve(modelPrefix, "_allOf", inner);
                        newAllOf.add(refSchema); // replace with ref
                        atLeastOneModel = true;
                    } else {
                        newAllOf.add(inner);
                    }
                }
                if (atLeastOneModel) {
                    m.setAllOf(newAllOf);
                } else {
                    // allOf is just one or more types only so do not generate the inline allOf model
                    if (m.getAllOf().size() == 1) {
                        // handle earlier in this function when looping through properites
                    } else if (m.getAllOf().size() > 1) {
                        LOGGER.warn("allOf schema `{}` containing multiple types (not model) is not supported at the moment.", schema.getName());
                    } else {
                        LOGGER.error("allOf schema `{}` contains no items.", schema.getName());
                    }
                }
            }
            if (m.getAnyOf() != null) {
                List<Schema> newAnyOf = new ArrayList<Schema>();
                for (Schema inner : m.getAnyOf()) {
                    // Recurse to create $refs for inner models
                    gatherInlineModels(inner, modelPrefix + "_anyOf");
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
                    gatherInlineModels(inner, modelPrefix + "_oneOf");
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
            gatherInlineModels(not, modelPrefix + "_not");
            if (isModelNeeded(not)) {
                Schema refSchema = this.makeSchemaResolve(modelPrefix, "_not", not);
                schema.setNot(refSchema);
            }
        }
    }

    /**
     * Flatten inline models in RequestBody
     *
     * @param pathname  target pathname
     * @param operation target operation
     */
    private void flattenRequestBody(String pathname, Operation operation) {
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
                    modelName = addSchemas(modelName, model);
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
                        modelName = addSchemas(modelName, innerModel);
                        Schema schema = new Schema().$ref(modelName);
                        schema.setRequired(op.getRequired());
                        am.setItems(schema);
                    }
                }
            }
        }
    }

    /**
     * Flatten inline models in parameters
     *
     * @param pathname  target pathname
     * @param operation target operation
     */
    private void flattenParameters(String pathname, Operation operation) {
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
                        modelName = addSchemas(modelName, model);
                        parameter.$ref(modelName);
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
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            am.setItems(schema);
                        }
                    }
                }
            }
        }
    }

    /**
     * Flatten inline models in ApiResponses
     *
     * @param pathname  target pathname
     * @param operation target operation
     */
    private void flattenResponses(String pathname, Operation operation) {
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
                            modelName = addSchemas(modelName, model);
                            Schema schema = this.makeSchema(modelName, property);
                            schema.setRequired(op.getRequired());
                            mediaType.setSchema(schema);
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
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = this.makeSchema(modelName, op);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
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
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            mp.setAdditionalProperties(schema);
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
     * <p>
     * In the example below, codegen may generate a 'Dog' class that extends from the
     * generated 'Animal' class. 'Dog' has additional properties 'name', 'age' and 'breed' that
     * are flattened as a single list of properties.
     * <p>
     * Dog:
     * allOf:
     * - $ref: '#/components/schemas/Animal'
     * - type: object
     * properties:
     * name:
     * type: string
     * age:
     * type: string
     * - type: object
     * properties:
     * breed:
     * type: string
     *
     * @param key      a unique name ofr the composed schema.
     * @param children the list of nested schemas within a composed schema (allOf, anyOf, oneOf).
     */
    private void flattenComposedChildren(String key, List<Schema> children) {
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
                    innerModelName = addSchemas(innerModelName, innerModel);
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
     */
    private void flattenComponents() {
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
                flattenComposedChildren(modelName + "_allOf", m.getAllOf());
                flattenComposedChildren(modelName + "_anyOf", m.getAnyOf());
                flattenComposedChildren(modelName + "_oneOf", m.getOneOf());
            } else if (model instanceof Schema) {
                //Schema m = model;
                //Map<String, Schema> properties = m.getProperties();
                //flattenProperties(openAPI, properties, modelName);
                //fixStringModel(m);
                gatherInlineModels(model, modelName);

            } /*else if (ModelUtils.isArraySchema(model)) {
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
            }*/
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
     * <p>
     * e.g. io.schema.User_name => io_schema_User_name
     *
     * @param title String title field in the schema if present
     * @param key   String model name
     * @return if provided the sanitized {@code title}, else the sanitized {@code key}
     */
    private String resolveModelName(String title, String key) {
        if (title == null) {
            if (key == null) {
                return uniqueName("inline_object");
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
     * <p>
     * e.g. 12.schema.User name => _2_schema_User_name
     */
    private String sanitizeName(final String name) {
        return name
                .replaceAll("^[0-9]", "_$0") // e.g. 12object => _12object
                .replaceAll("[^A-Za-z0-9]", "_"); // e.g. io.schema.User name => io_schema_User_name
    }

    private String uniqueName(final String name) {
        if (openAPI.getComponents().getSchemas() == null) { // no schema has been created
            uniqueNames.add(name);
            return name;
        }

        String uniqueName = name;
        int count = 0;
        while (true) {
            if (!openAPI.getComponents().getSchemas().containsKey(uniqueName) && !uniqueNames.contains(uniqueName)) {
                uniqueNames.add(uniqueName);
                return uniqueName;
            }
            uniqueName = name + "_" + ++count;
        }
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
                    modelName = addSchemas(modelName, model);
                    Schema schema = new Schema().$ref(modelName);
                    schema.setRequired(op.getRequired());
                    propsToUpdate.put(key, schema);
                    modelsToAdd.put(modelName, model);
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
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            ap.setItems(schema);
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
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            property.setAdditionalProperties(schema);
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
            openAPI.getComponents().addSchemas(key, modelsToAdd.get(key));
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
        // no need to set it again as it's set earlier
        //model.setExample(object.getExample());
        model.setDeprecated(object.getDeprecated());

        if (properties != null) {
            flattenProperties(openAPI, properties, path);
            model.setProperties(properties);
        }
        return model;
    }

    /**
     * Resolve namespace conflicts using:
     * title (if title exists) or
     * prefix + suffix (if title not specified)
     *
     * @param prefix used to form name if no title found in schema
     * @param suffix used to form name if no title found in schema
     * @param schema title property used to form name if exists and schema definition used
     *               to create new schema if doesn't exist
     * @return a new schema or $ref to an existing one if it was already created
     */
    private Schema makeSchemaResolve(String prefix, String suffix, Schema schema) {
        if (schema.getTitle() == null) {
            return makeSchemaInComponents(uniqueName(sanitizeName(prefix + suffix)), schema);
        }
        return makeSchemaInComponents(uniqueName(sanitizeName(schema.getTitle())), schema);
    }

    /**
     * Move schema to components (if new) and return $ref to schema or
     * existing schema.
     *
     * @param name   new schema name
     * @param schema schema to move to components or find existing ref
     * @return {@link Schema} $ref schema to new or existing schema
     */
    private Schema makeSchemaInComponents(String name, Schema schema) {
        String existing = matchGenerated(schema);
        Schema refSchema;
        if (existing != null) {
            refSchema = new Schema().$ref(existing);
        } else {
            if (resolveInlineEnums && schema.getEnum() != null && schema.getEnum().size() > 0) {
                LOGGER.warn("Model " + name + " promoted to its own schema due to resolveInlineEnums=true");
            }
            name = addSchemas(name, schema);
            refSchema = new Schema().$ref(name);
        }
        this.copyVendorExtensions(schema, refSchema);
        return refSchema;
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

    /**
     * Add the schemas to the components
     *
     * @param name   name of the inline schema
     * @param schema inilne schema
     * @return the actual model name (based on inlineSchemaNameMapping if provied)
     */
    private String addSchemas(String name, Schema schema) {
        //check inlineSchemaNameMapping
        if (inlineSchemaNameMapping.containsKey(name)) {
            name = inlineSchemaNameMapping.get(name);
        }

        addGenerated(name, schema);
        openAPI.getComponents().addSchemas(name, schema);
        if (!name.equals(schema.getTitle()) && !inlineSchemaNameMappingValues.contains(name)) {
            LOGGER.info("Inline schema created as {}. To have complete control of the model name, set the `title` field or use the inlineSchemaNameMapping option (--inline-schema-name-mapping in CLI).", name);
        }

        return name;
    }


}
