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
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class InlineModelResolver {
    private OpenAPI openAPI;
    private Map<String, Schema> addedModels = new HashMap<>();
    private Map<String, String> generatedSignature = new HashMap<>();
    // Structural signature: same as generatedSignature but serialised without 'description' or
    // 'type' fields at any level of the schema graph.  Used as a fallback when the Swagger Parser
    // produces two Schema objects from the same source file that differ only in volatile fields —
    // this happens in several ways:
    //
    //   1. OpenAPI 3.1 $ref + sibling description (valid per JSON Schema 2020-12): the property-
    //      level description overrides the one in the referenced schema, so the resolved Schema
    //      object gets a different description from its canonical definition.
    //
    //   2. The Swagger Parser shares a single resolved Schema object (e.g. uuid.json) across ALL
    //      usages of that external type.  Each property that references it with a sibling
    //      description overwrites that object's description field in-place, so two Schema objects
    //      for the same source file end up with different property-level descriptions depending on
    //      which other usages of the shared type were processed between the two resolutions.
    //
    //   3. The Swagger Parser additionally strips 'type' annotations (e.g. type:"string") from
    //      shared Schema object properties between processing passes, so identical schemas can
    //      appear with and without explicit type fields depending on processing order.
    //
    // By stripping description and type recursively via a Jackson MixIn (see structuralMapper),
    // we get a hash stable across all such variations while still distinguishing schemas with
    // genuinely different formats, patterns, property structures, enum values, etc.
    private Map<String, String> generatedStructuralSignature = new HashMap<>();
    private Map<String, String> inlineSchemaNameMapping = new HashMap<>();
    private Map<String, String> inlineSchemaOptions = new HashMap<>();
    private Set<String> inlineSchemaNameMappingValues = new HashSet<>();
    public boolean resolveInlineEnums = false;
    public boolean skipSchemaReuse = false; // skip reusing inline schema if set to true
    public Boolean refactorAllOfInlineSchemas = null; // refactor allOf inline schemas into $ref

    // structure mapper sorts properties alphabetically on write to ensure models are
    // serialized consistently for lookup of existing models
    private static ObjectMapper structureMapper;
    // structural mapper is like structureMapper but ignores 'description' and 'type' fields at
    // every level of the schema graph (via a MixIn applied to Schema.class and all its subclasses)
    private static ObjectMapper structuralMapper;

    // a set to keep track of names generated for inline schemas
    private Set<String> uniqueNames = new HashSet<>();

    // MixIn that suppresses volatile fields when any Schema instance is serialised for structural
    // comparison.  Registered on Schema.class so it applies recursively to all nested Schema
    // objects (properties, allOf/anyOf/oneOf items, array items, additionalProperties, etc.).
    // Suppressed fields and why:
    // - 'description': the Swagger Parser overwrites shared Schema object descriptions in-place
    //   when resolving $ref + sibling descriptions, and also strips them between passes.
    // - 'type': the Swagger Parser strips 'type' annotations (e.g. type:"string") from shared
    //   Schema object properties between processing passes.
    // - 'example': OAS 3.1 allows 'example' as a sibling to '$ref'; different usages of the same
    //   external schema may carry different (or no) sibling examples, making otherwise identical
    //   schemas appear structurally different.
    @com.fasterxml.jackson.annotation.JsonIgnoreProperties({"description", "type", "example"})
    private abstract static class IgnoreVolatileFieldsMixIn {}

    static {
        structureMapper = Json.mapper().copy();
        structureMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
        structureMapper.writer(new DefaultPrettyPrinter());

        structuralMapper = Json.mapper().copy();
        structuralMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
        structuralMapper.addMixIn(Schema.class, IgnoreVolatileFieldsMixIn.class);
    }

    /**
     * Compute a structural signature for a schema, normalising away parser-induced volatile
     * differences so that two schema objects that are semantically equal compare as equal.
     *
     * Beyond what the {@link #structuralMapper} already strips via {@link IgnoreVolatileFieldsMixIn}
     * (description, type, example), this method additionally normalises:
     * <ul>
     *   <li><b>default: null vs absent default</b> — the Swagger Parser represents an explicit
     *       {@code default: null} in YAML as a Jackson {@code NullNode} (a non-null Java object
     *       that the NON_NULL mapper serialises as {@code "default":null}), while a schema
     *       property with no {@code default} keyword at all results in a Java null (omitted by
     *       NON_NULL serialisation). Both are semantically "no default"; this method removes any
     *       {@code "default":null} entry from the JSON tree before returning the signature string.
     *       Real non-null defaults (e.g. {@code "default":"available"}) are preserved as-is.</li>
     * </ul>
     */
    private String computeStructuralSignature(Schema<?> model) throws JsonProcessingException {
        String raw = structuralMapper.writeValueAsString(model);
        com.fasterxml.jackson.databind.JsonNode tree = structuralMapper.readTree(raw);
        removeNullDefaultNodes(tree);
        return structuralMapper.writeValueAsString(tree);
    }

    /** Recursively remove any {@code "default":null} (NullNode) entries from a JsonNode tree. */
    private static void removeNullDefaultNodes(com.fasterxml.jackson.databind.JsonNode node) {
        if (node.isObject()) {
            com.fasterxml.jackson.databind.node.ObjectNode obj =
                    (com.fasterxml.jackson.databind.node.ObjectNode) node;
            com.fasterxml.jackson.databind.JsonNode dflt = obj.get("default");
            if (dflt != null && dflt.isNull()) {
                obj.remove("default");
            }
            node.forEach(InlineModelResolver::removeNullDefaultNodes);
        } else if (node.isArray()) {
            node.forEach(InlineModelResolver::removeNullDefaultNodes);
        }
    }

    final Logger LOGGER = LoggerFactory.getLogger(InlineModelResolver.class);

    public InlineModelResolver() {
        this.inlineSchemaOptions.put("ARRAY_ITEM_SUFFIX", "_inner");
        this.inlineSchemaOptions.put("MAP_ITEM_SUFFIX", "_value");
    }

    public void setInlineSchemaNameMapping(Map inlineSchemaNameMapping) {
        this.inlineSchemaNameMapping = inlineSchemaNameMapping;
        this.inlineSchemaNameMappingValues = new HashSet<>(inlineSchemaNameMapping.values());
    }

    public void setInlineSchemaOptions(Map inlineSchemaOptions) {
        this.inlineSchemaOptions.putAll(inlineSchemaOptions);

        if ("true".equalsIgnoreCase(
                this.inlineSchemaOptions.getOrDefault("SKIP_SCHEMA_REUSE", "false"))) {
            this.skipSchemaReuse = true;
        }

        if (this.inlineSchemaOptions.containsKey("REFACTOR_ALLOF_INLINE_SCHEMAS")) {
            this.refactorAllOfInlineSchemas = Boolean.valueOf(this.inlineSchemaOptions.get("REFACTOR_ALLOF_INLINE_SCHEMAS"));
        } else {
            // not set so default to null;
        }

        if (this.inlineSchemaOptions.containsKey("RESOLVE_INLINE_ENUMS")) {
            this.resolveInlineEnums = Boolean.valueOf(this.inlineSchemaOptions.get("RESOLVE_INLINE_ENUMS"));
        } else {
            // not set so default to null;
        }
    }

    void flatten(OpenAPI openAPI) {
        this.openAPI = openAPI;

        if (this.openAPI.getComponents() == null) {
            this.openAPI.setComponents(new Components());
        }

        if (this.openAPI.getComponents().getSchemas() == null) {
            this.openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        // Pre-populate generatedSignature with existing components/schemas entries so that
        // matchGenerated() can find them and avoid creating numbered duplicates (e.g. Foo_1)
        // when an inline schema with identical content is encountered during flattening.
        // Only titled schemas are pre-populated: a schema identified only by its YAML key in
        // components/schemas has no inherent identity — two anonymous schemas with the same
        // structure may be intentionally distinct (e.g. separate request/response bodies that
        // happen to share the same properties). A titled schema (e.g. title: "Container Mapping")
        // represents a named type defined in its own file and should be reused wherever referenced.
        for (Map.Entry<String, Schema> entry : this.openAPI.getComponents().getSchemas().entrySet()) {
            if (entry.getValue().getTitle() != null) {
                addGenerated(entry.getKey(), entry.getValue());
            }
        }

        flattenPaths();
        flattenWebhooks();
        flattenComponents();
        flattenComponentResponses();
        deduplicateComponents();
    }

    /**
     * Flatten inline models in Webhooks
     */
    private void flattenWebhooks() {
        Map<String, PathItem> webhooks = openAPI.getWebhooks();
        if (webhooks == null) {
            return;
        }
        flattenPathItems(webhooks);
    }

    /**
     * Flatten inline models in Paths
     */
    private void flattenPaths() {
        Paths paths = openAPI.getPaths();
        if (paths == null) {
            return;
        }
        flattenPathItems(paths);
    }

    /**
     * Flatten inline models in path items
     *
     * @param pathItemMap Map of path items
     */
    private void flattenPathItems(Map<String, PathItem> pathItemMap) {
        for (Map.Entry<String, PathItem> pathsEntry : pathItemMap.entrySet()) {
            PathItem path = pathsEntry.getValue();
            List<Map.Entry<HttpMethod, Operation>> toFlatten = new ArrayList<>(path.readOperationsMap().entrySet());

            // use path name (e.g. /foo/bar) and HTTP verb to come up with a name
            // in case operationId is not defined later in other methods
            String pathname = pathsEntry.getKey();

            // Include callback operation as well
            for (Map.Entry<HttpMethod, Operation> operationEntry : new LinkedHashMap<>(path.readOperationsMap()).entrySet()) {
                Operation operation = operationEntry.getValue();
                Map<String, Callback> callbacks = operation.getCallbacks();
                if (callbacks != null) {
                    for (Map.Entry<String, Callback> callbackEntry : callbacks.entrySet()) {
                        Callback callback = callbackEntry.getValue();
                        for (Map.Entry<String, PathItem> pathItemEntry : callback.entrySet()) {
                            PathItem pathItem = pathItemEntry.getValue();
                            toFlatten.addAll(pathItem.readOperationsMap().entrySet());
                        }
                    }
                }
            }

            // flatten path-level parameters
            flattenParameters(pathname, path.getParameters(), null);

            // flatten parameters for each operation
            for (Map.Entry<HttpMethod, Operation> operationEntry : toFlatten) {
                Operation operation = operationEntry.getValue();
                String inlineSchemaName = this.getInlineSchemaName(operationEntry.getKey(), pathname);
                flattenRequestBody(inlineSchemaName, operation);
                flattenParameters(inlineSchemaName, operation.getParameters(), operation.getOperationId());
                flattenResponses(inlineSchemaName, operation);
            }
        }
    }

    private String getInlineSchemaName(HttpMethod httpVerb, String pathname) {
        String name = pathname;
        if (httpVerb.equals(HttpMethod.DELETE)) {
            name += "_delete";
        } else if (httpVerb.equals(HttpMethod.GET)) {
            name += "_get";
        } else if (httpVerb.equals(HttpMethod.HEAD)) {
            name += "_head";
        } else if (httpVerb.equals(HttpMethod.OPTIONS)) {
            name += "_options";
        } else if (httpVerb.equals(HttpMethod.PATCH)) {
            name += "_patch";
        } else if (httpVerb.equals(HttpMethod.POST)) {
            name += "_post";
        } else if (httpVerb.equals(HttpMethod.PUT)) {
            name += "_put";
        } else if (httpVerb.equals(HttpMethod.TRACE)) {
            name += "_trace";
        } else {
            // no HTTP verb defined?
            // throw new RuntimeException("No HTTP verb found/detected in the inline model
            // resolver");
        }
        return name;
    }

    /**
     * Return false if model can be represented by primitives e.g. string, object
     * without properties, array or map of other model (model container), etc.
     * <p>
     * Return true if a model should be generated e.g. object with properties,
     * enum, oneOf, allOf, anyOf, etc.
     *
     * @param schema target schema
     */
    private boolean isModelNeeded(Schema schema) {
        return isModelNeeded(schema, new HashSet<>());
    }

    /**
     * Return false if model can be represented by primitives e.g. string, object
     * without properties, array or map of other model (model container), etc.
     * <p>
     * Return true if a model should be generated e.g. object with properties,
     * enum, oneOf, allOf, anyOf, etc.
     *
     * @param schema         target schema
     * @param visitedSchemas Visited schemas
     */
    private boolean isModelNeeded(Schema schema, Set<Schema> visitedSchemas) {
        if (visitedSchemas.contains(schema)) { // circular reference
            return true;
        } else {
            visitedSchemas.add(schema);
        }

        if (resolveInlineEnums && schema.getEnum() != null && schema.getEnum().size() > 0) {
            return true;
        }
        if (schema.getType() == null || ModelUtils.isObjectTypeOAS30(schema)) {
            // object or undeclared type with properties
            if (ModelUtils.hasProperties(schema)) {
                return true;
            }
        }
        if (ModelUtils.isComposedSchema(schema)) {
            // allOf, anyOf, oneOf
            boolean isSingleAllOf = schema.getAllOf() != null && schema.getAllOf().size() == 1;
            boolean isReadOnly = schema.getReadOnly() != null && schema.getReadOnly();
            boolean isNullable = schema.getNullable() != null && schema.getNullable();

            if (isSingleAllOf && (isReadOnly || isNullable)) {
                // Check if this composed schema only contains an allOf and a readOnly or nullable.
                ComposedSchema c = new ComposedSchema();
                c.setAllOf(schema.getAllOf());
                c.setReadOnly(schema.getReadOnly());
                c.setNullable(schema.getNullable());
                if (schema.equals(c)) {
                    return isModelNeeded((Schema) schema.getAllOf().get(0), visitedSchemas);
                }
            }

            if (isSingleAllOf && StringUtils.isNotEmpty(((Schema) schema.getAllOf().get(0)).get$ref())) {
                // single allOf and it's a ref
                return isModelNeeded((Schema) schema.getAllOf().get(0), visitedSchemas);
            }

            if (ModelUtils.hasAllOf(schema)) {
                // check to ensure at least one of the allOf item is model
                for (Object inner : schema.getAllOf()) {
                    if (isModelNeeded(ModelUtils.getReferencedSchema(openAPI, (Schema) inner), visitedSchemas)) {
                        return true;
                    }
                }
                // allOf items are all non-model (e.g. type: string) only
                return false;
            }

            if (ModelUtils.hasAnyOf(schema)) {
                return true;
            }
            if (ModelUtils.hasOneOf(schema)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Recursively gather inline models that need to be generated and
     * replace inline schemas with $ref to schema to-be-generated.
     *
     * @param schema      target schema
     * @param modelPrefix model name (usually the prefix of the inline model name)
     */
    private void gatherInlineModels(Schema schema, String modelPrefix) {
        if (schema.get$ref() != null) {
            // if ref already, no inline schemas should be present but check for
            // any to catch OpenAPI violations
            if (isModelNeeded(schema) || ModelUtils.isObjectTypeOAS30(schema) ||
                    schema.getProperties() != null || schema.getAdditionalProperties() != null ||
                ModelUtils.isComposedSchema(schema)) {
                LOGGER.error("Illegal schema found with $ref combined with other properties," +
                        " no properties should be defined alongside a $ref:\n " + schema.toString());
            }
            return;
        }
        // Check object models / any type models / composed models for properties,
        // if the schema has a type defined that is not "object" it should not define
        // any properties
        if (schema.getType() == null || ModelUtils.isObjectTypeOAS30(schema)) {
            // Check properties and recurse, each property could be its own inline model
            Map<String, Schema> props = schema.getProperties();
            if (props != null) {
                for (String propName : props.keySet()) {
                    Schema prop = props.get(propName);

                    if (prop == null) {
                        continue;
                    }

                    String schemaName = resolveModelName(prop.getTitle(), modelPrefix + "_" + propName);
                    // Recurse to create $refs for inner models
                    gatherInlineModels(prop, schemaName);
                    if (isModelNeeded(prop)) {
                        // If this schema should be split into its own model, do so
                        Schema refSchema = this.makeSchemaInComponents(schemaName, prop);
                        props.put(propName, refSchema);
                    } else if (ModelUtils.isComposedSchema(prop)) {
                        if (prop.getAllOf() != null && prop.getAllOf().size() == 1 &&
                                !(((Schema) prop.getAllOf().get(0)).getType() == null ||
                                        "object".equals(((Schema) prop.getAllOf().get(0)).getType()))) {
                            // allOf with only 1 type (non-model)
                            LOGGER.info("allOf schema used by the property `{}` replaced by its only item (a type)", propName);
                            props.put(propName, (Schema) prop.getAllOf().get(0));
                        }
                    }
                }
            }
            // Check additionalProperties for inline models
            if (schema.getAdditionalProperties() != null) {
                if (schema.getAdditionalProperties() instanceof Schema) {
                    Schema inner = (Schema) schema.getAdditionalProperties();
                    if (inner != null) {
                        String schemaName = resolveModelName(inner.getTitle(), modelPrefix + this.inlineSchemaOptions.get("MAP_ITEM_SUFFIX"));
                        // Recurse to create $refs for inner models
                        gatherInlineModels(inner, schemaName);
                        if (isModelNeeded(inner)) {
                            // If this schema should be split into its own model, do so
                            Schema refSchema = this.makeSchemaInComponents(schemaName, inner);
                            schema.setAdditionalProperties(refSchema);
                        }
                    }
                }
            }
        } else if (schema.getProperties() != null) {
            // If non-object type is specified but also properties
            LOGGER.error("Illegal schema found with non-object type combined with properties," +
                    " no properties should be defined:" +
                    " consider using --openapi-normalizer REMOVE_PROPERTIES_FROM_TYPE_OTHER_THAN_OBJECT=true\n " +
                    schema.toString());
            return;
        } else if (schema.getAdditionalProperties() != null) {
            // If non-object type is specified but also additionalProperties
            LOGGER.error("Illegal schema found with non-object type combined with" +
                    " additionalProperties, no additionalProperties should be defined:\n " +
                    schema.toString());
            return;
        }
        // Check array items
        if (ModelUtils.isArraySchema(schema)) {
            Schema items = ModelUtils.getSchemaItems(schema);
            if (items == null && schema.getPrefixItems() == null) {
                LOGGER.debug("Incorrect array schema with no items, prefixItems: {}", schema.toString());
                return;
            }

            if (items == null) {
                LOGGER.debug("prefixItems in array schema is not supported at the moment: {}", schema.toString());
                return;
            }
            String schemaName = resolveModelName(items.getTitle(), modelPrefix + this.inlineSchemaOptions.get("ARRAY_ITEM_SUFFIX"));

            // Recurse to create $refs for inner models
            gatherInlineModels(items, schemaName);

            if (isModelNeeded(items)) {
                // If this schema should be split into its own model, do so
                schema.setItems(this.makeSchemaInComponents(schemaName, items));
            }
        }
        // Check allOf, anyOf, oneOf for inline models
        if (ModelUtils.isComposedSchema(schema)) {
            if (schema.getAllOf() != null) {
                List<Schema> newAllOf = new ArrayList<Schema>();
                boolean atLeastOneModel = false;
                for (Object inner : schema.getAllOf()) {
                    if (inner == null) {
                        continue;
                    }
                    String schemaName = resolveModelName(((Schema) inner).getTitle(), modelPrefix + "_allOf");
                    // Recurse to create $refs for inner models
                    gatherInlineModels((Schema) inner, schemaName);
                    if (isModelNeeded((Schema) inner)) {
                        if (Boolean.TRUE.equals(this.refactorAllOfInlineSchemas)) {
                            newAllOf.add(this.makeSchemaInComponents(schemaName, (Schema) inner)); // replace with ref
                            atLeastOneModel = true;
                        } else { // do not refactor allOf inline schemas
                            newAllOf.add((Schema) inner);
                            atLeastOneModel = true;
                        }
                    } else {
                        newAllOf.add((Schema) inner);
                    }
                }
                if (atLeastOneModel) {
                    schema.setAllOf(newAllOf);
                } else {
                    // allOf is just one or more types only so do not generate the inline allOf model
                    if (schema.getAllOf().size() == 1) {
                        // handle earlier in this function when looping through properties
                    } else if (schema.getAllOf().size() > 1) {
                        LOGGER.warn("allOf schema `{}` containing multiple types (not model) is not supported at the moment.", schema.getName());
                    } else {
                        LOGGER.error("allOf schema `{}` contains no items.", schema.getName());
                    }
                }
            }
            if (schema.getAnyOf() != null) {
                List<Schema> newAnyOf = new ArrayList<Schema>();
                for (Object inner : schema.getAnyOf()) {
                    if (inner == null) {
                        continue;
                    }
                    String schemaName = resolveModelName(((Schema) inner).getTitle(), modelPrefix + "_anyOf");
                    // Recurse to create $refs for inner models
                    gatherInlineModels((Schema) inner, schemaName);
                    if (isModelNeeded((Schema) inner)) {
                        newAnyOf.add(this.makeSchemaInComponents(schemaName, (Schema) inner)); // replace with ref
                    } else {
                        newAnyOf.add((Schema) inner);
                    }
                }
                schema.setAnyOf(newAnyOf);
            }
            if (schema.getOneOf() != null) {
                List<Schema> newOneOf = new ArrayList<Schema>();
                for (Object inner : schema.getOneOf()) {
                    if (inner == null) {
                        continue;
                    }
                    String schemaName = resolveModelName(((Schema) inner).getTitle(), modelPrefix + "_oneOf");
                    // Recurse to create $refs for inner models
                    gatherInlineModels((Schema) inner, schemaName);
                    if (isModelNeeded((Schema) inner)) {
                        newOneOf.add(this.makeSchemaInComponents(schemaName, (Schema) inner)); // replace with ref
                    } else {
                        newOneOf.add((Schema) inner);
                    }
                }
                schema.setOneOf(newOneOf);
            }
        }
        // Check not schema
        if (schema.getNot() != null) {
            Schema not = schema.getNot();
            if (not != null) {
                String schemaName = resolveModelName(schema.getTitle(), modelPrefix + "_not");
                // Recurse to create $refs for inner models
                gatherInlineModels(not, schemaName);
                if (isModelNeeded(not)) {
                    Schema refSchema = this.makeSchemaInComponents(schemaName, not);
                    schema.setNot(refSchema);
                }
            }
        }
    }

    /**
     * Flatten inline models in content
     *
     * @param content target content
     * @param name    backup name if no title is found
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
            String schemaName = resolveModelName(schema.getTitle(), name); // name example: testPost_request
            // Recursively gather/make inline models within this schema if any
            gatherInlineModels(schema, schemaName);
            if (isModelNeeded(schema)) {
                // If this schema should be split into its own model, do so
                //Schema refSchema = this.makeSchema(schemaName, schema);
                mediaType.setSchema(this.makeSchemaInComponents(schemaName, schema));
            }
        }
    }

    /**
     * Flatten inline models in RequestBody
     *
     * @param modelName inline model name prefix
     * @param operation target operation
     */
    private void flattenRequestBody(String modelName, Operation operation) {
        RequestBody requestBody = operation.getRequestBody();
        if (requestBody == null) {
            return;
        }

        // unalias $ref
        if (requestBody.get$ref() != null) {
            String ref = ModelUtils.getSimpleRef(requestBody.get$ref());
            requestBody = openAPI.getComponents().getRequestBodies().get(ref);

            if (requestBody == null) {
                return;
            }
        }

        flattenContent(requestBody.getContent(),
                (operation.getOperationId() == null ? modelName : operation.getOperationId()) + "_request");
    }

    /**
     * Flatten inline models in parameters
     *
     * @param modelName   model name
     * @param parameters  list of parameters
     * @param operationId operation Id (optional)
     */
    private void flattenParameters(String modelName, List<Parameter> parameters, String operationId) {
        //List<Parameter> parameters = operation.getParameters();
        if (parameters == null) {
            return;
        }

        for (Parameter parameter : parameters) {
            if (StringUtils.isNotEmpty(parameter.get$ref())) {
                parameter = ModelUtils.getReferencedParameter(openAPI, parameter);
            }

            if (parameter.getSchema() == null) {
                continue;
            }

            Schema parameterSchema = parameter.getSchema();

            if (parameterSchema == null) {
                continue;
            }
            String schemaName = resolveModelName(parameterSchema.getTitle(),
                    (operationId == null ? modelName : operationId) + "_" + parameter.getName() + "_parameter");
            // Recursively gather/make inline models within this schema if any
            gatherInlineModels(parameterSchema, schemaName);
            if (isModelNeeded(parameterSchema)) {
                // If this schema should be split into its own model, do so
                parameter.setSchema(this.makeSchemaInComponents(schemaName, parameterSchema));
            }
        }
    }

    /**
     * Flatten inline models in ApiResponses
     *
     * @param modelName model name prefix
     * @param operation target operation
     */
    private void flattenResponses(String modelName, Operation operation) {
        ApiResponses responses = operation.getResponses();
        if (responses == null) {
            return;
        }

        for (Map.Entry<String, ApiResponse> responsesEntry : responses.entrySet()) {
            String key = responsesEntry.getKey();
            ApiResponse response = responsesEntry.getValue();

            flattenContent(response.getContent(),
                    (operation.getOperationId() == null ? modelName : operation.getOperationId()) + "_" + key + "_response");
        }
    }

    /**
     * Flatten inline models in the responses section in the components.
     */
    private void flattenComponentResponses() {
        Map<String, ApiResponse> apiResponses = openAPI.getComponents().getResponses();
        if (apiResponses == null) {
            return;
        }

        for (Map.Entry<String, ApiResponse> entry : apiResponses.entrySet()) {
            flattenContent(entry.getValue().getContent(), null);
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
     * @param key                    a unique name for the composed schema.
     * @param children               the list of nested schemas within a composed schema (allOf, anyOf, oneOf).
     * @param skipAllOfInlineSchemas true if allOf inline schemas need to be skipped.
     */
    private void flattenComposedChildren(String key, List<Schema> children, boolean skipAllOfInlineSchemas) {
        if (children == null || children.isEmpty()) {
            return;
        }
        ListIterator<Schema> listIterator = children.listIterator();
        while (listIterator.hasNext()) {
            Schema component = listIterator.next();
            boolean componentDoesNotHaveRef = component != null && component.get$ref() == null;
            if (componentDoesNotHaveRef && (ModelUtils.hasProperties(component) || ModelUtils.hasEnum(component))) {
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
                // Recurse to create $refs for inner models
                gatherInlineModels(innerModel, innerModelName);
                if (!skipAllOfInlineSchemas) {
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
                } else {
                    LOGGER.debug("Inline allOf schema {} not refactored into a separate model using $ref.", innerModelName);
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
            if (model == null) {
                continue;
            }
            if (ModelUtils.isAnyOf(model)) { // contains anyOf only
                gatherInlineModels(model, modelName);
            } else if (ModelUtils.isOneOf(model)) { // contains oneOf only
                gatherInlineModels(model, modelName);
            } else if (ModelUtils.isComposedSchema(model)) {
                // composed Schema can have properties!
                if (ModelUtils.hasProperties(model)) {
                    gatherInlineModels(model, modelName);
                }
                // inline child schemas
                flattenComposedChildren(modelName + "_allOf", model.getAllOf(), !Boolean.TRUE.equals(this.refactorAllOfInlineSchemas));
                flattenComposedChildren(modelName + "_anyOf", model.getAnyOf(), false);
                flattenComposedChildren(modelName + "_oneOf", model.getOneOf(), false);
            } else {
                gatherInlineModels(model, modelName);
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
     * <p>
     * e.g. io.schema.User_name => io_schema_User_name
     *
     * @param title     String title field in the schema if present
     * @param modelName String model name
     * @return if provided the sanitized {@code title}, else the sanitized {@code key}
     */
    private String resolveModelName(String title, String modelName) {
        if (title == null || "".equals(sanitizeName(title).replace("_", ""))) {
            if (modelName == null) {
                return uniqueName("inline_object");
            }
            return uniqueName(sanitizeName(modelName));
        } else {
            return uniqueName(sanitizeName(title));
        }
    }

    private String matchGenerated(Schema model) {
        if (skipSchemaReuse) { // skip reusing schema
            return null;
        }

        try {
            // Exact content match.
            String json = structureMapper.writeValueAsString(model);
            if (generatedSignature.containsKey(json)) {
                return generatedSignature.get(json);
            }
            // Structural match: compare with volatile fields stripped at every level.
            // See generatedStructuralSignature field for a full explanation of why this is needed.
            //
            // Only applied to *titled* schemas. A title denotes a named type that should be reused
            // wherever it appears, so parser-induced volatile differences (description, type,
            // example) must not split it into numbered duplicates. Anonymous/untitled inline
            // schemas, however, may be intentionally distinct even when structurally identical once
            // those volatile fields are stripped (e.g. two response properties that differ only by
            // description) — unifying them silently changes the generated type of one property and
            // breaks user code. This mirrors the titled-only guards in flatten() pre-population and
            // deduplicateComponents().
            if (model.getTitle() != null) {
                String structural = computeStructuralSignature(model);
                if (generatedStructuralSignature.containsKey(structural)) {
                    return generatedStructuralSignature.get(structural);
                }
            }
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        return null;
    }

    private void addGenerated(String name, Schema model) {
        try {
            generatedSignature.put(structureMapper.writeValueAsString(model), name);
            // Only register the volatile-stripped structural signature for titled schemas; untitled
            // inline schemas must not participate in the structural-match fallback (see matchGenerated).
            if (model.getTitle() != null) {
                generatedStructuralSignature.putIfAbsent(computeStructuralSignature(model), name);
            }
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
    }

    /**
     * Sanitizes the input so that it's valid name for a class or interface
     * <p>
     * e.g. 12.schema.User name => _2_schema_User_name
     *
     * @param name name to be processed to make sure it's sanitized
     */
    private String sanitizeName(final String name) {
        return name
                .replaceAll("^[0-9]", "_$0") // e.g. 12object => _12object
                .replaceAll("[^A-Za-z0-9]", "_"); // e.g. io.schema.User name => io_schema_User_name
    }

    /**
     * Generate a unique name for the input
     *
     * @param name name to be processed to make sure it's unique
     */
    private String uniqueName(final String name) {
        if (openAPI.getComponents().getSchemas() == null) { // no schema has been created
            return name;
        }

        String uniqueName = name;
        int count = 0;
        while (true) {
            if (!openAPI.getComponents().getSchemas().containsKey(uniqueName) && !uniqueNames.contains(uniqueName)) {
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
            if (ModelUtils.isObjectSchema(property)) {
                Schema op = property;
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
            } else if (ModelUtils.isArraySchema(property)) {
                Schema inner = ModelUtils.getSchemaItems(property);
                if (ModelUtils.isObjectSchema(inner)) {
                    Schema op = inner;
                    if (ModelUtils.hasProperties(op)) {
                        flattenProperties(openAPI, op.getProperties(), path);
                        String modelName = resolveModelName(op.getTitle(), path + "_" + key);
                        Schema innerModel = modelFromProperty(openAPI, op, modelName);
                        String existing = matchGenerated(innerModel);
                        if (existing != null) {
                            Schema schema = new Schema().$ref(existing);
                            schema.setRequired(op.getRequired());
                            property.setItems(schema);
                        } else {
                            modelName = addSchemas(modelName, innerModel);
                            Schema schema = new Schema().$ref(modelName);
                            schema.setRequired(op.getRequired());
                            property.setItems(schema);
                        }
                    }
                } else if (ModelUtils.isComposedSchema(inner)) {
                    String innerModelName = resolveModelName(inner.getTitle(), path + "_" + key);
                    gatherInlineModels(inner, innerModelName);
                    innerModelName = addSchemas(innerModelName, inner);
                    Schema schema = new Schema().$ref(innerModelName);
                    schema.setRequired(inner.getRequired());
                    property.setItems(schema);
                } else {
                    LOGGER.debug("Schema not yet handled in model resolver: {}", inner);
                }
            } else if (ModelUtils.isMapSchema(property)) {
                Schema inner = ModelUtils.getAdditionalProperties(property);
                if (ModelUtils.isObjectSchema(inner)) {
                    Schema op = inner;
                    if (ModelUtils.hasProperties(op)) {
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
                } else if (ModelUtils.isComposedSchema(inner)) {
                    String innerModelName = resolveModelName(inner.getTitle(), path + "_" + key);
                    gatherInlineModels(inner, innerModelName);
                    innerModelName = addSchemas(innerModelName, inner);
                    Schema schema = new Schema().$ref(innerModelName);
                    schema.setRequired(inner.getRequired());
                    property.setAdditionalProperties(schema);
                } else {
                    LOGGER.debug("Schema not yet handled in model resolver: {}", inner);
                }
            } else if (ModelUtils.isComposedSchema(property)) { // oneOf, anyOf, allOf etc
                if (property.getAllOf() != null && property.getAllOf().size() == 1 // allOf with a single item
                        && (property.getOneOf() == null || property.getOneOf().isEmpty()) // not oneOf
                        && (property.getAnyOf() == null || property.getAnyOf().isEmpty()) // not anyOf
                        && (property.getProperties() == null || property.getProperties().isEmpty())) { // no property
                    // don't do anything if it's allOf with a single item
                    LOGGER.debug("allOf with a single item (which can be handled by default codegen) skipped by inline model resolver: {}", property);
                } else {
                    String propertyModelName = resolveModelName(property.getTitle(), path + "_" + key);
                    gatherInlineModels(property, propertyModelName);
                    propertyModelName = addSchemas(propertyModelName, property);
                    Schema schema = new Schema().$ref(propertyModelName);
                    schema.setRequired(property.getRequired());
                    propsToUpdate.put(key, schema);
                }
            } else {
                LOGGER.debug("Schema not yet handled in model resolver: {}", property);
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

        if (object.getExample() != null) {
            model.setExample(example);
        }
        model.setDescription(description);
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
     * @param schema inline schema
     * @return the actual model name (based on inlineSchemaNameMapping if provided)
     */
    private String addSchemas(String name, Schema schema) {
        //check inlineSchemaNameMapping
        if (inlineSchemaNameMapping.containsKey(name)) {
            name = inlineSchemaNameMapping.get(name);
        }

        addGenerated(name, schema);
        openAPI.getComponents().addSchemas(name, schema);
        if (!name.equals(schema.getTitle()) && !inlineSchemaNameMappingValues.contains(name)) {
            LOGGER.info("Inline schema created as {}. To have complete control of the model name, set the `title` field or use the modelNameMapping option (e.g. --model-name-mappings {}=NewModel,ModelA=NewModelA in CLI) or inlineSchemaNameMapping option (--inline-schema-name-mappings {}=NewModel,ModelA=NewModelA in CLI).", name, name, name);
        }

        uniqueNames.add(name);

        return name;
    }

    /**
     * Post-flatten deduplication pass: removes titled component schemas that are structural
     * duplicates of each other (same title + same structural signature under structuralMapper)
     * and rewrites all $refs throughout the spec to point to the canonical (non-numbered) name.
     *
     * This handles the case where the Swagger Parser shares mutable Schema objects across
     * usages of the same external file and mutates their fields (e.g. stripping type or
     * description) between processing passes, causing matchGenerated() to miss during flattening
     * and producing numbered duplicates like FlowSegment_1 alongside FlowSegment.
     */
    private void deduplicateComponents() {
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        if (schemas == null || schemas.isEmpty()) {
            return;
        }

        // Sort: non-numbered names first so we always pick them as canonical over numbered ones.
        List<String> sortedKeys = new ArrayList<>(schemas.keySet());
        sortedKeys.sort((a, b) -> {
            boolean aNumbered = a.matches(".*_\\d+$");
            boolean bNumbered = b.matches(".*_\\d+$");
            if (aNumbered != bNumbered) return aNumbered ? 1 : -1;
            return a.compareTo(b);
        });

        // Map: (title + "||" + structural_sig) → first-seen (canonical) name
        Map<String, String> canonicalBySig = new LinkedHashMap<>();
        // Map: duplicate component name → canonical component name
        Map<String, String> duplicateToCanonical = new LinkedHashMap<>();

        for (String name : sortedKeys) {
            Schema schema = schemas.get(name);
            if (schema.getTitle() == null) {
                continue; // only deduplicate titled schemas — anonymous schemas may be intentionally distinct
            }
            try {
                String structural = computeStructuralSignature(schema);
                String sigKey = schema.getTitle() + "||" + structural;
                String canonical = canonicalBySig.get(sigKey);
                if (canonical != null) {
                    // Only collapse numbered duplicates (e.g. FlowSegment_1) — those are artifacts
                    // the Swagger Parser / inline resolver produced when it failed to reuse an
                    // existing component. A schema the user authored with its own distinct,
                    // non-numbered name is part of the spec's source of truth and must be kept even
                    // when it is structurally identical to another named schema (see #24177: two
                    // schemas 'upper1'/'upper2' with the same title, intentionally distinct).
                    if (name.matches(".*_\\d+$")) {
                        duplicateToCanonical.put(name, canonical);
                    }
                } else {
                    canonicalBySig.put(sigKey, name);
                }
            } catch (JsonProcessingException e) {
                e.printStackTrace();
            }
        }

        if (duplicateToCanonical.isEmpty()) {
            return;
        }

        // Build full-path ref replacement map for rewriting
        Map<String, String> refReplacements = new HashMap<>();
        for (Map.Entry<String, String> entry : duplicateToCanonical.entrySet()) {
            refReplacements.put(
                    "#/components/schemas/" + entry.getKey(),
                    "#/components/schemas/" + entry.getValue());
        }

        // Remove duplicate schemas from components
        for (String duplicate : duplicateToCanonical.keySet()) {
            schemas.remove(duplicate);
            LOGGER.info("Removed duplicate component schema '{}' (structural duplicate of '{}')",
                    duplicate, duplicateToCanonical.get(duplicate));
        }

        // Rewrite all $refs in remaining component schemas
        for (Schema schema : schemas.values()) {
            rewriteSchemaRefs(schema, refReplacements);
        }

        // Rewrite all $refs in paths
        if (openAPI.getPaths() != null) {
            for (PathItem pathItem : openAPI.getPaths().values()) {
                rewritePathItemRefs(pathItem, refReplacements);
            }
        }

        // Rewrite all $refs in webhooks
        if (openAPI.getWebhooks() != null) {
            for (PathItem pathItem : openAPI.getWebhooks().values()) {
                rewritePathItemRefs(pathItem, refReplacements);
            }
        }
    }

    /**
     * Recursively rewrites $ref values in a Schema (and all nested schemas) according to
     * the refReplacements map (full-path refs: "#/components/schemas/Old" → "#/components/schemas/New").
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    private void rewriteSchemaRefs(Schema schema, Map<String, String> refReplacements) {
        if (schema == null) {
            return;
        }
        if (schema.get$ref() != null) {
            String replacement = refReplacements.get(schema.get$ref());
            if (replacement != null) {
                schema.set$ref(replacement);
            }
        }
        if (schema.getProperties() != null) {
            for (Object prop : schema.getProperties().values()) {
                rewriteSchemaRefs((Schema) prop, refReplacements);
            }
        }
        if (schema.getItems() != null) {
            rewriteSchemaRefs(schema.getItems(), refReplacements);
        }
        if (schema.getAllOf() != null) {
            for (Object s : schema.getAllOf()) {
                rewriteSchemaRefs((Schema) s, refReplacements);
            }
        }
        if (schema.getAnyOf() != null) {
            for (Object s : schema.getAnyOf()) {
                rewriteSchemaRefs((Schema) s, refReplacements);
            }
        }
        if (schema.getOneOf() != null) {
            for (Object s : schema.getOneOf()) {
                rewriteSchemaRefs((Schema) s, refReplacements);
            }
        }
        if (schema.getNot() != null) {
            rewriteSchemaRefs(schema.getNot(), refReplacements);
        }
        if (schema.getAdditionalProperties() instanceof Schema) {
            rewriteSchemaRefs((Schema) schema.getAdditionalProperties(), refReplacements);
        }
    }

    /**
     * Rewrites $refs in all operations reachable from a PathItem (request bodies, parameters,
     * responses, and nested callbacks).
     */
    private void rewritePathItemRefs(PathItem pathItem, Map<String, String> refReplacements) {
        if (pathItem == null) {
            return;
        }
        // Path-level parameters
        if (pathItem.getParameters() != null) {
            for (Parameter p : pathItem.getParameters()) {
                rewriteSchemaRefs(p.getSchema(), refReplacements);
            }
        }
        // Operations
        for (Operation operation : pathItem.readOperations()) {
            if (operation.getParameters() != null) {
                for (Parameter p : operation.getParameters()) {
                    rewriteSchemaRefs(p.getSchema(), refReplacements);
                }
            }
            RequestBody requestBody = operation.getRequestBody();
            if (requestBody != null && requestBody.getContent() != null) {
                for (MediaType mediaType : requestBody.getContent().values()) {
                    rewriteSchemaRefs(mediaType.getSchema(), refReplacements);
                }
            }
            ApiResponses responses = operation.getResponses();
            if (responses != null) {
                for (ApiResponse response : responses.values()) {
                    if (response.getContent() != null) {
                        for (MediaType mediaType : response.getContent().values()) {
                            rewriteSchemaRefs(mediaType.getSchema(), refReplacements);
                        }
                    }
                }
            }
            if (operation.getCallbacks() != null) {
                for (Callback callback : operation.getCallbacks().values()) {
                    for (PathItem callbackPathItem : callback.values()) {
                        rewritePathItemRefs(callbackPathItem, refReplacements);
                    }
                }
            }
        }
    }
}
