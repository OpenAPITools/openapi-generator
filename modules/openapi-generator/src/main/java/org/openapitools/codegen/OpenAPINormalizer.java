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

import io.swagger.v3.oas.models.*;
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
import java.util.stream.Collectors;

public class OpenAPINormalizer {
    private OpenAPI openAPI;
    private Map<String, String> rules = new HashMap<>();

    final Logger LOGGER = LoggerFactory.getLogger(OpenAPINormalizer.class);

    // ============= a list of rules =============
    // when set to true, all rules are enabled
    final String ALL = "ALL";
    boolean enableAll;

    // when set to true, $ref in allOf is treated as parent so that x-parent: true will be added
    // to the schema in $ref (if x-parent is not present)
    final String REF_AS_PARENT_IN_ALLOF = "REF_AS_PARENT_IN_ALLOF";
    boolean enableRefAsParentInAllOf;

    // when set to true, only keep the first tag in operation if there are more than one tag defined.
    final String KEEP_ONLY_FIRST_TAG_IN_OPERATION = "KEEP_ONLY_FIRST_TAG_IN_OPERATION";
    boolean enableKeepOnlyFirstTagInOperation;

    // when set to true, complex composed schemas (a mix of oneOf/anyOf/anyOf and properties) with
    // oneOf/anyOf containing only `required` and no properties (these are properties inter-dependency rules)
    // are removed as most generators cannot handle such case at the moment
    final String REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY = "REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY";
    boolean removeAnyOfOneOfAndKeepPropertiesOnly;

    // when set to true, oneOf/anyOf with either string or enum string as sub schemas will be simplified
    // to just string
    final String SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING = "SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING";
    boolean simplifyAnyOfStringAndEnumString;

    // ============= end of rules =============

    /**
     * Initializes OpenAPI Normalizer with a set of rules
     *
     * @param openAPI OpenAPI
     * @param rules   a map of rules
     */
    public OpenAPINormalizer(OpenAPI openAPI, Map<String, String> rules) {
        this.openAPI = openAPI;
        this.rules = rules;
        parseRules(rules);
    }

    /**
     * Parses the rules.
     *
     * @param rules a map of rules
     */
    public void parseRules(Map<String, String> rules) {
        if (rules == null) {
            return;
        }

        if ("true".equalsIgnoreCase(rules.get(ALL))) {
            enableAll = true;
        }

        if (enableAll || "true".equalsIgnoreCase(rules.get(REF_AS_PARENT_IN_ALLOF))) {
            enableRefAsParentInAllOf = true;
        }

        if (enableAll || "true".equalsIgnoreCase(rules.get(KEEP_ONLY_FIRST_TAG_IN_OPERATION))) {
            enableKeepOnlyFirstTagInOperation = true;
        }

        if (enableAll || "true".equalsIgnoreCase(rules.get(REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY))) {
            removeAnyOfOneOfAndKeepPropertiesOnly = true;
        }

        if (enableAll || "true".equalsIgnoreCase(rules.get(SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING))) {
            simplifyAnyOfStringAndEnumString = true;
        }
    }

    /**
     * Normalizes the OpenAPI input, which may not perfectly conform to
     * the specification.
     */
    void normalize() {
        if (rules == null || rules.isEmpty()) {
            return;
        }

        if (this.openAPI.getComponents() == null) {
            this.openAPI.setComponents(new Components());
        }

        if (this.openAPI.getComponents().getSchemas() == null) {
            this.openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        normalizePaths();
        normalizeComponents();
    }

    /**
     * Normalizes inline models in Paths
     */
    private void normalizePaths() {
        Paths paths = openAPI.getPaths();
        if (paths == null) {
            return;
        }

        for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
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
                normalizeOperation(operation);
                normalizeRequestBody(operation);
                normalizeParameters(operation);
                normalizeResponses(operation);
            }
        }
    }

    /**
     * Normalizes operation
     *
     * @param operation Operation
     */
    private void normalizeOperation(Operation operation) {
        processKeepOnlyFirstTagInOperation(operation);
    }

    /**
     * Normalizes schemas in content
     *
     * @param content target content
     */
    private void normalizeContent(Content content) {
        if (content == null || content.isEmpty()) {
            return;
        }

        for (String contentType : content.keySet()) {
            MediaType mediaType = content.get(contentType);
            if (mediaType == null) {
                continue;
            } else if (mediaType.getSchema() == null) {
                continue;
            } else {
                normalizeSchema(mediaType.getSchema(), new HashSet<>());
            }
        }
    }

    /**
     * Normalizes schemas in RequestBody
     *
     * @param operation target operation
     */
    private void normalizeRequestBody(Operation operation) {
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

        normalizeContent(requestBody.getContent());
    }

    /**
     * Normalizes schemas in parameters
     *
     * @param operation target operation
     */
    private void normalizeParameters(Operation operation) {
        List<Parameter> parameters = operation.getParameters();
        if (parameters == null) {
            return;
        }

        for (Parameter parameter : parameters) {
            if (parameter.getSchema() == null) {
                continue;
            } else {
                normalizeSchema(parameter.getSchema(), new HashSet<>());
            }
        }
    }

    /**
     * Normalizes schemas in ApiResponses
     *
     * @param operation target operation
     */
    private void normalizeResponses(Operation operation) {
        ApiResponses responses = operation.getResponses();
        if (responses == null) {
            return;
        }

        for (Map.Entry<String, ApiResponse> responsesEntry : responses.entrySet()) {
            if (responsesEntry.getValue() == null) {
                continue;
            } else {
                normalizeContent(responsesEntry.getValue().getContent());
            }
        }
    }

    /**
     * Normalizes schemas in components
     */
    private void normalizeComponents() {
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        if (schemas == null) {
            return;
        }

        List<String> schemaNames = new ArrayList<String>(schemas.keySet());
        for (String schemaName : schemaNames) {
            Schema schema = schemas.get(schemaName);
            if (schema == null) {
                LOGGER.warn("{} not fount found in openapi/components/schemas.", schemaName);
            } else {
                Schema result = normalizeSchema(schema, new HashSet<>());
                schemas.put(schemaName, result);
            }
        }
    }

    /**
     * Normalizes a schema
     *
     * @param schema         Schema
     * @param visitedSchemas a set of visited schemas
     * @return Schema
     */
    public Schema normalizeSchema(Schema schema, Set<Schema> visitedSchemas) {
        if (schema == null) {
            return schema;
        }

        if (StringUtils.isNotEmpty(schema.get$ref())) {
            // not need to process $ref
            return schema;
        }

        if ((visitedSchemas.contains(schema))) {
            return schema; // skip due to circular reference
        } else {
            visitedSchemas.add(schema);
        }

        if (schema instanceof ArraySchema) {
            normalizeSchema(schema.getItems(), visitedSchemas);
        } else if (schema.getAdditionalProperties() instanceof Schema) { // map
            normalizeSchema((Schema) schema.getAdditionalProperties(), visitedSchemas);
        } else if (ModelUtils.isComposedSchema(schema)) {
            ComposedSchema cs = (ComposedSchema) schema;

            if (ModelUtils.isComplexComposedSchema(cs)) {
                cs = (ComposedSchema) normalizeComplexComposedSchema(cs, visitedSchemas);
            }

            if (cs.getAllOf() != null && !cs.getAllOf().isEmpty()) {
                return normalizeAllOf(cs, visitedSchemas);
            }

            if (cs.getOneOf() != null && !cs.getOneOf().isEmpty()) {
                return normalizeOneOf(cs, visitedSchemas);
            }

            if (cs.getAnyOf() != null && !cs.getAnyOf().isEmpty()) {
                return normalizeAnyOf(cs, visitedSchemas);
            }

            if (cs.getProperties() != null && !cs.getProperties().isEmpty()) {
                normalizeProperties(cs.getProperties(), visitedSchemas);
            }

            if (cs.getAdditionalProperties() != null) {
                // normalizeAdditionalProperties(m);
            }

            return cs;
        } else if (schema.getNot() != null) {// not schema
            normalizeSchema(schema.getNot(), visitedSchemas);
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            normalizeProperties(schema.getProperties(), visitedSchemas);
        } else if (schema instanceof Schema) {
            normalizeSchemaWithOnlyProperties(schema, visitedSchemas);
        } else {
            throw new RuntimeException("Unknown schema type found in normalizer: " + schema);
        }

        return schema;
    }

    private void normalizeSchemaWithOnlyProperties(Schema schema, Set<Schema> visitedSchemas) {
        // normalize non-composed schema (e.g. schema with only properties)
    }

    private void normalizeProperties(Map<String, Schema> properties, Set<Schema> visitedSchemas) {
        if (properties == null) {
            return;
        }
        for (Map.Entry<String, Schema> propertiesEntry : properties.entrySet()) {
            Schema property = propertiesEntry.getValue();
            normalizeSchema(property, visitedSchemas);
        }
    }

    private Schema normalizeAllOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize allOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
        processUseAllOfRefAsParent(schema);

        return schema;
    }

    private Schema normalizeOneOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getOneOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize oenOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }

        // process rules here
        return schema;
    }

    private Schema normalizeAnyOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAnyOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize anyOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }

        // process rules here

        // last rule to process as the schema may become String schema (not "anyOf") after the completion
        return processSimplifyAnyOfStringAndEnumString(schema);
    }

    private Schema normalizeComplexComposedSchema(Schema schema, Set<Schema> visitedSchemas) {

        processRemoveAnyOfOneOfAndKeepPropertiesOnly(schema);

        return schema;
    }

    // ===================== a list of rules =====================
    // all rules (fuctions) start with the word "process"

    /**
     * Child schemas in `allOf` is considered a parent if it's a `$ref` (instead of inline schema).
     *
     * @param schema Schema
     */
    private void processUseAllOfRefAsParent(Schema schema) {
        if (!enableRefAsParentInAllOf && !enableAll) {
            return;
        }

        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            Schema s = (Schema) item;

            if (StringUtils.isNotEmpty(s.get$ref())) {
                String ref = ModelUtils.getSimpleRef(s.get$ref());
                // TODO need to check for requestBodies?
                Schema refSchema = openAPI.getComponents().getSchemas().get(ref);
                if (refSchema == null) {
                    throw new RuntimeException("schema cannot be null with ref " + ref);
                }
                if (refSchema.getExtensions() == null) {
                    refSchema.setExtensions(new HashMap<>());
                }

                if (refSchema.getExtensions().containsKey("x-parent")) {
                    // doing nothing as x-parent already exists
                } else {
                    refSchema.getExtensions().put("x-parent", true);
                }

                LOGGER.debug("processUseAllOfRefAsParent added `x-parent: true` to {}", refSchema);
            }
        }
    }

    /**
     * Keep only first tag in the operation if the operation has more than
     * one tag.
     *
     * @param operation Operation
     */
    private void processKeepOnlyFirstTagInOperation(Operation operation) {
        if (!enableKeepOnlyFirstTagInOperation) {
            return;
        }

        if (operation.getTags() != null && !operation.getTags().isEmpty() && operation.getTags().size() > 1) {
            // has more than 1 tag
            String firstTag = operation.getTags().get(0);
            operation.setTags(null);
            operation.addTagsItem(firstTag);
        }
    }

    /**
     * If the schema contains anyOf/oneOf and properties, remove oneOf/anyOf as these serve as rules to
     * ensure inter-dependency between properties. It's a workaround as such validation is not supported at the moment.
     *
     * @param schema Schema
     */
    private void processRemoveAnyOfOneOfAndKeepPropertiesOnly(Schema schema) {

        if (!removeAnyOfOneOfAndKeepPropertiesOnly && !enableAll) {
            return;
        }

        if (((schema.getOneOf() != null && !schema.getOneOf().isEmpty())
                || (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty())) // has anyOf or oneOf
                && (schema.getProperties() != null && !schema.getProperties().isEmpty()) // has properties
                && schema.getAllOf() == null) { // not allOf
            // clear oneOf, anyOf
            schema.setOneOf(null);
            schema.setAnyOf(null);
        }
    }

    /**
     * If the schema is anyOf and the sub-schemas are either string or enum of string,
     * then simply it to just string as many generators do not yet support anyOf.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
        if (!simplifyAnyOfStringAndEnumString && !enableAll) {
            return schema;
        }

        Schema s0 = null, s1 = null;
        if (schema.getAnyOf().size() == 2) {
            s0 = ModelUtils.unaliasSchema(openAPI, (Schema) schema.getAnyOf().get(0));
            s1 = ModelUtils.unaliasSchema(openAPI, (Schema) schema.getAnyOf().get(1));
        } else {
            return schema;
        }

        s0 = ModelUtils.getReferencedSchema(openAPI, s0);
        s1 = ModelUtils.getReferencedSchema(openAPI, s1);

        // find the string schema (not enum)
        if (s0 instanceof StringSchema && s1 instanceof StringSchema) {
            if (((StringSchema) s0).getEnum() != null) { // s0 is enum, s1 is string
                return (StringSchema) s1;
            } else if (((StringSchema) s1).getEnum() != null) { // s1 is enum, s0 is string
                return (StringSchema) s0;
            } else { // both are string
                return schema;
            }
        } else {
            return schema;
        }
    }

    // ===================== end of rules =====================
}
