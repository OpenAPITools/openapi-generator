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
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Collectors;

public class OpenAPINormalizer {
    private OpenAPI openAPI;
    private Map<String, Schema> addedModels = new HashMap<>();
    private Map<String, String> rules = new HashMap<>();

    final Logger LOGGER = LoggerFactory.getLogger(OpenAPINormalizer.class);

    // ============= a list of rules =============
    //final String ALL = "ALL";
    // when set to true, $ref in allOf is treated as parent so that x-parent: true will be added
    // to the schema in $ref (if x-parent is not present)
    final String REF_AS_PARENT_IN_ALLOF = "REF_AS_PARENT_IN_ALLOF";
    // ============= end of rules =============

    /**
     * Initializes OpenAPI Normalizer with a set of rules
     *
     * @param openAPI OpenAPI
     * @param rules a map of rules
     */
    public OpenAPINormalizer(OpenAPI openAPI, Map<String, String> rules) {
        this.openAPI = openAPI;

        if (rules == null) {
            return;
        } else {
            this.rules = rules;
        }
    }

    /**
     * Gets the rule.
     *
     * @return a map of rules
     */
    public Map<String, String> getRules() {
        return this.rules;
    }

    /**
     * Sets the rule.
     *
     * @param rules a map of rules
     */
    public void setRules(Map<String, String> rules) {
        this.rules = rules;
    }

    /**
     * Returns true if the rule is enabled.
     *
     * @param rule name of the rule
     * @return boolean
     */
    public boolean isRuleEnabled(String rule) {
        if ("true".equalsIgnoreCase(this.rules.get(rule))) {
            // true or false
            return true;
        } else if (StringUtils.isNotEmpty(this.rules.get(rule))) {
            // string value, e.g. pascal, snake, original
            return true;
        } else if (StringUtils.isEmpty(this.rules.get(rule))) {
            return false;
        } else {
            return false;
        }
    }

    /**
     * Sets the rule.
     *
     * @param key   rule name
     * @param value rule value
     */
    public void setRule(String key, String value) {
        this.rules.put(key, value);
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
                normalizeRequestBody(operation);
                normalizeParameters(operation);
                normalizeResponses(operation);
            }
        }
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
                normalizeSchema(schema, new HashSet<>());
            }
        }
    }

    /**
     * Normalizes a schema
     *
     * @param schema Schema
     * @param visitedSchemas a set of visited schemas
     */
    public void normalizeSchema(Schema schema, Set<Schema> visitedSchemas) {
        if (schema == null) {
            return;
        }

        if (StringUtils.isNotEmpty(schema.get$ref())) {
            // not need to process $ref
            return;
        }

        if ((visitedSchemas.contains(schema))) {
            return; // skip due to circular reference
        } else {
            visitedSchemas.add(schema);
        }

        if (schema instanceof ArraySchema) {
            if (schema.getItems() != null) {
                normalizeSchema(schema.getItems(), visitedSchemas);
            } else {
                LOGGER.warn("Array item cannot be null: {}", schema);
            }

        } else if (schema.getAdditionalProperties() instanceof Schema) { // map
            if (schema.getAdditionalProperties() != null) {
                normalizeSchema((Schema) schema.getAdditionalProperties(), visitedSchemas);
            } else {
                LOGGER.warn("Map item cannot be null: {}", schema);
            }
        } else if (ModelUtils.isComposedSchema(schema)) {
            ComposedSchema m = (ComposedSchema) schema;
            if (m.getAllOf() != null && !m.getAllOf().isEmpty()) {
                normalizeAllOf(m, visitedSchemas);
            }

            if (m.getOneOf() != null && !m.getOneOf().isEmpty()) {
                normalizeOneOf(m, visitedSchemas);
            }

            if (m.getAnyOf() != null && !m.getAnyOf().isEmpty()) {
                normalizeAnyOf(m, visitedSchemas);
            }

            if (m.getProperties() != null && !m.getProperties().isEmpty()) {
                normalizeProperties(m.getProperties(), visitedSchemas);
            }

            if (m.getAdditionalProperties() != null) {
                // normalizeAdditionalProperties(m);
            }
        } else if (schema.getNot() != null) {// not schema
            normalizeSchema(schema.getNot(), visitedSchemas);
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            normalizeProperties(schema.getProperties(), visitedSchemas);
        } else if (schema instanceof Schema) {
            normalizeNonComposedSchema(schema, visitedSchemas);
        } else {
            throw new RuntimeException("Unknown schema type found in normalizer: " + schema);
        }
    }

    private void normalizeNonComposedSchema(Schema schema, Set<Schema> visitedSchemas) {

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

    private void normalizeAllOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize allOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
        processUseAllOfRefAsParent(schema);
    }

    private void normalizeOneOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize oenOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
    }

    private void normalizeAnyOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize anyOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
    }

    // ===================== a list of rules =====================
    private void processUseAllOfRefAsParent(Schema schema) {
        if (!isRuleEnabled(REF_AS_PARENT_IN_ALLOF)) {
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
                    refSchema.getExtensions().put("x-parent", true);
                } else {
                    if (refSchema.getExtensions().containsKey("x-parent")) {
                        // doing nothing as x-parent already exists
                    } else {
                        refSchema.getExtensions().put("x-parent", true);
                    }
                }
                LOGGER.debug("processUseAllOfRefAsParent added `x-parent: true` to {}", refSchema);
            }
        }
    }
    // ===================== end of rules =====================
}
