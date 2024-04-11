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
import io.swagger.v3.oas.models.headers.Header;
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
    private Map<String, String> inputRules = new HashMap<>();
    private Map<String, Boolean> rules = new HashMap<>();

    private TreeSet<String> anyTypeTreeSet = new TreeSet<>();

    final Logger LOGGER = LoggerFactory.getLogger(OpenAPINormalizer.class);

    Set<String> ruleNames = new TreeSet<>();
    Set<String> rulesDefaultToTrue = new TreeSet<>();

    // ============= a list of rules =============
    // when set to true, all rules (true or false) are enabled
    final String ENABLE_ALL = "ENABLE_ALL";
    boolean enableAll;

    // when set to true, all rules (true or false) are disabled
    final String DISABLE_ALL = "DISABLE_ALL";
    boolean disableAll;

    // when set to true, $ref in allOf is treated as parent so that x-parent: true will be added
    // to the schema in $ref (if x-parent is not present)
    final String REF_AS_PARENT_IN_ALLOF = "REF_AS_PARENT_IN_ALLOF";

    // when set to true, only keep the first tag in operation if there are more than one tag defined.
    final String KEEP_ONLY_FIRST_TAG_IN_OPERATION = "KEEP_ONLY_FIRST_TAG_IN_OPERATION";

    // when set to true, complex composed schemas (a mix of oneOf/anyOf/anyOf and properties) with
    // oneOf/anyOf containing only `required` and no properties (these are properties inter-dependency rules)
    // are removed as most generators cannot handle such case at the moment
    final String REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY = "REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY";

    // when set to true, oneOf/anyOf with either string or enum string as sub schemas will be simplified
    // to just string
    final String SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING = "SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING";

    // when set to true, oneOf/anyOf schema with only one sub-schema is simplified to just the sub-schema
    // and if sub-schema contains "null", remove it and set nullable to true instead
    // and if sub-schema contains enum of "null", remove it and set nullable to true instead
    final String SIMPLIFY_ONEOF_ANYOF = "SIMPLIFY_ONEOF_ANYOF";

    // when set to true, boolean enum will be converted to just boolean
    final String SIMPLIFY_BOOLEAN_ENUM = "SIMPLIFY_BOOLEAN_ENUM";

    // when set to a string value, tags in all operations will be reset to the string value provided
    final String SET_TAGS_FOR_ALL_OPERATIONS = "SET_TAGS_FOR_ALL_OPERATIONS";
    String setTagsForAllOperations;

    // when set to true, tags in all operations will be set to operationId or "default" if operationId
    // is empty
    final String SET_TAGS_TO_OPERATIONID = "SET_TAGS_TO_OPERATIONID";
    String setTagsToOperationId;

    // when set to true, auto fix integer with maximum value 4294967295 (2^32-1) or long with 18446744073709551615 (2^64-1)
    // by adding x-unsigned to the schema
    final String ADD_UNSIGNED_TO_INTEGER_WITH_INVALID_MAX_VALUE = "ADD_UNSIGNED_TO_INTEGER_WITH_INVALID_MAX_VALUE";

    // when set to true, refactor schema with allOf and properties in the same level to a schema with allOf only and
    // the allOf contains a new schema containing the properties in the top level
    final String REFACTOR_ALLOF_WITH_PROPERTIES_ONLY = "REFACTOR_ALLOF_WITH_PROPERTIES_ONLY";

    // when set to true, normalize OpenAPI 3.1 spec to make it work with the generator
    final String NORMALIZE_31SPEC = "NORMALIZE_31SPEC";

    // when set to true, remove x-internal: true from models, operations
    final String REMOVE_X_INTERNAL = "REMOVE_X_INTERNAL";
    final String X_INTERNAL = "x-internal";
    boolean removeXInternal;

    // when set (e.g. operationId:getPetById|addPet), filter out (or remove) everything else
    final String FILTER = "FILTER";
    HashSet<String> operationIdFilters = new HashSet<>();

    // when set (e.g. operationId:getPetById|addPet), filter out (or remove) everything else
    final String SET_CONTAINER_TO_NULLABLE = "SET_CONTAINER_TO_NULLABLE";
    HashSet<String> setContainerToNullable = new HashSet<>();
    boolean updateArrayToNullable;
    boolean updateSetToNullable;
    boolean updateMapToNullable;

    // when set (e.g. operationId:getPetById|addPet), filter out (or remove) everything else
    final String SET_PRIMITIVE_TYPES_TO_NULLABLE = "SET_PRIMITIVE_TYPES_TO_NULLABLE";
    HashSet<String> setPrimitiveTypesToNullable = new HashSet<>();
    boolean updateStringToNullable;
    boolean updateIntegerToNullable;
    boolean updateNumberToNullable;
    boolean updateBooleanToNullable;

    // ============= end of rules =============

    /**
     * Initializes OpenAPI Normalizer with a set of rules
     *
     * @param openAPI    OpenAPI
     * @param inputRules a map of rules
     */
    public OpenAPINormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
        this.openAPI = openAPI;
        this.inputRules = inputRules;

        if (Boolean.parseBoolean(inputRules.get(DISABLE_ALL))) {
            this.disableAll = true;
            return; // skip the rest
        }

        // a set of ruleNames
        ruleNames.add(REF_AS_PARENT_IN_ALLOF);
        ruleNames.add(REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY);
        ruleNames.add(SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING);
        ruleNames.add(SIMPLIFY_ONEOF_ANYOF);
        ruleNames.add(SIMPLIFY_BOOLEAN_ENUM);
        ruleNames.add(KEEP_ONLY_FIRST_TAG_IN_OPERATION);
        ruleNames.add(SET_TAGS_FOR_ALL_OPERATIONS);
        ruleNames.add(SET_TAGS_TO_OPERATIONID);
        ruleNames.add(ADD_UNSIGNED_TO_INTEGER_WITH_INVALID_MAX_VALUE);
        ruleNames.add(REFACTOR_ALLOF_WITH_PROPERTIES_ONLY);
        ruleNames.add(NORMALIZE_31SPEC);
        ruleNames.add(REMOVE_X_INTERNAL);
        ruleNames.add(FILTER);
        ruleNames.add(SET_CONTAINER_TO_NULLABLE);
        ruleNames.add(SET_PRIMITIVE_TYPES_TO_NULLABLE);


        // rules that are default to true
        rules.put(SIMPLIFY_ONEOF_ANYOF, true);
        rules.put(SIMPLIFY_BOOLEAN_ENUM, true);

        processRules(inputRules);

        // represent any type in tree set
        anyTypeTreeSet.add("string");
        anyTypeTreeSet.add("number");
        anyTypeTreeSet.add("integer");
        anyTypeTreeSet.add("boolean");
        anyTypeTreeSet.add("object");
        anyTypeTreeSet.add("array");
    }

    /**
     * Get the rule.
     *
     * @param ruleName the name of the rule
     * @return true if the rule is set
     */
    public boolean getRule(String ruleName) {
        if (!rules.containsKey(ruleName)) {
            return false;
        }
        return rules.get(ruleName);
    }

    /**
     * Process the rules.
     *
     * @param inputRules a map of rules
     */
    public void processRules(Map<String, String> inputRules) {
        if (Boolean.TRUE.equals(rules.get("enableAll"))) {
            enableAll = true;
        }

        // loop through all the rules
        for (Map.Entry<String, String> rule : inputRules.entrySet()) {
            LOGGER.debug("processing rule {} => {}", rule.getKey(), rule.getValue());
            if (!ruleNames.contains(rule.getKey())) { // invalid rule name
                LOGGER.warn("Invalid openapi-normalizer rule name: {}", rule.getKey());
            } else if (enableAll) {
                rules.put(rule.getKey(), true); // set rule
            } else {
                rules.put(rule.getKey(), Boolean.parseBoolean(rule.getValue()));
            }
        }

        // non-boolean rule(s)
        setTagsForAllOperations = inputRules.get(SET_TAGS_FOR_ALL_OPERATIONS);
        if (setTagsForAllOperations != null) {
            rules.put(SET_TAGS_FOR_ALL_OPERATIONS, true);
        }

        if (inputRules.get(FILTER) != null) {
            rules.put(FILTER, true);

            String[] filterStrs = inputRules.get(FILTER).split(":");
            if (filterStrs.length != 2) { // only support operationId with : at the moment
                LOGGER.error("FILTER rule must be in the form of `operationId:name1|name2|name3`: {}", inputRules.get(FILTER));
            } else {
                if ("operationId".equals(filterStrs[0])) {
                    operationIdFilters = new HashSet<>(Arrays.asList(filterStrs[1].split("[|]")));
                } else {
                    LOGGER.error("FILTER rule must be in the form of `operationId:name1|name2|name3`: {}", inputRules.get(FILTER));
                }
            }
        }

        if (inputRules.get(SET_CONTAINER_TO_NULLABLE) != null) {
            rules.put(SET_CONTAINER_TO_NULLABLE, true);
            setContainerToNullable = new HashSet<>(Arrays.asList(inputRules.get(SET_CONTAINER_TO_NULLABLE).split("[|]")));
            if (setContainerToNullable.contains("array")) {
                updateArrayToNullable = true;
            }
            if (setContainerToNullable.contains("set")) {
                updateSetToNullable = true;
            }
            if (setContainerToNullable.contains("map")) {
                updateMapToNullable = true;
            }
            if (!updateArrayToNullable && !updateSetToNullable && !updateMapToNullable) {
                LOGGER.error("SET_CONTAINER_TO_NULLABLE rule must be in the form of `array|set|map`, e.g. `set`, `array|map`: {}", inputRules.get(SET_CONTAINER_TO_NULLABLE));
            }
        }

        if (inputRules.get(SET_PRIMITIVE_TYPES_TO_NULLABLE) != null) {
            rules.put(SET_PRIMITIVE_TYPES_TO_NULLABLE, true);
            setPrimitiveTypesToNullable = new HashSet<>(Arrays.asList(inputRules.get(SET_PRIMITIVE_TYPES_TO_NULLABLE).split("[|]")));
            if (setPrimitiveTypesToNullable.contains("string")) {
                updateStringToNullable = true;
            }
            if (setPrimitiveTypesToNullable.contains("integer")) {
                updateIntegerToNullable = true;
            }
            if (setPrimitiveTypesToNullable.contains("number")) {
                updateNumberToNullable = true;
            }
            if (setPrimitiveTypesToNullable.contains("boolean")) {
                updateBooleanToNullable = true;
            }
            if (!updateStringToNullable && !updateIntegerToNullable && !updateNumberToNullable && !updateBooleanToNullable) {
                LOGGER.error("SET_PRIMITIVE_TYPES_TO_NULLABLE rule must be in the form of `string|integer|number|boolean`, e.g. `string`, `integer|number`: {}", inputRules.get(SET_PRIMITIVE_TYPES_TO_NULLABLE));
            }
        }
    }

    /**
     * Normalizes the OpenAPI input, which may not perfectly conform to
     * the specification.
     */
    void normalize() {
        if (rules == null || rules.isEmpty() || disableAll) {
            return;
        }

        if (this.openAPI.getComponents() == null) {
            this.openAPI.setComponents(new Components());
        }

        if (this.openAPI.getComponents().getSchemas() == null) {
            this.openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        normalizePaths();
        normalizeComponentsSchemas();
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

            // normalize PathItem common parameters
            normalizeParameters(path.getParameters());

            for (Operation operation : operations) {
                if (operationIdFilters.size() > 0) {
                    if (operationIdFilters.contains(operation.getOperationId())) {
                        operation.addExtension("x-internal", false);
                    } else {
                        LOGGER.info("operation `{}` marked as internal only (x-internal: true) by the FILTER", operation.getOperationId());
                        operation.addExtension("x-internal", true);
                    }
                }

                normalizeOperation(operation);
                normalizeRequestBody(operation);
                normalizeParameters(operation.getParameters());
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
        processRemoveXInternalFromOperation(operation);

        processKeepOnlyFirstTagInOperation(operation);

        processSetTagsForAllOperations(operation);

        processSetTagsToOperationId(operation);
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
                Schema newSchema = normalizeSchema(mediaType.getSchema(), new HashSet<>());
                mediaType.setSchema(newSchema);
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
     * @param parameters List parameters
     */
    private void normalizeParameters(List<Parameter> parameters) {
        if (parameters == null) {
            return;
        }

        for (Parameter parameter : parameters) {
            // dereference parameter
            if (StringUtils.isNotEmpty(parameter.get$ref())) {
                parameter = ModelUtils.getReferencedParameter(openAPI, parameter);
            }

            if (parameter.getSchema() == null) {
                continue;
            } else {
                Schema newSchema = normalizeSchema(parameter.getSchema(), new HashSet<>());
                parameter.setSchema(newSchema);
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
                normalizeContent(ModelUtils.getReferencedApiResponse(openAPI, responsesEntry.getValue()).getContent());
                normalizeHeaders(ModelUtils.getReferencedApiResponse(openAPI, responsesEntry.getValue()).getHeaders());
            }
        }
    }

    /**
     * Normalizes schemas in headers
     *
     * @param headers a map of headers
     */
    private void normalizeHeaders(Map<String, Header> headers) {
        if (headers == null || headers.isEmpty()) {
            return;
        }

        for (String headerKey : headers.keySet()) {
            Header h = headers.get(headerKey);
            Schema updatedHeader = normalizeSchema(h.getSchema(), new HashSet<>());
            h.setSchema(updatedHeader);
        }
    }

    /**
     * Normalizes schemas in components
     */
    private void normalizeComponentsSchemas() {
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
                // remove x-internal if needed
                if (schema.getExtensions() != null && getRule(REMOVE_X_INTERNAL)) {
                    if (Boolean.parseBoolean(String.valueOf(schema.getExtensions().get(X_INTERNAL)))) {
                        schema.getExtensions().remove(X_INTERNAL);
                    }
                }

                // normalize the schemas
                schemas.put(schemaName, normalizeSchema(schema, new HashSet<>()));
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
            // no need to process $ref
            return schema;
        }

        if ((visitedSchemas.contains(schema))) {
            return schema; // skip due to circular reference
        } else {
            visitedSchemas.add(schema);
        }

        if (ModelUtils.isArraySchema(schema)) { // array
            Schema result = normalizeArraySchema(schema);
            normalizeSchema(result.getItems(), visitedSchemas);
            return result;
        } else if (schema.getAdditionalProperties() instanceof Schema) { // map
            normalizeMapSchema(schema);
            normalizeSchema((Schema) schema.getAdditionalProperties(), visitedSchemas);
        } else if (ModelUtils.isOneOf(schema)) { // oneOf
            return normalizeOneOf(schema, visitedSchemas);
        } else if (ModelUtils.isAnyOf(schema)) { // anyOf
            return normalizeAnyOf(schema, visitedSchemas);
        } else if (ModelUtils.isAllOfWithProperties(schema)) { // allOf with properties
            schema = normalizeAllOfWithProperties(schema, visitedSchemas);
            normalizeSchema(schema, visitedSchemas);
        } else if (ModelUtils.isAllOf(schema)) { // allOf
            return normalizeAllOf(schema, visitedSchemas);
        } else if (ModelUtils.isComposedSchema(schema)) { // composed schema
            if (ModelUtils.isComplexComposedSchema(schema)) {
                schema = normalizeComplexComposedSchema(schema, visitedSchemas);
            }

            if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
                return normalizeAllOf(schema, visitedSchemas);
            }

            if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
                return normalizeOneOf(schema, visitedSchemas);
            }

            if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
                return normalizeAnyOf(schema, visitedSchemas);
            }

            if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
                normalizeProperties(schema.getProperties(), visitedSchemas);
            }

            if (schema.getAdditionalProperties() != null) {
                // normalizeAdditionalProperties(m);
            }

            return schema;
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            normalizeProperties(schema.getProperties(), visitedSchemas);
        } else if (schema instanceof BooleanSchema) {
            normalizeBooleanSchema(schema, visitedSchemas);
        } else if (schema instanceof IntegerSchema) {
            normalizeIntegerSchema(schema, visitedSchemas);
        } else if (schema instanceof Schema) {
            return normalizeSimpleSchema(schema, visitedSchemas);
        } else {
            throw new RuntimeException("Unknown schema type found in normalizer: " + schema);
        }

        return schema;
    }

    private Schema normalizeArraySchema(Schema schema) {
        Schema result = processNormalize31Spec(schema, new HashSet<>());
        return processSetArraytoNullable(result);
    }

    private Schema normalizeMapSchema(Schema schema) {
        return processSetMapToNullable(schema);
    }

    private Schema normalizeSimpleSchema(Schema schema, Set<Schema> visitedSchemas) {
        Schema result = processNormalize31Spec(schema, visitedSchemas);
        return processSetPrimitiveTypesToNullable(result);
    }

    private void normalizeBooleanSchema(Schema schema, Set<Schema> visitedSchemas) {
        processSimplifyBooleanEnum(schema);
        processSetPrimitiveTypesToNullable(schema);
    }

    private void normalizeIntegerSchema(Schema schema, Set<Schema> visitedSchemas) {
        processAddUnsignedToIntegerWithInvalidMaxValue(schema);
        processSetPrimitiveTypesToNullable(schema);
    }

    private void normalizeProperties(Map<String, Schema> properties, Set<Schema> visitedSchemas) {
        if (properties == null) {
            return;
        }
        for (Map.Entry<String, Schema> propertiesEntry : properties.entrySet()) {
            Schema property = propertiesEntry.getValue();
            Schema newProperty = normalizeSchema(property, new HashSet<>());
            propertiesEntry.setValue(newProperty);
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

    private Schema normalizeAllOfWithProperties(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize allOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
        schema = processRefactorAllOfWithPropertiesOnly(schema);

        return schema;
    }

    private Schema normalizeOneOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getOneOf()) {
            if (item == null) {
                continue;
            }
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! oneOf schema is not of the type Schema: " + item);
            }
            // normalize oenOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }
        // process rules here
        schema = processSimplifyOneOf(schema);

        return schema;
    }

    private Schema normalizeAnyOf(Schema schema, Set<Schema> visitedSchemas) {
        for (Object item : schema.getAnyOf()) {
            if (item == null) {
                continue;
            }

            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! anyOf schema is not of the type Schema: " + item);
            }
            // normalize anyOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }

        // process rules here
        schema = processSimplifyAnyOf(schema);

        // last rule to process as the schema may become String schema (not "anyOf") after the completion
        return processSimplifyAnyOfStringAndEnumString(schema);
    }

    private Schema normalizeComplexComposedSchema(Schema schema, Set<Schema> visitedSchemas) {
        // loop through properties, if any
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            normalizeProperties(schema.getProperties(), visitedSchemas);
        }

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
        if (!getRule(REF_AS_PARENT_IN_ALLOF)) {
            return;
        }

        if (schema.getAllOf() == null) {
            return;
        }

        if (schema.getAllOf().size() == 1) {
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
    private void processRemoveXInternalFromOperation(Operation operation) {
        if (!getRule(REMOVE_X_INTERNAL)) {
            return;
        }

        if (operation.getExtensions() == null) {
            return;
        }

        if (Boolean.parseBoolean(String.valueOf(operation.getExtensions().get("x-internal")))) {
            operation.getExtensions().remove(X_INTERNAL);
        }
    }

    /**
     * Keep only first tag in the operation if the operation has more than
     * one tag.
     *
     * @param operation Operation
     */
    private void processKeepOnlyFirstTagInOperation(Operation operation) {
        if (!getRule(KEEP_ONLY_FIRST_TAG_IN_OPERATION)) {
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
     * Set the tag name for all operations
     *
     * @param operation Operation
     */
    private void processSetTagsForAllOperations(Operation operation) {
        if (StringUtils.isEmpty(setTagsForAllOperations)) {
            return;
        }

        operation.setTags(null);
        operation.addTagsItem(setTagsForAllOperations);
    }

    /**
     * Set the tag name to operationId (or "default" if operationId is empty)
     *
     * @param operation Operation
     */
    private void processSetTagsToOperationId(Operation operation) {
        if (!getRule(SET_TAGS_TO_OPERATIONID)) {
            return;
        }

        operation.setTags(null);
        if (StringUtils.isNotEmpty(operation.getOperationId())) {
            operation.addTagsItem(operation.getOperationId());
        } else { // default to "default" if operationId is empty
            operation.addTagsItem("default");
        }
    }

    /**
     * If the schema contains anyOf/oneOf and properties, remove oneOf/anyOf as these serve as rules to
     * ensure inter-dependency between properties. It's a workaround as such validation is not supported at the moment.
     *
     * @param schema Schema
     */
    private void processRemoveAnyOfOneOfAndKeepPropertiesOnly(Schema schema) {
        if (!getRule(REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY)) {
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
     * then simplify it to just enum of string as many generators do not yet support anyOf.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
        if (!getRule(SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING)) {
            return schema;
        }

        if (schema.getAnyOf() == null) {
            // ComposedSchema, Schema with `type: null`
            return schema;
        }

        Schema result = null, s0 = null, s1 = null;
        if (schema.getAnyOf().size() == 2) {
            s0 = ModelUtils.unaliasSchema(openAPI, (Schema) schema.getAnyOf().get(0));
            s1 = ModelUtils.unaliasSchema(openAPI, (Schema) schema.getAnyOf().get(1));
        } else {
            return schema;
        }

        s0 = ModelUtils.getReferencedSchema(openAPI, s0);
        s1 = ModelUtils.getReferencedSchema(openAPI, s1);

        // find the string schema (enum)
        if (s0 instanceof StringSchema && s1 instanceof StringSchema) {
            if (((StringSchema) s0).getEnum() != null) { // s0 is enum, s1 is string
                result = (StringSchema) s0;
            } else if (((StringSchema) s1).getEnum() != null) { // s1 is enum, s0 is string
                result = (StringSchema) s1;
            } else { // both are string
                result = schema;
            }
        } else {
            result = schema;
        }

        // set nullable
        if (schema.getNullable() != null) {
            result.setNullable(schema.getNullable());
        }

        // set default
        if (schema.getDefault() != null) {
            result.setDefault(schema.getDefault());
        }

        return result;
    }

    /**
     * Check if the schema is of type 'null'
     * <p>
     * Return true if the schema's type is 'null' or not specified
     *
     * @param schema Schema
     */
    public boolean isNullTypeSchema(Schema schema) {
        if (schema == null) {
            return true;
        }

        if (ModelUtils.hasAllOf(schema) || ModelUtils.hasOneOf(schema) || ModelUtils.hasAnyOf(schema)) {
            return false;
        }

        if (schema.getTypes() != null && !schema.getTypes().isEmpty()) {
            // 3.1 spec
            if (schema.getTypes().size() == 1) { // 1 type only
                String type = (String) schema.getTypes().iterator().next();
                return type == null || "null".equals(type);
            } else { // more than 1 type so must not be just null
                return false;
            }
        }

        if (schema instanceof JsonSchema) { // 3.1 spec
            if (Boolean.TRUE.equals(schema.getNullable())) {
                return true;
            }

            // for `type: null`
            if (schema.getTypes() == null && schema.get$ref() == null) {
                return true;
            }
        } else { // 3.0.x or 2.x spec
            if ((schema.getType() == null || schema.getType().equals("null")) && schema.get$ref() == null) {
                return true;
            }
        }

        // convert referenced enum of null only to `nullable:true`
        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, schema);
        if (referencedSchema.getEnum() != null && referencedSchema.getEnum().size() == 1) {
            if ("null".equals(String.valueOf(referencedSchema.getEnum().get(0)))) {
                return true;
            }
        }

        return false;
    }

    /**
     * If the schema is oneOf and the sub-schemas is null, set `nullable: true`
     * instead.
     * If there's only one sub-schema, simply return the sub-schema directly.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSimplifyOneOf(Schema schema) {
        if (!getRule(SIMPLIFY_ONEOF_ANYOF)) {
            return schema;
        }

        List<Schema> oneOfSchemas = schema.getOneOf();
        if (oneOfSchemas != null) {
            // simplify any type with 6 sub-schemas (string, integer, etc) in oneOf
            if (oneOfSchemas.size() == 6) {
                TreeSet<String> ts = new TreeSet<>();
                for (Schema s: oneOfSchemas) {
                    ts.add(ModelUtils.getType(s));
                }

                if (ts.equals(anyTypeTreeSet)) {
                    Schema anyType = new Schema();
                    anyType.setDescription(schema.getDescription());
                    anyType.setNullable(schema.getNullable());
                    anyType.setExtensions(schema.getExtensions());
                    anyType.setTitle(schema.getTitle());
                    anyType.setExample(schema.getExample());
                    anyType.setExamples(schema.getExamples());
                    anyType.setDefault(schema.getDefault());
                    anyType.setDeprecated(schema.getDeprecated());
                    return anyType;
                }
            }

            if (oneOfSchemas.removeIf(oneOf -> isNullTypeSchema(oneOf))) {
                schema.setNullable(true);

                // if only one element left, simplify to just the element (schema)
                if (oneOfSchemas.size() == 1) {
                    if (Boolean.TRUE.equals(schema.getNullable())) { // retain nullable setting
                        ((Schema) oneOfSchemas.get(0)).setNullable(true);
                    }
                    return (Schema) oneOfSchemas.get(0);
                }
            }
        }

        return schema;
    }

    /**
     * Set nullable to true in array/set if needed.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSetArraytoNullable(Schema schema) {
        if (!getRule(SET_CONTAINER_TO_NULLABLE)) {
            return schema;
        }

        if (Boolean.TRUE.equals(schema.getUniqueItems())) { // a set
            if (updateSetToNullable) {
                return setNullable(schema);
            }
        } else { // array
            if (updateArrayToNullable) {
                return setNullable(schema);
            }
        }

        return schema;
    }

    /**
     * Set nullable to true in primitive types (e.g. string) if needed.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSetPrimitiveTypesToNullable(Schema schema) {
        if (!getRule(SET_PRIMITIVE_TYPES_TO_NULLABLE)) {
            return schema;
        }

        if (updateStringToNullable && "string".equals(schema.getType())) {
            return setNullable(schema);
        } else if (updateIntegerToNullable && "integer".equals(schema.getType())) {
            return setNullable(schema);
        } else if (updateNumberToNullable && "number".equals(schema.getType())) {
            return setNullable(schema);
        } else if (updateBooleanToNullable && "boolean".equals(schema.getType())) {
            return setNullable(schema);
        }

        return schema;
    }

    private Schema setNullable(Schema schema) {
        if (schema.getNullable() != null || (schema.getExtensions() != null && schema.getExtensions().containsKey("x-nullable"))) {
            // already set, don't overwrite
            return schema;
        }
        schema.setNullable(true);
        return schema;
    }
    /**
     * Set nullable to true in map if needed.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSetMapToNullable(Schema schema) {
        if (!getRule(SET_CONTAINER_TO_NULLABLE)) {
            return schema;
        }

        if (updateMapToNullable) {
            return setNullable(schema);
        }

        return schema;
    }

    /**
     * If the schema is anyOf and the sub-schemas is null, set `nullable: true` instead.
     * If there's only one sub-schema, simply return the sub-schema directly.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processSimplifyAnyOf(Schema schema) {
        if (!getRule(SIMPLIFY_ONEOF_ANYOF)) {
            return schema;
        }

        List<Schema> anyOfSchemas = schema.getAnyOf();
        if (anyOfSchemas != null) {
            // simplify any type with 6 sub-schemas (string, integer, etc) in anyOf
            if (anyOfSchemas.size() == 6) {
                TreeSet<String> ts = new TreeSet<>();
                for (Schema s: anyOfSchemas) {
                    ts.add(ModelUtils.getType(s));
                }

                if (ts.equals(anyTypeTreeSet)) {
                    Schema anyType = new Schema();
                    anyType.setDescription(schema.getDescription());
                    anyType.setNullable(schema.getNullable());
                    anyType.setExtensions(schema.getExtensions());
                    anyType.setTitle(schema.getTitle());
                    anyType.setExample(schema.getExample());
                    anyType.setExamples(schema.getExamples());
                    anyType.setDefault(schema.getDefault());
                    anyType.setDeprecated(schema.getDeprecated());
                    return anyType;
                }
            }

            if (anyOfSchemas.removeIf(anyOf -> isNullTypeSchema(anyOf))) {
                schema.setNullable(true);
            }

            // if only one element left, simplify to just the element (schema)
            if (anyOfSchemas.size() == 1) {
                if (Boolean.TRUE.equals(schema.getNullable())) { // retain nullable setting
                    ((Schema) anyOfSchemas.get(0)).setNullable(true);
                }
                return (Schema) anyOfSchemas.get(0);
            }
        }

        return schema;
    }

    /**
     * If the schema is boolean and its enum is defined,
     * then simply it to just boolean.
     *
     * @param schema Schema
     * @return Schema
     */
    private void processSimplifyBooleanEnum(Schema schema) {
        if (!getRule(SIMPLIFY_BOOLEAN_ENUM)) {
            return;
        }

        if (schema instanceof BooleanSchema) {
            BooleanSchema bs = (BooleanSchema) schema;
            if (bs.getEnum() != null && !bs.getEnum().isEmpty()) { // enum defined
                bs.setEnum(null);
            }
        }
    }

    /**
     * If the schema is integer and the max value is invalid (out of bound)
     * then add x-unsigned to use unsigned integer/long instead.
     *
     * @param schema Schema
     * @return Schema
     */
    private void processAddUnsignedToIntegerWithInvalidMaxValue(Schema schema) {
        if (!getRule(ADD_UNSIGNED_TO_INTEGER_WITH_INVALID_MAX_VALUE)) {
            return;
        }

        if (schema instanceof IntegerSchema) {
            if (ModelUtils.isLongSchema(schema)) {
                if ("18446744073709551615".equals(String.valueOf(schema.getMaximum())) &&
                        "0".equals(String.valueOf(schema.getMinimum()))) {
                    schema.addExtension("x-unsigned", true);
                }
            } else {
                if ("4294967295".equals(String.valueOf(schema.getMaximum())) &&
                        "0".equals(String.valueOf(schema.getMinimum()))) {
                    schema.addExtension("x-unsigned", true);
                }
            }
        }
    }

    /**
     * When set to true, refactor schema with allOf and properties in the same level to a schema with allOf only and
     * the allOf contains a new schema containing the properties in the top level.
     *
     * @param schema Schema
     * @return Schema
     */
    private Schema processRefactorAllOfWithPropertiesOnly(Schema schema) {
        if (!getRule(REFACTOR_ALLOF_WITH_PROPERTIES_ONLY)) {
            return schema;
        }

        ObjectSchema os = new ObjectSchema();
        // set the properties, etc of the new schema to the properties of schema
        os.setProperties(schema.getProperties());
        os.setRequired(schema.getRequired());
        os.setAdditionalProperties(schema.getAdditionalProperties());
        os.setNullable(schema.getNullable());
        os.setDescription(schema.getDescription());
        os.setDeprecated(schema.getDeprecated());
        os.setExample(schema.getExample());
        os.setExamples(schema.getExamples());
        os.setTitle(schema.getTitle());
        schema.getAllOf().add(os); // move new schema as a child schema of allOf
        // clean up by removing properties, etc
        schema.setProperties(null);
        schema.setRequired(null);
        schema.setAdditionalProperties(null);
        schema.setNullable(null);
        schema.setDescription(null);
        schema.setDeprecated(null);
        schema.setExample(null);
        schema.setExamples(null);
        schema.setTitle(null);

        // at this point the schema becomes a simple allOf (no properties) with an additional schema containing
        // the properties

        return schema;
    }

    /**
     * When set to true, normalize schema so that it works well with the generator.
     *
     * @param schema         Schema
     * @param visitedSchemas a set of visited schemas
     * @return Schema
     */
    private Schema processNormalize31Spec(Schema schema, Set<Schema> visitedSchemas) {
        if (!getRule(NORMALIZE_31SPEC)) {
            return schema;
        }

        if (schema == null) {
            return null;
        }

        if (schema instanceof JsonSchema &&
                schema.get$schema() == null &&
                schema.getTypes() == null && schema.getType() == null) {
            // convert any type in v3.1 to empty schema (any type in v3.0 spec), any type example:
            // components:
            //  schemas:
            //    any_type: {}
            return new Schema();
        }

        // return schema if nothing in 3.1 spec types to normalize
        if (schema.getTypes() == null) {
            return schema;
        }

        // process null
        if (schema.getTypes().contains("null")) {
            schema.setNullable(true);
            schema.getTypes().remove("null");
        }

        // only one item (type) left
        if (schema.getTypes().size() == 1) {
            String type = String.valueOf(schema.getTypes().iterator().next());
            if (ModelUtils.isArraySchema(schema)) {
                ArraySchema as = new ArraySchema();
                as.setDescription(schema.getDescription());
                as.setDefault(schema.getDefault());
                if (schema.getExample() != null) {
                    as.setExample(schema.getExample());
                }
                if (schema.getExamples() != null) {
                    as.setExamples(schema.getExamples());
                }
                as.setMinItems(schema.getMinItems());
                as.setMaxItems(schema.getMaxItems());
                as.setExtensions(schema.getExtensions());
                as.setXml(schema.getXml());
                as.setUniqueItems(schema.getUniqueItems());
                if (schema.getItems() != null) {
                    // `items` is also a json schema
                    if (StringUtils.isNotEmpty(schema.getItems().get$ref())) {
                        Schema ref = new Schema();
                        ref.set$ref(schema.getItems().get$ref());
                        as.setItems(ref);
                    } else { // inline schema (e.g. model, string, etc)
                        Schema updatedItems = normalizeSchema(schema.getItems(), visitedSchemas);
                        as.setItems(updatedItems);
                    }
                }

                return as;
            } else { // other primitive type such as string
                // set type (3.0 spec) directly
                schema.setType(type);
            }
        } else { // more than 1 item
            // convert to anyOf and keep all other attributes (e.g. nullable, description)
            // the same. No need to handle null as it should have been removed at this point.
            for (Object type : schema.getTypes()) {
                switch (String.valueOf(type)) {
                    case "string":
                        schema.addAnyOfItem(new StringSchema());
                        break;
                    case "integer":
                        schema.addAnyOfItem(new IntegerSchema());
                        break;
                    case "number":
                        schema.addAnyOfItem(new NumberSchema());
                        break;
                    case "boolean":
                        schema.addAnyOfItem(new BooleanSchema());
                        break;
                    default:
                        LOGGER.error("Type {} not yet supported in openapi-normalizer to process OpenAPI 3.1 spec with multiple types.");
                        LOGGER.error("Please report the issue via https://github.com/OpenAPITools/openapi-generator/issues/new/.");
                }
            }
        }

        return schema;
    }

    // ===================== end of rules =====================
}
