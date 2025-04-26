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
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.ModelUtils.simplyOneOfAnyOfWithOnlyOneNonNullSubSchema;
import static org.openapitools.codegen.utils.StringUtils.getUniqueString;

public class OpenAPINormalizer {
    protected OpenAPI openAPI;
    private Map<String, String> inputRules = new HashMap<>();
    private Map<String, Boolean> rules = new HashMap<>();

    private TreeSet<String> anyTypeTreeSet = new TreeSet<>();

    protected final Logger LOGGER = LoggerFactory.getLogger(OpenAPINormalizer.class);

    Set<String> ruleNames = new TreeSet<>();
    Set<String> rulesDefaultToTrue = new TreeSet<>();

    // ============= a list of rules =============
    // when set to true, all rules (true or false) are enabled
    final String ENABLE_ALL = "ENABLE_ALL";
    boolean enableAll;

    // when set to true, all rules (true or false) are disabled
    final String DISABLE_ALL = "DISABLE_ALL";
    boolean disableAll;

    // when defined, use the class name to instantiate the normalizer
    static final String NORMALIZER_CLASS = "NORMALIZER_CLASS";

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

    // when set to a string value, tags will be set to the value of the provided vendor extension
    final String SET_TAGS_TO_VENDOR_EXTENSION = "SET_TAGS_TO_VENDOR_EXTENSION";
    String setTagsToVendorExtension;

    // when set to true, tags in all operations will be set to operationId or "default" if operationId
    // is empty
    final String FIX_DUPLICATED_OPERATIONID = "FIX_DUPLICATED_OPERATIONID";
    String fixDuplicatedOperationId;
    HashSet<String> operationIdSet = new HashSet<>();

    // when set to true, if a securityScheme is found with the specified name, it will be converted to bearerAuth
    final String SET_BEARER_AUTH_FOR_NAME = "SET_BEARER_AUTH_FOR_NAME";
    String bearerAuthSecuritySchemeName;

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
    HashSet<String> methodFilters = new HashSet<>();

    HashSet<String> tagFilters = new HashSet<>();

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
     * Factory constructor for OpenAPINormalizer.
     *
     * Default can be overriden by setting the NORMALIZER_CLASS rule
     */
    public static OpenAPINormalizer createNormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
        if (inputRules.containsKey(NORMALIZER_CLASS)) {
            try {
                Class clazz = Class.forName(inputRules.get(NORMALIZER_CLASS));
                Constructor constructor = clazz.getConstructor(OpenAPI.class, Map.class);
                return (OpenAPINormalizer) constructor.newInstance(openAPI, inputRules);
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException(e);
            }
        } else {
            return new OpenAPINormalizer(openAPI, inputRules);
        }
    }

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
        ruleNames.add(NORMALIZER_CLASS);
        ruleNames.add(REF_AS_PARENT_IN_ALLOF);
        ruleNames.add(REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY);
        ruleNames.add(SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING);
        ruleNames.add(SIMPLIFY_ONEOF_ANYOF);
        ruleNames.add(SIMPLIFY_BOOLEAN_ENUM);
        ruleNames.add(KEEP_ONLY_FIRST_TAG_IN_OPERATION);
        ruleNames.add(SET_TAGS_FOR_ALL_OPERATIONS);
        ruleNames.add(SET_TAGS_TO_OPERATIONID);
        ruleNames.add(SET_TAGS_TO_VENDOR_EXTENSION);
        ruleNames.add(FIX_DUPLICATED_OPERATIONID);
        ruleNames.add(SET_BEARER_AUTH_FOR_NAME);
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

        setTagsToVendorExtension = inputRules.get(SET_TAGS_TO_VENDOR_EXTENSION);
        if (setTagsToVendorExtension != null) {
            rules.put(SET_TAGS_TO_VENDOR_EXTENSION, true);
        }

        if (inputRules.get(FILTER) != null) {
            rules.put(FILTER, true);

            String[] filterStrs = inputRules.get(FILTER).split(":");
            if (filterStrs.length != 2) { // only support operationId with : at the moment
                LOGGER.error("FILTER rule must be in the form of `operationId:name1|name2|name3` or `method:get|post|put` or `tag:tag1|tag2|tag3`: {}", inputRules.get(FILTER));
            } else {
                if ("operationId".equals(filterStrs[0])) {
                    operationIdFilters = Arrays.stream(filterStrs[1].split("[|]"))
                            .filter(Objects::nonNull)
                            .map(String::trim)
                            .collect(Collectors.toCollection(HashSet::new));
                } else if ("method".equals(filterStrs[0])) {
                    methodFilters = Arrays.stream(filterStrs[1].split("[|]"))
                            .filter(Objects::nonNull)
                            .map(String::trim)
                            .collect(Collectors.toCollection(HashSet::new));
                } else if ("tag".equals(filterStrs[0])) {
                    tagFilters = Arrays.stream(filterStrs[1].split("[|]"))
                            .filter(Objects::nonNull)
                            .map(String::trim)
                            .collect(Collectors.toCollection(HashSet::new));
                } else {
                    LOGGER.error("FILTER rule must be in the form of `operationId:name1|name2|name3` or `method:get|post|put` or `tag:tag1|tag2|tag3`: {}", inputRules.get(FILTER));
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

        bearerAuthSecuritySchemeName = inputRules.get(SET_BEARER_AUTH_FOR_NAME);
        if (bearerAuthSecuritySchemeName != null) {
            rules.put(SET_BEARER_AUTH_FOR_NAME, true);
        }
    }

    /**
     * Normalizes the OpenAPI input, which may not perfectly conform to
     * the specification.
     */
    protected void normalize() {
        if (rules == null || rules.isEmpty() || disableAll) {
            return;
        }

        if (this.openAPI.getComponents() == null) {
            this.openAPI.setComponents(new Components());
        }

        if (this.openAPI.getComponents().getSchemas() == null) {
            this.openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        normalizeInfo();
        normalizePaths();
        normalizeComponentsSecuritySchemes();
        normalizeComponentsSchemas();
        normalizeComponentsResponses();
    }

    /**
     * Pre-populate info if it's not defined.
     */
    protected void normalizeInfo() {
        if (this.openAPI.getInfo() == null) {
            Info info = new Info();
            info.setTitle("OpenAPI");
            info.setVersion("0.0.1");
            info.setDescription("OpenAPI");
            this.openAPI.setInfo(info);
        }
    }

    /**
     * Normalizes inline models in Paths
     */
    protected void normalizePaths() {
        Paths paths = openAPI.getPaths();
        if (paths == null) {
            return;
        }

        for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
            PathItem path = pathsEntry.getValue();
            List<Operation> operations = new ArrayList<>(path.readOperations());

            Map<String, Function<PathItem, Operation>> methodMap = Map.of(
                    "get", PathItem::getGet,
                    "put", PathItem::getPut,
                    "head", PathItem::getHead,
                    "post", PathItem::getPost,
                    "delete", PathItem::getDelete,
                    "patch", PathItem::getPatch,
                    "options", PathItem::getOptions,
                    "trace", PathItem::getTrace
            );

            // Iterates over each HTTP method in methodMap, retrieves the corresponding Operation from the PathItem,
            // and marks it as internal (`x-internal`) if the method is not in methodFilters.
            methodMap.forEach((method, getter) -> {
                Operation operation = getter.apply(path);
                if (operation != null && !methodFilters.isEmpty()) {
                    LOGGER.info("operation `{}` marked internal only (x-internal: `{}`) by the method FILTER", operation.getOperationId(), !methodFilters.contains(method));
                    operation.addExtension("x-internal", !methodFilters.contains(method));
                }
            });

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
                        LOGGER.info("operation `{}` marked as internal only (x-internal: true) by the operationId FILTER", operation.getOperationId());
                        operation.addExtension("x-internal", true);
                    }
                } else if (!tagFilters.isEmpty()) {
                    if (operation.getTags().stream().anyMatch(tagFilters::contains)) {
                        operation.addExtension("x-internal", false);
                    } else {
                        LOGGER.info("operation `{}` marked as internal only (x-internal: true) by the tag FILTER", operation.getOperationId());
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
    protected void normalizeOperation(Operation operation) {
        processRemoveXInternalFromOperation(operation);

        processKeepOnlyFirstTagInOperation(operation);

        processSetTagsForAllOperations(operation);

        processSetTagsToOperationId(operation);

        processSetTagsToVendorExtension(operation);

        processFixDuplicatedOperationId(operation);
    }

    /**
     * Normalizes schemas in content
     *
     * @param content target content
     */
    protected void normalizeContent(Content content) {
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
    protected void normalizeRequestBody(Operation operation) {
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
    protected void normalizeParameters(List<Parameter> parameters) {
        if (parameters == null) {
            return;
        }

        for (Parameter parameter : parameters) {
            // dereference parameter
            if (StringUtils.isNotEmpty(parameter.get$ref())) {
                parameter = ModelUtils.getReferencedParameter(openAPI, parameter);
            }

            if (parameter.getSchema() != null) {
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
    protected void normalizeResponses(Operation operation) {
        ApiResponses responses = operation.getResponses();
        if (responses == null) {
            return;
        }

        for (Map.Entry<String, ApiResponse> responsesEntry : responses.entrySet()) {
            normalizeResponse(responsesEntry.getValue());
        }
    }

    /**
     * Normalizes schemas in ApiResponse
     *
     * @param apiResponse API response
     */
    protected void normalizeResponse(ApiResponse apiResponse) {
        if (apiResponse != null) {
            normalizeContent(ModelUtils.getReferencedApiResponse(openAPI, apiResponse).getContent());
            normalizeHeaders(ModelUtils.getReferencedApiResponse(openAPI, apiResponse).getHeaders());
        }
    }

    /**
     * Normalizes schemas in headers
     *
     * @param headers a map of headers
     */
    protected void normalizeHeaders(Map<String, Header> headers) {
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
     * Normalizes securitySchemes in components
     */
    protected void normalizeComponentsSecuritySchemes() {
         if (StringUtils.isEmpty(bearerAuthSecuritySchemeName)) {
             return;
         }

        Map<String, SecurityScheme> schemes = openAPI.getComponents().getSecuritySchemes();
        if (schemes == null) {
            return;
        }

        for (String schemeKey : schemes.keySet()) {
            if (schemeKey.equals(bearerAuthSecuritySchemeName)) {
                SecurityScheme scheme = schemes.get(schemeKey);
                scheme.setType(SecurityScheme.Type.HTTP);
                scheme.setScheme("bearer");
                scheme.setIn(null);
                scheme.setName(null);
                scheme.setBearerFormat(null);
                scheme.setFlows(null);
                scheme.setOpenIdConnectUrl(null);
                scheme.setExtensions(null);
                scheme.set$ref(null);
                schemes.put(schemeKey, scheme);
            }
        }
    }

    /**
     * Normalizes schemas in components
     */
    protected void normalizeComponentsSchemas() {
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        if (schemas == null) {
            return;
        }

        List<String> schemaNames = new ArrayList<String>(schemas.keySet());
        for (String schemaName : schemaNames) {
            Schema schema = schemas.get(schemaName);
            if (schema == null) {
                LOGGER.warn("{} not found in openapi/components/schemas.", schemaName);
            } else {
                // remove x-internal if needed
                if (schema.getExtensions() != null && getRule(REMOVE_X_INTERNAL)) {
                    if (Boolean.parseBoolean(String.valueOf(schema.getExtensions().get(X_INTERNAL)))) {
                        schema.getExtensions().remove(X_INTERNAL);
                    }
                }

                // auto fix self reference schema to avoid stack overflow
                fixSelfReferenceSchema(schemaName, schema);

                // normalize the schemas
                schemas.put(schemaName, normalizeSchema(schema, new HashSet<>()));
            }
        }
    }

    /**
     * Normalizes schemas in component's responses.
     */
    protected void normalizeComponentsResponses() {
        Map<String, ApiResponse> apiResponses = openAPI.getComponents().getResponses();
        if (apiResponses == null) {
            return;
        }

        for (Map.Entry<String, ApiResponse> entry : apiResponses.entrySet()) {
            normalizeResponse(entry.getValue());
        }
    }

    /**
     * Auto fix a self referencing schema using any type to replace the self-referencing sub-item.
     *
     * @param name   Schema name
     * @param schema Schema
     */
    public void fixSelfReferenceSchema(String name, Schema schema) {
        if (ModelUtils.isArraySchema(schema)) {
            if (isSelfReference(name, schema.getItems())) {
                LOGGER.error("Array schema {} has a sub-item referencing itself. Worked around the self-reference schema using any type instead.", name);
                schema.setItems(new Schema<>());
            }
        }

        if (ModelUtils.isOneOf(schema)) {
            for (int i = 0; i < schema.getOneOf().size(); i++) {
                if (isSelfReference(name, (Schema) schema.getOneOf().get(i))) {
                    LOGGER.error("oneOf schema {} has a sub-item referencing itself. Worked around the self-reference schema by removing it.", name);
                    schema.getOneOf().remove(i);
                }
            }
        }

        if (ModelUtils.isAnyOf(schema)) {
            for (int i = 0; i < schema.getAnyOf().size(); i++) {
                if (isSelfReference(name, (Schema) schema.getAnyOf().get(i))) {
                    LOGGER.error("anyOf schema {} has a sub-item referencing itself. Worked around the self-reference schema by removing it.", name);
                    schema.getAnyOf().remove(i);
                }
            }
        }

        if (schema.getAdditionalProperties() != null && schema.getAdditionalProperties() instanceof Schema) {
            if (isSelfReference(name, (Schema) schema.getAdditionalProperties())) {
                LOGGER.error("Schema {} (with additional properties) has a sub-item referencing itself. Worked around the self-reference schema using any type instead.", name);
                schema.setAdditionalProperties(new Schema<>());
            }
        }

    }

    protected boolean isSelfReference(String name, Schema subSchema) {
        if (subSchema != null && name.equals(ModelUtils.getSimpleRef(subSchema.get$ref()))) {
            return true;
        } else {
            return false;
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
        if (skipNormalization(schema, visitedSchemas)) {
            return schema;
        }
        markSchemaAsVisited(schema, visitedSchemas);

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


    /**
     * Check if normalization is needed.
     *
     * No normalization needed if the schema is null or is a $ref or already processed.
     *
     * @param schema         Schema
     * @param visitedSchemas a set of visited schemas
     * @return false if normalization is needed
     */
    protected boolean skipNormalization(Schema schema, Set<Schema> visitedSchemas) {
        if (schema == null) {
            return true;
        }

        if (StringUtils.isNotEmpty(schema.get$ref())) {
            // no need to process $ref
            return true;
        }

        if (visitedSchemas.contains(schema)) {
            return true; // skip due to circular reference
        } else {
            return false;
        }
    }

    /**
     * Add the schema to the collection of visited schemas.
     *
     * @param schema schema to add
     * @param visitedSchemas current visited schemas
     */
    protected void markSchemaAsVisited(Schema schema, Set<Schema> visitedSchemas) {
        if (schema != null) {
            visitedSchemas.add(schema);
        }
    }

    protected Schema normalizeArraySchema(Schema schema) {
        Schema result = processNormalize31Spec(schema, new HashSet<>());
        return processSetArraytoNullable(result);
    }

    protected Schema normalizeMapSchema(Schema schema) {
        return processSetMapToNullable(schema);
    }

    protected Schema normalizeSimpleSchema(Schema schema, Set<Schema> visitedSchemas) {
        Schema result = processNormalize31Spec(schema, visitedSchemas);
        return processSetPrimitiveTypesToNullable(result);
    }

    protected void normalizeBooleanSchema(Schema schema, Set<Schema> visitedSchemas) {
        processSimplifyBooleanEnum(schema);
        processSetPrimitiveTypesToNullable(schema);
    }

    protected void normalizeIntegerSchema(Schema schema, Set<Schema> visitedSchemas) {
        processAddUnsignedToIntegerWithInvalidMaxValue(schema);
        processSetPrimitiveTypesToNullable(schema);
    }

    protected void normalizeProperties(Map<String, Schema> properties, Set<Schema> visitedSchemas) {
        if (properties == null) {
            return;
        }
        for (Map.Entry<String, Schema> propertiesEntry : properties.entrySet()) {
            Schema property = propertiesEntry.getValue();
            Schema newProperty = normalizeSchema(property, new HashSet<>());
            propertiesEntry.setValue(newProperty);
        }
    }

    /*
     * Remove unsupported schemas (e.g. if, then) from allOf.
     *
     * @param schema Schema
     */
    protected void removeUnsupportedSchemasFromAllOf(Schema schema) {
        if (schema.getAllOf() == null) {
            return;
        }

        Iterator<Schema> iterator = schema.getAllOf().iterator();
        while (iterator.hasNext()) {
            Schema item = iterator.next();

            // remove unsupported schemas (e.g. if, then)
            if (ModelUtils.isUnsupportedSchema(openAPI, item)) {
                LOGGER.debug("Removed allOf sub-schema that's not yet supported: {}", item);
                iterator.remove();
            }
        }

        if (schema.getAllOf().size() == 0) {
            // no more schema in allOf so reset to null instead
            LOGGER.info("Unset/Removed allOf after cleaning up allOf sub-schemas that are not yet supported.");
            schema.setAllOf(null);
        }
    }

    protected Schema normalizeAllOf(Schema schema, Set<Schema> visitedSchemas) {
        removeUnsupportedSchemasFromAllOf(schema);

        if (schema.getAllOf() == null) {
            return schema;
        }

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

    protected Schema normalizeAllOfWithProperties(Schema schema, Set<Schema> visitedSchemas) {
        removeUnsupportedSchemasFromAllOf(schema);

        if (schema.getAllOf() == null) {
            return schema;
        }

        // process rule to refactor properties into allOf sub-schema
        schema = processRefactorAllOfWithPropertiesOnly(schema);

        for (Object item : schema.getAllOf()) {
            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! allOf schema is not of the type Schema: " + item);
            }
            // normalize allOf sub schemas one by one
            normalizeSchema((Schema) item, visitedSchemas);
        }

        return schema;
    }

    protected Schema normalizeOneOf(Schema schema, Set<Schema> visitedSchemas) {
        // simplify first as the schema may no longer be a oneOf after processing the rule below
        schema = processSimplifyOneOf(schema);

        // if it's still a oneOf, loop through the sub-schemas
        if (schema.getOneOf() != null) {
            for (int i = 0; i < schema.getOneOf().size(); i++) {
                // normalize oneOf sub schemas one by one
                Object item = schema.getOneOf().get(i);

                if (item == null) {
                    continue;
                }
                if (!(item instanceof Schema)) {
                    throw new RuntimeException("Error! oneOf schema is not of the type Schema: " + item);
                }

                // update sub-schema with the updated schema
                schema.getOneOf().set(i, normalizeSchema((Schema) item, visitedSchemas));
            }
        } else {
            // normalize it as it's no longer an oneOf
            schema = normalizeSchema(schema, visitedSchemas);
        }

        return schema;
    }

    protected Schema normalizeAnyOf(Schema schema, Set<Schema> visitedSchemas) {
        for (int i = 0; i < schema.getAnyOf().size(); i++) {
            // normalize anyOf sub schemas one by one
            Object item = schema.getAnyOf().get(i);

            if (item == null) {
                continue;
            }

            if (!(item instanceof Schema)) {
                throw new RuntimeException("Error! anyOf schema is not of the type Schema: " + item);
            }

            // update sub-schema with the updated schema
            schema.getAnyOf().set(i, normalizeSchema((Schema) item, visitedSchemas));
        }

        // process rules here
        schema = processSimplifyAnyOf(schema);

        // last rule to process as the schema may become String schema (not "anyOf") after the completion
        return normalizeSchema(processSimplifyAnyOfStringAndEnumString(schema), visitedSchemas);
    }

    protected Schema normalizeComplexComposedSchema(Schema schema, Set<Schema> visitedSchemas) {
        // loop through properties, if any
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            normalizeProperties(schema.getProperties(), visitedSchemas);
        }

        processRemoveAnyOfOneOfAndKeepPropertiesOnly(schema);

        return normalizeSchema(schema, visitedSchemas);
    }

    // ===================== a list of rules =====================
    // all rules (functions ) start with the word "process"

    /**
     * Child schemas in `allOf` is considered a parent if it's a `$ref` (instead of inline schema).
     *
     * @param schema Schema
     */
    protected void processUseAllOfRefAsParent(Schema schema) {
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
     * Remove/hide the x-internal in operations and model.
     *
     * @param operation Operation
     */
    protected void processRemoveXInternalFromOperation(Operation operation) {
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
    protected void processKeepOnlyFirstTagInOperation(Operation operation) {
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
    protected void processSetTagsForAllOperations(Operation operation) {
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
    protected void processSetTagsToOperationId(Operation operation) {
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
     * Set the tag name to the value of the provided vendor extension
     *
     * @param operation Operation
     */
    protected void processSetTagsToVendorExtension(Operation operation) {
        if (StringUtils.isEmpty(setTagsToVendorExtension)) {
            return;
        }

        if (operation.getExtensions() == null) {
            return;
        }

        if (operation.getExtensions().containsKey(setTagsToVendorExtension)) {
            operation.setTags(null);
            Object argObj = operation.getExtensions().get(setTagsToVendorExtension);
            if (argObj instanceof List) {
                List<String> tags = (List<String>) argObj;
                for (String tag : tags) {
                    operation.addTagsItem(tag);
                }
            } else {
                operation.addTagsItem(String.valueOf(argObj));
            }
        }
    }

    protected void processFixDuplicatedOperationId(Operation operation) {
        if (!getRule(FIX_DUPLICATED_OPERATIONID)) {
            return;
        }

        // skip null as default codegen will automatically generate one using path, http verb, etc
        if (operation.getOperationId() == null) {
            return;
        }

        String uniqueName = getUniqueString(operationIdSet, operation.getOperationId());

        if (!uniqueName.equals(operation.getOperationId())) {
            LOGGER.info("operationId {} renamed to {} to ensure uniqueness (enabled by openapi normalizer rule `FIX_DUPLICATED_OPERATIONID`)", operation.getOperationId(), uniqueName);
            operation.setOperationId(uniqueName);
        }
    }

    /**
     * If the schema contains anyOf/oneOf and properties, remove oneOf/anyOf as these serve as rules to
     * ensure inter-dependency between properties. It's a workaround as such validation is not supported at the moment.
     *
     * @param schema Schema
     */
    protected void processRemoveAnyOfOneOfAndKeepPropertiesOnly(Schema schema) {
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
    protected Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
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
     * If the schema is oneOf and the sub-schemas is null, set `nullable: true`
     * instead.
     * If there's only one sub-schema, simply return the sub-schema directly.
     *
     * @param schema Schema
     * @return Schema
     */
    protected Schema processSimplifyOneOf(Schema schema) {
        if (!getRule(SIMPLIFY_ONEOF_ANYOF)) {
            return schema;
        }

        List<Schema> oneOfSchemas = schema.getOneOf();
        if (oneOfSchemas != null) {
            // simplify any type with 6 sub-schemas (string, integer, etc) in oneOf
            if (oneOfSchemas.size() == 6) {
                TreeSet<String> ts = new TreeSet<>();
                for (Schema s : oneOfSchemas) {
                    s = ModelUtils.getReferencedSchema(openAPI, s);
                    String type = ModelUtils.getType(s);
                    if (type == null) {
                        LOGGER.debug("Error null type found in schema when simplifying any type with 6 sub-schemas: {}", s);
                    } else {
                        ts.add(type);
                    }
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

            schema = simplyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, schema, oneOfSchemas);

            if (ModelUtils.isIntegerSchema(schema) || ModelUtils.isNumberSchema(schema) || ModelUtils.isStringSchema(schema)) {
                // TODO convert oneOf const to enum
                schema.setOneOf(null);
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
    protected Schema processSetArraytoNullable(Schema schema) {
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
    protected Schema processSetPrimitiveTypesToNullable(Schema schema) {
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

    protected Schema setNullable(Schema schema) {
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
    protected Schema processSetMapToNullable(Schema schema) {
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
    protected Schema processSimplifyAnyOf(Schema schema) {
        if (!getRule(SIMPLIFY_ONEOF_ANYOF)) {
            return schema;
        }

        List<Schema> anyOfSchemas = schema.getAnyOf();
        if (anyOfSchemas != null) {
            // simplify any type with 6 sub-schemas (string, integer, etc) in anyOf
            if (anyOfSchemas.size() == 6) {
                TreeSet<String> ts = new TreeSet<>();
                for (Schema s : anyOfSchemas) {
                    s = ModelUtils.getReferencedSchema(openAPI, s);
                    String type = ModelUtils.getType(s);
                    if (type == null) {
                        LOGGER.debug("Error null type found in schema when simplifying any type with 6 sub-schemas: {}", s);
                    } else {
                        ts.add(type);
                    }
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

            schema = simplyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, schema, anyOfSchemas);
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
    protected void processSimplifyBooleanEnum(Schema schema) {
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
    protected void processAddUnsignedToIntegerWithInvalidMaxValue(Schema schema) {
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
    protected Schema processRefactorAllOfWithPropertiesOnly(Schema schema) {
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
        // the properties. Normalize it before returning.
        return normalizeSchema(schema, new HashSet<>());
    }

    /**
     * When set to true, normalize schema so that it works well with the generator.
     *
     * @param schema         Schema
     * @param visitedSchemas a set of visited schemas
     * @return Schema
     */
    protected Schema processNormalize31Spec(Schema schema, Set<Schema> visitedSchemas) {
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

        // process const
        if (schema.getConst() != null) {
            schema.setEnum(Arrays.asList(schema.getConst()));
            schema.setConst(null);
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
                as.setNullable(schema.getNullable());
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
                } else {
                    // when items is not defined, default to any type
                    as.setItems(new Schema());
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
