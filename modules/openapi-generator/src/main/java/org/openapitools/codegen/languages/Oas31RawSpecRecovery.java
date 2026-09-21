package org.openapitools.codegen.languages;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.cfg.JsonNodeFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import io.swagger.v3.core.util.Json31;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Recovers OAS 3.1 schema facts that swagger-parser or model normalization
 * cannot retain. Recovery follows the parsed YAML/JSON tree by schema position;
 * it never associates constraints by textual proximity.
 */
final class Oas31RawSpecRecovery {

    private static final String EMPTY_ENUM_EXT = "x-oas31-empty-enum";
    private static final String ENUM_JSON_EXT = "x-oas31-enum-json";
    private static final String DEFAULT_PRESENT_EXT = "x-oas31-default-present";
    private static final String DEFAULT_JSON_EXT = "x-oas31-default-json";
    private static final String EXAMPLES_JSON_EXT = "x-oas31-examples-json";
    private static final String CONST_PRESENT_EXT = "x-oas31-const-present";
    private static final String CONST_JSON_EXT = "x-oas31-const-json";
    private static final String TYPE_NULL_EXT = "x-oas31-pristine-type-null";
    private static final String DEPENDENT_REQUIRED_EXT = "x-oas31-dependent-required";
    static final String LEGACY_NULLABLE_EXT = "x-oas31-legacy-nullable";
    private static final List<String> COUNT_KEYWORDS = List.of(
            "minItems", "maxItems",
            "minProperties", "maxProperties",
            "minLength", "maxLength",
            "minContains", "maxContains");

    private Oas31RawSpecRecovery() {
    }

    /**
     * Restores prefixItems from a pristine parse before the normalized graph is
     * converted to generator models.
     */
    static void restoreNormalizerDroppedPrefixItems(OpenAPI api, String inputSpec) {
        if (!needsRawRecovery(api, inputSpec)) {
            return;
        }

        OpenAPI pristine;
        try {
            io.swagger.v3.parser.core.models.ParseOptions options =
                    new io.swagger.v3.parser.core.models.ParseOptions();
            options.setResolve(false);
            options.setResolveResponses(false);
            pristine = new io.swagger.v3.parser.OpenAPIV3Parser()
                    .readLocation(inputSpec, null, options).getOpenAPI();
        } catch (RuntimeException ex) {
            throw new IllegalStateException(
                    "Unable to parse the pristine OAS 3.1 document for prefixItems recovery",
                    ex);
        }
        if (pristine == null) {
            throw new IllegalStateException(
                    "Pristine OAS 3.1 parse did not produce an OpenAPI document");
        }

        Map<String, Schema> mutatedSchemas = Oas31KeywordScanner.rootSchemaPositions(api);
        Oas31KeywordScanner.rootSchemaPositions(pristine).forEach((location, schema) -> {
            Schema mutated = mutatedSchemas.get(location);
            if (mutated != null) {
                mergePristineArrayStructure(schema,
                        recoveryTarget(api, schema.get$ref() == null, mutated), api);
            }
        });
    }

    private static boolean needsRawRecovery(OpenAPI api, String inputSpec) {
        return api != null
                && Oas31KeywordScanner.isOas31(api)
                && inputSpec != null;
    }

    private static void mergePristineArrayStructure(
            Schema pristine, Schema mutated, OpenAPI api) {
        if (pristine == null || mutated == null) {
            return;
        }
        if (pristine.getPrefixItems() != null
                && !pristine.getPrefixItems().isEmpty()
                && (mutated.getPrefixItems() == null
                        || mutated.getPrefixItems().isEmpty())) {
            mutated.setPrefixItems(pristine.getPrefixItems());
        }

        mergeSchemaLists(pristine.getAllOf(), mutated.getAllOf(), api);
        mergeSchemaLists(pristine.getAnyOf(), mutated.getAnyOf(), api);
        mergeSchemaLists(pristine.getOneOf(), mutated.getOneOf(), api);
        mergeSchemaMaps(pristine.getProperties(), mutated.getProperties(), api);
        mergeSchemaMaps(pristine.getPatternProperties(), mutated.getPatternProperties(), api);
        mergeSchemaMaps(pristine.getDependentSchemas(), mutated.getDependentSchemas(), api);
        mergeSchemaLists(pristine.getPrefixItems(), mutated.getPrefixItems(), api);
        mergeSchemaValue(pristine.getItems(), mutated.getItems(), api);
        mergeSchemaValue(pristine.getContains(), mutated.getContains(), api);
        mergeSchemaValue(pristine.getNot(), mutated.getNot(), api);
        mergeSchemaValue(pristine.getIf(), mutated.getIf(), api);
        mergeSchemaValue(pristine.getThen(), mutated.getThen(), api);
        mergeSchemaValue(pristine.getElse(), mutated.getElse(), api);
        mergeSchemaValue(pristine.getPropertyNames(), mutated.getPropertyNames(), api);
        mergeSchemaValue(pristine.getContentSchema(), mutated.getContentSchema(), api);
        mergeSchemaValue(
                pristine.getAdditionalProperties(), mutated.getAdditionalProperties(), api);
        mergeSchemaValue(
                pristine.getUnevaluatedProperties(), mutated.getUnevaluatedProperties(), api);
        mergeSchemaValue(pristine.getUnevaluatedItems(), mutated.getUnevaluatedItems(), api);
    }

    private static void mergeSchemaLists(List pristine, List mutated, OpenAPI api) {
        if (pristine == null || mutated == null) {
            return;
        }
        int count = Math.min(pristine.size(), mutated.size());
        for (int i = 0; i < count; i++) {
            mergeSchemaValue(pristine.get(i), mutated.get(i), api);
        }
    }

    private static void mergeSchemaMaps(Map pristine, Map mutated, OpenAPI api) {
        if (pristine == null || mutated == null) {
            return;
        }
        for (Object key : pristine.keySet()) {
            mergeSchemaValue(pristine.get(key), mutated.get(key), api);
        }
    }

    private static void mergeSchemaValue(Object pristine, Object mutated, OpenAPI api) {
        if (pristine instanceof Schema && mutated instanceof Schema) {
            Schema pristineSchema = (Schema) pristine;
            mergePristineArrayStructure(pristineSchema,
                    recoveryTarget(api, pristineSchema.get$ref() == null, (Schema) mutated), api);
        }
    }


    /**
     * Recovers exact count bounds, enum values, dependentRequired, and null
     * type members from the raw document tree.
     */
    static void recoverPristineLiterals(OpenAPI api, String inputSpec) {
        if (!needsRawRecovery(api, inputSpec)) {
            return;
        }

        JsonNode document;
        try {
            document = readRawDocument(inputSpec);
        } catch (Exception ex) {
            throw new IllegalStateException(
                    "Unable to read the raw OAS 3.1 document for exact schema recovery",
                    ex);
        }

        Map<String, Schema> parsedSchemas = Oas31KeywordScanner.rootSchemaPositions(api);
        rawRootSchemaPositions(document).forEach((location, raw) -> {
            Schema parsed = parsedSchemas.get(location);
            if (parsed != null) {
                restoreRawObjectKeywords(api, raw, parsed, location);
                recoverSchema(raw, recoveryTarget(api,
                        raw.isObject() && !raw.has("$ref"), parsed), location, api);
            }
        });
    }

    /** Reads the input document with swagger-parser's configured YAML limits. */
    static JsonNode readRawDocument(String inputSpec) throws Exception {
        String text = readInputSpec(inputSpec);
        YAMLFactory yamlFactory = YAMLFactory.builder()
                .loaderOptions(io.swagger.v3.parser.util.DeserializationUtils
                        .buildLoaderOptions())
                .build();
        ObjectMapper mapper = YAMLMapper.builder(yamlFactory)
                .enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS)
                .enable(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS)
                .disable(JsonNodeFeature.STRIP_TRAILING_BIGDECIMAL_ZEROES)
                .build();
        return mapper.readTree(text);
    }

    static String readInputSpec(String inputSpec) throws Exception {
        boolean windowsDrivePath = inputSpec.matches("^[A-Za-z]:[\\\\/].*");
        if (!windowsDrivePath) {
            try {
                URI uri = URI.create(inputSpec);
                if (uri.getScheme() != null) {
                    try (InputStream input = uri.toURL().openStream()) {
                        return new String(input.readAllBytes(), StandardCharsets.UTF_8);
                    }
                }
            } catch (IllegalArgumentException ignored) {
                // A local path may contain URI-reserved characters such as spaces.
            }
        }
        return Files.readString(Paths.get(inputSpec), StandardCharsets.UTF_8);
    }

    private static Map<String, JsonNode> rawRootSchemaPositions(JsonNode document) {
        Map<String, JsonNode> positions = new LinkedHashMap<>();
        JsonNode components = document.path("components");
        collectRawSchemas(components.path("schemas"), "#/components/schemas", positions);
        collectRawMap(components.path("parameters"), "#/components/parameters", positions,
                (parameter, location) -> collectRawParameter(parameter, location, positions));
        collectRawMap(components.path("headers"), "#/components/headers", positions,
                (header, location) -> collectRawHeader(header, location, positions));
        collectRawMap(components.path("requestBodies"), "#/components/requestBodies", positions,
                (body, location) -> collectRawRequestBody(body, location, positions));
        collectRawMap(components.path("responses"), "#/components/responses", positions,
                (response, location) -> collectRawResponse(response, location, positions));
        collectRawMap(components.path("callbacks"), "#/components/callbacks", positions,
                (callback, location) -> collectRawCallback(callback, location, positions));
        collectRawPathItems(components.path("pathItems"), "#/components/pathItems", positions);
        collectRawPathItems(document.path("paths"), "#/paths", positions);
        collectRawPathItems(document.path("webhooks"), "#/webhooks", positions);
        return positions;
    }

    private static void collectRawSchemas(JsonNode schemas, String location,
                                          Map<String, JsonNode> positions) {
        collectRawMap(schemas, location, positions,
                (schema, schemaLocation) -> positions.put(schemaLocation, schema));
    }

    private static void collectRawPathItems(JsonNode pathItems, String location,
                                            Map<String, JsonNode> positions) {
        collectRawMap(pathItems, location, positions,
                (pathItem, pathLocation) -> collectRawPathItem(pathItem, pathLocation, positions));
    }

    private static void collectRawPathItem(JsonNode pathItem, String location,
                                           Map<String, JsonNode> positions) {
        if (!pathItem.isObject()) {
            return;
        }
        collectRawParameterArray(pathItem.path("parameters"), location + "/parameters", positions);
        for (String method : List.of(
                "get", "put", "post", "delete", "options", "head", "patch", "trace")) {
            JsonNode operation = pathItem.get(method);
            if (operation != null) {
                collectRawOperation(operation, location + "/" + method, positions);
            }
        }
    }

    private static void collectRawOperation(JsonNode operation, String location,
                                            Map<String, JsonNode> positions) {
        if (!operation.isObject()) {
            return;
        }
        collectRawParameterArray(operation.path("parameters"), location + "/parameters", positions);
        collectRawRequestBody(operation.get("requestBody"), location + "/requestBody", positions);
        collectRawMap(operation.path("responses"), location + "/responses", positions,
                (response, responseLocation) ->
                        collectRawResponse(response, responseLocation, positions));
        collectRawMap(operation.path("callbacks"), location + "/callbacks", positions,
                (callback, callbackLocation) ->
                        collectRawCallback(callback, callbackLocation, positions));
    }

    private static void collectRawParameterArray(JsonNode parameters, String location,
                                                 Map<String, JsonNode> positions) {
        if (!parameters.isArray()) {
            return;
        }
        for (int i = 0; i < parameters.size(); i++) {
            collectRawParameter(parameters.get(i), location + "/" + i, positions);
        }
    }

    private static void collectRawParameter(JsonNode parameter, String location,
                                            Map<String, JsonNode> positions) {
        if (!isObject(parameter)) {
            return;
        }
        putRawSchema(parameter.get("schema"), location + "/schema", positions);
        collectRawContent(parameter.path("content"), location + "/content", positions);
    }

    private static void collectRawHeader(JsonNode header, String location,
                                         Map<String, JsonNode> positions) {
        if (!isObject(header)) {
            return;
        }
        putRawSchema(header.get("schema"), location + "/schema", positions);
        collectRawContent(header.path("content"), location + "/content", positions);
    }

    private static void collectRawRequestBody(JsonNode requestBody, String location,
                                              Map<String, JsonNode> positions) {
        if (isObject(requestBody)) {
            collectRawContent(requestBody.path("content"), location + "/content", positions);
        }
    }

    private static void collectRawResponse(JsonNode response, String location,
                                           Map<String, JsonNode> positions) {
        if (!isObject(response)) {
            return;
        }
        collectRawContent(response.path("content"), location + "/content", positions);
        collectRawMap(response.path("headers"), location + "/headers", positions,
                (header, headerLocation) -> collectRawHeader(header, headerLocation, positions));
    }

    private static void collectRawCallback(JsonNode callback, String location,
                                           Map<String, JsonNode> positions) {
        if (!isObject(callback)) {
            return;
        }
        Iterator<Map.Entry<String, JsonNode>> fields = callback.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            if (!"$ref".equals(field.getKey())) {
                collectRawPathItem(field.getValue(),
                        location + "/" + pointerSegment(field.getKey()), positions);
            }
        }
    }

    private static void collectRawContent(JsonNode content, String location,
                                          Map<String, JsonNode> positions) {
        collectRawMap(content, location, positions,
                (mediaType, mediaLocation) ->
                        collectRawMediaType(mediaType, mediaLocation, positions));
    }

    private static void collectRawMediaType(JsonNode mediaType, String location,
                                            Map<String, JsonNode> positions) {
        if (!isObject(mediaType)) {
            return;
        }
        putRawSchema(mediaType.get("schema"), location + "/schema", positions);
        collectRawMap(mediaType.path("encoding"), location + "/encoding", positions,
                (encoding, encodingLocation) -> {
                    if (isObject(encoding)) {
                        collectRawMap(encoding.path("headers"), encodingLocation + "/headers",
                                positions, (header, headerLocation) ->
                                        collectRawHeader(header, headerLocation, positions));
                    }
                });
    }

    private static void collectRawMap(
            JsonNode object, String location, Map<String, JsonNode> positions,
            java.util.function.BiConsumer<JsonNode, String> collector) {
        if (!object.isObject()) {
            return;
        }
        Iterator<Map.Entry<String, JsonNode>> fields = object.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            collector.accept(field.getValue(), location + "/" + pointerSegment(field.getKey()));
        }
    }

    private static void putRawSchema(JsonNode schema, String location,
                                     Map<String, JsonNode> positions) {
        if (schema != null) {
            positions.put(location, schema);
        }
    }

    private static boolean isObject(JsonNode node) {
        return node != null && node.isObject();
    }

    private static String pointerSegment(String value) {
        return value.replace("~", "~0").replace("/", "~1");
    }

    private static Schema recoveryTarget(OpenAPI api, boolean dereference, Schema parsed) {
        if (!dereference || parsed == null || parsed.get$ref() == null) {
            return parsed;
        }
        Schema<?> referenced = ModelUtils.getReferencedSchema(api, parsed);
        return referenced == null ? parsed : referenced;
    }

    private static void recoverSchema(
            JsonNode raw, Schema parsed, String location, OpenAPI api) {
        if (raw == null || parsed == null || !raw.isObject()) {
            return;
        }
        if (raw.has("default")) {
            addExtension(parsed, DEFAULT_PRESENT_EXT, true);
            addExtension(parsed, DEFAULT_JSON_EXT, raw.get("default").toString());
        }
        if (raw.has("const")) {
            addExtension(parsed, CONST_PRESENT_EXT, true);
            addExtension(parsed, CONST_JSON_EXT, raw.get("const").toString());
        }
        if (raw.has("examples")) {
            addExtension(parsed, EXAMPLES_JSON_EXT, raw.get("examples").toString());
        }


        JsonNode rawEnum = raw.get("enum");
        if (rawEnum != null && rawEnum.isArray()) {
            addExtension(parsed, ENUM_JSON_EXT, rawEnum.toString());
            if (rawEnum.isEmpty()) {
                addExtension(parsed, EMPTY_ENUM_EXT, true);
            }
        }

        JsonNode rawType = raw.get("type");
        if (rawType != null && rawType.isArray()) {
            for (JsonNode type : rawType) {
                if (type.isTextual() && "null".equals(type.textValue())) {
                    addExtension(parsed, TYPE_NULL_EXT, true);
                    break;
                }
            }
        }

        for (String keyword : COUNT_KEYWORDS) {
            JsonNode bound = raw.get(keyword);
            if (bound != null) {
                recoverCountBound(parsed, keyword, bound, location);
            }
        }

        JsonNode dependentRequired = raw.get("dependentRequired");
        if (dependentRequired != null) {
            addExtension(parsed, DEPENDENT_REQUIRED_EXT,
                    parseDependentRequired(dependentRequired, location));
        }

        recoverArray(raw.get("allOf"), parsed.getAllOf(), location + "/allOf", api);
        parsed.setAnyOf(restoreCollapsedAllNullBranches(
                raw.get("anyOf"), parsed.getAnyOf()));
        parsed.setOneOf(restoreCollapsedAllNullBranches(
                raw.get("oneOf"), parsed.getOneOf()));
        recoverArray(raw.get("anyOf"), parsed.getAnyOf(), location + "/anyOf", api);
        recoverArray(raw.get("oneOf"), parsed.getOneOf(), location + "/oneOf", api);
        recoverMap(raw.get("properties"), parsed.getProperties(), location + "/properties", api);
        recoverMap(raw.get("patternProperties"), parsed.getPatternProperties(),
                location + "/patternProperties", api);
        recoverMap(raw.get("dependentSchemas"), parsed.getDependentSchemas(),
                location + "/dependentSchemas", api);
        recoverArray(raw.get("prefixItems"), parsed.getPrefixItems(),
                location + "/prefixItems", api);
        recoverChild(raw.get("items"), parsed.getItems(), location + "/items", api);
        recoverChild(raw.get("contains"), parsed.getContains(), location + "/contains", api);
        recoverChild(raw.get("not"), parsed.getNot(), location + "/not", api);
        recoverChild(raw.get("if"), parsed.getIf(), location + "/if", api);
        recoverChild(raw.get("then"), parsed.getThen(), location + "/then", api);
        recoverChild(raw.get("else"), parsed.getElse(), location + "/else", api);
        recoverChild(raw.get("propertyNames"), parsed.getPropertyNames(),
                location + "/propertyNames", api);
        recoverChild(raw.get("contentSchema"), parsed.getContentSchema(),
                location + "/contentSchema", api);
        recoverChild(raw.get("additionalProperties"), parsed.getAdditionalProperties(),
                location + "/additionalProperties", api);
        recoverChild(raw.get("unevaluatedProperties"), parsed.getUnevaluatedProperties(),
                location + "/unevaluatedProperties", api);
        recoverChild(raw.get("unevaluatedItems"), parsed.getUnevaluatedItems(),
                location + "/unevaluatedItems", api);
    }

    private static void restoreRawObjectKeywords(
            OpenAPI api, JsonNode raw, Schema parsed, String location) {
        if (raw == null || !raw.isObject() || parsed == null) {
            return;
        }

        // OAS 3.1 parsers may discard the legacy OAS 3.0 nullable keyword.
        // Restore it on the carrier, not the referenced target, because a $ref
        // sibling applies only at this schema location.
        JsonNode nullable = raw.get("nullable");
        if (nullable != null && nullable.isBoolean()) {
            parsed.setNullable(nullable.booleanValue());
            if (nullable.booleanValue()) {
                addExtension(parsed, LEGACY_NULLABLE_EXT, true);
            }
        }

        // Inline-model extraction replaces an object schema with a synthetic
        // ref. Reuse the normalized target's schema children so references
        // rewritten by InlineModelResolver stay canonical during raw recovery.
        Schema normalizedSource = parsed;
        if (parsed.get$ref() != null && !raw.has("$ref")) {
            normalizedSource = recoveryTarget(api, true, parsed);
        }

        JsonNode additionalProperties = raw.get("additionalProperties");
        if (additionalProperties != null) {
            if (additionalProperties.isBoolean()) {
                parsed.setAdditionalProperties(additionalProperties.booleanValue());
            } else if (normalizedSource.getAdditionalProperties() instanceof Schema) {
                parsed.setAdditionalProperties(normalizedSource.getAdditionalProperties());
            } else {
                parsed.setAdditionalProperties(schemaFromRaw(
                        additionalProperties, location + "/additionalProperties"));
            }
        }
        JsonNode properties = raw.get("properties");
        if (parsed.get$ref() != null && properties != null && properties.isObject()) {
            if (normalizedSource.getProperties() != null) {
                parsed.setProperties(new LinkedHashMap<>(normalizedSource.getProperties()));
            } else {
                parsed.setProperties(schemaMapFromRaw(
                        properties, location + "/properties"));
            }
        }

        JsonNode patternProperties = raw.get("patternProperties");
        if (patternProperties != null && patternProperties.isObject()) {
            if (normalizedSource.getPatternProperties() != null) {
                parsed.setPatternProperties(
                        new LinkedHashMap<>(normalizedSource.getPatternProperties()));
            } else {
                parsed.setPatternProperties(schemaMapFromRaw(
                        patternProperties, location + "/patternProperties"));
            }
        }

        JsonNode propertyNames = raw.get("propertyNames");
        if (propertyNames != null) {
            if (normalizedSource.getPropertyNames() != null) {
                parsed.setPropertyNames(normalizedSource.getPropertyNames());
            } else {
                parsed.setPropertyNames(schemaFromRaw(
                        propertyNames, location + "/propertyNames"));
            }
        }
    }

    private static Map<String, Schema> schemaMapFromRaw(
            JsonNode raw, String location) {
        Map<String, Schema> recovered = new LinkedHashMap<>();
        Iterator<Map.Entry<String, JsonNode>> fields = raw.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            recovered.put(field.getKey(), schemaFromRaw(
                    field.getValue(), location + "/" + pointerSegment(field.getKey())));
        }
        return recovered;
    }

    private static Schema schemaFromRaw(JsonNode raw, String location) {
        if (raw == null || (!raw.isObject() && !raw.isBoolean())) {
            throw new IllegalArgumentException(
                    "Expected a schema at " + location);
        }
        try {
            return Json31.mapper().treeToValue(raw.deepCopy(), Schema.class);
        } catch (Exception ex) {
            throw new IllegalStateException(
                    "Unable to recover raw OAS 3.1 schema at " + location, ex);
        }
    }

    private static List<Schema> restoreCollapsedAllNullBranches(
            JsonNode rawBranches, List<Schema> parsedBranches) {
        if (rawBranches == null || !rawBranches.isArray()) {
            return parsedBranches;
        }
        for (JsonNode rawBranch : rawBranches) {
            if (!isRawNullSchema(rawBranch)) {
                return parsedBranches;
            }
        }
        List<Schema> restored = parsedBranches == null
                ? new ArrayList<>() : parsedBranches;
        while (restored.size() < rawBranches.size()) {
            restored.add(new Schema<>().type("null"));
        }
        return restored;
    }

    private static boolean isRawNullSchema(JsonNode rawSchema) {
        if (rawSchema == null || !rawSchema.isObject()) {
            return false;
        }
        JsonNode rawType = rawSchema.get("type");
        if (rawType == null) {
            return false;
        }
        if (rawType.isTextual()) {
            return "null".equals(rawType.textValue());
        }
        return rawType.isArray()
                && rawType.size() == 1
                && rawType.get(0).isTextual()
                && "null".equals(rawType.get(0).textValue());
    }

    private static void recoverCountBound(
            Schema parsed, String keyword, JsonNode bound, String location) {
        if (!bound.isNumber()) {
            throw new IllegalArgumentException(
                    keyword + " must be a non-negative integer at " + location);
        }
        BigDecimal value = bound.decimalValue().stripTrailingZeros();
        if (value.signum() < 0 || value.scale() > 0) {
            throw new IllegalArgumentException(
                    keyword + " must be a non-negative integer at " + location);
        }
        addExtension(parsed, countBoundExtensionName(keyword), bound.toString());
    }

    private static Map<String, Object> parseDependentRequired(
            JsonNode raw, String location) {
        if (!raw.isObject()) {
            throw new IllegalArgumentException(
                    "dependentRequired must be an object at " + location);
        }
        Map<String, Object> result = new LinkedHashMap<>();
        Iterator<Map.Entry<String, JsonNode>> fields = raw.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            if (!field.getValue().isArray()) {
                throw new IllegalArgumentException(
                        "dependentRequired entry '" + field.getKey()
                                + "' must be an array at " + location);
            }
            List<String> names = new ArrayList<>();
            for (JsonNode name : field.getValue()) {
                if (!name.isTextual()) {
                    throw new IllegalArgumentException(
                            "dependentRequired entry '" + field.getKey()
                                    + "' must contain strings at " + location);
                }
                names.add(name.textValue());
            }
            result.put(field.getKey(), names);
        }
        return result;
    }

    private static void recoverArray(
            JsonNode raw, List parsed, String location, OpenAPI api) {
        if (raw == null || !raw.isArray() || parsed == null) {
            return;
        }
        int count = Math.min(raw.size(), parsed.size());
        for (int i = 0; i < count; i++) {
            recoverChild(raw.get(i), parsed.get(i), location + "/" + i, api);
        }
    }

    private static void recoverMap(
            JsonNode raw, Map parsed, String location, OpenAPI api) {
        if (raw == null || !raw.isObject() || parsed == null) {
            return;
        }
        Iterator<Map.Entry<String, JsonNode>> fields = raw.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            Object parsedChild = parsed.get(field.getKey());
            if (parsedChild != null) {
                recoverChild(field.getValue(), parsedChild,
                        location + "/" + pointerSegment(field.getKey()), api);
            }
        }
    }

    private static void recoverChild(
            JsonNode raw, Object parsed, String location, OpenAPI api) {
        if (parsed instanceof Schema) {
            Schema parsedSchema = (Schema) parsed;
            // Synthetic inline-model refs need the raw object surface on the
            // carrier so additionalProperties can recognize declared names.
            restoreRawObjectKeywords(api, raw, parsedSchema, location);
            Schema target = recoveryTarget(api,
                    raw != null && raw.isObject() && !raw.has("$ref"), parsedSchema);
            recoverSchema(raw, target, location, api);
        }
    }

    static boolean hasExplicitDefault(Schema schema) {
        return hasTrueExtension(schema, DEFAULT_PRESENT_EXT);
    }

    static String defaultJsonOf(Schema schema) {
        if (schema == null || schema.getExtensions() == null) {
            return null;
        }
        Object value = schema.getExtensions().get(DEFAULT_JSON_EXT);
        return value == null ? null : String.valueOf(value);
    }

    static String examplesJsonOf(Schema schema) {
        if (schema == null || schema.getExtensions() == null) {
            return null;
        }
        Object value = schema.getExtensions().get(EXAMPLES_JSON_EXT);
        return value == null ? null : String.valueOf(value);
    }
    static boolean hasExplicitConst(Schema schema) {
        return hasTrueExtension(schema, CONST_PRESENT_EXT);
    }

    static String constJsonOf(Schema schema) {
        if (schema == null || schema.getExtensions() == null) {
            return null;
        }
        Object value = schema.getExtensions().get(CONST_JSON_EXT);
        return value == null ? null : String.valueOf(value);
    }

    static void restoreExplicitConst(Schema schema, String constJson) {
        if (schema == null) {
            return;
        }
        addExtension(schema, CONST_PRESENT_EXT, true);
        if (constJson != null) {
            addExtension(schema, CONST_JSON_EXT, constJson);
        }
    }

    static void restorePristineTypeNull(Schema schema) {
        if (schema != null) {
            addExtension(schema, TYPE_NULL_EXT, true);
        }
    }


    private static void addExtension(Schema schema, String key, Object value) {
        if (schema.getExtensions() == null) {
            schema.setExtensions(new LinkedHashMap<>());
        }
        schema.addExtension(key, value);
    }

    static String enumJsonOf(Schema schema) {
        if (schema == null || schema.getExtensions() == null) {
            return null;
        }
        Object value = schema.getExtensions().get(ENUM_JSON_EXT);
        return value == null ? null : String.valueOf(value);
    }

    static boolean isEmptyEnumMarked(Schema schema) {
        return hasTrueExtension(schema, EMPTY_ENUM_EXT);
    }

    static boolean pristineTypeHasNull(Schema schema) {
        return hasTrueExtension(schema, TYPE_NULL_EXT);
    }

    private static boolean hasTrueExtension(Schema schema, String key) {
        return schema != null
                && schema.getExtensions() != null
                && Boolean.TRUE.equals(schema.getExtensions().get(key));
    }

    private static String countBoundExtensionName(String keyword) {
        return "x-oas31-" + keyword + "-lexeme";
    }

    static String countBoundLexemeOf(Schema schema, String keyword) {
        if (schema == null || schema.getExtensions() == null) {
            return null;
        }
        Object value = schema.getExtensions().get(countBoundExtensionName(keyword));
        return value == null ? null : String.valueOf(value);
    }
}
