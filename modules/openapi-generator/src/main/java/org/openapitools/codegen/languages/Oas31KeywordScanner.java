package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.Encoding;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * OAS 3.1 dialect resolution and exhaustive schema-keyword scanning.
 *
 * <p>swagger-parser and swagger-models do not retain every OAS 3.1 schema
 * surface. This class resolves the effective document dialect and records the
 * schema keywords visible after parsing. Keywords are classified as emitted,
 * annotation-only, or explicitly rejected; unsupported required vocabularies
 * and unnormalized dynamic references fail closed.
 */
public final class Oas31KeywordScanner {

    // Bound recursion before a default JVM thread stack can be exhausted.
    private static final int MAX_SCHEMA_NESTING = 256;

    private Oas31KeywordScanner() {
    }

    /**
     * Resolve the effective schema dialect from the top-level
     * {@code jsonSchemaDialect} and/or the root {@code $schema}. Per OAS 3.1
     * the root {@code $schema} (when present at a document/schema-resource
     * root) takes precedence over {@code jsonSchemaDialect} for that resource.
     */
    public static CppBoostBeastClientCodegen.OasDialect resolveEffectiveDialect(
            String jsonSchemaDialect, String rootSchema) {
        String effective = StringUtils.isNotBlank(rootSchema) ? rootSchema : jsonSchemaDialect;
        if (StringUtils.isBlank(effective)) {
            return CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED;
        }
        String trimmed = effective.trim();
        if (CppBoostBeastClientCodegen.OAS_31_DIALECT.equals(trimmed)
                || CppBoostBeastClientCodegen.OAS_31_DIALECT_BASE_ALIAS.equals(trimmed)) {
            return CppBoostBeastClientCodegen.OasDialect.OAS_31;
        }
        if (CppBoostBeastClientCodegen.DRAFT_2020_12.equals(trimmed)) {
            return CppBoostBeastClientCodegen.OasDialect.DRAFT_2020_12_REC;
        }
        return CppBoostBeastClientCodegen.OasDialect.UNRECOGNIZED;
    }

    /** Resolve the effective dialect of an OpenAPI document from its knobs. */
    public static CppBoostBeastClientCodegen.OasDialect resolveDocumentDialect(
            OpenAPI api) {
        if (api == null) {
            return CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED;
        }
        String jsonSchemaDialect = api.getJsonSchemaDialect();
        if (jsonSchemaDialect != null) {
            return resolveEffectiveDialect(jsonSchemaDialect, null);
        }
        // No jsonSchemaDialect: for OAS 3.1 the pinned dialect is the default.
        return isOas31(api) ? CppBoostBeastClientCodegen.OasDialect.OAS_31
                : CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED;
    }

    /**
     * OAS 3 structural normative checks. Returns a list of human-readable
     * diagnostics; an empty list means the structure is normative. The caller
     * decides whether to fail generation (strict mode).
     */
    public static List<String> validateNormativeOas3Structure(OpenAPI api) {
        List<String> diagnostics = new ArrayList<>();
        if (api == null) {
            diagnostics.add("document is null; cannot satisfy OAS structural requirements");
            return diagnostics;
        }
        String version = api.getOpenapi();
        if (StringUtils.isBlank(version)) {
            diagnostics.add("missing root `openapi` version field (required for OAS 3.x)");
        } else if (!version.matches("3(\\.[0-9]+)*")) {
            diagnostics.add("unsupported openapi version '" + version
                    + "' (program targets OAS 3.0.x/3.1.x)");
        }
        io.swagger.v3.oas.models.info.Info info = api.getInfo();
        if (info == null) {
            diagnostics.add("missing root `info` object (required by OAS)");
        } else {
            if (StringUtils.isBlank(info.getTitle())) {
                diagnostics.add("missing `info.title` (required by OAS)");
            }
            if (StringUtils.isBlank(info.getVersion())) {
                diagnostics.add("missing `info.version` (required by OAS)");
            }
        }
        boolean hasPaths = api.getPaths() != null && !api.getPaths().isEmpty();
        boolean hasComponents = api.getComponents() != null;
        boolean hasWebhooks = api.getWebhooks() != null && !api.getWebhooks().isEmpty();
        if (!hasPaths && !hasComponents && !hasWebhooks) {
            diagnostics.add("missing at least one of `paths`, `components`, or `webhooks`");
        }
        return diagnostics;
    }

    /**
     * Dialect/metaschema policy gate: a dialect identifier that is not
     * recognized by this program must be refused (unknown required
     * vocabulary). The OAS 3.1 default applies when an OAS 3.1 document
     * declares no {@code jsonSchemaDialect}.
     *
     * <p>Required vocabularies and resource-level dialect selectors are checked
     * on every schema surface visible through swagger-models.
     */
    public static List<String> validateDialectPolicy(OpenAPI api) {
        List<String> diagnostics = new ArrayList<>();
        if (api == null) {
            return diagnostics;
        }
        CppBoostBeastClientCodegen.OasDialect dialect = resolveDocumentDialect(api);
        if (dialect == CppBoostBeastClientCodegen.OasDialect.UNRECOGNIZED) {
            diagnostics.add("unrecognized jsonSchemaDialect '" + api.getJsonSchemaDialect()
                    + "' — unknown required vocabulary/dialect, fail generation");
        }
        for (KeywordOccurrence occurrence
                : scanSchemaKeywordOccurrences(api).withStatus(
                        KeywordOccurrenceStatus.FAIL_CLOSED)) {
            if ("$schema".equals(occurrence.getKeyword())
                    || "$vocabulary".equals(occurrence.getKeyword())
                    || "$dynamicRef".equals(occurrence.getKeyword())) {
                diagnostics.add(occurrence.getDetail() + " at "
                        + occurrence.getLocation());
            }
        }
        return diagnostics;
    }

    /** True when the document declares an OAS 3.1 version. */
    static boolean isOas31(OpenAPI api) {
        if (api == null) {
            return false;
        }
        String version = api.getOpenapi();
        return version != null
                && version.matches("3\\.1(?:\\.[0-9]+)?(?:[-+][0-9A-Za-z.-]+)?");
    }

    // ========================================================================
    // Exhaustive schema-valued-position scanner
    // ========================================================================

    /** Core vocabulary identifier (2020-12). */
    public static final String VOCAB_CORE = "https://json-schema.org/draft/2020-12/vocab/core";
    /** Applicator vocabulary identifier (2020-12). */
    public static final String VOCAB_APPLICATOR = "https://json-schema.org/draft/2020-12/vocab/applicator";
    /** Unevaluated vocabulary identifier (2020-12). */
    public static final String VOCAB_UNEVALUATED = "https://json-schema.org/draft/2020-12/vocab/unevaluated";
    /** Validation vocabulary identifier (2020-12). */
    public static final String VOCAB_VALIDATION = "https://json-schema.org/draft/2020-12/vocab/validation";
    /** Metadata vocabulary identifier (2020-12). */
    public static final String VOCAB_METADATA = "https://json-schema.org/draft/2020-12/vocab/meta-data";
    /** Format-annotation vocabulary identifier (2020-12). */
    public static final String VOCAB_FORMAT = "https://json-schema.org/draft/2020-12/vocab/format-annotation";
    /** Content vocabulary identifier (2020-12). */
    public static final String VOCAB_CONTENT = "https://json-schema.org/draft/2020-12/vocab/content";
    /** OAS base vocabulary identifier (OAS 3.1). */
    public static final String VOCAB_OAS_BASE = "https://spec.openapis.org/oas/3.1/vocab/base";
    private static final Set<String> SUPPORTED_REQUIRED_VOCABULARIES = Set.of(
            VOCAB_CORE,
            VOCAB_APPLICATOR,
            VOCAB_UNEVALUATED,
            VOCAB_VALIDATION,
            VOCAB_METADATA,
            VOCAB_FORMAT,
            VOCAB_CONTENT,
            VOCAB_OAS_BASE);

    /** Classification of one visible keyword occurrence. */
    public enum KeywordOccurrenceStatus {
        /** A validator or structural handler is emitted for this keyword. */
        EMITTED,
        /** The generator rejects this keyword rather than ignoring it. */
        FAIL_CLOSED,
        /** The keyword has no validity effect in the active vocabulary. */
        ANNOTATION
    }

    /** One keyword occurrence at a schema-valued position. */
    public static final class KeywordOccurrence {
        private final String keyword;
        private final String location;
        private final String vocabularyUri;
        private final KeywordOccurrenceStatus status;
        private final String detail;

        /** Create an occurrence record. */
        public KeywordOccurrence(String keyword, String location, String vocabularyUri,
                                 KeywordOccurrenceStatus status, String detail) {
            this.keyword = keyword;
            this.location = location;
            this.vocabularyUri = vocabularyUri;
            this.status = status;
            this.detail = detail;
        }

        /** The keyword that occurred. */
        public String getKeyword() {
            return keyword;
        }

        /** JSON-pointer-like location of the occurrence. */
        public String getLocation() {
            return location;
        }

        /** Vocabulary URI declaring the keyword. */
        public String getVocabularyUri() {
            return vocabularyUri;
        }

        /** Classification of the occurrence. */
        public KeywordOccurrenceStatus getStatus() {
            return status;
        }

        /** Human-readable detail, or null. */
        public String getDetail() {
            return detail;
        }

        @Override
        public String toString() {
            return status + "[" + keyword + "]@" + location
                    + (detail == null || detail.isEmpty() ? "" : " (" + detail + ")");
        }
    }

    /** Ordered keyword occurrence ledger with status aggregation. */
    public static final class KeywordOccurrenceLedger {
        private final List<KeywordOccurrence> occurrences = new ArrayList<>();
        private final LinkedHashMap<String, List<KeywordOccurrence>> byKeyword =
                new LinkedHashMap<>();

        /** Record one occurrence. */
        void add(KeywordOccurrence occurrence) {
            occurrences.add(occurrence);
            byKeyword.computeIfAbsent(occurrence.getKeyword(), k -> new ArrayList<>())
                    .add(occurrence);
        }

        /** All recorded occurrences, in discovery order. */
        public List<KeywordOccurrence> getOccurrences() {
            return Collections.unmodifiableList(new ArrayList<>(occurrences));
        }

        /** Occurrences for one keyword, or an empty list when none. */
        public List<KeywordOccurrence> forKeyword(String keyword) {
            return byKeyword.getOrDefault(keyword, Collections.emptyList());
        }

        /** Whether any occurrence was recorded for this keyword. */
        public boolean hasKeyword(String keyword) {
            return byKeyword.containsKey(keyword);
        }

        /** All keywords with at least one occurrence. */
        public Set<String> getKeywords() {
            return Collections.unmodifiableSet(new LinkedHashSet<>(byKeyword.keySet()));
        }

        /** Occurrences filtered by status. */
        public List<KeywordOccurrence> withStatus(KeywordOccurrenceStatus status) {
            List<KeywordOccurrence> out = new ArrayList<>();
            for (KeywordOccurrence o : occurrences) {
                if (o.getStatus() == status) {
                    out.add(o);
                }
            }
            return out;
        }


        /** Keywords with at least one FAIL_CLOSED occurrence. */
        public Set<String> failClosed() {
            Set<String> out = new LinkedHashSet<>();
            for (KeywordOccurrence o : occurrences) {
                if (o.getStatus() == KeywordOccurrenceStatus.FAIL_CLOSED) {
                    out.add(o.getKeyword());
                }
            }
            return Collections.unmodifiableSet(out);
        }

        /** Keywords with at least one EMITTED occurrence. */
        public Set<String> emitted() {
            Set<String> out = new LinkedHashSet<>();
            for (KeywordOccurrence o : occurrences) {
                if (o.getStatus() == KeywordOccurrenceStatus.EMITTED) {
                    out.add(o.getKeyword());
                }
            }
            return Collections.unmodifiableSet(out);
        }

        /** Total number of recorded occurrences. */
        public int size() {
            return occurrences.size();
        }
    }

    /**
     * Exhaustive schema-valued-position scanner. Walks component schemas plus
     * every inline schema reachable from paths, webhooks, callbacks, reusable
     * parameters, headers, request bodies, responses, and path items. Each
     * schema is then traversed through every schema-valued JSON Schema 2020-12
     * child keyword. Reference targets ({@code $ref}) are not followed here;
     * reusable targets are scanned at their component location.
     */
    public static KeywordOccurrenceLedger scanSchemaKeywordOccurrences(OpenAPI api) {
        KeywordOccurrenceLedger ledger = new KeywordOccurrenceLedger();
        rootSchemaPositions(api).forEach((location, schema) ->
                scanSchemaNode(schema, location, ledger, 0));
        return ledger;
    }

    /** Maps every OAS-hosted root schema without following schema keywords. */
    static Map<String, Schema> rootSchemaPositions(OpenAPI api) {
        Map<String, Schema> positions = new LinkedHashMap<>();
        if (api == null) {
            return positions;
        }

        Components components = api.getComponents();
        if (components != null) {
            collectSchemas(components.getSchemas(), "#/components/schemas", positions);
            collectParameters(components.getParameters(), "#/components/parameters", positions);
            collectHeaders(components.getHeaders(), "#/components/headers", positions);
            collectRequestBodies(
                    components.getRequestBodies(), "#/components/requestBodies", positions);
            collectResponses(components.getResponses(), "#/components/responses", positions);
            collectCallbacks(components.getCallbacks(), "#/components/callbacks", positions);
            collectPathItems(components.getPathItems(), "#/components/pathItems", positions);
        }
        collectPathItems(api.getPaths(), "#/paths", positions);
        collectPathItems(api.getWebhooks(), "#/webhooks", positions);
        return positions;
    }

    private static void collectSchemas(Map<String, Schema> schemas, String location,
                                       Map<String, Schema> positions) {
        if (schemas == null) {
            return;
        }
        schemas.forEach((name, schema) -> putSchema(schema,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectPathItems(Map<String, PathItem> pathItems, String location,
                                         Map<String, Schema> positions) {
        if (pathItems == null) {
            return;
        }
        pathItems.forEach((name, pathItem) -> collectPathItem(pathItem,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectPathItem(PathItem pathItem, String location,
                                        Map<String, Schema> positions) {
        if (pathItem == null) {
            return;
        }
        collectParameterList(pathItem.getParameters(), location + "/parameters", positions);
        if (pathItem.readOperationsMap() != null) {
            pathItem.readOperationsMap().forEach((method, operation) -> collectOperation(operation,
                    location + "/" + method.name().toLowerCase(Locale.ROOT), positions));
        }
    }

    private static void collectOperation(Operation operation, String location,
                                         Map<String, Schema> positions) {
        if (operation == null) {
            return;
        }
        collectParameterList(operation.getParameters(), location + "/parameters", positions);
        collectRequestBody(operation.getRequestBody(), location + "/requestBody", positions);
        collectResponses(operation.getResponses(), location + "/responses", positions);
        collectCallbacks(operation.getCallbacks(), location + "/callbacks", positions);
    }

    private static void collectParameters(Map<String, Parameter> parameters, String location,
                                          Map<String, Schema> positions) {
        if (parameters == null) {
            return;
        }
        parameters.forEach((name, parameter) -> collectParameter(parameter,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectParameterList(List<Parameter> parameters, String location,
                                             Map<String, Schema> positions) {
        if (parameters == null) {
            return;
        }
        for (int i = 0; i < parameters.size(); i++) {
            collectParameter(parameters.get(i), location + "/" + i, positions);
        }
    }

    private static void collectParameter(Parameter parameter, String location,
                                         Map<String, Schema> positions) {
        if (parameter != null) {
            putSchema(parameter.getSchema(), location + "/schema", positions);
            collectContent(parameter.getContent(), location + "/content", positions);
        }
    }

    private static void collectHeaders(Map<String, Header> headers, String location,
                                       Map<String, Schema> positions) {
        if (headers == null) {
            return;
        }
        headers.forEach((name, header) -> collectHeader(header,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectHeader(Header header, String location,
                                      Map<String, Schema> positions) {
        if (header != null) {
            putSchema(header.getSchema(), location + "/schema", positions);
            collectContent(header.getContent(), location + "/content", positions);
        }
    }

    private static void collectRequestBodies(Map<String, RequestBody> requestBodies,
                                             String location, Map<String, Schema> positions) {
        if (requestBodies == null) {
            return;
        }
        requestBodies.forEach((name, requestBody) -> collectRequestBody(requestBody,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectRequestBody(RequestBody requestBody, String location,
                                           Map<String, Schema> positions) {
        if (requestBody != null) {
            collectContent(requestBody.getContent(), location + "/content", positions);
        }
    }

    private static void collectResponses(Map<String, ApiResponse> responses, String location,
                                         Map<String, Schema> positions) {
        if (responses == null) {
            return;
        }
        responses.forEach((name, response) -> collectResponse(response,
                location + "/" + pointerSegment(name), positions));
    }

    private static void collectResponse(ApiResponse response, String location,
                                        Map<String, Schema> positions) {
        if (response != null) {
            collectContent(response.getContent(), location + "/content", positions);
            collectHeaders(response.getHeaders(), location + "/headers", positions);
        }
    }

    private static void collectCallbacks(Map<String, Callback> callbacks, String location,
                                         Map<String, Schema> positions) {
        if (callbacks == null) {
            return;
        }
        callbacks.forEach((name, callback) -> {
            if (callback != null) {
                collectPathItems(callback, location + "/" + pointerSegment(name), positions);
            }
        });
    }

    private static void collectContent(Content content, String location,
                                       Map<String, Schema> positions) {
        if (content == null) {
            return;
        }
        content.forEach((mediaName, mediaType) -> collectMediaType(mediaType,
                location + "/" + pointerSegment(mediaName), positions));
    }

    private static void collectMediaType(MediaType mediaType, String location,
                                         Map<String, Schema> positions) {
        if (mediaType == null) {
            return;
        }
        putSchema(mediaType.getSchema(), location + "/schema", positions);
        if (mediaType.getEncoding() == null) {
            return;
        }
        for (Map.Entry<String, Encoding> entry : mediaType.getEncoding().entrySet()) {
            Encoding encoding = entry.getValue();
            if (encoding != null) {
                collectHeaders(encoding.getHeaders(), location + "/encoding/"
                        + pointerSegment(entry.getKey()) + "/headers", positions);
            }
        }
    }

    private static void putSchema(Schema schema, String location,
                                  Map<String, Schema> positions) {
        if (schema != null) {
            positions.put(location, schema);
        }
    }

    private static String pointerSegment(String value) {
        return value == null ? "null" : value.replace("~", "~0").replace("/", "~1");
    }

    private static void scanSchemaNode(Schema<?> schema, String location,
                                       KeywordOccurrenceLedger ledger, int depth) {
        if (schema == null) {
            return;
        }
        if (depth > MAX_SCHEMA_NESTING) {
            throw new IllegalArgumentException(
                    "Schema nesting exceeds maximum depth of "
                            + MAX_SCHEMA_NESTING + " at " + location);
        }

        // ---- Core / identifier keywords ----
        if (schema.get$id() != null) {
            record(ledger, "$id", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.ANNOTATION, "resource identifier");
        }
        if (schema.get$schema() != null) {
            boolean recognized = resolveEffectiveDialect(null, schema.get$schema())
                    != CppBoostBeastClientCodegen.OasDialect.UNRECOGNIZED;
            record(ledger, "$schema", location, VOCAB_CORE,
                    recognized ? KeywordOccurrenceStatus.ANNOTATION
                            : KeywordOccurrenceStatus.FAIL_CLOSED,
                    recognized ? "recognized resource dialect"
                            : "unrecognized schema resource dialect '"
                                    + schema.get$schema() + "'");
        }
        if (schema.get$ref() != null) {
            record(ledger, "$ref", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.EMITTED, "reference");
        }
        if (schema.get$anchor() != null) {
            record(ledger, "$anchor", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.ANNOTATION, "plain-name fragment");
        }
        if (schema.get$dynamicAnchor() != null) {
            record(ledger, "$dynamicAnchor", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.ANNOTATION,
                    "dynamic plain-name fragment");
        }
        if (schema.get$dynamicRef() != null) {
            boolean normalized = schema.getExtensions() != null
                    && schema.getExtensions().containsKey("x-oas31-dynref");
            record(ledger, "$dynamicRef", location, VOCAB_CORE,
                    normalized ? KeywordOccurrenceStatus.EMITTED
                            : KeywordOccurrenceStatus.FAIL_CLOSED,
                    normalized ? "normalized dynamic reference"
                            : "unsupported unnormalized $dynamicRef '"
                                    + schema.get$dynamicRef() + "'");
        }
        if (schema.get$comment() != null) {
            record(ledger, "$comment", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.ANNOTATION, "no validity effect");
        }
        if (schema.get$vocabulary() != null) {
            // swagger-models exposes $vocabulary as a String, so required
            // vocabulary flags cannot be inspected without guessing.
            record(ledger, "$vocabulary", location, VOCAB_CORE,
                    KeywordOccurrenceStatus.FAIL_CLOSED,
                    "$vocabulary is not representable by swagger-models");
        }

        // ---- Validation vocabulary ----
        if (schema.getType() != null
                || (schema.getTypes() != null && !schema.getTypes().isEmpty())) {
            record(ledger, "type", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-type / validation-type-array");
        }
        if (schema.getEnum() != null) {
            record(ledger, "enum", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-enum-values");
        }
        if (schema.getConst() != null || Oas31RawSpecRecovery.hasExplicitConst(schema)) {
            record(ledger, "const", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-const; exact-math caveat follows");
        }
        boolean hasMinimum = schema.getMinimum() != null
                || schema.getExclusiveMinimum() != null
                || schema.getExclusiveMinimumValue() != null;
        boolean hasMaximum = schema.getMaximum() != null
                || schema.getExclusiveMaximum() != null
                || schema.getExclusiveMaximumValue() != null;
        if (hasMinimum) {
            record(ledger, "minimum", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "numeric-range; 3.0 boolean exclusiveMinimum preserved");
        }
        if (hasMaximum) {
            record(ledger, "maximum", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "numeric-range; 3.0 boolean exclusiveMaximum preserved");
        }
        if (schema.getMultipleOf() != null) {
            record(ledger, "multipleOf", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED, "validation-multiple-of; exact-math caveat");
        }
        if (schema.getMinLength() != null) {
            record(ledger, "minLength", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-min-length; Unicode code points; decimal lexemes preserved");
        }
        if (schema.getMaxLength() != null) {
            record(ledger, "maxLength", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-max-length; Unicode code points; decimal lexemes preserved");
        }
        if (schema.getPattern() != null) {
            record(ledger, "pattern", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "ECMAScript-subset unanchored regex_search; \\p{Letter} range translation");
        }
        if (schema.getMinItems() != null) {
            record(ledger, "minItems", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-min-items");
        }
        if (schema.getMaxItems() != null) {
            record(ledger, "maxItems", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-max-items");
        }
        if (Boolean.TRUE.equals(schema.getUniqueItems())) {
            record(ledger, "uniqueItems", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED, "validation-unique-items; exact-math caveat");
        }
        if (schema.getRequired() != null && !schema.getRequired().isEmpty()) {
            record(ledger, "required", location, VOCAB_VALIDATION, KeywordOccurrenceStatus.EMITTED,
                    "validation-required / object-properties");
        }
        if (schema.getMinProperties() != null) {
            record(ledger, "minProperties", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED, "validation-min-properties");
        }
        if (schema.getMaxProperties() != null) {
            record(ledger, "maxProperties", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED, "validation-max-properties");
        }
        if (schema.getMinContains() != null) {
            record(ledger, "minContains", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-min-contains; exact count bound, inert without contains");
        }
        if (schema.getMaxContains() != null) {
            record(ledger, "maxContains", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-max-contains; exact count bound, inert without contains");
        }
        if (schema.getDependentRequired() != null && !schema.getDependentRequired().isEmpty()) {
            record(ledger, "dependentRequired", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED, "validation-dependent-required");
        }

        // ---- Applicator vocabulary ----
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            record(ledger, "properties", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED, "object model emission");
            for (Map.Entry<String, Schema> p : schema.getProperties().entrySet()) {
                scanSchemaNode(p.getValue(), location + "/properties/" + p.getKey(),
                        ledger, depth + 1);
            }
        }
        if (schema.getPatternProperties() != null && !schema.getPatternProperties().isEmpty()) {
            record(ledger, "patternProperties", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-pattern-properties; pattern engine — no longer a silent skip");
            for (Map.Entry<String, Schema> p : schema.getPatternProperties().entrySet()) {
                scanSchemaNode(p.getValue(), location + "/patternProperties/" + p.getKey(),
                        ledger, depth + 1);
            }
        }
        if (schema.getAdditionalProperties() != null) {
            record(ledger, "additionalProperties", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED,
                    "additionalProperties tri-state; pattern-covered keys exempt");
            Object addProp = schema.getAdditionalProperties();
            if (addProp instanceof Schema) {
                scanSchemaNode((Schema) addProp, location + "/additionalProperties",
                        ledger, depth + 1);
            }
        }
        if (schema.getPropertyNames() != null) {
            record(ledger, "propertyNames", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED, "validation-property-names");
            scanSchemaNode(schema.getPropertyNames(), location + "/propertyNames",
                    ledger, depth + 1);
        }
        if (schema.getDependentSchemas() != null && !schema.getDependentSchemas().isEmpty()) {
            record(ledger, "dependentSchemas", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED, "validation-dependent-schemas");
            for (Map.Entry<String, Schema> d : schema.getDependentSchemas().entrySet()) {
                scanSchemaNode(d.getValue(), location + "/dependentSchemas/" + d.getKey(),
                        ledger, depth + 1);
            }
        }
        if (schema.getPrefixItems() != null && !schema.getPrefixItems().isEmpty()) {
            record(ledger, "prefixItems", location, VOCAB_APPLICATOR,
                    KeywordOccurrenceStatus.EMITTED, "validation-prefix-items; tuple by index");
            for (int i = 0; i < schema.getPrefixItems().size(); i++) {
                scanSchemaNode(schema.getPrefixItems().get(i),
                        location + "/prefixItems/" + i, ledger, depth + 1);
            }
        }
        if (schema.getItems() != null) {
            record(ledger, "items", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "element typing via array storage");
            scanSchemaNode(schema.getItems(), location + "/items", ledger, depth + 1);
        }
        if (schema.getContains() != null) {
            record(ledger, "contains", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "validation-contains-schema; matched indices feed unevaluatedItems");
            scanSchemaNode(schema.getContains(), location + "/contains", ledger, depth + 1);
        }
        if (schema.getNot() != null) {
            record(ledger, "not", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "validation-not-schema; shared evaluator");
            scanSchemaNode(schema.getNot(), location + "/not", ledger, depth + 1);
        }
        if (schema.getIf() != null || schema.getThen() != null || schema.getElse() != null) {
            if (schema.getIf() != null) {
                record(ledger, "if", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                        "validation-if; transactional guard");
                scanSchemaNode(schema.getIf(), location + "/if", ledger, depth + 1);
            }
            if (schema.getThen() != null) {
                record(ledger, "then", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                        "validation-then; applied branch only");
                scanSchemaNode(schema.getThen(), location + "/then", ledger, depth + 1);
            }
            if (schema.getElse() != null) {
                record(ledger, "else", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                        "validation-else; applied branch only");
                scanSchemaNode(schema.getElse(), location + "/else", ledger, depth + 1);
            }
        }
        if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
            record(ledger, "allOf", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "composition; getUnsupportedAssertions may carry FAIL_CLOSED branches");
            for (int i = 0; i < schema.getAllOf().size(); i++) {
                scanSchemaNode(schema.getAllOf().get(i), location + "/allOf/" + i,
                        ledger, depth + 1);
            }
        }
        if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
            record(ledger, "anyOf", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "composition");
            for (int i = 0; i < schema.getAnyOf().size(); i++) {
                scanSchemaNode(schema.getAnyOf().get(i), location + "/anyOf/" + i,
                        ledger, depth + 1);
            }
        }
        if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
            record(ledger, "oneOf", location, VOCAB_APPLICATOR, KeywordOccurrenceStatus.EMITTED,
                    "composition");
            for (int i = 0; i < schema.getOneOf().size(); i++) {
                scanSchemaNode(schema.getOneOf().get(i), location + "/oneOf/" + i,
                        ledger, depth + 1);
            }
        }

        // ---- Unevaluated vocabulary (validity semantics live; interplay with
        //      if/then/else, contains, $dynamicRef handled by the evaluator) ----
        if (schema.getUnevaluatedProperties() != null) {
            record(ledger, "unevaluatedProperties", location, VOCAB_UNEVALUATED,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-unevaluated-properties (reject/schema forms)");
            Schema<?> up = schema.getUnevaluatedProperties();
            scanSchemaNode(up, location + "/unevaluatedProperties", ledger, depth + 1);
        }
        if (schema.getUnevaluatedItems() != null) {
            record(ledger, "unevaluatedItems", location, VOCAB_UNEVALUATED,
                    KeywordOccurrenceStatus.EMITTED,
                    "validation-unevaluated-items; evaluation-path semantics");
            Schema ui = schema.getUnevaluatedItems();
            scanSchemaNode(ui, location + "/unevaluatedItems", ledger, depth + 1);
        }

        // ---- Metadata vocabulary: annotation only ----
        if (schema.getTitle() != null) {
            record(ledger, "title", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (schema.getDescription() != null) {
            record(ledger, "description", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (schema.getDefault() != null) {
            record(ledger, "default", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, "annotation only; never injected");
        }
        if (Boolean.TRUE.equals(schema.getDeprecated())) {
            record(ledger, "deprecated", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (Boolean.TRUE.equals(schema.getReadOnly())) {
            record(ledger, "readOnly", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (Boolean.TRUE.equals(schema.getWriteOnly())) {
            record(ledger, "writeOnly", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (schema.getExamples() != null && !schema.getExamples().isEmpty()) {
            record(ledger, "examples", location, VOCAB_METADATA,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }

        // ---- Format-annotation vocabulary ----
        if (schema.getFormat() != null) {
            record(ledger, "format", location, VOCAB_FORMAT,
                    KeywordOccurrenceStatus.ANNOTATION, "annotation by default; strict opt-in");
        }

        // ---- Content vocabulary ----
        if (schema.getContentEncoding() != null) {
            record(ledger, "contentEncoding", location, VOCAB_CONTENT,
                    KeywordOccurrenceStatus.ANNOTATION, "annotation; no auto-decode");
        }
        if (schema.getContentMediaType() != null) {
            record(ledger, "contentMediaType", location, VOCAB_CONTENT,
                    KeywordOccurrenceStatus.ANNOTATION, "annotation; no auto-decode");
        }
        if (schema.getContentSchema() != null) {
            record(ledger, "contentSchema", location, VOCAB_CONTENT,
                    KeywordOccurrenceStatus.ANNOTATION, "schema-valued annotation; child indexed");
            scanSchemaNode(schema.getContentSchema(), location + "/contentSchema",
                    ledger, depth + 1);
        }

        // ---- OAS base vocabulary: annotation only ----
        if (schema.getDiscriminator() != null) {
            record(ledger, "discriminator", location, VOCAB_OAS_BASE,
                    KeywordOccurrenceStatus.ANNOTATION, "validation-neutral candidate-order hint");
        }
        if (schema.getXml() != null) {
            record(ledger, "xml", location, VOCAB_OAS_BASE, KeywordOccurrenceStatus.ANNOTATION,
                    null);
        }
        if (schema.getExternalDocs() != null) {
            record(ledger, "externalDocs", location, VOCAB_OAS_BASE,
                    KeywordOccurrenceStatus.ANNOTATION, null);
        }
        if (schema.getExample() != null || schema.getExampleSetFlag()) {
            record(ledger, "example", location, VOCAB_OAS_BASE,
                    KeywordOccurrenceStatus.ANNOTATION, "OAS singular example, 3.0 dual-path");
        }

        // ---- 3.0 dual-path compatibility keywords ----
        if (Boolean.TRUE.equals(schema.getNullable())) {
            record(ledger, "nullable", location, VOCAB_OAS_BASE, KeywordOccurrenceStatus.EMITTED,
                    "3.0 nullable dual-path; tri-state NullableField");
        }
        if (schema.getBooleanSchemaValue() != null) {
            record(ledger, "boolean-schema", location, VOCAB_VALIDATION,
                    KeywordOccurrenceStatus.EMITTED,
                    "boolean value-schema; SUPPORTED in OAS 3.1; OAS 3.0 rejects a bare boolean"
                            + " schema (documented dual-path limitation)");
        }
    }

    /** Record one occurrence in the ledger. */
    private static void record(KeywordOccurrenceLedger ledger, String keyword, String location,
                               String vocabularyUri, KeywordOccurrenceStatus status,
                               String detail) {
        ledger.add(new KeywordOccurrence(keyword, location, vocabularyUri, status, detail));
    }


    /**
     * Set of fail-closed required-vocabulary keywords actually encountered for this
     * document (the keywords the generator refuses rather than silently accepting).
     */
    public static Set<String> failClosedKeywords(OpenAPI api) {
        return scanSchemaKeywordOccurrences(api).failClosed();
    }

}