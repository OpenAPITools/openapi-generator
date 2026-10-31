package org.openapitools.codegen.config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import io.swagger.v3.core.util.Json31;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.openapitools.codegen.utils.Oas32Extensions;
import org.openapitools.codegen.utils.SpecUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * A {@link SpecLoader} that lets the released swagger-parser (which only knows
 * OpenAPI up to 3.1) read {@code openapi: 3.2.x} documents.
 *
 * <p>The raw document is rewritten as a Jackson tree before parsing:
 * the {@code openapi} version is lowered to {@code 3.1.0} and every key that
 * only exists in OpenAPI 3.2 (a fixed table taken from the 3.2.1
 * specification) is moved to an {@code x-oas32-*} specification extension.
 * The {@code query} and {@code additionalOperations} path-item members,
 * {@code in: "querystring"} parameters and a parameter {@code style} of
 * {@code "cookie"} (3.2-only, lowered to {@code "form"}) get the same
 * treatment, and response objects without a {@code description} — legal in
 * 3.2 — are given an empty one so they pass validation. The moved
 * operations are read back through
 * {@link org.openapitools.codegen.utils.Oas32Extensions}.
 *
 * <p>Known limits of the compatibility read, each warned about when it
 * applies: {@code $ref}s that point into a member the rewrite moved (for
 * example {@code "#/components/mediaTypes/..."}) can no longer resolve;
 * a schema {@code $id} is not used as the base URI — relative
 * {@code $ref}s inside its scope resolve against the document location
 * like every other ref (the same result the 3.1 parser produces when
 * the {@code $id} base is unavailable), so a document relying on a
 * working {@code $id} base would differ; and a root {@code $self} is
 * preserved as an extension but never used as the reference base.
 * Documents reached through external {@code $ref}s are parsed without
 * the rewrite, so 3.2-only members in them may be dropped (a warning
 * names each such document).
 *
 * <p>The rewritten text is parsed with
 * {@link OpenAPIV3Parser#readContents(String, List, ParseOptions, String)}
 * passing a {@code null} location: the dereferencer would otherwise re-read
 * the original (unrewritten) file even for same-file {@code "#/..."}
 * references. Relative {@code $ref}s are made absolute against the
 * {@code inputSpec} location up front instead — URLs stay URLs and local
 * files become plain absolute filesystem paths, the only form the parser's
 * {@code Visitor.readURI}/{@code readFile} reads correctly on every OS —
 * so they still resolve against the original document location and
 * independent of the working directory. A relative {@code $ref} that
 * resolves back to the input document itself (for example
 * {@code "selfname.yaml#/components/schemas/X"} inside
 * {@code selfname.yaml}) collapses to its {@code "#fragment"}, so it is
 * served from the rewritten tree like any same-document ref.
 *
 * <p>The rewritten root carries the {@code x-oas32-openapi} extension to
 * keep the original version readable. Whether a document went through
 * this loader is not written into it — a marker extension would leak
 * into generated output, and an authored document could declare the
 * marker's name — so the parsed {@link OpenAPI} instance is remembered
 * by identity instead (see
 * {@link org.openapitools.codegen.utils.Oas32Extensions#markCompatDocument}):
 * only instances this loader returned have their {@code x-oas32-*}
 * members interpreted.
 *
 * <p>Operations moved to {@code x-oas32-query} /
 * {@code x-oas32-additionalOperations} are additionally mounted under
 * synthetic {@code /__oas32__/<n>} paths as {@code post} operations before
 * parsing, so the single parser read applies the same parameter defaults and
 * {@code $ref} resolution to them that fixed-method operations get. After
 * parsing each synthetic path's operation is grafted back onto the path
 * item's extension (replacing the loosely-typed map the extension
 * deserializer produced), matched by the {@code x-oas32-op-id} marker stamped
 * into the operation, and the synthetic paths are removed — they never
 * appear in the returned document, and parser messages that name them are
 * rewritten to the operation's original location (for example
 * {@code paths.'/t'(query)}). Members of the patterned maps that spec
 * 3.2.1 allows to carry specification extensions — the {@code paths}
 * map (Paths Object), an Operation's {@code responses} map (Responses
 * Object) and Callback Objects — whose key starts with {@code x-} are
 * specification extensions: opaque data, never path items, so they are
 * neither rewritten nor staged. Every other name -&gt; object map is a
 * plain map without extension slots — {@code webhooks} and the
 * {@code callbacks} maps included — where an {@code x-}-prefixed name
 * (for example a webhook called {@code x-ping}) is a real member.
 *
 * <p>See <a href="https://github.com/OpenAPITools/openapi-generator/issues/25477">issue #25477</a>.
 */
class Oas32CompatSpecLoader implements SpecLoader {

    private static final Logger LOGGER = LoggerFactory.getLogger(Oas32CompatSpecLoader.class);

    private static final String EXT_PREFIX = "x-oas32-";
    private static final String COMPAT_VERSION = "3.1.0";
    /**
     * Path name prefix under which moved operations are mounted for parsing.
     * Removed from the parsed document; a real path starting with it is left
     * untouched (mounting skips names already present).
     */
    private static final String SYNTHETIC_PATH_PREFIX = "/__oas32__/";
    /**
     * Marker stamped into every moved operation before parsing, holding its
     * synthetic path name; it lets the graft step match the loosely-typed
     * extension map to the parsed {@link Operation}. Removed wherever found.
     * The {@code x-oas32-} prefix is this loader's reserved namespace.
     */
    private static final String OPERATION_MARKER_EXT = EXT_PREFIX + "op-id";

    /**
     * Keys whose values are payload data, not structural objects: a
     * {@code "$ref"} member inside them is document content (for example an
     * example value that happens to contain a property named {@code "$ref"})
     * and must never be treated as a resolvable reference. Includes the
     * 3.2 Example Object value fields both before and after the rewrite
     * renames them ({@code dataValue} -&gt; {@code x-oas32-dataValue}).
     */
    private static final Set<String> DATA_VALUE_KEYS = Set.of(
            "example", "examples", "default", "const", "enum", "value",
            "dataValue", "serializedValue",
            "x-oas32-dataValue", "x-oas32-serializedValue");

    /**
     * The {@code x-oas32-*} members this loader creates whose values are
     * structural objects — moved operations, media types, schemas and
     * encodings — that may themselves contain relative {@code $ref}s to
     * absolutize. Every other {@code x-*} member (user extensions, and the
     * loader's moved data payloads such as {@code x-oas32-dataValue}) is
     * opaque document content whose {@code $ref}-looking strings are never
     * rewritten. Kept in sync with the {@code moveKey} calls in the
     * rewrite pass below.
     */
    private static final Set<String> STRUCTURAL_EXTENSIONS = Set.of(
            EXT_PREFIX + "query", EXT_PREFIX + "additionalOperations",
            EXT_PREFIX + "mediaTypes", EXT_PREFIX + "itemSchema",
            EXT_PREFIX + "itemEncoding", EXT_PREFIX + "prefixEncoding",
            EXT_PREFIX + "encoding", EXT_PREFIX + "deviceAuthorization");

    /**
     * Link Object members whose values are payload data — literal values or
     * {@code {expression}} strings — not objects to walk: a {@code "$ref"}
     * inside them is document content, not a reference. ({@code parameters}
     * is context-dependent: inside a Link Object it is an expression map;
     * elsewhere it is an array of Parameter Objects or the
     * {@code components.parameters} name map, both still walked.)
     */
    private static final Set<String> LINK_DATA_KEYS =
            Set.of("parameters", "requestBody");

    /**
     * Object members whose values are name -&gt; object maps (components
     * sections, schema property maps, media-type {@code "examples"}, ...):
     * the member names are arbitrary and may collide with
     * {@link #DATA_VALUE_KEYS} — a property named {@code "example"} or a
     * {@code "default"} response holding a real {@code $ref} must still be
     * rewritten, so member names in these maps are never treated as data
     * keywords. Covers the name -&gt; object members of OAS 3.1/3.2
     * ({@code paths}, {@code webhooks}, {@code content}, {@code encoding},
     * {@code variables}, the Components Object maps and the 3.1/3.2-added
     * {@code pathItems}/{@code mediaTypes}) and the JSON Schema name -&gt;
     * schema maps ({@code properties}, {@code patternProperties},
     * {@code dependentSchemas}, {@code $defs}, legacy {@code definitions}).
     * {@code components.mediaTypes} and {@code additionalOperations} are
     * already renamed to {@code x-oas32-*} keys by the rewrite, so those
     * names are listed too.
     * Name -&gt; string(-list) maps such as {@code discriminator.mapping},
     * OAuth {@code scopes}, {@code dependentRequired} or {@code security}
     * are not listed: their contents cannot hold references, and their
     * member names are opaque data (see {@link #STRING_MAP_KEYS}).
     */
    private static final Set<String> NAME_MAP_KEYS = Set.of(
            "properties", "patternProperties", "dependentSchemas",
            "$defs", "definitions",
            "paths", "webhooks", "content", "encoding", "examples", "variables",
            "schemas", "responses", "parameters", "requestBodies", "headers",
            "securitySchemes", "links", "callbacks", "pathItems",
            "mediaTypes", "x-oas32-mediaTypes", "x-oas32-additionalOperations");

    /**
     * Object members whose values are name -&gt; string(-list) maps
     * (OAuth Flow {@code scopes}, discriminator {@code mapping},
     * {@code dependentRequired}, JSON Schema {@code $vocabulary}): the
     * member names are data — a scope name or a schema name, never a
     * reference — so a member literally called {@code "$ref"} must not be
     * absolutized. Their contents are not descended into. ({@code security}
     * is not listed: it maps to an array of Security Requirement Objects,
     * so this object-valued check never applies to it — the requirement
     * member names are scheme names whose values are scope lists, which
     * the generic array walk reaches as plain strings.)
     */
    private static final Set<String> STRING_MAP_KEYS = Set.of(
            "scopes", "mapping", "dependentRequired", "$vocabulary");

    /**
     * Path-item fields whose Operation Object values are walked like the fixed
     * 3.1 HTTP-method fields, before {@code query}/{@code additionalOperations}
     * themselves are moved under extensions.
     */
    private static final List<String> PATH_ITEM_METHOD_KEYS = List.of(
            "get", "put", "post", "delete", "options", "head", "patch", "trace");
    private static final List<String> PATH_ITEM_32_OPERATION_KEYS = List.of(
            "query", "additionalOperations");
    /** OAuth Flows Object fields that hold OAuth Flow Objects. */
    private static final List<String> OAUTH_FLOW_KEYS = List.of(
            "implicit", "password", "clientCredentials", "authorizationCode");
    /** Schema Object keywords whose values are schemas keyed by name. */
    private static final List<String> SCHEMA_MAP_KEYWORDS = List.of(
            "properties", "patternProperties", "dependentSchemas", "$defs");
    /** Schema Object keywords whose value is itself a schema. */
    private static final List<String> SCHEMA_CHILD_KEYWORDS = List.of(
            "items", "contains", "not", "if", "then", "else",
            "additionalProperties", "unevaluatedProperties", "unevaluatedItems",
            "propertyNames", "contentSchema");
    /** Schema Object keywords whose value is an array of schemas. */
    private static final List<String> SCHEMA_ARRAY_KEYWORDS = List.of(
            "allOf", "anyOf", "oneOf", "prefixItems");

    /** The raw document, when already read by the caller; otherwise read lazily. */
    private final JsonNode rawDocument;

    /** Reads the raw document in {@link #load}. */
    Oas32CompatSpecLoader() {
        this(null);
    }

    /**
     * Uses a raw document already read (for example by
     * {@link SpecLoader#forInputSpec}) instead of reading it again.
     */
    Oas32CompatSpecLoader(JsonNode rawDocument) {
        this.rawDocument = rawDocument;
    }

    /**
     * Whether {@code document}'s root {@code openapi} field declares a 3.2
     * version ({@code 3.2} or {@code 3.2.x}).
     */
    public static boolean isOas32Document(JsonNode document) {
        if (document == null || !document.isObject()) {
            return false;
        }
        JsonNode version = document.get("openapi");
        if (version == null || !version.isValueNode()) {
            return false;
        }
        String text = version.asText();
        return "3.2".equals(text) || text.startsWith("3.2.");
    }

    @Override
    public SwaggerParseResult load(String inputSpec, List<AuthorizationValue> authorizationValues,
                                   ParseOptions options) {
        JsonNode document = this.rawDocument;
        if (document == null) {
            try {
                document = SpecUtils.readRawDocument(inputSpec, authorizationValues);
            } catch (Exception e) {
                // Keep the same failure behaviour as reading via readLocation.
                return new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);
            }
        }
        if (!isOas32Document(document)) {
            return new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);
        }

        rewriteForCompatibility(document);
        // The dereferencer resolves same-file "#/..." references by re-reading
        // the file at the given location, which would bypass this rewrite, so
        // location must stay null and relative "$ref"s are made absolute
        // against the original inputSpec instead (issue #25477).
        URI refBase = refBaseUri(inputSpec);
        // Single pass: warns on external refs and rewrites relative ones when
        // a base is available (refBase==null still warns, rewrites nothing).
        absolutizeRefs((ObjectNode) document, refBase);
        boolean hadPaths = document.get("paths") instanceof ObjectNode;
        List<StagedOperation> staged = stageMovedOperations((ObjectNode) document);
        String rewritten;
        try {
            rewritten = Json31.mapper().writeValueAsString(document);
        } catch (Exception e) {
            throw new IllegalStateException(
                    "Unable to serialize the rewritten OpenAPI 3.2 document", e);
        }

        SwaggerParseResult result = new OpenAPIV3Parser().readContents(
                rewritten, authorizationValues, options, refBase != null ? null : inputSpec);
        rewriteSyntheticPathMessages(result, staged);
        if (result.getOpenAPI() != null) {
            Oas32Extensions.markCompatDocument(result.getOpenAPI());
            graftSyntheticOperations(result.getOpenAPI(), staged, hadPaths);
            LOGGER.info("OpenAPI 3.2 document read via 3.1 compatibility: {}", inputSpec);
        }
        return result;
    }

    /**
     * Rewrites a 3.2 document tree in place so it parses as 3.1. Keys that
     * only exist in OpenAPI 3.2 are moved to {@code x-oas32-<key>} extension
     * members so they survive the parse. Package-visible for unit tests.
     */
    static void rewriteForCompatibility(JsonNode document) {
        ObjectNode root = (ObjectNode) document;
        // isOas32Document() guarantees the openapi field is a value node.
        root.put(EXT_PREFIX + "openapi", root.get("openapi").asText());
        root.put("openapi", COMPAT_VERSION);
        if (root.has("$self")) {
            // $self is the document's self-declared base URI (spec 3.2.1).
            // This loader never resolves relative $refs against it — the
            // document location is the base — so it is only preserved.
            LOGGER.warn("$self {}: kept as {}$self but not used as the base "
                    + "URI for relative $refs (the document location is)",
                    root.get("$self").asText(), EXT_PREFIX);
        }
        moveKey(root, "$self");
        forEach(root.get("servers"), Oas32CompatSpecLoader::rewriteServer);
        forEach(root.get("tags"), Oas32CompatSpecLoader::rewriteTag);
        if (root.get("components") instanceof ObjectNode) {
            rewriteComponents((ObjectNode) root.get("components"));
        }
        // Only the "paths" member is a Paths Object whose x- members are
        // specification extensions; "webhooks" is a plain name -> Path
        // Item map, where an x- name is a real webhook (see the table on
        // isExtensionKey)
        rewritePathItemMap(root.get("paths"), true);
        rewritePathItemMap(root.get("webhooks"), false);
    }

    // ------------------------------------------------------------------
    // Fixed key table (OpenAPI 3.2.1 vs 3.1.2), by object position
    // ------------------------------------------------------------------

    private static void moveKey(ObjectNode object, String key) {
        JsonNode value = object.remove(key);
        if (value != null) {
            object.set(EXT_PREFIX + key, value);
        }
    }

    private static void forEach(JsonNode node, Consumer<JsonNode> consumer) {
        if (node instanceof ObjectNode) {
            Iterator<Map.Entry<String, JsonNode>> fields = node.fields();
            while (fields.hasNext()) {
                consumer.accept(fields.next().getValue());
            }
        } else if (node != null && node.isArray()) {
            node.forEach(consumer);
        }
    }

    /**
     * Like {@link #forEach} but lets the consumer see the member names of
     * objects, for walks that must tell a member name (e.g. a path name or
     * an {@code x-} extension key) apart from its value.
     */
    private static void forEach(JsonNode node, BiConsumer<String, JsonNode> consumer) {
        if (node instanceof ObjectNode) {
            Iterator<Map.Entry<String, JsonNode>> fields = node.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> field = fields.next();
                consumer.accept(field.getKey(), field.getValue());
            }
        } else if (node != null && node.isArray()) {
            node.forEach(child -> consumer.accept(null, child));
        }
    }

    private static void rewriteComponents(ObjectNode components) {
        forEach(components.get("schemas"), Oas32CompatSpecLoader::rewriteSchema);
        forEach(components.get("responses"), Oas32CompatSpecLoader::rewriteResponse);
        forEach(components.get("parameters"), Oas32CompatSpecLoader::rewriteParameter);
        forEach(components.get("examples"), Oas32CompatSpecLoader::rewriteExample);
        forEach(components.get("requestBodies"), Oas32CompatSpecLoader::rewriteRequestBody);
        forEach(components.get("headers"), Oas32CompatSpecLoader::rewriteHeader);
        forEach(components.get("securitySchemes"), Oas32CompatSpecLoader::rewriteSecurityScheme);
        forEach(components.get("links"), Oas32CompatSpecLoader::rewriteLink);
        forEach(components.get("callbacks"), Oas32CompatSpecLoader::rewriteCallback);
        forEach(components.get("pathItems"), Oas32CompatSpecLoader::rewritePathItem);
        moveKey(components, "mediaTypes");
    }

    /**
     * Whether a member name of a name -&gt; object map is a specification
     * extension ({@code null} for the array elements {@link #forEach}
     * reports, which are never extension keys). Only the maps that spec
     * 3.2.1 allows to carry extensions have {@code x-} member names that
     * mean extensions — see the call sites: {@code paths} (Paths Object),
     * an Operation's {@code responses} (Responses Object) and Callback
     * Objects. Every other name -&gt; object map ({@code webhooks}, the
     * {@code callbacks} maps, {@code components.*} maps, {@code content},
     * {@code encoding}, {@code variables}, the schema name maps and
     * {@code additionalOperations}) is a plain map where an
     * {@code x-}-prefixed member name is a real member, never an
     * extension.
     */
    private static boolean isExtensionKey(String name) {
        return name != null && name.startsWith("x-");
    }

    /**
     * Rewrites every Path Item in a name -&gt; Path Item map. When
     * {@code extensibleMap} is set ({@code paths} only — spec 3.2.1
     * gives the Paths Object extension slots), members whose name starts
     * with {@code x-} are specification extensions — opaque data, not
     * Path Items — and are passed through untouched.
     */
    private static void rewritePathItemMap(JsonNode pathItems, boolean extensibleMap) {
        forEach(pathItems, (name, node) -> {
            if (!extensibleMap || !isExtensionKey(name)) {
                rewritePathItem(node);
            }
        });
    }

    private static void rewritePathItem(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode pathItem = (ObjectNode) node;
        forEach(pathItem.get("parameters"), Oas32CompatSpecLoader::rewriteParameter);
        forEach(pathItem.get("servers"), Oas32CompatSpecLoader::rewriteServer);
        for (String method : PATH_ITEM_METHOD_KEYS) {
            rewriteOperation(pathItem.get(method));
        }
        // Walk the 3.2 operation members while they still occupy their field, so
        // parameters and nested objects inside them are rewritten too.
        rewriteOperation(pathItem.get("query"));
        forEach(pathItem.get("additionalOperations"), Oas32CompatSpecLoader::rewriteOperation);
        for (String key : PATH_ITEM_32_OPERATION_KEYS) {
            moveKey(pathItem, key);
        }
    }

    private static void rewriteOperation(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode operation = (ObjectNode) node;
        forEach(operation.get("parameters"), Oas32CompatSpecLoader::rewriteParameter);
        rewriteRequestBody(operation.get("requestBody"));
        // An Operation's "responses" member is a Responses Object, whose
        // x- members are specification extensions, not responses (a
        // components.responses member named x-... is a real response —
        // rewriteComponents walks every member there)
        forEach(operation.get("responses"), (name, response) -> {
            if (!isExtensionKey(name)) {
                rewriteResponse(response);
            }
        });
        forEach(operation.get("callbacks"), Oas32CompatSpecLoader::rewriteCallback);
        forEach(operation.get("servers"), Oas32CompatSpecLoader::rewriteServer);
    }

    private static void rewriteCallback(JsonNode node) {
        // A Callback Object maps an expression to a Path Item Object;
        // x- members are its specification extensions, not path items.
        if (node instanceof ObjectNode) {
            forEach(node, (name, pathItem) -> {
                if (!isExtensionKey(name)) {
                    rewritePathItem(pathItem);
                }
            });
        }
    }

    private static void rewriteParameter(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode parameter = (ObjectNode) node;
        JsonNode in = parameter.get("in");
        if (in != null && in.isValueNode() && "querystring".equals(in.asText())) {
            parameter.put("in", "query");
            parameter.put(EXT_PREFIX + "in", "querystring");
        }
        // "cookie" is a style value added in 3.2 for in:"cookie" parameters
        // (RFC6265 separators, no percent-encoding); the released parser's
        // StyleEnum rejects it, so it is lowered to "form" — the 3.1
        // default for cookie parameters — keeping the original value.
        JsonNode style = parameter.get("style");
        if (style != null && style.isValueNode()
                && "cookie".equals(style.asText())) {
            parameter.put("style", "form");
            parameter.put(EXT_PREFIX + "style", "cookie");
        }
        rewriteSchema(parameter.get("schema"));
        forEach(parameter.get("content"), Oas32CompatSpecLoader::rewriteMediaType);
        forEach(parameter.get("examples"), Oas32CompatSpecLoader::rewriteExample);
    }

    private static void rewriteHeader(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode header = (ObjectNode) node;
        rewriteSchema(header.get("schema"));
        forEach(header.get("content"), Oas32CompatSpecLoader::rewriteMediaType);
        forEach(header.get("examples"), Oas32CompatSpecLoader::rewriteExample);
    }

    private static void rewriteRequestBody(JsonNode node) {
        if (node instanceof ObjectNode) {
            forEach(((ObjectNode) node).get("content"), Oas32CompatSpecLoader::rewriteMediaType);
        }
    }

    private static void rewriteResponse(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode response = (ObjectNode) node;
        moveKey(response, "summary");
        // A Response Object without a description is valid in 3.2 but fails
        // the released parser's 3.1 schema, so an empty description is
        // supplied and the absence is recorded. An object holding "$ref" is
        // a Reference Object and is left alone.
        if (!response.has("$ref") && !response.has("description")) {
            response.put("description", "");
            response.put(EXT_PREFIX + "description-absent", true);
        }
        forEach(response.get("headers"), Oas32CompatSpecLoader::rewriteHeader);
        forEach(response.get("content"), Oas32CompatSpecLoader::rewriteMediaType);
        forEach(response.get("links"), Oas32CompatSpecLoader::rewriteLink);
    }

    private static void rewriteLink(JsonNode node) {
        if (node instanceof ObjectNode) {
            rewriteServer(((ObjectNode) node).get("server"));
        }
    }

    private static void rewriteMediaType(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode mediaType = (ObjectNode) node;
        moveKey(mediaType, "itemSchema");
        moveKey(mediaType, "itemEncoding");
        moveKey(mediaType, "prefixEncoding");
        rewriteSchema(mediaType.get("schema"));
        forEach(mediaType.get("examples"), Oas32CompatSpecLoader::rewriteExample);
        forEach(mediaType.get("encoding"), Oas32CompatSpecLoader::rewriteEncoding);
    }

    private static void rewriteEncoding(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode encoding = (ObjectNode) node;
        moveKey(encoding, "encoding");
        moveKey(encoding, "prefixEncoding");
        moveKey(encoding, "itemEncoding");
        forEach(encoding.get("headers"), Oas32CompatSpecLoader::rewriteHeader);
    }

    private static void rewriteExample(JsonNode node) {
        if (node instanceof ObjectNode) {
            ObjectNode example = (ObjectNode) node;
            moveKey(example, "dataValue");
            moveKey(example, "serializedValue");
        }
    }

    private static void rewriteTag(JsonNode node) {
        if (node instanceof ObjectNode) {
            ObjectNode tag = (ObjectNode) node;
            moveKey(tag, "summary");
            moveKey(tag, "parent");
            moveKey(tag, "kind");
        }
    }

    private static void rewriteServer(JsonNode node) {
        if (node instanceof ObjectNode) {
            moveKey((ObjectNode) node, "name");
        }
    }

    private static void rewriteSecurityScheme(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode scheme = (ObjectNode) node;
        moveKey(scheme, "oauth2MetadataUrl");
        moveKey(scheme, "deprecated");
        JsonNode flows = scheme.get("flows");
        if (flows instanceof ObjectNode) {
            ObjectNode flowsObject = (ObjectNode) flows;
            moveKey(flowsObject, "deviceAuthorization");
            for (String flow : OAUTH_FLOW_KEYS) {
                JsonNode flowNode = flowsObject.get(flow);
                if (flowNode instanceof ObjectNode) {
                    moveKey((ObjectNode) flowNode, "deviceAuthorizationUrl");
                }
            }
        }
    }

    private static void rewriteSchema(JsonNode node) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode schema = (ObjectNode) node;
        JsonNode discriminator = schema.get("discriminator");
        if (discriminator instanceof ObjectNode) {
            moveKey((ObjectNode) discriminator, "defaultMapping");
        }
        JsonNode xml = schema.get("xml");
        if (xml instanceof ObjectNode) {
            moveKey((ObjectNode) xml, "nodeType");
        }
        for (String keyword : SCHEMA_MAP_KEYWORDS) {
            forEach(schema.get(keyword), Oas32CompatSpecLoader::rewriteSchema);
        }
        for (String keyword : SCHEMA_CHILD_KEYWORDS) {
            rewriteSchemaChild(schema.get(keyword));
        }
        for (String keyword : SCHEMA_ARRAY_KEYWORDS) {
            forEach(schema.get(keyword), Oas32CompatSpecLoader::rewriteSchema);
        }
    }

    /** A child-schema keyword: a single schema, or an array form such as draft-07 {@code items: [...]}. */
    private static void rewriteSchemaChild(JsonNode node) {
        if (node instanceof ObjectNode) {
            rewriteSchema(node);
        } else if (node != null && node.isArray()) {
            node.forEach(Oas32CompatSpecLoader::rewriteSchema);
        }
    }

    // ------------------------------------------------------------------
    // Synthetic paths: the moved operations are mounted as "post"
    // operations so the single parser read gives them parser defaults and
    // $ref resolution; after parsing they are grafted back into the path
    // items' extension values and the synthetic paths are removed.
    // ------------------------------------------------------------------

    /**
     * Mounts every operation that {@link #rewritePathItem} moved to
     * {@code x-oas32-query} / {@code x-oas32-additionalOperations} under a
     * synthetic {@code /__oas32__/<n>} path as a {@code post} operation,
     * stamping {@link #OPERATION_MARKER_EXT} with the synthetic path name.
     * The operation node is shared between the extension value and the
     * synthetic path, so the marker appears both in the loosely-typed
     * extension map and in the parsed {@link Operation}'s extensions — that
     * is what {@link #graftSyntheticOperations} matches on.
     *
     * @return the staged operations (synthetic name plus the original
     * location each operation was moved from, so grafting removes exactly
     * the paths this method created — a genuine document path starting
     * with {@link #SYNTHETIC_PATH_PREFIX} is never touched — and parser
     * messages naming them can be rewritten back)
     */
    private static List<StagedOperation> stageMovedOperations(ObjectNode root) {
        List<StagedOperation> moved = new ArrayList<>();
        // x- members of the paths map are specification extensions —
        // opaque data, never path items. webhooks is a plain name ->
        // Path Item map, so an x- name there is a real webhook.
        collectPathItems(root.get("paths"), moved, "paths", true);
        collectPathItems(root.get("webhooks"), moved, "webhooks", false);
        if (root.get("components") instanceof ObjectNode) {
            ObjectNode components = (ObjectNode) root.get("components");
            // components.pathItems is a plain name -> Path Item map: every
            // member is a path item, even one named "x-..."
            collectPathItems(components.get("pathItems"), moved,
                    "components.pathItems", false);
            // A Components Callback Object maps a name to an
            // expression -> Path Item map.
            forEach(components.get("callbacks"),
                    (name, callback) -> collectCallbackMap(callback, moved,
                            "components.callbacks.'" + name + "'"));
        }
        if (moved.isEmpty()) {
            return moved;
        }
        ObjectNode paths = root.get("paths") instanceof ObjectNode
                ? (ObjectNode) root.get("paths") : root.putObject("paths");
        int seq = 0;
        for (StagedOperation operation : moved) {
            String name;
            do {
                name = SYNTHETIC_PATH_PREFIX + seq++;
            } while (paths.has(name));
            operation.node.put(OPERATION_MARKER_EXT, name);
            operation.name = name;
            paths.putObject(name).set("post", operation.node);
        }
        return moved;
    }

    /**
     * One operation staged under a synthetic path: {@link #name} is the
     * synthetic path name assigned by {@link #stageMovedOperations} and
     * {@link #location} the member path the operation was moved from, in
     * the parser's own {@code map.'name'(member)} form (e.g.
     * {@code paths.'/t'(query)}, {@code webhooks.'ping'(query)},
     * {@code paths.'/things'(additionalOperations.PURGE)}).
     */
    private static final class StagedOperation {
        private final ObjectNode node;
        private final String location;
        private String name;

        private StagedOperation(ObjectNode node, String location) {
            this.node = node;
            this.location = location;
        }
    }

    /**
     * Finds path items inside a name -> object map ({@code paths},
     * {@code webhooks}, {@code components.pathItems}) and collects their
     * moved operations, recording each member's location for later message
     * rewriting. When {@code skipExtensions} is set, members named
     * {@code x-*} are specification extensions — opaque data — and are not
     * treated as path items.
     */
    private static void collectPathItems(JsonNode nameMap, List<StagedOperation> moved,
                                         String container, boolean skipExtensions) {
        forEach(nameMap, (name, node) -> {
            if (!skipExtensions || !isExtensionKey(name)) {
                collectMovedOperations(node, moved, container + ".'" + name + "'");
            }
        });
    }

    /**
     * Finds path items inside one Callback Object (an expression -> Path
     * Item map). Members named {@code x-*} are the callback's extensions —
     * opaque data, not path items.
     */
    private static void collectCallbackMap(JsonNode callback, List<StagedOperation> moved,
                                           String location) {
        forEach(callback, (expression, pathItem) -> {
            if (!isExtensionKey(expression)) {
                collectMovedOperations(pathItem, moved,
                        location + ".'" + expression + "'");
            }
        });
    }

    /**
     * Collects the moved operations of one path item node at
     * {@code location}, then descends into the callbacks of all its
     * operations (fixed methods and moved ones) — callback path items may
     * carry moved operations of their own at any depth. Runs on the
     * Jackson tree, where {@code $ref}s are plain strings, so no cycle can
     * occur.
     */
    private static void collectMovedOperations(JsonNode node, List<StagedOperation> moved,
                                               String location) {
        if (!(node instanceof ObjectNode)) {
            return;
        }
        ObjectNode pathItem = (ObjectNode) node;
        List<JsonNode> operations = new ArrayList<>();
        List<String> memberLocations = new ArrayList<>();
        for (String method : PATH_ITEM_METHOD_KEYS) {
            JsonNode operation = pathItem.get(method);
            if (operation instanceof ObjectNode) {
                operations.add(operation);
                memberLocations.add(location + "(" + method + ")");
            }
        }
        JsonNode query = pathItem.get(Oas32Extensions.QUERY_OPERATION_EXT);
        if (query instanceof ObjectNode) {
            moved.add(new StagedOperation((ObjectNode) query, location + "(query)"));
            operations.add(query);
            memberLocations.add(location + "(query)");
        }
        JsonNode additional = pathItem.get(Oas32Extensions.ADDITIONAL_OPERATIONS_EXT);
        if (additional instanceof ObjectNode) {
            additional.fields().forEachRemaining(entry -> {
                if (entry.getValue() instanceof ObjectNode) {
                    String memberLocation =
                            location + "(additionalOperations." + entry.getKey() + ")";
                    moved.add(new StagedOperation((ObjectNode) entry.getValue(),
                            memberLocation));
                    operations.add(entry.getValue());
                    memberLocations.add(memberLocation);
                }
            });
        }
        for (int i = 0; i < operations.size(); i++) {
            JsonNode callbacks = operations.get(i).get("callbacks");
            if (callbacks instanceof ObjectNode) {
                String operationLocation = memberLocations.get(i);
                forEach(callbacks, (name, callback) -> collectCallbackMap(
                        callback, moved,
                        operationLocation + ".callbacks.'" + name + "'"));
            }
        }
    }

    /**
     * Rewrites parser messages that name a synthetic path back to the
     * member the operation was moved from: {@code paths.'/__oas32__/0'(post)}
     * becomes e.g. {@code paths.'/t'(query)}, and a bare
     * {@code '/__oas32__/0'} becomes the owning member's location
     * {@code paths.'/t'}, so validation output never exposes the staging.
     */
    private static void rewriteSyntheticPathMessages(SwaggerParseResult result,
                                                     List<StagedOperation> staged) {
        if (result.getMessages() == null || staged.isEmpty()) {
            return;
        }
        List<String> messages = new ArrayList<>(result.getMessages().size());
        for (String message : result.getMessages()) {
            if (message != null && message.contains(SYNTHETIC_PATH_PREFIX)) {
                for (StagedOperation operation : staged) {
                    // longest forms first: paths.'name'(post) names the
                    // staged operation itself, paths.'name' the synthetic
                    // path (a member of it, e.g. .parameters, keeps its
                    // suffix), a bare 'name' any other mention
                    message = message
                            .replace("paths.'" + operation.name + "'(post)",
                                    operation.location)
                            .replace("paths.'" + operation.name + "'",
                                    pathLocationOf(operation.location))
                            .replace("'" + operation.name + "'",
                                    pathLocationOf(operation.location));
                }
            }
            messages.add(message);
        }
        result.setMessages(messages);
    }

    /** The {@code map.'name'} head of a staged {@link StagedOperation#location}. */
    private static String pathLocationOf(String location) {
        int member = location.lastIndexOf('(');
        return member < 0 ? location : location.substring(0, member);
    }

    /**
     * Replaces the loosely-typed maps the extension deserializer produced for
     * {@code x-oas32-query} / {@code x-oas32-additionalOperations} with the
     * {@link Operation}s the parser built for the synthetic paths, then
     * removes the synthetic paths and the {@link #OPERATION_MARKER_EXT}
     * marker. An operation whose parsed counterpart cannot be found keeps its
     * extension map (the marker is still stripped), which is the same shape
     * it had before this staging existed.
     */
    private static void graftSyntheticOperations(OpenAPI openAPI, List<StagedOperation> staged,
                                                 boolean hadPaths) {
        Map<String, Operation> parsedByName = new HashMap<>();
        io.swagger.v3.oas.models.Paths paths = openAPI.getPaths();
        if (paths != null) {
            for (StagedOperation operation : staged) {
                PathItem pathItem = paths.remove(operation.name);
                Operation parsed = pathItem == null ? null : pathItem.getPost();
                if (parsed == null || parsed.getExtensions() == null) {
                    continue;
                }
                Object marker = parsed.getExtensions().remove(OPERATION_MARKER_EXT);
                if (marker != null) {
                    parsedByName.put(String.valueOf(marker), parsed);
                }
            }
            // paths did not exist in the source document: it was created only
            // to host the synthetic operations
            if (!hadPaths && paths.isEmpty()) {
                openAPI.setPaths(null);
            }
        }
        // The walk below still runs when parsedByName is empty: it strips the
        // OPERATION_MARKER_EXT markers from the loosely-typed extension maps
        // even when no parsed counterpart was produced.
        Set<PathItem> visited =
                Collections.newSetFromMap(new IdentityHashMap<>());
        if (paths != null) {
            paths.values().forEach(pathItem -> graftPathItem(pathItem, parsedByName, visited));
        }
        if (openAPI.getWebhooks() != null) {
            openAPI.getWebhooks().values()
                    .forEach(pathItem -> graftPathItem(pathItem, parsedByName, visited));
        }
        Components components = openAPI.getComponents();
        if (components != null) {
            if (components.getPathItems() != null) {
                components.getPathItems().values()
                        .forEach(pathItem -> graftPathItem(pathItem, parsedByName, visited));
            }
            if (components.getCallbacks() != null) {
                components.getCallbacks().values().forEach(
                        callback -> callback.values().forEach(
                                pathItem -> graftPathItem(pathItem, parsedByName, visited)));
            }
        }
    }

    /**
     * Grafts the parsed operations onto one path item's extensions, then
     * recurses into the callbacks of every operation the item exposes —
     * including the just-grafted ones, whose callback path items may carry
     * moved operations themselves.
     */
    private static void graftPathItem(PathItem pathItem, Map<String, Operation> parsedByName,
                                      Set<PathItem> visited) {
        if (pathItem == null || !visited.add(pathItem)) {
            return;
        }
        Map<String, Object> extensions = pathItem.getExtensions();
        if (extensions != null) {
            Object query = extensions.get(Oas32Extensions.QUERY_OPERATION_EXT);
            if (query instanceof Map) {
                Operation parsed = parsedOperation(query, parsedByName);
                if (parsed != null) {
                    extensions.put(Oas32Extensions.QUERY_OPERATION_EXT, parsed);
                }
            }
            Object additional = extensions.get(Oas32Extensions.ADDITIONAL_OPERATIONS_EXT);
            if (additional instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> additionalOperations = (Map<String, Object>) additional;
                additionalOperations.forEach((method, value) -> {
                    Operation parsed = parsedOperation(value, parsedByName);
                    if (parsed != null) {
                        additionalOperations.put(method, parsed);
                    }
                });
            }
        }

        List<Operation> operations = new ArrayList<>();
        List<Operation> fixed = pathItem.readOperations();
        if (fixed != null) {
            operations.addAll(fixed);
        }
        if (extensions != null) {
            if (extensions.get(Oas32Extensions.QUERY_OPERATION_EXT) instanceof Operation) {
                operations.add((Operation) extensions.get(Oas32Extensions.QUERY_OPERATION_EXT));
            }
            Object additional = extensions.get(Oas32Extensions.ADDITIONAL_OPERATIONS_EXT);
            if (additional instanceof Map) {
                ((Map<?, ?>) additional).values().stream()
                        .filter(Operation.class::isInstance)
                        .map(Operation.class::cast)
                        .forEach(operations::add);
            }
        }
        for (Operation operation : operations) {
            Map<String, Callback> callbacks = operation.getCallbacks();
            if (callbacks != null) {
                for (Callback callback : callbacks.values()) {
                    for (PathItem callbackPathItem : callback.values()) {
                        graftPathItem(callbackPathItem, parsedByName, visited);
                    }
                }
            }
        }
    }

    /**
     * Looks up the parsed operation for a loosely-typed extension value: a
     * map holding the {@link #OPERATION_MARKER_EXT} marker. The marker is
     * always stripped so it cannot leak into downstream processing, even when
     * no parsed counterpart exists.
     */
    private static Operation parsedOperation(Object extensionValue,
                                             Map<String, Operation> parsedByName) {
        if (!(extensionValue instanceof Map)) {
            return null;
        }
        @SuppressWarnings("unchecked")
        Map<String, Object> value = (Map<String, Object>) extensionValue;
        Object marker = value.remove(OPERATION_MARKER_EXT);
        return marker == null ? null : parsedByName.get(String.valueOf(marker));
    }

    // ------------------------------------------------------------------
    // Relative "$ref" resolution (the dereferencer cannot use the original
    // location because it would re-read the unrewritten document)
    // ------------------------------------------------------------------

    /**
     * The base URI that relative {@code $ref}s in {@code inputSpec} resolve
     * against. URL inputs keep their URL; local paths become absolute
     * {@code file:} URIs; Windows drive paths become {@code file:///C:/...}
     * (built by string so the result is correct on any OS, where
     * {@code Paths.get("C:\\...")} would produce a bogus relative path).
     * Resolved refs are rendered for the parser by
     * {@link #toParserReadableRef(URI)}. {@code null} when no base can be
     * computed. Package-visible for tests.
     */
    static URI refBaseUri(String inputSpec) {
        if (inputSpec.matches("^[A-Za-z]:[\\\\/].*")) {
            try {
                return new URI("file", "",
                        "/" + inputSpec.replace('\\', '/'), null);
            } catch (URISyntaxException e) {
                return null;
            }
        }
        try {
            URI uri = URI.create(inputSpec);
            if (uri.getScheme() != null) {
                return uri;
            }
        } catch (IllegalArgumentException ignored) {
            // A local path may contain URI-reserved characters such as spaces.
        }
        try {
            return Paths.get(inputSpec).toAbsolutePath().toUri();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Resolves a {@code $ref} value against {@code baseUri} to an absolute
     * reference in the form {@link #toParserReadableRef(URI)} renders.
     * Returns {@code null} for same-document ({@code #...}) and
     * already-absolute references, and for values that cannot be parsed,
     * leaving the original value untouched. Package-visible for tests.
     */
    static String absolutizeRef(String ref, URI baseUri) {
        if (ref.startsWith("#") || baseUri == null) {
            return null;
        }
        URI refUri;
        try {
            refUri = new URI(ref);
        } catch (URISyntaxException e) {
            // Quote characters that are illegal in a URI (e.g. spaces) while
            // keeping the "#fragment" part verbatim.
            int hash = ref.indexOf('#');
            try {
                refUri = new URI(null, null,
                        hash < 0 ? ref : ref.substring(0, hash),
                        hash < 0 ? null : ref.substring(hash + 1));
            } catch (URISyntaxException e2) {
                return null;
            }
        }
        if (refUri.isAbsolute()) {
            return null;
        }
        try {
            URI resolved = baseUri.resolve(refUri).normalize();
            // A ref that resolves back to the input document itself (e.g.
            // "selfname.yaml#/components/schemas/X" inside selfname.yaml)
            // collapses to a same-document fragment: it is served from the
            // rewritten tree like "#/..." and keeps working if the file is
            // renamed or moved together with its referrers.
            if (resolved.getRawFragment() != null && sameDocument(resolved, baseUri)) {
                return "#" + resolved.getRawFragment();
            }
            if ("file".equals(resolved.getScheme()) && resolved.getAuthority() == null) {
                // URI.resolve() drops an empty authority ("file:///...yaml"
                // base resolves to "file:/..."); restore the canonical form.
                resolved = new URI(resolved.getScheme(), "", resolved.getPath(),
                        resolved.getQuery(), resolved.getFragment());
            }
            return toParserReadableRef(resolved);
        } catch (IllegalArgumentException | URISyntaxException e) {
            return null;
        }
    }

    /**
     * Whether {@code resolved} (normalized) names the same document as
     * {@code baseUri}, ignoring any fragment: scheme, authority, path and
     * query all equal. Compared on raw components so equivalent escaping
     * rules apply on both sides.
     */
    private static boolean sameDocument(URI resolved, URI baseUri) {
        URI base = baseUri.normalize();
        return java.util.Objects.equals(resolved.getScheme(), base.getScheme())
                && java.util.Objects.equals(resolved.getRawAuthority(), base.getRawAuthority())
                && java.util.Objects.equals(resolved.getRawPath(), base.getRawPath())
                && java.util.Objects.equals(resolved.getRawQuery(), base.getRawQuery());
    }

    /**
     * Renders a resolved absolute reference in the only form the parser's
     * {@code Visitor.readURI}/{@code readFile} reads correctly on every OS:
     * URLs keep their URL form, while {@code file:} URIs become plain
     * filesystem paths ({@code /dir/file.yaml}, {@code C:/dir/file.yaml},
     * {@code //server/share/file.yaml}). A {@code file:///C:/...} URI would
     * lose its drive letter on Windows (readURI strips the scheme through
     * {@code URI.getPath()}, yielding {@code /C:/...}), whereas a bare
     * {@code C:/...} string reaches {@code new FileInputStream} untouched.
     * Names needing percent-escapes (spaces, {@code #}, non-ASCII) keep the
     * {@code file:} URI form: as plain text the escapes would either make
     * {@code new URI} fail or reach the filesystem undecoded.
     */
    static String toParserReadableRef(URI resolved) {
        if (!"file".equalsIgnoreCase(resolved.getScheme())
                || resolved.getRawQuery() != null) {
            return resolved.toString();
        }
        String rawPath = resolved.getRawPath();
        if (rawPath != null && rawPath.equals(resolved.getPath())) {
            String rawAuthority = resolved.getRawAuthority();
            String path;
            if (rawAuthority != null && !rawAuthority.isEmpty()) {
                path = "//" + rawAuthority + rawPath;
            } else if (rawPath.matches("^/[A-Za-z]:/.*")) {
                // "/C:/dir/file.yaml" -> "C:/dir/file.yaml"
                path = rawPath.substring(1);
            } else {
                path = rawPath;
            }
            try {
                // readURI parses the value with new URI() before dispatching,
                // so the plain path must still be a legal URI string.
                new URI(path);
                return resolved.getRawFragment() == null
                        ? path : path + "#" + resolved.getRawFragment();
            } catch (URISyntaxException e) {
                // keep the file: URI form below
            }
        }
        return resolved.toString();
    }

    /**
     * Rewrites every relative {@code $ref} in the document to an absolute
     * reference based on {@code baseUri}, so the dereferencer can resolve
     * them without a document location, and warns about external-document
     * {@code $ref}s (which are parsed without the compatibility rewrite) —
     * once per referenced document, ignoring any {@code #fragment} part.
     * Payload-data members (see {@link #DATA_VALUE_KEYS}) are not descended
     * into, but member names of name -&gt; object maps (see
     * {@link #NAME_MAP_KEYS}) are not data and keep their {@code $ref}s
     * rewritten. Warnings are emitted even when {@code baseUri} is
     * {@code null}; nothing is rewritten then.
     *
     * <p>Three kinds of {@code $ref} are intentionally left untouched:
     * same-document JSON Pointers that name a member the compatibility
     * rewrite moved to an {@code x-oas32-*} extension (they can no longer
     * resolve; one warning per distinct ref), members literally named
     * {@code "$ref"} inside string-valued maps
     * ({@link #STRING_MAP_KEYS} — they hold scope or schema names, not
     * references), and {@code $ref}-looking strings inside payload data —
     * moved example values, Link Object {@code parameters}/
     * {@code requestBody} expressions and {@code x-*} extension values —
     * which are document content, not references. Relative refs inside a
     * schema {@code $id} scope are absolutized like any other ref — the
     * {@code $id} is not used as their base URI, matching the result the
     * 3.1 parser produces; one warning per load.
     */
    static void absolutizeRefs(ObjectNode node, URI baseUri) {
        new RefWalk(node, baseUri).walk(node, false, false);
    }

    /**
     * State for a single {@link #absolutizeRefs} walk: the resolution base,
     * the document root (for moved-member pointer checks) and the
     * deduplication / once-per-load flags for the warnings the walk emits.
     */
    private static final class RefWalk {
        private final URI baseUri;
        private final ObjectNode root;
        private final Set<String> warnedDocuments = new HashSet<>();
        private final Set<String> warnedMovedRefs = new HashSet<>();
        private boolean warnedIdScope;

        private RefWalk(ObjectNode root, URI baseUri) {
            this.root = root;
            this.baseUri = baseUri;
        }

        /**
         * Walks one object's fields. {@code insideLink} marks the object as
         * a Link Object (reached through a {@code "links"} name map), whose
         * {@code parameters}/{@code requestBody} members are payload data.
         * {@code underId} marks the object as inside a schema {@code $id}
         * scope, where relative refs keep the {@code $id} as their base URI
         * and must not be absolutized against the file location.
         */
        private void walkObject(ObjectNode node, boolean insideLink,
                                boolean underId) {
            boolean idScope = underId || node.has("$id");
            node.fields().forEachRemaining(field -> {
                String key = field.getKey();
                JsonNode value = field.getValue();
                if (insideLink && LINK_DATA_KEYS.contains(key)) {
                    return;
                }
                if (NAME_MAP_KEYS.contains(key) && value.isObject()) {
                    // name -> object map: the member names are arbitrary (a
                    // property may be called "example", a response
                    // "default"), so only their values are walked, not the
                    // names. "links" members are Link Objects. An x-
                    // member is a specification extension — opaque data,
                    // never descended into — only in the maps spec 3.2.1
                    // lets carry extensions: the Paths Object and an
                    // Operation's Responses Object (a components.responses
                    // member named x-... is a real response name, as is an
                    // x- member of webhooks — a plain name -> Path Item
                    // map).
                    boolean links = "links".equals(key);
                    boolean extensible = "paths".equals(key)
                            || ("responses".equals(key) && node != root.get("components"));
                    value.fields().forEachRemaining(e -> {
                        if (!extensible || !e.getKey().startsWith("x-")) {
                            walk(e.getValue(), links, idScope);
                        }
                    });
                    return;
                }
                if (STRING_MAP_KEYS.contains(key) && value.isObject()) {
                    // name -> string map (OAuth scopes, discriminator
                    // mappings, ...): member names are data, never fields —
                    // a member literally named "$ref" holds a scope name or
                    // a schema name, not a reference
                    return;
                }
                if (DATA_VALUE_KEYS.contains(key)) {
                    return;
                }
                if (key.startsWith("x-")
                        && !STRUCTURAL_EXTENSIONS.contains(key)) {
                    // user extension values and the loader's moved data
                    // payloads are opaque content
                    return;
                }
                if ("$ref".equals(key) && value.isTextual()) {
                    handleRef(field, idScope);
                    return;
                }
                walk(value, false, idScope);
            });
        }

        private void walk(JsonNode node, boolean insideLink,
                          boolean underId) {
            if (node instanceof ObjectNode) {
                walkObject((ObjectNode) node, insideLink, underId);
            } else if (node != null && node.isArray()) {
                // the insideLink flag describes one object only, so array
                // elements never inherit it
                node.forEach(child -> walk(child, false, underId));
            }
        }

        private void handleRef(Map.Entry<String, JsonNode> field,
                               boolean underId) {
            String ref = field.getValue().asText();
            if (ref.startsWith("#")) {
                warnOnMovedPointer(ref);
                return;
            }
            if (underId && !warnedIdScope) {
                // The enclosing schema $id would be the ref's base URI per
                // spec 3.2.1 ("Establishing the Base URI"), but this loader
                // does not use it — matching the released parser's 3.1
                // result, where the document location is the base — so the
                // ref is absolutized like any other and the gap (a working
                // $id base) is warned about once per load.
                warnedIdScope = true;
                LOGGER.warn("the $id of a schema is not used as the base "
                        + "URI for relative $refs inside it (e.g. {}): "
                        + "they are resolved against the document "
                        + "location, the same result the 3.1 parser "
                        + "produces", ref);
            }
            String absolute = absolutizeRef(ref, baseUri);
            if (absolute != null) {
                // a ref that names the input document itself collapsed to a
                // same-document fragment — not an external read, no warning
                if (!absolute.startsWith("#")) {
                    warnOnce(warnedDocuments, absolute);
                }
                field.setValue(TextNode.valueOf(absolute));
            } else {
                warnOnce(warnedDocuments, ref);
            }
        }

        /**
         * Warns once per distinct ref when {@code ref} is a JSON Pointer
         * that names a member the compatibility rewrite moved to an
         * {@code x-oas32-*} extension: the original member no longer exists
         * in the rewritten tree, so the pointer cannot resolve. A segment
         * that still resolves (e.g. a schema property literally named
         * {@code "query"}) or a pointer that was already dangling is not
         * reported — the parser reports genuinely broken refs itself.
         */
        private void warnOnMovedPointer(String ref) {
            if (warnedMovedRefs.add(ref) && pointsIntoMovedMember(ref)) {
                LOGGER.warn("{}: this $ref names a member moved to an "
                        + "x-oas32-* extension by the 3.2 compatibility "
                        + "rewrite and can no longer be resolved", ref);
            }
        }

        private boolean pointsIntoMovedMember(String ref) {
            String pointer = ref.substring(1);
            if (pointer.isEmpty() || pointer.charAt(0) != '/') {
                return false;
            }
            JsonNode current = root;
            for (String raw : pointer.substring(1).split("/")) {
                String segment = raw.replace("~1", "/").replace("~0", "~");
                JsonNode next = child(current, segment);
                if (next != null) {
                    current = next;
                    continue;
                }
                return current.isObject() && current.has(EXT_PREFIX + segment);
            }
            return false;
        }

        private JsonNode child(JsonNode node, String segment) {
            if (node.isObject()) {
                return node.get(segment);
            }
            if (node.isArray() && segment.matches("[0-9]{1,9}")) {
                int index = Integer.parseInt(segment);
                return index < node.size() ? node.get(index) : null;
            }
            return null;
        }
    }

    /**
     * Logs the external-document warning once per referenced document: two
     * {@code $ref}s differing only in {@code #fragment} target the same
     * file, so the document part is what is deduplicated on.
     */
    private static void warnOnce(Set<String> warnedDocuments, String ref) {
        int hash = ref.indexOf('#');
        String document = hash < 0 ? ref : ref.substring(0, hash);
        if (warnedDocuments.add(document)) {
            LOGGER.warn("{}: the referenced document is parsed without the "
                    + "OpenAPI 3.2 compatibility rewrite; 3.2-only members in "
                    + "it may be dropped", document);
        }
    }
}
