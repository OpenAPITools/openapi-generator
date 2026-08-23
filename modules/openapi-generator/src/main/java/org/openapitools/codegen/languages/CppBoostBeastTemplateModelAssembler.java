package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariable;
import org.openapitools.codegen.CodegenMediaType;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Assembles operation and response vendor extensions consumed by API templates. */
final class CppBoostBeastTemplateModelAssembler {
    private static final String SSE_SCHEMA_MODE_JSON_EVENT_DATA = "jsonEventData";
    private static final String X_SSE_EVENT_DATA_SCHEMA = "x-sse-event-data-schema";
    private static final String X_CODEGEN_DEFAULT_RESPONSE_IS_RETURN_COMPATIBLE =
            "x-codegen-default-response-is-return-compatible";
    private static final String X_CODEGEN_EMPTY_BODY_TOLERANT = "x-codegen-empty-body-tolerant";
    private static final String X_CODEGEN_HAS_DEFAULT_RESPONSE = "x-codegen-has-default-response";
    private static final String X_CODEGEN_OP_SERVER = "x-codegen-op-server";
    private static final String X_CODEGEN_OP_SECURITY_GROUPS = "x-codegen-op-security-groups";
    private static final String X_CODEGEN_OP_HAS_SECURITY = "x-codegen-op-has-security";
    private static final String X_CODEGEN_OP_CALLBACKS = "x-codegen-op-callbacks";
    private static final String X_CODEGEN_OP_LINKS = "x-codegen-op-links";
    private static final String X_CODEGEN_WEBHOOK_METADATA = "x-codegen-webhook-metadata";
    private static final String X_CODEGEN_RESPONSE_RANGE = "x-codegen-response-range";
    private static final String X_CODEGEN_RESPONSE_IS_ONE_OF = "x-codegen-response-is-oneof";
    private static final String X_CODEGEN_STREAM_IS_ONE_OF = "x-codegen-stream-is-oneof";
    private static final String X_CODEGEN_DUAL_STREAM_IS_ONE_OF = "x-codegen-dual-stream-is-oneof";
    private static final String X_CODEGEN_RESPONSE_UNION = "x-codegen-response-union";
    private static final String X_CODEGEN_RESPONSE_UNION_BODY_TYPE =
            "x-codegen-response-union-body-type";

    private final OpenAPI phaseOpenApi;
    private final List<String> webhookPreservation;
    private final Map<String, List<String>> operationCallbacks;
    private final Map<String, List<String>> operationLinks;
    private final Map<String, String> composedKeywordsByModel;
    private final String sseSchemaMode;

    CppBoostBeastTemplateModelAssembler(
            OpenAPI phaseOpenApi,
            List<String> webhookPreservation,
            Map<String, List<String>> operationCallbacks,
            Map<String, List<String>> operationLinks,
            Map<String, String> composedKeywordsByModel,
            String sseSchemaMode) {
        this.phaseOpenApi = phaseOpenApi;
        this.webhookPreservation = webhookPreservation;
        this.operationCallbacks = operationCallbacks;
        this.operationLinks = operationLinks;
        this.composedKeywordsByModel = composedKeywordsByModel;
        this.sseSchemaMode = sseSchemaMode;
    }

    private static String stripSharedPtr(String type) {
        if (type == null) {
            return null;
        }
        if (type.startsWith("std::shared_ptr<") && type.endsWith(">")) {
            return stripSharedPtr(type.substring(16, type.length() - 1));
        }
        int firstLt = type.indexOf('<');
        int lastGt = type.lastIndexOf('>');
        if (firstLt > 0 && lastGt > firstLt) {
            String prefix = type.substring(0, firstLt);
            List<String> args = splitTemplateArgs(type.substring(firstLt + 1, lastGt));
            for (int index = 0; index < args.size(); ++index) {
                args.set(index, stripSharedPtr(args.get(index).trim()));
            }
            return prefix + "<" + String.join(", ", args) + ">";
        }
        return type;
    }

    private static List<String> splitTemplateArgs(String args) {
        List<String> result = new ArrayList<>();
        int depth = 0;
        int start = 0;
        for (int index = 0; index < args.length(); ++index) {
            char character = args.charAt(index);
            if (character == '<') {
                ++depth;
            } else if (character == '>') {
                --depth;
            } else if (character == ',' && depth == 0) {
                result.add(args.substring(start, index));
                start = index + 1;
            }
        }
        result.add(args.substring(start));
        return result;
    }

    private static String cppString(String value) {
        return CppBoostBeastClientCodegen.escapeCppStringContent(
                value == null ? "" : value);
    }

    private static String commentText(String value) {
        return value == null ? "" : value
                .replace("*/", "* /")
                .replace('\r', ' ')
                .replace('\n', ' ')
                .replace('\u2028', ' ')
                .replace('\u2029', ' ');
    }

    /** True when the list is exactly the swagger-parser's implicit default
     *  (a single Server with url "/") — i.e. the spec declared no servers
     *  at that level. */
    private static boolean isParserDefaultServerList(List<Server> servers) {
        return servers != null && servers.size() == 1
                && "/".equals(servers.get(0).getUrl());
    }

    /** The operation's effective security requirements as template-ready
     *  groups. Each group is an OR alternative containing AND-required scheme
     *  maps. An empty group is anonymous access; operation `security: []`
     *  clears inherited requirements. */
    private List<List<Map<String, Object>>> effectiveSecurityGroups(CodegenOperation op) {
        List<List<Map<String, Object>>> groups = new ArrayList<>();
        List<SecurityRequirement> requirements = null;
        io.swagger.v3.oas.models.Operation raw = operationFor(op);
        if (raw != null && raw.getSecurity() != null) {
            requirements = raw.getSecurity();        // includes `[]` clears
        } else if (phaseOpenApi != null
                && phaseOpenApi.getSecurity() != null) {
            requirements = phaseOpenApi.getSecurity();
        }
        if (requirements == null) {
            return groups;                            // no security declared
        }
        Map<String, SecurityScheme> schemes = phaseOpenApi != null
                && phaseOpenApi.getComponents() != null
                ? phaseOpenApi.getComponents().getSecuritySchemes()
                : null;
        for (SecurityRequirement req : requirements) {
            List<Map<String, Object>> ands = new ArrayList<>();
            if (req != null) {
                for (Map.Entry<String, List<String>> e : req.entrySet()) {
                    SecurityScheme scheme = schemes == null
                            ? null : schemes.get(e.getKey());
                    Map<String, Object> use = new LinkedHashMap<>();
                    use.put("name", cppString(e.getKey()));
                    use.put("type", cppString(scheme == null || scheme.getType() == null
                            ? "unknown" : scheme.getType().toString()));
                    if (scheme != null && scheme.getType() == SecurityScheme.Type.APIKEY) {
                        use.put("in", cppString(scheme.getIn() == null ? "header"
                                : scheme.getIn().toString()));
                        use.put("paramName", cppString(scheme.getName() == null
                                ? "" : scheme.getName()));
                    } else {
                        use.put("in", "");
                        use.put("paramName", "");
                    }
                    use.put("httpScheme", cppString(scheme != null
                            && scheme.getType() == SecurityScheme.Type.HTTP
                            && scheme.getScheme() != null
                            ? scheme.getScheme() : ""));
                    List<String> scopes = e.getValue() == null
                            ? new ArrayList<String>() : e.getValue();
                    use.put("scopes", scopes);
                    use.put("scopesRendered", scopes.isEmpty() ? null
                            : scopes.stream()
                                    .map(s -> "\"" + cppString(s) + "\"")
                                    .collect(java.util.stream.Collectors
                                            .joining(", ")));
                    ands.add(use);
                }
            }
            groups.add(ands);                          // empty ands = {}
        }
        return groups;
    }

    /** The raw Operation behind a CodegenOperation (PathItem-method lookup). */
    private io.swagger.v3.oas.models.Operation operationFor(CodegenOperation op) {
        if (phaseOpenApi == null || phaseOpenApi.getPaths() == null) {
            return null;
        }
        PathItem item = phaseOpenApi.getPaths().get(op.path);
        if (item == null) {
            return null;
        }
        if ("GET".equals(op.httpMethod)) {
            return item.getGet();
        }
        if ("PUT".equals(op.httpMethod)) {
            return item.getPut();
        }
        if ("POST".equals(op.httpMethod)) {
            return item.getPost();
        }
        if ("DELETE".equals(op.httpMethod)) {
            return item.getDelete();
        }
        if ("OPTIONS".equals(op.httpMethod)) {
            return item.getOptions();
        }
        if ("HEAD".equals(op.httpMethod)) {
            return item.getHead();
        }
        if ("PATCH".equals(op.httpMethod)) {
            return item.getPatch();
        }
        if ("TRACE".equals(op.httpMethod)) {
            return item.getTrace();
        }
        return null;
    }

    /** Returns the effective operation server URL with first-level variables
     *  substituted by declared defaults. Precedence is operation, path item,
     *  then root; an empty result leaves server selection to the caller. */
    private String resolveEffectiveServerUrl(CodegenOperation op) {
        List<Server> servers = null;
        io.swagger.v3.oas.models.Operation raw = operationFor(op);
        if (raw != null && raw.getServers() != null
                && !raw.getServers().isEmpty()) {
            servers = raw.getServers();
        }
        // swagger-parser injects a DEFAULT Server("/") on operations and
        // path items that declare no servers; per OAS that means "no server
        // override" (the enclosing level applies). Treat it as absent so the
        // precedence falls through.
        if (isParserDefaultServerList(servers)) {
            servers = null;
        }
        if (servers == null || servers.isEmpty()) {
            if (phaseOpenApi != null && phaseOpenApi.getPaths() != null
                    && phaseOpenApi.getPaths().get(op.path) != null) {
                PathItem item = phaseOpenApi.getPaths().get(op.path);
                if (item.getServers() != null && !item.getServers().isEmpty()) {
                    servers = item.getServers();
                    if (isParserDefaultServerList(servers)) {
                        servers = null;
                    }
                }
                if ((servers == null || servers.isEmpty())
                        && phaseOpenApi.getServers() != null
                        && !phaseOpenApi.getServers().isEmpty()) {
                    servers = phaseOpenApi.getServers();
                    if (isParserDefaultServerList(servers)) {
                        servers = null;
                    }
                }
            }
        }
        if (servers == null || servers.isEmpty()) {
            return "";
        }
        // The FIRST entry of the effective list is the default (user
        // selection among multiple entries is the caller's context
        // override at construction time).
        Server target = servers.get(0);
        String url = target.getUrl() == null ? "" : target.getUrl();
        if (target.getVariables() != null) {
            for (Map.Entry<String, ServerVariable> e
                    : target.getVariables().entrySet()) {
                String value = e.getValue() != null && e.getValue().getDefault() != null
                        ? e.getValue().getDefault() : "";
                url = url.replace("{" + e.getKey() + "}", value);
            }
        }
        return url;
    }

    @SuppressWarnings("unchecked")
    OperationsMap assemble(OperationsMap objs, List<ModelMap> allModels) {
        // API templates need to know whether a model namespace exists. Upstream
        // does not populate hasModels in this generator's API context.
        objs.put("x-codegen-has-models", !allModels.isEmpty());
        objs.put(X_CODEGEN_WEBHOOK_METADATA,
                webhookPreservation.isEmpty() ? null
                        : commentText(String.join("; ", webhookPreservation)));
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        List<CodegenOperation> newOpList = new ArrayList<>();
        Set<String> nullDefaultModels = nullDefaultModelNames(allModels);

        for (CodegenOperation op : operationList) {
            addApiResponseMetadata(op, nullDefaultModels);
            addResponseUnionMetadata(op);
            op.vendorExtensions.put(X_CODEGEN_OP_SERVER,
                    cppString(resolveEffectiveServerUrl(op)));
            if (op.consumes != null) {
                for (Map<String, String> media : op.consumes) {
                    media.put("cppMediaType", cppString(media.get("mediaType")));
                }
            }
            if (op.produces != null) {
                for (Map<String, String> media : op.produces) {
                    media.put("cppMediaType", cppString(media.get("mediaType")));
                }
            }
            List<List<Map<String, Object>>> securityGroups = effectiveSecurityGroups(op);
            op.vendorExtensions.put(X_CODEGEN_OP_SECURITY_GROUPS, securityGroups);
            op.vendorExtensions.put(X_CODEGEN_OP_HAS_SECURITY,
                    !securityGroups.isEmpty());
            String opKey = op.path + '\0' + op.httpMethod;
            op.vendorExtensions.put(X_CODEGEN_OP_CALLBACKS,
                    operationCallbacks.getOrDefault(opKey, new ArrayList<String>())
                            .stream().map(CppBoostBeastTemplateModelAssembler::commentText)
                            .collect(java.util.stream.Collectors.toList()));
            op.vendorExtensions.put(X_CODEGEN_OP_LINKS,
                    operationLinks.getOrDefault(opKey, new ArrayList<String>())
                            .stream().map(CppBoostBeastTemplateModelAssembler::commentText)
                            .collect(java.util.stream.Collectors.toList()));
            String path = op.path;

            String[] items = path.split("/", -1);
            String resourceNameCamelCase = "";
            for (String item : items) {
                if (item.length() > 1) {
                    if (item.matches("^\\{(.*)\\}$")) {
                        String tmpResourceName = item.substring(1, item.length() - 1);
                        resourceNameCamelCase += Character.toUpperCase(tmpResourceName.charAt(0))
                                + tmpResourceName.substring(1);
                    } else {
                        resourceNameCamelCase += Character.toUpperCase(item.charAt(0))
                                + item.substring(1);
                    }
                } else if (item.length() == 1) {
                    resourceNameCamelCase += Character.toUpperCase(item.charAt(0));
                }
            }
            op.path = path.replaceFirst("/$", "");
            op.vendorExtensions.put("x-codegen-cpp-path", cppString(op.path));

            op.vendorExtensions.put("x-codegen-resource-name", resourceNameCamelCase);

            boolean foundInNewList = false;
            for (CodegenOperation op1 : newOpList) {
                if (!foundInNewList) {
                    if (op1.path.equals(op.path)) {
                        foundInNewList = true;
                        final String otherMethodsKey = "x-codegen-other-methods";
                        List<CodegenOperation> currentOtherMethodList =
                                (List<CodegenOperation>) op1.vendorExtensions.get(otherMethodsKey);
                        if (currentOtherMethodList == null) {
                            currentOtherMethodList = new ArrayList<>();
                        }
                        op.operationIdCamelCase = op1.operationIdCamelCase;
                        currentOtherMethodList.add(op);
                        op1.vendorExtensions.put(otherMethodsKey, currentOtherMethodList);
                    }
                }
            }
            if (!foundInNewList) {
                newOpList.add(op);
            }
        }
        operations.put("operation", newOpList);
        return objs;
    }

    @SuppressWarnings("unchecked")
    private static Set<String> nullDefaultModelNames(List<ModelMap> allModels) {
        Set<String> result = new HashSet<>();
        Map<String, String> aliases = new LinkedHashMap<>();
        for (ModelMap modelMap : allModels) {
            CodegenModel model = modelMap.getModel();
            Object cppTypeValue = model.vendorExtensions.get("x-cpp-type");
            String cppType = cppTypeValue instanceof String
                    ? (String) cppTypeValue : model.dataType;
            aliases.put(model.classname, stripSharedPtr(cppType));
            if (cppType == null) {
                continue;
            }

            // Preserve the prior empty-body behavior only when the default
            // variant branch is null and anyOf therefore accepts that value.
            Object metadataValue = model.vendorExtensions.get("x-cpp-composition-branches");
            if (!cppType.startsWith("std::variant<")
                    || !(metadataValue instanceof Map)) {
                continue;
            }
            Map<String, Object> metadata = (Map<String, Object>) metadataValue;
            if (!"anyOf".equals(metadata.get("keyword"))) {
                continue;
            }
            Object branchesValue = metadata.get("branches");
            if (!(branchesValue instanceof List) || ((List<?>) branchesValue).isEmpty()) {
                continue;
            }
            Object firstBranch = ((List<?>) branchesValue).get(0);
            if (firstBranch instanceof Map
                    && "always".equals(((Map<?, ?>) firstBranch).get("null-capability"))) {
                result.add(model.classname);
            }
        }

        boolean changed;
        do {
            changed = false;
            for (Map.Entry<String, String> alias : aliases.entrySet()) {
                if (result.contains(alias.getValue()) && result.add(alias.getKey())) {
                    changed = true;
                }
            }
        } while (changed);
        return result;
    }

    private void addApiResponseMetadata(
            CodegenOperation operation, Set<String> nullDefaultModels) {
        boolean hasDefaultResponse = false;
        for (CodegenResponse response : operation.responses) {
            response.vendorExtensions.put("x-codegen-cpp-message", response.message);
            response.vendorExtensions.put("x-codegen-return-compatible",
                    Objects.equals(operation.returnType, response.dataType));
            response.vendorExtensions.put(X_CODEGEN_RESPONSE_IS_ONE_OF,
                    isOneOfResponse(response));
            String responseType = stripSharedPtr(response.dataType);
            response.vendorExtensions.put(X_CODEGEN_EMPTY_BODY_TOLERANT,
                    response.isMap || response.isFreeFormObject || response.isAnyType
                            || nullDefaultModels.contains(responseType));
            if (response.isRange()) {
                response.vendorExtensions.put(
                        X_CODEGEN_RESPONSE_RANGE, response.code.substring(0, 1));
            }

            if (response.isDefault) {
                hasDefaultResponse = true;
                response.vendorExtensions.put(X_CODEGEN_DEFAULT_RESPONSE_IS_RETURN_COMPATIBLE,
                        operation.returnType != null
                                && Objects.equals(operation.returnType, response.dataType));
            }

            // Every oneOf/anyOf response must use the generated model decoder.
            // Distinct C++ alternatives may be structurally interchangeable,
            // so the generic variant converter cannot honor schema membership.
            if (response.dataType != null) {
                String unwrapped = stripSharedPtr(response.dataType);
                String compositionKeyword = composedKeywordsByModel.get(unwrapped);
                if ("oneOf".equals(compositionKeyword)
                        || "anyOf".equals(compositionKeyword)) {
                    response.vendorExtensions.put("x-cpp-use-model-from-json-value", true);
                }
            }
        }
        operation.vendorExtensions.put(X_CODEGEN_HAS_DEFAULT_RESPONSE, hasDefaultResponse);

        // Detect text/event-stream produces for SSE streaming responses.
        // sseSchemaMode selects whether the response schema describes the media
        // representation or each JSON event data payload. The operation vendor
        // extension can opt into typed event-data decoding.
        //
        // Mode split:
        //   representation (default): the WHATWG framer delivers raw event
        //     data strings. No JSON conversion is applied. The return type
        //     is std::vector<std::string>.
        //   jsonEventData: each event data payload is parsed as JSON against
        //     the response schema. The return type is std::vector<EventType>
        //     with generated fromJsonValue_ converters.
        //
        // Dual-content operations always keep the normal JSON return type
        // for the application/json path. A dedicated {operationId}Stream
        // method is always emitted. In representation mode the stream method
        // returns std::vector<std::string>; in jsonEventData mode it returns
        // std::vector<EventType>.
        //
        // The WHATWG framer (SseEventFramer, in HttpClientImpl) is always
        // independent from JSON conversion — it operates on raw bytes and
        // fires string data payloads. JSON conversion is applied only in
        // jsonEventData mode at the template (callback) level.
        if (operation.produces != null && !operation.produces.isEmpty()) {
            boolean hasEventStream = false;
            boolean hasJsonStream = false;
            for (Map<String, String> produce : operation.produces) {
                String mediaType = produce.get("mediaType");
                if ("text/event-stream".equalsIgnoreCase(mediaType)) {
                    hasEventStream = true;
                } else if (mediaType != null && mediaType.contains("json")) {
                    hasJsonStream = true;
                }
            }
            boolean isPureSse = hasEventStream && !hasJsonStream;
            boolean isDualContent = hasEventStream && hasJsonStream;
            operation.vendorExtensions.put("x-codegen-streaming-response", isPureSse);
            // Determine whether to apply typed event-data decoding.
            // jsonEventData mode or per-operation x-sse-event-data-schema
            // opt-in triggers typed JSON-per-data conversion.
            boolean useJsonEventData = SSE_SCHEMA_MODE_JSON_EVENT_DATA.equals(sseSchemaMode)
                    || Boolean.TRUE.equals(
                        operation.vendorExtensions.get(X_SSE_EVENT_DATA_SCHEMA));
            // Set the representation-mode flag so templates can emit the
            // correct return type and callback body (raw push_back vs
            // appendParsedEvent with JSON converter).
            if (!useJsonEventData) {
                operation.vendorExtensions.put("x-codegen-sse-representation-mode", true);
            }
            // For pure SSE ops, flag all 2xx responses as streaming and
            // set the stripped element type (without shared_ptr) for use in
            // the event vector element and converter name.
            // For dual-content ops, mark SSE responses (different datatype from returnType)
            // as streaming so the stream method template can identify them.
            // Also mark each response with x-codegen-return-compatible so the normal
            // method template can skip responses whose dataType doesn't match the
            // operation return type (avoids type mismatch in deserializedResponse).
            for (CodegenResponse response : operation.responses) {
                if (isPureSse) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (useJsonEventData) {
                        // Typed event-data mode: emit oneOf metadata for JSON conversion.
                        if (isOneOfResponse(response)
                                || isOneOfMediaType(response, "text/event-stream")) {
                            operation.vendorExtensions.put(X_CODEGEN_STREAM_IS_ONE_OF, true);
                            operation.vendorExtensions.put(
                                    "x-codegen-sse-event-data-is-oneof", true);
                        }
                        if (response.dataType != null) {
                            String eventDataType = stripSharedPtr(response.dataType);
                            // Only set element type for model types (uppercase first char).
                            if (!eventDataType.startsWith("std::")
                                    && !eventDataType.startsWith("boost::")
                                    && Character.isUpperCase(eventDataType.charAt(0))) {
                                response.vendorExtensions.put("x-codegen-stream-element-type",
                                        eventDataType);
                                operation.vendorExtensions.put("x-codegen-stream-element-type",
                                        eventDataType);
                                response.vendorExtensions.put("x-codegen-sse-event-data-type",
                                        eventDataType);
                                operation.vendorExtensions.put("x-codegen-sse-event-data-type",
                                        eventDataType);
                            }
                        }
                    }
                } else if (isDualContent && response.is2xx && response.dataType != null
                        && !response.dataType.equals(operation.returnType)) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (!useJsonEventData) {
                        response.vendorExtensions.put("x-codegen-sse-representation-mode", true);
                    }
                    if (response.dataType != null) {
                        String streamElementType = stripSharedPtr(response.dataType);
                        response.vendorExtensions.put("x-codegen-stream-element-type",
                                streamElementType);
                        if (useJsonEventData) {
                            response.vendorExtensions.put("x-codegen-sse-event-data-type",
                                    streamElementType);
                        }
                    }
                }
            }
            // If a pure SSE operation has no response schema (no data type
            // on any 2xx response), returnType will be null and the
            // mustache template would produce std::vector<void>, which
            // is invalid C++. Clear the streaming flag so the normal
            // non-streaming void path is used instead.
            if (isPureSse && operation.returnType == null) {
                operation.vendorExtensions.put("x-codegen-streaming-response", false);
                for (CodegenResponse r : operation.responses) {
                    r.vendorExtensions.put("x-codegen-streaming-response", false);
                }
            }
            // Dual-content: generate stream method
            // Only emit the stream method if we can resolve a concrete SSE
            // element type from the response content. Without it, the template
            // would produce an invalid std::vector<> with an empty parameter.
            if (isDualContent) {
                // Resolve SSE response type from the response content media-type map.
                // Specs may expose a single 200 with both application/json and
                // text/event-stream. Look for text/event-stream in any 2xx response.
                String sseReturnType = null;
                String sseBaseModelName = null;
                for (CodegenResponse response : operation.responses) {
                    if (!response.is2xx || response.getContent() == null) {
                        continue;
                    }
                    CodegenMediaType sseMediaType = response.getContent().get("text/event-stream");
                    if (sseMediaType != null && sseMediaType.getSchema() != null) {
                        CodegenProperty sseSchema = sseMediaType.getSchema();
                        String rawType = sseSchema.dataType;
                        if (rawType != null) {
                            sseReturnType = rawType;
                            // Derive a valid C++ identifier for the fromJsonValue_ converter.
                            // Strip std::shared_ptr<X> wrapper down to just X.
                            sseBaseModelName = stripSharedPtr(rawType);
                            if (useJsonEventData && isOneOfSchema(sseSchema)) {
                                operation.vendorExtensions.put(
                                        X_CODEGEN_DUAL_STREAM_IS_ONE_OF, true);
                                operation.vendorExtensions.put(
                                        "x-codegen-dual-sse-event-data-is-oneof", true);
                            }
                            break;
                        }
                    }
                }
                // Fallback: use response dataType (works for split-status fixtures)
                if (sseReturnType == null) {
                    for (CodegenResponse response : operation.responses) {
                        if (response.is2xx && response.dataType != null
                                && !response.dataType.equals(operation.returnType)) {
                            sseReturnType = response.dataType;
                            sseBaseModelName = stripSharedPtr(response.dataType);
                            break;
                        }
                    }
                }
                if (sseReturnType == null) {
                    // Final fallback: first 2xx response
                    for (CodegenResponse response : operation.responses) {
                        if (response.is2xx && response.dataType != null) {
                            sseReturnType = response.dataType;
                            sseBaseModelName = stripSharedPtr(response.dataType);
                            break;
                        }
                    }
                }
                if (sseReturnType != null && sseBaseModelName != null) {
                    if (useJsonEventData && isOneOfType(sseReturnType)) {
                        operation.vendorExtensions.put(X_CODEGEN_DUAL_STREAM_IS_ONE_OF, true);
                        operation.vendorExtensions.put(
                                "x-codegen-dual-sse-event-data-is-oneof", true);
                    }
                    operation.vendorExtensions.put("x-codegen-dual-content", true);
                    // Full C++ type for the vector element; it may contain shared_ptr.
                    operation.vendorExtensions.put(
                            "x-codegen-dual-stream-return-type", sseReturnType);
                    // Base name for the fromJsonValue_ converter, without shared_ptr.
                    operation.vendorExtensions.put(
                            "x-codegen-dual-stream-base-name", sseBaseModelName);
                    // Stripped element type for event conversion and the vector element
                    // (same as base name since both strip shared_ptr, but semantically distinct)
                    String dualStreamElementType = stripSharedPtr(sseReturnType);
                    operation.vendorExtensions.put(
                            "x-codegen-dual-stream-element-type", dualStreamElementType);
                    if (useJsonEventData) {
                        operation.vendorExtensions.put(
                                "x-codegen-dual-sse-event-data-type", dualStreamElementType);
                    }
                    // Also propagate to each response so the template can access it
                    // from within the {{#responses}} context scope.
                    for (CodegenResponse response : operation.responses) {
                        response.vendorExtensions.put(
                                "x-codegen-dual-stream-return-type", sseReturnType);
                        response.vendorExtensions.put(
                                "x-codegen-dual-stream-base-name", sseBaseModelName);
                        response.vendorExtensions.put(
                                "x-codegen-dual-stream-element-type", dualStreamElementType);
                        if (useJsonEventData) {
                            response.vendorExtensions.put(
                                    "x-codegen-dual-sse-event-data-type", dualStreamElementType);
                        }
                    }
                }
            }
        }
    }

    /**
     * Detects operations with heterogeneous successful response shapes and tags
     * them for response-union generation. A heterogeneous operation has multiple
     * 2xx responses with different body types, or a mix of body/no-body responses.
     *
     * <p>Sets on the operation:
     *   x-codegen-response-union: the generated union struct name
     * Sets on each response used in the union:
     *   x-codegen-response-union: the union struct name (same as operation-level)
     *   x-codegen-response-union-body-type: the variant alternative body type
     *     (e.g., {@code std::shared_ptr<FullResource>} or {@code std::monostate}).
     *     Duplicate C++ body types are wrapped in
     *     {@code StatusTaggedValue<boost::beast::http::status(N), T>}.
     *
     * <p>Single-shape operations (one success type) are left unchanged so the
     * existing simple-signature path is used.
     */
    private void addResponseUnionMetadata(CodegenOperation operation) {
        // Collect union-eligible responses: exact 2xx, range 2xx, or default
        // responses with a body type.  At least two distinct body shapes are
        // required for union generation.
        List<CodegenResponse> unionEligible = new ArrayList<>();
        for (CodegenResponse response : operation.responses) {
            boolean isSuccessWithBody = response.is2xx
                    || (response.isDefault && response.dataType != null);
            if (isSuccessWithBody) {
                unionEligible.add(response);
            }
        }
        if (unionEligible.size() < 2) {
            return;
        }

        // Detect whether eligible responses have distinct body shapes.
        // "Distinct" means different dataType, or mixed body/no-body.
        boolean hasMixedShapes = false;
        String firstDataType = unionEligible.get(0).dataType;
        for (int idx = 1; idx < unionEligible.size(); ++idx) {
            if (!Objects.equals(firstDataType, unionEligible.get(idx).dataType)) {
                hasMixedShapes = true;
                break;
            }
        }
        if (!hasMixedShapes) {
            boolean hasBody = false;
            boolean hasNoBody = false;
            for (CodegenResponse r : unionEligible) {
                if (r.dataType != null) {
                    hasBody = true;
                } else {
                    hasNoBody = true;
                }
            }
            if (hasBody && hasNoBody) {
                hasMixedShapes = true;
            }
        }
        if (!hasMixedShapes) {
            return;
        }

        // Build the union struct name: capitalize the operationId + "Response"
        String operationId = operation.operationIdCamelCase != null
                ? operation.operationIdCamelCase
                : operation.operationId;
        if (operationId == null || operationId.isEmpty()) {
            return;
        }
        String unionName = Character.toUpperCase(operationId.charAt(0))
                + operationId.substring(1) + "Response";

        operation.vendorExtensions.put(X_CODEGEN_RESPONSE_UNION, unionName);

        // Detect duplicate raw body types and build StatusTaggedValue wrappers.
        // Key = raw C++ type string, value = list of responses using it.
        Map<String, List<CodegenResponse>> rawTypeToResponses = new LinkedHashMap<>();
        for (CodegenResponse response : unionEligible) {
            String rawType = response.dataType != null
                    ? response.dataType : "std::monostate";
            rawTypeToResponses.computeIfAbsent(rawType,
                    k -> new ArrayList<>()).add(response);
        }

        // Assign the final body type to each response.
        for (CodegenResponse response : unionEligible) {
            // Propagate union name to per-response scope so templates can
            // access x-codegen-response-union directly without parent lookup.
            response.vendorExtensions.put(X_CODEGEN_RESPONSE_UNION, unionName);

            String rawType = response.dataType != null
                    ? response.dataType : "std::monostate";
            List<CodegenResponse> sharingResponses = rawTypeToResponses.get(rawType);
            String finalBodyType;
            if (sharingResponses != null && sharingResponses.size() > 1) {
                // Two or more statuses share the same C++ body type.
                // Wrap in StatusTaggedValue<status(N), T> to preserve
                // distinct status identity in the variant.
                String statusCodeStr = response.code;
                int statusCodeInt;
                try {
                    statusCodeInt = Integer.parseInt(
                            statusCodeStr.replaceAll("[^0-9]", ""));
                } catch (NumberFormatException exception) {
                    // Range or default code; use 0 as placeholder.
                    statusCodeInt = 0;
                }
                finalBodyType = "StatusTaggedValue<boost::beast::http::status("
                        + statusCodeInt + "), " + rawType + ">";
            } else {
                finalBodyType = rawType;
            }
            response.vendorExtensions.put(
                    X_CODEGEN_RESPONSE_UNION_BODY_TYPE, finalBodyType);
        }

    }

    private boolean isOneOfResponse(CodegenResponse response) {
        if (response.getContent() != null) {
            for (Map.Entry<String, CodegenMediaType> contentEntry
                    : response.getContent().entrySet()) {
                String mediaType = contentEntry.getKey();
                CodegenMediaType codegenMediaType = contentEntry.getValue();
                if (mediaType != null && mediaType.toLowerCase(Locale.ROOT).contains("json")
                        && codegenMediaType != null
                        && isOneOfSchema(codegenMediaType.getSchema())) {
                    return true;
                }
            }
        }
        return isOneOfType(response.dataType);
    }

    private boolean isOneOfMediaType(CodegenResponse response, String mediaType) {
        if (response.getContent() == null) {
            return false;
        }
        CodegenMediaType codegenMediaType = response.getContent().get(mediaType);
        return codegenMediaType != null && isOneOfSchema(codegenMediaType.getSchema());
    }

    private boolean isOneOfSchema(CodegenProperty schema) {
        return schema != null
                && (Boolean.TRUE.equals(schema.vendorExtensions.get("x-cpp-is-oneof"))
                || isOneOfType(schema.dataType));
    }

    private boolean isOneOfType(String dataType) {
        String unwrappedType = stripSharedPtr(dataType);
        return "oneOf".equals(composedKeywordsByModel.get(unwrappedType));
    }
}
