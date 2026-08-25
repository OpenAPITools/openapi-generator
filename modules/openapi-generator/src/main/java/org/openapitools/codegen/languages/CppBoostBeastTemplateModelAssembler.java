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
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Assembles operation and response vendor extensions consumed by API templates. */
final class CppBoostBeastTemplateModelAssembler {
    private static final String SSE_SCHEMA_MODE_JSON_EVENT_DATA = "jsonEventData";
    private static final String X_SSE_EVENT_DATA_SCHEMA = "x-sse-event-data-schema";
    private static final String X_SSE_REQUEST_PROPERTY = "x-sse-request-property";
    private static final String X_SSE_EVENT_TYPE = "x-sse-event-type";
    private static final String X_CODEGEN_CONDITIONAL_SSE = "x-codegen-conditional-sse";
    private static final String X_CODEGEN_SSE_REQUEST_PARAM = "x-codegen-sse-request-param";
    private static final String X_CODEGEN_SSE_REQUEST_GETTER = "x-codegen-sse-request-getter";
    private static final String X_CODEGEN_SSE_REQUEST_SETTER = "x-codegen-sse-request-setter";
    private static final String X_CODEGEN_SSE_REQUEST_FALSE_VALUE =
            "x-codegen-sse-request-false-value";
    private static final String X_CODEGEN_SSE_REQUEST_TRUE_VALUE =
            "x-codegen-sse-request-true-value";
    private static final String X_CODEGEN_SSE_REQUEST_TYPE = "x-codegen-sse-request-type";
    private static final String X_CODEGEN_SSE_REQUEST_SHARED_PTR =
            "x-codegen-sse-request-shared-ptr";
    private static final String X_CODEGEN_SSE_REQUEST_LOCAL = "x-codegen-sse-request-local";
    private static final String X_CODEGEN_SSE_EVENT_TYPE_OVERRIDE =
            "x-codegen-sse-event-type-override";
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
    private static final String X_CODEGEN_RESPONSE_UNION_MEMBERS =
            "x-codegen-response-union-members";
    private static final String X_CODEGEN_MULTIPART_FILENAME_PARAM =
            "x-codegen-multipart-filename-param";
    private static final String X_CODEGEN_MULTIPART_FILENAME_PARAM_NAME =
            "x-codegen-multipart-filename-param-name";
    private static final String X_CODEGEN_IS_OPTIONAL_FORM_PARAMETER =
            "x-codegen-is-optional-form-parameter";
    private static final String X_CODEGEN_HAS_OPTIONAL_FORM_PARAMETER =
            "x-codegen-has-optional-form-parameter";

    private final OpenAPI phaseOpenApi;
    private final List<String> webhookPreservation;
    private final Map<String, List<String>> operationCallbacks;
    private final Map<String, List<String>> operationLinks;
    private final Map<String, String> composedKeywordsByModel;
    private final String sseSchemaMode;
    private final Set<String> sseOperationIds;
    private final Map<String, String> sseRequestPropertyMappings;
    private final Map<String, String> sseEventTypeMappings;
    private final boolean inferConditionalSseOperations;
    private final boolean hasExplicitRootServers;


    CppBoostBeastTemplateModelAssembler(
            OpenAPI phaseOpenApi,
            List<String> webhookPreservation,
            Map<String, List<String>> operationCallbacks,
            Map<String, List<String>> operationLinks,
            Map<String, String> composedKeywordsByModel,
            String sseSchemaMode,
            Set<String> sseOperationIds,
            Map<String, String> sseRequestPropertyMappings,
            Map<String, String> sseEventTypeMappings,
            boolean inferConditionalSseOperations,
            boolean hasExplicitRootServers) {
        this.phaseOpenApi = phaseOpenApi;
        this.webhookPreservation = webhookPreservation;
        this.operationCallbacks = operationCallbacks;
        this.operationLinks = operationLinks;
        this.composedKeywordsByModel = composedKeywordsByModel;
        this.sseSchemaMode = sseSchemaMode;
        this.sseOperationIds = Collections.unmodifiableSet(new HashSet<>(sseOperationIds));
        this.sseRequestPropertyMappings = Collections.unmodifiableMap(
                new LinkedHashMap<>(sseRequestPropertyMappings));
        this.sseEventTypeMappings = Collections.unmodifiableMap(
                new LinkedHashMap<>(sseEventTypeMappings));
        this.inferConditionalSseOperations = inferConditionalSseOperations;
        this.hasExplicitRootServers = hasExplicitRootServers;
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

    /** True when the list is exactly swagger-parser's implicit root default
     *  (a single Server with url "/") and the raw source omitted `servers`. */
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
            // Operation-level lists are present only when the source declared
            // them, including the meaningful explicit root URL "/".
            servers = raw.getServers();
        }
        if (servers == null || servers.isEmpty()) {
            if (phaseOpenApi != null && phaseOpenApi.getPaths() != null
                    && phaseOpenApi.getPaths().get(op.path) != null) {
                PathItem item = phaseOpenApi.getPaths().get(op.path);
                if (item.getServers() != null && !item.getServers().isEmpty()) {
                    // Path-level lists likewise preserve an explicit root URL.
                    servers = item.getServers();
                }
                if ((servers == null || servers.isEmpty())
                        && phaseOpenApi.getServers() != null
                        && !phaseOpenApi.getServers().isEmpty()) {
                    servers = phaseOpenApi.getServers();
                    if (!hasExplicitRootServers && isParserDefaultServerList(servers)) {
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

    private static void addMultipartParameterMetadata(CodegenOperation operation) {
        Set<String> occupiedNames = new HashSet<>();
        for (CodegenParameter parameter : operation.allParams) {
            occupiedNames.add(parameter.paramName);
        }
        boolean hasOptionalFormParameter = false;
        for (CodegenParameter parameter : operation.allParams) {
            if (parameter.isFormParam && !parameter.required) {
                parameter.vendorExtensions.put(X_CODEGEN_IS_OPTIONAL_FORM_PARAMETER, true);
                hasOptionalFormParameter = true;
            }
            if (!parameter.isFormParam || (!parameter.isFile && !parameter.isBinary)) {
                continue;
            }
            String filenameParamName = parameter.paramName + "Filename";
            while (!occupiedNames.add(filenameParamName)) {
                filenameParamName += "_";
            }
            parameter.vendorExtensions.put(X_CODEGEN_MULTIPART_FILENAME_PARAM, true);
            parameter.vendorExtensions.put(
                    X_CODEGEN_MULTIPART_FILENAME_PARAM_NAME, filenameParamName);
        }
        if (hasOptionalFormParameter) {
            operation.vendorExtensions.put(X_CODEGEN_HAS_OPTIONAL_FORM_PARAMETER, true);
        }
    }

    private static Map<String, CodegenModel> indexModels(List<ModelMap> allModels) {
        Map<String, CodegenModel> result = new LinkedHashMap<>();
        for (ModelMap modelMap : allModels) {
            CodegenModel model = modelMap.getModel();
            if (model.name != null) result.put(model.name, model);
            if (model.schemaName != null) result.put(model.schemaName, model);
            if (model.classname != null) result.put(model.classname, model);
        }
        return result;
    }

    private static String extensionString(CodegenOperation operation, String key) {
        Object rawValue = operation.vendorExtensions.get(key);
        if (rawValue == null) return null;
        String value = rawValue.toString().trim();
        if (value.isEmpty()) {
            throw new IllegalArgumentException(operation.operationId + ": " + key
                    + " must not be empty");
        }
        return value;
    }

    private static List<String> operationKeys(CodegenOperation operation) {
        List<String> keys = new ArrayList<>();
        if (operation.operationIdOriginal != null
                && !operation.operationIdOriginal.isBlank()) {
            keys.add(operation.operationIdOriginal);
        }
        if (operation.operationId != null && !operation.operationId.isBlank()
                && !keys.contains(operation.operationId)) {
            keys.add(operation.operationId);
        }
        return keys;
    }

    private static String configuredValue(CodegenOperation operation,
            Map<String, String> configuredMappings, String optionName) {
        String resolved = null;
        for (String key : operationKeys(operation)) {
            String candidate = configuredMappings.get(key);
            if (candidate == null) continue;
            if (resolved != null && !resolved.equals(candidate)) {
                throw new IllegalArgumentException(operation.operationId + ": "
                        + optionName + " maps the raw and generated operation names"
                        + " to different values");
            }
            resolved = candidate;
        }
        return resolved;
    }

    private static boolean configuredOperation(CodegenOperation operation,
            Set<String> configuredOperationIds) {
        for (String key : operationKeys(operation)) {
            if (configuredOperationIds.contains(key)) return true;
        }
        return false;
    }

    private static String resolveMapping(CodegenOperation operation, String extensionKey,
            Map<String, String> configuredMappings, String optionName) {
        String extensionValue = extensionString(operation, extensionKey);
        String configured = configuredValue(operation, configuredMappings, optionName);
        if (extensionValue != null && configured != null
                && !extensionValue.equals(configured)) {
            throw new IllegalArgumentException(operation.operationId + ": conflicting "
                    + extensionKey + " and " + optionName);
        }
        return extensionValue != null ? extensionValue : configured;
    }

    private static boolean produces(CodegenOperation operation, String expectedMediaType) {
        if (operation.produces == null) return false;
        for (Map<String, String> media : operation.produces) {
            String mediaType = media.get("mediaType");
            if (mediaType != null && mediaType.equalsIgnoreCase(expectedMediaType)) {
                return true;
            }
        }
        return false;
    }

    private static CodegenModel findRequestModel(CodegenOperation operation,
            Map<String, CodegenModel> modelsByName) {
        if (operation.bodyParam == null) return null;
        String[] candidates = {operation.bodyParam.baseType, operation.bodyParam.dataType};
        for (String candidate : candidates) {
            if (candidate == null) continue;
            String type = stripSharedPtr(candidate.trim());
            CodegenModel model = modelsByName.get(type);
            if (model != null) return model;
            int namespace = type.lastIndexOf("::");
            if (namespace >= 0) {
                model = modelsByName.get(type.substring(namespace + 2));
                if (model != null) return model;
            }
        }
        return null;
    }

    private static boolean isBooleanProperty(CodegenProperty property) {
        return property.isBoolean
                || "bool".equals(property.dataType)
                || "boolean".equalsIgnoreCase(property.baseType);
    }

    private static boolean propertyMatches(CodegenProperty property,
            String propertyName) {
        return propertyName.equals(property.baseName)
                || propertyName.equals(property.name);
    }

    private static CodegenProperty findBooleanRequestProperty(CodegenOperation operation,
            Map<String, CodegenModel> modelsByName, String propertyName,
            boolean required) {
        CodegenModel requestModel = findRequestModel(operation, modelsByName);
        if (requestModel == null) {
            if (required) {
                throw new IllegalArgumentException(operation.operationId
                        + ": conditional SSE requires an object request body");
            }
            return null;
        }
        List<CodegenProperty> matches = new ArrayList<>();
        for (CodegenProperty property : requestModel.allVars) {
            if (propertyMatches(property, propertyName)) matches.add(property);
        }
        if (matches.size() > 1) {
            throw new IllegalArgumentException(operation.operationId + ": request property '"
                    + propertyName + "' is ambiguous in model " + requestModel.classname);
        }
        if (matches.isEmpty() || !isBooleanProperty(matches.get(0))) {
            if (required) {
                throw new IllegalArgumentException(operation.operationId + ": request property '"
                        + propertyName + "' must exist and have type boolean in model "
                        + requestModel.classname);
            }
            return null;
        }
        return matches.get(0);
    }

    private static CodegenProperty inferBooleanRequestProperty(CodegenOperation operation,
            Map<String, CodegenModel> modelsByName) {
        CodegenModel requestModel = findRequestModel(operation, modelsByName);
        if (requestModel == null) return null;
        List<CodegenProperty> booleanProperties = new ArrayList<>();
        for (CodegenProperty property : requestModel.allVars) {
            if (isBooleanProperty(property)) booleanProperties.add(property);
        }
        if (booleanProperties.size() == 1) return booleanProperties.get(0);
        List<CodegenProperty> conventional = new ArrayList<>();
        for (CodegenProperty property : booleanProperties) {
            String normalized = normalizeIdentifier(property.baseName != null
                    ? property.baseName : property.name);
            if ("stream".equals(normalized) || "streaming".equals(normalized)
                    || "sse".equals(normalized)) {
                conventional.add(property);
            }
        }
        return conventional.size() == 1 ? conventional.get(0) : null;
    }

    private static String normalizeIdentifier(String value) {
        return value == null ? "" : value.replaceAll("[^A-Za-z0-9]", "")
                .toLowerCase(Locale.ROOT);
    }

    private static CodegenModel modelForType(String type,
            Map<String, CodegenModel> modelsByName) {
        if (type == null || type.isBlank()) return null;
        String unwrapped = stripSharedPtr(type.trim());
        CodegenModel model = modelsByName.get(unwrapped);
        if (model != null) return model;
        int namespace = unwrapped.lastIndexOf("::");
        return namespace < 0 ? null : modelsByName.get(unwrapped.substring(namespace + 2));
    }

    private static CodegenModel inferSseEventModel(CodegenOperation operation,
            Map<String, CodegenModel> modelsByName) {
        Set<CodegenModel> candidates = new LinkedHashSet<>();
        for (CodegenResponse response : operation.responses) {
            if (!response.is2xx || response.getContent() == null) continue;
            for (Map.Entry<String, CodegenMediaType> media : response.getContent().entrySet()) {
                if (!"text/event-stream".equalsIgnoreCase(media.getKey())
                        || media.getValue() == null
                        || media.getValue().getSchema() == null) {
                    continue;
                }
                CodegenProperty schema = media.getValue().getSchema();
                String[] types = {schema.dataType, schema.baseType, schema.complexType};
                for (String type : types) {
                    CodegenModel model = modelForType(type, modelsByName);
                    if (model != null) candidates.add(model);
                }
            }
        }
        if (candidates.size() > 1) {
            throw new IllegalArgumentException(operation.operationId
                    + ": multiple generated models match the SSE response; configure "
                    + "sseEventTypeMappings explicitly");
        }
        return candidates.isEmpty() ? null : candidates.iterator().next();
    }

    private void addSseOperationMetadata(CodegenOperation operation,
            Map<String, CodegenModel> modelsByName, Set<String> extraModelImports) {
        boolean producesSse = produces(operation, "text/event-stream");
        boolean producesJson = produces(operation, "application/json");
        String requestProperty = resolveMapping(operation, X_SSE_REQUEST_PROPERTY,
                sseRequestPropertyMappings, "sseRequestPropertyMappings");
        boolean explicitlyConditional = requestProperty != null
                || configuredOperation(operation, sseOperationIds);
        if (explicitlyConditional && requestProperty == null) requestProperty = "stream";

        CodegenProperty selector = null;
        if (requestProperty != null) {
            if (!producesSse || !producesJson) {
                throw new IllegalArgumentException(operation.operationId
                        + ": conditional SSE requires both application/json and"
                        + " text/event-stream responses");
            }
            selector = findBooleanRequestProperty(
                    operation, modelsByName, requestProperty, true);
        } else if (inferConditionalSseOperations && producesSse && producesJson) {
            selector = inferBooleanRequestProperty(operation, modelsByName);
        }

        if (selector != null) {
            CodegenModel requestModel = findRequestModel(operation, modelsByName);
            Set<String> parameterNames = new HashSet<>();
            for (CodegenParameter parameter : operation.allParams) {
                parameterNames.add(parameter.paramName);
            }
            String requestLocal = "conditionalSseRequestBody";
            while (parameterNames.contains(requestLocal)) requestLocal += "_";
            operation.vendorExtensions.put(X_CODEGEN_CONDITIONAL_SSE, true);
            operation.vendorExtensions.put(
                    X_CODEGEN_SSE_REQUEST_PARAM, operation.bodyParam.paramName);
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_GETTER, selector.getter);
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_SETTER, selector.setter);
            String selectorType = selector.dataType == null ? "bool" : selector.dataType;
            String falseValue = "bool".equals(selectorType)
                    ? "false" : selectorType + "{false}";
            String trueValue = "bool".equals(selectorType)
                    ? "true" : selectorType + "{true}";
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_FALSE_VALUE, falseValue);
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_TRUE_VALUE, trueValue);
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_TYPE,
                    requestModel.classname);
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_SHARED_PTR,
                    operation.bodyParam.dataType != null
                            && operation.bodyParam.dataType.startsWith("std::shared_ptr<"));
            operation.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_LOCAL, requestLocal);
            operation.bodyParam.vendorExtensions.put(X_CODEGEN_CONDITIONAL_SSE, true);
            operation.bodyParam.vendorExtensions.put(X_CODEGEN_SSE_REQUEST_LOCAL,
                    requestLocal);
        }

        String eventTypeName = resolveMapping(operation, X_SSE_EVENT_TYPE,
                sseEventTypeMappings, "sseEventTypeMappings");
        boolean explicitEventType = eventTypeName != null;
        CodegenModel eventModel = eventTypeName == null ? null
                : modelsByName.get(eventTypeName);
        if (eventTypeName != null && eventModel == null) {
            throw new IllegalArgumentException(operation.operationId
                    + ": SSE event type does not name a generated model: " + eventTypeName);
        }
        if (eventModel == null && selector != null && inferConditionalSseOperations) {
            eventModel = inferSseEventModel(operation, modelsByName);
        }
        if (eventModel == null) return;
        if (!producesSse) {
            throw new IllegalArgumentException(operation.operationId
                    + ": an SSE event type requires a text/event-stream response");
        }
        operation.vendorExtensions.put(X_CODEGEN_SSE_EVENT_TYPE_OVERRIDE,
                eventModel.classname);
        if (explicitEventType) {
            operation.vendorExtensions.put(X_SSE_EVENT_DATA_SCHEMA, true);
        }
        extraModelImports.add(eventModel.classname);
    }

    private void validateConfiguredOperationIds(Set<String> seenOperationIds) {
        Set<String> configured = new HashSet<>(sseOperationIds);
        configured.addAll(sseRequestPropertyMappings.keySet());
        configured.addAll(sseEventTypeMappings.keySet());
        configured.removeAll(seenOperationIds);
        if (!configured.isEmpty()) {
            throw new IllegalArgumentException(
                    "SSE configuration references unknown operationIds: " + configured);
        }
    }

    private static void addModelImports(OperationsMap operations,
            Set<String> modelNames) {
        List<Map<String, String>> imports = operations.getImports();
        if (imports == null) {
            imports = new ArrayList<>();
            operations.setImports(imports);
        }

        // API headers live beside api/HttpClient.h while model headers live in
        // the sibling model directory. Relative imports prevent a second
        // generated client's include roots from satisfying these dependencies.
        Set<String> existing = new HashSet<>();
        for (Map<String, String> item : imports) {
            String include = item.get("import");
            if (include != null && include.startsWith("#include \"")
                    && include.endsWith(".h\"")
                    && include.indexOf('/') < 0) {
                include = "#include \"../model/" + include.substring(10);
                item.put("import", include);
            }
            existing.add(include);
        }
        for (String modelName : modelNames) {
            String include = "#include \"../model/" + modelName + ".h\"";
            if (existing.add(include)) {
                Map<String, String> item = new LinkedHashMap<>();
                item.put("import", include);
                imports.add(item);
            }
        }
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
        Map<String, CodegenModel> modelsByName = indexModels(allModels);
        Set<String> seenOperationIds = new HashSet<>();
        Set<String> extraModelImports = new HashSet<>();

        for (CodegenOperation op : operationList) {
            seenOperationIds.addAll(operationKeys(op));
            addSseOperationMetadata(op, modelsByName, extraModelImports);
            addMultipartParameterMetadata(op);
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
        validateConfiguredOperationIds(seenOperationIds);
        addModelImports(objs, extraModelImports);
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
        // representation (default) delivers structured SseEvent values without
        // decoding data. jsonEventData parses each event's data as JSON against
        // the response schema. A per-operation event type forces typed decoding.
        // Dual-content operations expose a stream companion only when the
        // operation was explicitly configured or safely inferred as conditional.
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
            boolean isConditionalSse = isDualContent && Boolean.TRUE.equals(
                    operation.vendorExtensions.get(X_CODEGEN_CONDITIONAL_SSE));
            operation.vendorExtensions.put("x-codegen-streaming-response", isPureSse);
            // Determine whether to apply typed event-data decoding.
            // jsonEventData mode or per-operation x-sse-event-data-schema
            // opt-in triggers typed JSON-per-data conversion.
            boolean useJsonEventData = SSE_SCHEMA_MODE_JSON_EVENT_DATA.equals(sseSchemaMode)
                    || Boolean.TRUE.equals(
                        operation.vendorExtensions.get(X_SSE_EVENT_DATA_SCHEMA));
            String eventTypeOverride = (String) operation.vendorExtensions.get(
                    X_CODEGEN_SSE_EVENT_TYPE_OVERRIDE);
            if (isPureSse && eventTypeOverride == null && operation.returnType == null) {
                useJsonEventData = false;
            }
            // Representation mode forwards complete structured events directly.
            if (!useJsonEventData) {
                operation.vendorExtensions.put("x-codegen-sse-representation-mode", true);
            }
            // Mark pure SSE responses for the incremental callback path. For
            // conditional operations, response metadata identifies typed SSE
            // alternatives without changing the normal JSON return contract.
            for (CodegenResponse response : operation.responses) {
                if (isPureSse) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (useJsonEventData) {
                        String eventDataType = eventTypeOverride != null
                                ? eventTypeOverride : stripSharedPtr(response.dataType);
                        if (isOneOfResponse(response)
                                || isOneOfMediaType(response, "text/event-stream")
                                || isOneOfType(eventDataType)) {
                            operation.vendorExtensions.put(X_CODEGEN_STREAM_IS_ONE_OF, true);
                            operation.vendorExtensions.put(
                                    "x-codegen-sse-event-data-is-oneof", true);
                        }
                        if (eventDataType != null && !eventDataType.isEmpty()
                                && !eventDataType.startsWith("std::")
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
                } else if (isConditionalSse && response.is2xx
                        && response.dataType != null
                        && !response.dataType.equals(operation.returnType)) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (!useJsonEventData) {
                        response.vendorExtensions.put("x-codegen-sse-representation-mode", true);
                    }
                    String streamElementType = eventTypeOverride != null
                            ? eventTypeOverride : stripSharedPtr(response.dataType);
                    response.vendorExtensions.put("x-codegen-stream-element-type",
                            streamElementType);
                    if (useJsonEventData) {
                        response.vendorExtensions.put("x-codegen-sse-event-data-type",
                                streamElementType);
                    }
                }
            }
            // Conditional dual-content operations get a dedicated stream method
            // when a concrete event type can be resolved.
            if (isConditionalSse) {
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
                if (eventTypeOverride != null) {
                    sseReturnType = eventTypeOverride;
                    sseBaseModelName = eventTypeOverride;
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
     *   x-codegen-response-union: the generated union struct name
     *   x-codegen-response-union-members: filtered variant-member rows with
     *     a terminal marker for comma rendering
     * Sets on each response used in the union:
     *   x-codegen-response-union: the union struct name (same as operation-level)
     *   x-codegen-response-union-body-type: the variant alternative body type
     *     (e.g., {@code std::shared_ptr<FullResource>} or {@code std::monostate}).
     *     Duplicate C++ body types are wrapped in
     *     {@code StatusTaggedValue<boost::beast::http::status(N), T>}.
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

        List<Map<String, Object>> unionMembers = new ArrayList<>();
        for (CodegenResponse response : unionEligible) {
            Map<String, Object> member = new LinkedHashMap<>();
            member.put("bodyType", response.vendorExtensions.get(
                    X_CODEGEN_RESPONSE_UNION_BODY_TYPE));
            unionMembers.add(member);
        }
        unionMembers.get(unionMembers.size() - 1).put("last", true);
        operation.vendorExtensions.put(X_CODEGEN_RESPONSE_UNION_MEMBERS, unionMembers);

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
