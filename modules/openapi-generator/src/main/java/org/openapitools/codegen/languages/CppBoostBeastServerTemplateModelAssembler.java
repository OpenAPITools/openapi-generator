/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * Template-model assembly for the Boost.Beast server generator: converts each
 * {@link CodegenOperation} into the vendor-extension facts consumed by the
 * server api-header/api-source templates (route table, typed request structs,
 * responder methods, security groups, parameter validation constraints).
 */
final class CppBoostBeastServerTemplateModelAssembler {

    private final org.slf4j.Logger LOGGER =
            org.slf4j.LoggerFactory.getLogger(
                    CppBoostBeastServerTemplateModelAssembler.class);

    private final OpenAPI sourceOpenApi;
    private final String modelNamespace;
    private final Function<String, String> modelImportFunction;

    CppBoostBeastServerTemplateModelAssembler(
            OpenAPI sourceOpenApi, String modelNamespace,
            Function<String, String> modelImportFunction) {
        this.sourceOpenApi = sourceOpenApi;
        this.modelNamespace = modelNamespace;
        this.modelImportFunction = modelImportFunction;
    }

    OperationsMap assemble(OperationsMap objs, List<ModelMap> allModels) {
        if (objs == null || objs.getOperations() == null) {
            return objs;
        }
        Set<String> modelClassNames = new HashSet<>();
        Map<String, String> modelDataTypes = new HashMap<>();
        for (ModelMap modelMap : allModels) {
            if (modelMap != null && modelMap.getModel() != null
                    && modelMap.getModel().classname != null) {
                modelClassNames.add(modelMap.getModel().classname);
                modelDataTypes.put(modelMap.getModel().classname,
                        modelMap.getModel().dataType);
            }
        }
        Set<String> emittedModelImports = new HashSet<>();
        for (Map<String, String> existing : objs.getImports()) {
            if (existing != null && existing.get("classname") != null) {
                emittedModelImports.add(existing.get("classname"));
            }
        }
        for (CodegenOperation op : objs.getOperations().getOperation()) {
            if (op == null) {
                continue;
            }
            Operation raw = CppBoostBeastOperationFacts.operationFor(sourceOpenApi, op);

            String pascal = pascalCase(
                    op.operationId != null ? op.operationId : op.operationIdLowerCase);
            op.vendorExtensions.put("x-server-operation-pascal", pascal);

            op.vendorExtensions.put("x-server-params",
                    serverParams(op, raw));

            Map<String, Object> body =
                    requestBodyFacts(op, raw, pascal, modelClassNames, modelDataTypes);
            op.vendorExtensions.put("x-server-has-request-body", body.get("hasBody"));
            op.vendorExtensions.put("x-server-request-model", body.get("model"));
            op.vendorExtensions.put("x-server-request-model-collides",
                    body.get("modelCollides"));
            op.vendorExtensions.put("x-server-request-media-types",
                    body.get("mediaTypes"));
            // Mixed bodies (multipart + JSON): the JSON member's model is not in
            // DefaultCodegen's op.imports (it flattened the form payload instead),
            // so the recovered typed field would reference an un-included header.
            // Append its include to the frozen operations import list.
            String bodyModel = (String) body.get("model");
            if (Boolean.TRUE.equals(body.get("hasBody")) && bodyModel != null
                    && modelClassNames.contains(bodyModel)
                    && emittedModelImports.add(bodyModel)) {
                Map<String, String> im = new LinkedHashMap<>();
                im.put("import", modelImportFunction.apply(bodyModel));
                im.put("classname", bodyModel);
                objs.getImports().add(im);
            }

            op.vendorExtensions.put("x-server-responses", serverResponses(op, pascal));

            op.vendorExtensions.put("x-server-security-groups",
                    CppBoostBeastOperationFacts.effectiveSecurityGroups(sourceOpenApi, op));
        }
        boolean anyModelUse = false;
        for (CodegenOperation op : objs.getOperations().getOperation()) {
            if (op == null) {
                continue;
            }
            if (op.imports != null) {
                for (String imported : op.imports) {
                    if (imported != null && modelClassNames.contains(imported)) {
                        anyModelUse = true;
                        break;
                    }
                }
            }
            // A recovered mixed-body model (JSON member of a multipart body) is
            // not in op.imports; its unqualified field still needs the model
            // using-directive unless it was namespace-qualified for a collision.
            if (Boolean.TRUE.equals(op.vendorExtensions.get("x-server-has-request-body"))
                    && !Boolean.TRUE.equals(
                            op.vendorExtensions.get("x-server-request-model-collides"))) {
                String model = (String) op.vendorExtensions.get("x-server-request-model");
                if (model != null && modelClassNames.contains(model)) {
                    anyModelUse = true;
                }
            }
            if (anyModelUse) {
                break;
            }
        }
        objs.put("x-server-has-model-use", anyModelUse);
        return objs;
    }

    // ------------------------------------------------------------------
    // Parameters
    // ------------------------------------------------------------------

    private List<Map<String, Object>> serverParams(CodegenOperation op, Operation raw) {
        List<Map<String, Object>> params = new ArrayList<>();
        for (CodegenParameter param : op.allParams) {
            if (param == null || param.isBodyParam) {
                continue;
            }
            String in = param.isPathParam ? "path"
                    : param.isQueryParam ? "query"
                    : param.isHeaderParam ? "header"
                    : param.isCookieParam ? "cookie" : "body";
            Object style = param.vendorExtensions.get("x-codegen-param-style");
            String styleText = style == null ? "" : style.toString();
            if (styleText.isEmpty()) {
                styleText = "query".equals(in) || "cookie".equals(in)
                        ? "form" : "simple";
            }
            String dataType = param.dataType == null ? "" : param.dataType;
            if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
                dataType = dataType.substring(
                        "std::shared_ptr<".length(), dataType.length() - 1).trim();
            }
            boolean isContainer = Boolean.TRUE.equals(param.isContainer)
                    || Boolean.TRUE.equals(param.isArray);

            Parameter rawParam = findRawParameter(op, raw, param.baseName, in);
            Schema<?> rawSchema = rawParam == null || rawParam.getSchema() == null
                    ? null
                    : ModelUtils.getReferencedSchema(sourceOpenApi, rawParam.getSchema());

            String issue = parameterDecodeIssue(
                    param, in, styleText, dataType, isContainer, rawSchema);
            if (issue != null) {
                LOGGER.warn("cpp-boost-beast-server: operation '{}' parameter"
                                + " '{}' (in {}) {}; it is dropped from the"
                                + " generated handler and will not reach the"
                                + " service code",
                        op.operationId, param.baseName, in, issue);
                continue;
            }

            Map<String, Object> facts = new LinkedHashMap<>();
            facts.put("cppName", param.paramName != null ? param.paramName : param.baseName);
            facts.put("baseName", param.baseName);
            facts.put("in", in);
            facts.put("isPath", "path".equals(in));
            facts.put("isQuery", "query".equals(in));
            facts.put("isHeader", "header".equals(in));
            facts.put("isCookie", "cookie".equals(in));
            Object explode = param.vendorExtensions.get("x-codegen-param-explode");
            boolean explodeFlag = Boolean.TRUE.equals(explode);
            if (explode == null) {
                explodeFlag = "form".equals(styleText);
            }
            facts.put("style", styleText);
            facts.put("styleSimple", "simple".equals(styleText));
            facts.put("styleLabel", "label".equals(styleText));
            facts.put("styleMatrix", "matrix".equals(styleText));
            facts.put("styleForm", "form".equals(styleText));
            facts.put("styleSpaceDelimited", "spaceDelimited".equals(styleText));
            facts.put("stylePipeDelimited", "pipeDelimited".equals(styleText));
            facts.put("styleDeepObject", "deepObject".equals(styleText));
            facts.put("explode", explodeFlag);
            facts.put("required", param.required);
            facts.put("isContainer", isContainer);
            facts.put("dataType", dataType);
            facts.put("innerType", innerTemplateArg(dataType));
            facts.put("stringKind", "std::string".equals(dataType));
            facts.put("integerKind", "std::int32_t".equals(dataType)
                    || "std::int64_t".equals(dataType));
            facts.put("numberKind", "float".equals(dataType)
                    || "double".equals(dataType));
            facts.put("boolKind", "bool".equals(dataType));
            applySchemaConstraints(facts, rawSchema);

            params.add(facts);
        }
        return params;
    }

    /**
     * Single owner of parameter degradation: returns a short reason (grammar:
     * "<verb phrase>") why the codecs cannot faithfully decode this parameter,
     * or null when they can. Rules mirror the generated decode paths exactly:
     * content-style and flattened form fields never decode; scalars need a
     * plain-scalar dataType (parseScalar has six overloads and no optional or
     * nullable wrappers); containers need vector-of-plain-scalar, or — with
     * style=deepObject — map<string,string>; cookie containers have no codec;
     * styles outside the per-location allow-list have no serializer; mixed
     * enums have no renderable allow-list.
     */
    private static String parameterDecodeIssue(
            CodegenParameter param, String in, String styleText,
            String dataType, boolean isContainer, Schema<?> schema) {
        if (param.getContent() != null) {
            return "uses content-style serialization, which the JSON runtime"
                    + " cannot decode from a raw string";
        }
        if ("body".equals(in)) {
            return "is a form-field parameter; the runtime decodes JSON"
                    + " bodies, not form-encoded fields";
        }
        if (dataType.isEmpty()) {
            return "does not map to a generated C++ type";
        }
        String enumIssue = CppBoostBeastServerCodegen.heterogeneousEnumIssue(schema);
        if (enumIssue != null) {
            return enumIssue;
        }
        if (!CppBoostBeastServerCodegen.isStyleAllowedForLocation(in, styleText)) {
            return "uses style '" + styleText + "' for in='" + in
                    + "', which the runtime does not serialize";
        }
        if (isContainer) {
            if ("cookie".equals(in)) {
                return "is an array cookie parameter; the cookie codec decodes"
                        + " scalars only";
            }
            if (dataType.startsWith("std::map<")) {
                if (!"deepObject".equals(styleText)
                        || !"std::string".equals(innerTemplateArg(dataType))) {
                    return "is a map-typed parameter; only query deepObject"
                            + " string maps are decoded";
                }
            } else if (dataType.startsWith("std::vector<")) {
                if (!CppBoostBeastServerCodegen.isPlainScalarDataType(
                        innerTemplateArg(dataType))) {
                    return "is an array with non-scalar items; the query codec"
                            + " splits plain scalar values only";
                }
            } else {
                return "is a container the codecs cannot build from dataType"
                        + " '" + dataType + "'";
            }
        } else if (!CppBoostBeastServerCodegen.isPlainScalarDataType(dataType)) {
            return "has dataType '" + dataType + "', which no parseScalar"
                    + " overload accepts";
        }
        return null;
    }

    private Parameter findRawParameter(
            CodegenOperation op, Operation raw, String baseName, String in) {
        if (raw != null && raw.getParameters() != null) {
            for (Parameter candidate : raw.getParameters()) {
                if (candidate == null) {
                    continue;
                }
                // $ref parameters carry null name/in in the raw document;
                // resolve to the target before comparing.
                Parameter resolved =
                        ModelUtils.getReferencedParameter(sourceOpenApi, candidate);
                Parameter effective = resolved != null ? resolved : candidate;
                if (baseName.equals(effective.getName())
                        && (effective.getIn() == null || effective.getIn().equals(in))) {
                    return effective;
                }
            }
        }
        return null;
    }

    private void applySchemaConstraints(
            Map<String, Object> facts, io.swagger.v3.oas.models.media.Schema schema) {
        List<String> enumValues = new ArrayList<>();
        String enumKind = "";
        if (schema != null && schema.getEnum() != null && !schema.getEnum().isEmpty()) {
            for (Object value : schema.getEnum()) {
                if (value instanceof Boolean) {
                    enumValues.add(value.toString());
                    if (enumKind.isEmpty()) {
                        enumKind = "bool";
                    }
                } else if (value instanceof Integer || value instanceof Long
                        || value instanceof Short || value instanceof Byte) {
                    enumValues.add(value.toString());
                    if (enumKind.isEmpty() || "bool".equals(enumKind)) {
                        enumKind = "integer";
                    }
                } else if (value instanceof Double || value instanceof Float
                        || value instanceof java.math.BigDecimal) {
                    enumValues.add(value.toString());
                    if (enumKind.isEmpty() || "bool".equals(enumKind)) {
                        enumKind = "number";
                    }
                } else {
                    enumValues.add("\""
                            + CppBoostBeastModelCodegen.escapeCppStringContent(
                                    value == null ? "" : value.toString())
                            + "\"");
                    if (enumKind.isEmpty()) {
                        enumKind = "string";
                    }
                }
            }
        }
        facts.put("enumValues", enumValues);
        facts.put("enumKind", enumKind);
        facts.put("hasEnum", !enumValues.isEmpty());
        String pattern = schema != null && schema.getPattern() != null
                ? CppBoostBeastModelCodegen.escapeCppStringContent(schema.getPattern()) : "";
        facts.put("pattern", pattern);
        facts.put("hasPattern", !pattern.isEmpty());
        String minimum = schema != null && schema.getMinimum() != null
                ? toLongDoubleLiteral(schema.getMinimum()) : "";
        facts.put("minimum", minimum);
        facts.put("hasMinimum", !minimum.isEmpty());
        String maximum = schema != null && schema.getMaximum() != null
                ? toLongDoubleLiteral(schema.getMaximum()) : "";
        facts.put("maximum", maximum);
        facts.put("hasMaximum", !maximum.isEmpty());
        String minLength = schema != null && schema.getMinLength() != null
                ? schema.getMinLength().toString() : "";
        facts.put("minLength", minLength);
        facts.put("hasMinLength", !minLength.isEmpty());
        String maxLength = schema != null && schema.getMaxLength() != null
                ? schema.getMaxLength().toString() : "";
        facts.put("maxLength", maxLength);
        facts.put("hasMaxLength", !maxLength.isEmpty());
    }

    // ------------------------------------------------------------------
    // Request body
    // ------------------------------------------------------------------

    private Map<String, Object> requestBodyFacts(
            CodegenOperation op, Operation raw, String pascal,
            Set<String> modelClassNames, Map<String, String> modelDataTypes) {
        Map<String, Object> facts = new LinkedHashMap<>();
        List<String> mediaTypes = new ArrayList<>();
        RequestBody body = raw != null
                ? ModelUtils.getReferencedRequestBody(sourceOpenApi, raw.getRequestBody())
                : null;
        String jsonModelRef = null;
        if (body != null && body.getContent() != null) {
            for (String mediaType : body.getContent().keySet()) {
                // Degrade policy: the runtime parses request bodies as JSON
                // only. Non-JSON declarations (XML, form, multipart) are
                // dropped here and reported as warnings at preprocess time;
                // an operation left with no JSON media type loses its typed
                // body entirely rather than promising bytes it cannot parse.
                if (!CppBoostBeastServerCodegen.isSupportedMediaType(mediaType)) {
                    continue;
                }
                if (jsonModelRef == null && body.getContent().get(mediaType) != null) {
                    jsonModelRef = body.getContent().get(mediaType)
                            .getSchema() == null
                            ? "" : body.getContent().get(mediaType).getSchema().get$ref();
                }
                String normalized = mediaType == null ? "" : mediaType.trim();
                int semicolon = normalized.indexOf(';');
                if (semicolon >= 0) {
                    normalized = normalized.substring(0, semicolon).trim();
                }
                normalized = normalized.toLowerCase(java.util.Locale.ROOT);
                if (!normalized.isEmpty() && !mediaTypes.contains(normalized)) {
                    mediaTypes.add(normalized);
                }
            }
        }
        String rendered = "";
        for (String mediaType : mediaTypes) {
            if (!rendered.isEmpty()) {
                rendered += ", ";
            }
            rendered += "\"" + CppBoostBeastModelCodegen.escapeCppStringContent(mediaType)
                    + "\"";
        }

        String dataType = op.bodyParam != null && op.bodyParam.dataType != null
                ? op.bodyParam.dataType : "";
        if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
            dataType = dataType.substring(
                    "std::shared_ptr<".length(), dataType.length() - 1).trim();
        }
        if (dataType.isEmpty() && jsonModelRef != null) {
            // Mixed bodies (e.g. multipart + JSON): DefaultCodegen flattens the
            // form payload into parameters and leaves no body model, yet the
            // JSON member still declares a schema. When that member names a
            // generated model, the handler can type the JSON body exactly;
            // anything else (inline objects, unions) degrades to no body.
            String simple = ModelUtils.getSimpleRef(jsonModelRef);
            dataType = simple != null && modelClassNames.contains(simple) ? simple : "";
        }
        if (isUntypableBodyDataType(dataType, modelDataTypes)) {
            // Composition unions arrive as std::variant, either directly or
            // through a model that is an alias to one. Deserializing JSON into
            // the right branch needs the schema-driven matcher, which the
            // request path does not run, and fromJsonLeaf only reaches types
            // that expose a member fromJsonValue (generated classes). Degrade
            // rather than emit a call that cannot compile.
            dataType = "";
        }
        boolean hasBody = !mediaTypes.isEmpty() && !dataType.isEmpty();
        if (!mediaTypes.isEmpty() && dataType.isEmpty()) {
            LOGGER.warn("cpp-boost-beast-server: operation '{}' declares a JSON"
                            + " request body whose schema the runtime cannot type"
                            + "; the handler receives no typed body",
                    op.operationId);
        }
        facts.put("hasBody", hasBody);
        facts.put("mediaTypes", hasBody ? rendered : "");
        facts.put("model", dataType);
        facts.put("modelCollides", hasBody && dataType.equals(pascal + "Request"));
        return facts;
    }

    /**
     * A request body is typable only when {@code fromJsonLeaf} has an overload
     * for its C++ type: a generated class (member fromJsonValue), a concrete
     * scalar, boost::json::value, or a shared_ptr/vector/map thereof. Union
     * bodies (std::variant) and optionals have no such overload, and neither
     * does a model that is merely an alias to one, so those degrade to no body.
     */
    private static boolean isUntypableBodyDataType(
            String dataType, Map<String, String> modelDataTypes) {
        String current = dataType;
        // A body model may alias through a couple of names before reaching the
        // concrete variant; bound the walk so a pathological cycle can't hang.
        for (int hop = 0; hop < 4 && current != null && !current.isEmpty(); hop++) {
            if (current.startsWith("std::variant<") || current.startsWith("std::optional<")) {
                return true;
            }
            String resolved = modelDataTypes.get(current);
            if (resolved == null || resolved.equals(current)) {
                break;
            }
            current = resolved;
        }
        return false;
    }


    // ------------------------------------------------------------------
    // Responses
    // ------------------------------------------------------------------

    private List<Map<String, Object>> serverResponses(
            CodegenOperation op, String pascal) {
        List<Map<String, Object>> responses = new ArrayList<>();
        if (op.responses == null) {
            return responses;
        }
        for (CodegenResponse response : op.responses) {
            if (response == null) {
                continue;
            }
            Map<String, Object> facts = new LinkedHashMap<>();
            boolean isDefault = Boolean.TRUE.equals(response.isDefault)
                    || (response.code != null && "default".equals(response.code));
            String code = response.code == null ? "default" : response.code;
            facts.put("code", code);
            facts.put("isDefault", isDefault);
            facts.put("sendMethod", isDefault
                    ? "sendDefault" : "send" + sanitizeCode(code));
            String dataType = response.dataType == null ? "" : response.dataType;
            if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
                dataType = dataType.substring(
                        "std::shared_ptr<".length(), dataType.length() - 1).trim();
            }
            facts.put("hasModel", !dataType.isEmpty());
            // A model class named exactly like this operation's generated
            // Request/Responder type would shadow it as an injected-class-name
            // inside its own declaration; qualify that reference with the
            // model namespace so the parameter type resolves to the model.
            boolean collides = dataType.equals(pascal + "Responder")
                    || dataType.equals(pascal + "Request");
            facts.put("sendType",
                    collides && !modelNamespace.isEmpty()
                            ? modelNamespace + "::" + dataType : dataType);
            responses.add(facts);
        }
        return responses;
    }


    /** Inner template argument for containers ("" for scalars). */
    static String innerTemplateArg(String dataType) {
        if (dataType == null) {
            return "";
        }
        if (dataType.startsWith("std::vector<") && dataType.endsWith(">")) {
            return dataType.substring("std::vector<".length(), dataType.length() - 1).trim();
        }
        if (dataType.startsWith("std::map<") && dataType.endsWith(">")) {
            String args = dataType.substring("std::map<".length(), dataType.length() - 1);
            int depth = 0;
            for (int i = 0; i < args.length(); i++) {
                char c = args.charAt(i);
                if (c == '<') {
                    depth++;
                } else if (c == '>') {
                    depth--;
                } else if (c == ',' && depth == 0) {
                    return args.substring(i + 1).trim();
                }
            }
        }
        return "";
    }

    /** Renders a numeric bound so the template's `{{minimum}}L` composes a
     *  valid long double literal: plain integers get a `.0` fraction, since
     *  `-9223372036854775808L` is not a literal (unary minus overflows the
     *  positive form) while `-9223372036854775808.0L` is well-formed. */
    private static String toLongDoubleLiteral(java.math.BigDecimal value) {
        String text = value.toString();
        return text.indexOf('.') < 0 && text.indexOf('e') < 0 && text.indexOf('E') < 0
                ? text + ".0" : text;
    }

    private static String sanitizeCode(String code) {
        StringBuilder out = new StringBuilder();
        for (char c : code.toCharArray()) {
            out.append(Character.isDigit(c) ? c : '_');
        }
        return out.toString();
    }

    private static String pascalCase(String operationId) {
        if (operationId == null || operationId.isEmpty()) {
            return "Operation";
        }
        StringBuilder out = new StringBuilder();
        boolean upperNext = true;
        for (char c : operationId.toCharArray()) {
            if (c == '_' || c == '-' || c == ' ' || c == '.') {
                upperNext = true;
            } else if (upperNext) {
                out.append(Character.toUpperCase(c));
                upperNext = false;
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }
}
