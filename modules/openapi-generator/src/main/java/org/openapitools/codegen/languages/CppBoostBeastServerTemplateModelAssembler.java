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
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Template-model assembly for the Boost.Beast server generator: converts each
 * {@link CodegenOperation} into the vendor-extension facts consumed by the
 * server api-header/api-source templates (route table, typed request structs,
 * responder methods, security groups, parameter validation constraints).
 */
final class CppBoostBeastServerTemplateModelAssembler {

    private final OpenAPI sourceOpenApi;

    CppBoostBeastServerTemplateModelAssembler(OpenAPI sourceOpenApi) {
        this.sourceOpenApi = sourceOpenApi;
    }

    OperationsMap assemble(OperationsMap objs, List<ModelMap> allModels) {
        if (objs == null || objs.getOperations() == null) {
            return objs;
        }
        for (CodegenOperation op : objs.getOperations().getOperation()) {
            if (op == null) {
                continue;
            }
            Operation raw = CppBoostBeastOperationFacts.operationFor(sourceOpenApi, op);

            op.vendorExtensions.put("x-server-operation-pascal",
                    pascalCase(op.operationId != null ? op.operationId : op.operationIdLowerCase));

            op.vendorExtensions.put("x-server-params",
                    serverParams(op, raw));

            Map<String, Object> body = requestBodyFacts(op, raw);
            op.vendorExtensions.put("x-server-has-request-body", body.get("hasBody"));
            op.vendorExtensions.put("x-server-request-model", body.get("model"));
            op.vendorExtensions.put("x-server-request-media-types",
                    body.get("mediaTypes"));

            op.vendorExtensions.put("x-server-responses", serverResponses(op));

            op.vendorExtensions.put("x-server-security-groups",
                    CppBoostBeastOperationFacts.effectiveSecurityGroups(sourceOpenApi, op));
        }
        Set<String> modelClassNames = new HashSet<>();
        for (ModelMap modelMap : allModels) {
            if (modelMap != null && modelMap.getModel() != null
                    && modelMap.getModel().classname != null) {
                modelClassNames.add(modelMap.getModel().classname);
            }
        }
        boolean anyModelUse = false;
        for (CodegenOperation op : objs.getOperations().getOperation()) {
            if (op == null || op.imports == null) {
                continue;
            }
            for (String imported : op.imports) {
                if (imported != null && modelClassNames.contains(imported)) {
                    anyModelUse = true;
                    break;
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
            Map<String, Object> facts = new LinkedHashMap<>();
            facts.put("cppName", param.paramName != null ? param.paramName : param.baseName);
            facts.put("baseName", param.baseName);
            String in = param.isPathParam ? "path"
                    : param.isQueryParam ? "query"
                    : param.isHeaderParam ? "header"
                    : param.isCookieParam ? "cookie" : "body";
            facts.put("in", in);
            facts.put("isPath", "path".equals(in));
            facts.put("isQuery", "query".equals(in));
            facts.put("isHeader", "header".equals(in));
            facts.put("isCookie", "cookie".equals(in));
            Object style = param.vendorExtensions.get("x-codegen-param-style");
            String styleText = style == null ? "" : style.toString();
            if (styleText.isEmpty()) {
                styleText = "query".equals(in) || "cookie".equals(in)
                        ? "form" : "simple";
            }
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
            facts.put("isContainer", Boolean.TRUE.equals(param.isContainer)
                    || Boolean.TRUE.equals(param.isArray));
            String dataType = param.dataType == null ? "" : param.dataType;
            if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
                dataType = dataType.substring(
                        "std::shared_ptr<".length(), dataType.length() - 1).trim();
            }
            facts.put("dataType", dataType);
            facts.put("innerType", innerTemplateArg(dataType));
            facts.put("defaultValue", param.defaultValue == null ? "" : param.defaultValue);
            facts.put("stringKind", "std::string".equals(dataType));
            facts.put("integerKind", "std::int32_t".equals(dataType)
                    || "std::int64_t".equals(dataType));
            facts.put("numberKind", "float".equals(dataType)
                    || "double".equals(dataType));
            facts.put("boolKind", "bool".equals(dataType));

            Parameter rawParam = findRawParameter(op, raw, param.baseName, in);
            applySchemaConstraints(facts, rawParam == null ? null : rawParam.getSchema());

            params.add(facts);
        }
        return params;
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
                ? schema.getMinimum().toString() : "";
        facts.put("minimum", minimum);
        facts.put("hasMinimum", !minimum.isEmpty());
        String maximum = schema != null && schema.getMaximum() != null
                ? schema.getMaximum().toString() : "";
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

    private Map<String, Object> requestBodyFacts(CodegenOperation op, Operation raw) {
        Map<String, Object> facts = new LinkedHashMap<>();
        List<String> mediaTypes = new ArrayList<>();
        RequestBody body = raw != null
                ? ModelUtils.getReferencedRequestBody(sourceOpenApi, raw.getRequestBody())
                : null;
        if (body != null && body.getContent() != null) {
            for (String mediaType : body.getContent().keySet()) {
                // The runtime strips parameters and lowercases the received
                // Content-Type before comparing; emit the same normalized
                // form so a declared "application/json; charset=utf-8" key
                // still matches plain "application/json" requests.
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
        facts.put("hasBody", !mediaTypes.isEmpty());
        facts.put("mediaTypes", rendered);

        String dataType = op.bodyParam != null && op.bodyParam.dataType != null
                ? op.bodyParam.dataType : "";
        if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
            dataType = dataType.substring(
                    "std::shared_ptr<".length(), dataType.length() - 1).trim();
        }
        facts.put("model", dataType);
        return facts;
    }

    // ------------------------------------------------------------------
    // Responses
    // ------------------------------------------------------------------

    private List<Map<String, Object>> serverResponses(CodegenOperation op) {
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
            facts.put("cppType", dataType);
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
