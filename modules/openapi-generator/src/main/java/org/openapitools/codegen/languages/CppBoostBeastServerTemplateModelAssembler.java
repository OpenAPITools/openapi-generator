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
import io.swagger.v3.oas.models.responses.ApiResponse;
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

    /** Types the generated runtime declares in the API namespace. A model
     *  with one of these names would shadow the runtime type wherever the
     *  generated code references it unqualified, so those references get an
     *  explicit api-namespace prefix and the model references get a
     *  model-namespace prefix. */
    static final Set<String> RUNTIME_TYPE_NAMES = Set.of(
            "HttpServer", "Problem", "ProblemError", "ResponderCore", "Responder",
            "RequestContext", "Router", "RouteMatch", "Handler", "SecurityGroups",
            "SchemeRequirement", "Authorizer", "AuthCredentials", "ServerOptions",
            "ParamCodecs");
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
        // The API classes live in the same namespace as the runtime types
        // (Router/Problem/HttpServer/...). The api header emits
        // `using namespace <model>;`, so a generated MODEL named like a
        // runtime type makes every unqualified mention of that name
        // ambiguous. Fix both sides explicitly: templates prefix runtime
        // references with `{{apiNsQualified}}` (the api namespace), and model
        // references (field/body/response types) are rewritten to their
        // model-namespace spelling here. With no collision the prefix is
        // empty and generated code stays readable.
        Set<String> collidingModels = new HashSet<>(modelClassNames);
        collidingModels.retainAll(RUNTIME_TYPE_NAMES);
        Object apiNamespaceValue = objs.get("apiNamespace");
        String apiNamespace = apiNamespaceValue == null ? "" : apiNamespaceValue.toString();
        objs.put("apiNsQualified",
                !collidingModels.isEmpty() && !apiNamespace.isEmpty()
                        ? apiNamespace + "::" : "");
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

            // Contract types are declared INSIDE the API class (see
            // api-header.mustache), so the same operation shared across two
            // tags yields two independently-scoped definitions instead of
            // duplicate namespace-scope types.
            String pascal = pascalCase(
                    op.operationId != null ? op.operationId : op.operationIdLowerCase);
            op.vendorExtensions.put("x-server-operation-pascal", pascal);

            op.vendorExtensions.put("x-server-params",
                    serverParams(op, raw));

            Map<String, Object> body = requestBodyFacts(
                    op, raw, pascal, modelClassNames, modelDataTypes, collidingModels);
            op.vendorExtensions.put("x-server-has-request-body", body.get("hasBody"));
            op.vendorExtensions.put("x-server-request-model", body.get("model"));
            op.vendorExtensions.put("x-server-request-model-collides",
                    body.get("modelCollides"));
            op.vendorExtensions.put("x-server-request-media-types",
                    body.get("mediaTypes"));
            op.vendorExtensions.put("x-server-request-body-required",
                    body.get("required"));
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

            op.vendorExtensions.put("x-server-responses",
                    serverResponses(op, raw, pascal, collidingModels));

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
            if (issue == null && "path".equals(in)
                    && ("label".equals(styleText) || "matrix".equals(styleText))
                    && !CppBoostBeastServerCodegen.isWholeSegmentPathParameter(
                            op.path, param.baseName)) {
                // Label/matrix serialization prefixes the WHOLE segment with
                // '.' or ';name='; when the template also has literal text in
                // that segment, the prefix cannot be both at segment start and
                // after the literal. The capture is the expression text only,
                // indistinguishable from a simple-style value: drop it.
                issue = "uses style=" + styleText + " inside a segment that also"
                        + " has literal text; the segment prefix cannot decode";
            }
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
            facts.put("mapKind", dataType.startsWith("std::map<"));
            facts.put("vectorKind", dataType.startsWith("std::vector<"));
            // A plain-scalar default renders as a member initializer so an
            // absent optional parameter reaches the service as the declared
            // default, not the zero value. Containers/complex defaults stay
            // brace-initialized (the codecs write them only when present).
            String defaultValue = param.defaultValue;
            boolean plainScalarDefault = !isContainer
                    && (CppBoostBeastServerCodegen.isPlainScalarDataType(dataType)
                        || "std::string".equals(dataType))
                    && defaultValue != null && !defaultValue.isEmpty()
                    && !defaultValue.startsWith("std::");
            facts.put("hasDefaultInit", plainScalarDefault);
            facts.put("defaultInit", plainScalarDefault ? defaultValue : "");
            Schema<?> itemSchema = isContainer && rawSchema != null
                    ? ModelUtils.getReferencedSchema(sourceOpenApi, rawSchema.getItems())
                    : null;
            applySchemaConstraints(facts, rawSchema, itemSchema, dataType);
            // The path-scalar branch declares `present` only when the shared
            // constraint ladder actually reads it (a POD bool with no uses
            // would trip -Wunused-variable under the generated -Werror).
            facts.put("hasScalarConstraints",
                    Boolean.TRUE.equals(facts.get("hasEnum"))
                            || Boolean.TRUE.equals(facts.get("hasPattern"))
                            || Boolean.TRUE.equals(facts.get("hasMinLength"))
                            || Boolean.TRUE.equals(facts.get("hasMaxLength"))
                            || Boolean.TRUE.equals(facts.get("hasMinimum"))
                            || Boolean.TRUE.equals(facts.get("hasMaximum"))
                            || Boolean.TRUE.equals(facts.get("minimumAlwaysInvalid"))
                            || Boolean.TRUE.equals(facts.get("maximumAlwaysInvalid")));

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

    /** Applies scalar validation facts for a parameter schema (prefix ""),
     *  its item schema (prefix "item"), and the collection-level bounds.
     *  Bounds are resolved through ModelUtils so OAS 3.0 boolean
     *  exclusiveMinimum/Maximum and OAS 3.1 numeric forms agree. For integer
     *  kinds the comparison runs in the parameter's exact integer type: the
     *  bound is folded to the nearest violated integer (exclusive) or the
     *  ceiling/floor (fractional inclusive), so no value above 2^53 can be
     *  smuggled through a lossy long double conversion. */
    private void applySchemaConstraints(
            Map<String, Object> facts,
            io.swagger.v3.oas.models.media.Schema schema,
            io.swagger.v3.oas.models.media.Schema itemSchema,
            String dataType) {
        applyScalarConstraints(facts, schema, dataType, "");
        String innerType = innerTemplateArg(dataType);
        if (itemSchema != null && !innerType.isEmpty()) {
            applyScalarConstraints(facts, itemSchema, innerType, "item");
        }
        String minItems = schema != null && schema.getMinItems() != null
                ? schema.getMinItems().toString() : "";
        facts.put("minItems", minItems);
        facts.put("hasMinItems", !minItems.isEmpty());
        String maxItems = schema != null && schema.getMaxItems() != null
                ? schema.getMaxItems().toString() : "";
        facts.put("maxItems", maxItems);
        facts.put("hasMaxItems", !maxItems.isEmpty());
        facts.put("uniqueItems", schema != null
                && Boolean.TRUE.equals(schema.getUniqueItems()));
    }

    private void applyScalarConstraints(
            Map<String, Object> facts,
            io.swagger.v3.oas.models.media.Schema schema,
            String dataType,
            String prefix) {
        boolean integerKind = "std::int32_t".equals(dataType)
                || "std::int64_t".equals(dataType);
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
                    enumValues.add(integerKind ? longLongLiteral(value.toString()) : value.toString());
                    if (enumKind.isEmpty() || "bool".equals(enumKind)) {
                        enumKind = "integer";
                    }
                } else if (value instanceof Double || value instanceof Float
                        || value instanceof java.math.BigDecimal) {
                    java.math.BigDecimal decimal = new java.math.BigDecimal(value.toString());
                    if (integerKind) {
                        // An integer parameter can only match an integer-valued
                        // member; fractional members are unreachable values,
                        // and members outside the long long range can never
                        // arrive through parseScalar. Keep the list exact.
                        java.math.BigDecimal integral = decimal.setScale(0, java.math.RoundingMode.DOWN);
                        if (integral.compareTo(decimal) == 0
                                && integral.compareTo(INT64_MIN) >= 0
                                && integral.compareTo(INT64_MAX) <= 0) {
                            enumValues.add(longLongLiteral(integral.toPlainString()));
                        }
                        if (enumKind.isEmpty()) {
                            enumKind = "integer";
                        }
                        continue;
                    }
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
        boolean stringKind = "std::string".equals(dataType);
        boolean boolKind = "bool".equals(dataType);
        boolean numberKind = "float".equals(dataType) || "double".equals(dataType);
        emit(facts, prefix, "stringKind", stringKind);
        emit(facts, prefix, "boolKind", boolKind);
        emit(facts, prefix, "integerKind", integerKind);
        emit(facts, prefix, "numberKind", numberKind);
        emit(facts, prefix, "enumValues", enumValues);
        emit(facts, prefix, "enumKind", enumKind);
        emit(facts, prefix, "hasEnum", !enumValues.isEmpty());
        String pattern = schema != null && schema.getPattern() != null
                ? CppBoostBeastModelCodegen.escapeCppStringContent(schema.getPattern()) : "";
        emit(facts, prefix, "pattern", pattern);
        emit(facts, prefix, "hasPattern", !pattern.isEmpty());
        String minLength = schema != null && schema.getMinLength() != null
                ? schema.getMinLength().toString() : "";
        emit(facts, prefix, "minLength", minLength);
        emit(facts, prefix, "hasMinLength", !minLength.isEmpty());
        String maxLength = schema != null && schema.getMaxLength() != null
                ? schema.getMaxLength().toString() : "";
        emit(facts, prefix, "maxLength", maxLength);
        emit(facts, prefix, "hasMaxLength", !maxLength.isEmpty());

        ModelUtils.ResolvedMinBound min = schema == null ? null
                : ModelUtils.resolveMinimumBound(sourceOpenApi, schema);
        ModelUtils.ResolvedMaxBound max = schema == null ? null
                : ModelUtils.resolveMaximumBound(sourceOpenApi, schema);
        if (integerKind) {
            // Fold to an exclusive-integer threshold: reject x < t. The fold
            // clamps at the PARAMETER's own range (parseScalar already rejects
            // values outside it), so the generated comparison can never be
            // provably constant (which -Wtype-limits would flag under -Werror).
            java.math.BigDecimal typeMin = "std::int64_t".equals(dataType)
                    ? INT64_MIN : INT32_MIN;
            java.math.BigDecimal typeMax = "std::int64_t".equals(dataType)
                    ? INT64_MAX : INT32_MAX;
            java.math.BigDecimal minThreshold = null;
            boolean minAlways = false;
            if (min != null) {
                minThreshold = min.exclusive
                        ? min.minBound.add(java.math.BigDecimal.ONE)
                                .setScale(0, java.math.RoundingMode.FLOOR)
                        : min.minBound.setScale(0, java.math.RoundingMode.CEILING);
                if (minThreshold.compareTo(typeMax) > 0) {
                    minAlways = true;   // no representable value satisfies it
                    minThreshold = null;
                } else if (minThreshold.compareTo(typeMin) <= 0) {
                    minThreshold = null;   // every value satisfies it
                }
            }
            java.math.BigDecimal maxThreshold = null;   // reject x > t
            boolean maxAlways = false;
            if (max != null) {
                maxThreshold = max.exclusive
                        ? max.maxBound.subtract(java.math.BigDecimal.ONE)
                                .setScale(0, java.math.RoundingMode.CEILING)
                        : max.maxBound.setScale(0, java.math.RoundingMode.FLOOR);
                if (maxThreshold.compareTo(typeMin) < 0) {
                    maxAlways = true;
                    maxThreshold = null;
                } else if (maxThreshold.compareTo(typeMax) >= 0) {
                    maxThreshold = null;
                }
            }
            emit(facts, prefix, "minimum",
                    minThreshold == null ? "" : longLongLiteral(minThreshold.toPlainString()));
            emit(facts, prefix, "hasMinimum", minThreshold != null);
            emit(facts, prefix, "maximum",
                    maxThreshold == null ? "" : longLongLiteral(maxThreshold.toPlainString()));
            emit(facts, prefix, "hasMaximum", maxThreshold != null);
            emit(facts, prefix, "minimumAlwaysInvalid", minAlways);
            emit(facts, prefix, "maximumAlwaysInvalid", maxAlways);
        } else {
            emit(facts, prefix, "minimum",
                    min == null ? "" : toLongDoubleLiteral(min.minBound));
            emit(facts, prefix, "hasMinimum", min != null);
            emit(facts, prefix, "minimumExclusive", min != null && min.exclusive);
            emit(facts, prefix, "maximum",
                    max == null ? "" : toLongDoubleLiteral(max.maxBound));
            emit(facts, prefix, "hasMaximum", max != null);
            emit(facts, prefix, "maximumExclusive", max != null && max.exclusive);
            emit(facts, prefix, "minimumAlwaysInvalid", false);
            emit(facts, prefix, "maximumAlwaysInvalid", false);
        }
    }

    /** Stores a constraint fact under its plain key (empty prefix) or a
     *  prefixed, camel-cased key (e.g. "item" -> itemMinimum). */
    private static void emit(
            Map<String, Object> facts, String prefix, String name, Object value) {
        if (prefix.isEmpty()) {
            facts.put(name, value);
        } else {
            facts.put(prefix + Character.toUpperCase(name.charAt(0)) + name.substring(1), value);
        }
    }


    /** Renders an integer (plain digits) as a long long literal that is also
     *  valid for INT64_MIN and negative bounds. */
    private static String longLongLiteral(String digits) {
        if ("-9223372036854775808".equals(digits)) {
            return "(-9223372036854775807LL - 1)";
        }
        return "(" + digits + "LL)";
    }

    private static final java.math.BigDecimal INT64_MAX =
            new java.math.BigDecimal(Long.MAX_VALUE);
    private static final java.math.BigDecimal INT64_MIN =
            new java.math.BigDecimal(Long.MIN_VALUE);
    private static final java.math.BigDecimal INT32_MAX =
            new java.math.BigDecimal(Integer.MAX_VALUE);
    private static final java.math.BigDecimal INT32_MIN =
            new java.math.BigDecimal(Integer.MIN_VALUE);

    // ------------------------------------------------------------------
    // Request body
    // ------------------------------------------------------------------

    private Map<String, Object> requestBodyFacts(
            CodegenOperation op, Operation raw, String pascal,
            Set<String> modelClassNames, Map<String, String> modelDataTypes,
            Set<String> collidingModels) {
        Map<String, Object> facts = new LinkedHashMap<>();
        List<String> mediaTypes = new ArrayList<>();
        RequestBody body = raw != null
                ? ModelUtils.getReferencedRequestBody(sourceOpenApi, raw.getRequestBody())
                : null;
        String jsonModelRef = null;
        if (body != null && body.getContent() != null) {
            for (String mediaType : body.getContent().keySet()) {
                // Degrade policy: the runtime parses request bodies as JSON
                // only. Non-JSON declarations (XML, form, multipart) and the
                // wildcard (which declares no JSON-specific representation)
                // are dropped here and reported as warnings at preprocess
                // time; an operation left with no JSON media type loses its
                // typed body entirely rather than promising bytes it cannot
                // parse.
                if (!CppBoostBeastServerCodegen.isSupportedRequestMediaType(mediaType)) {
                    continue;
                }
                Schema<?> declared = body.getContent().get(mediaType) == null
                        ? null : body.getContent().get(mediaType).getSchema();
                String ref = declared == null ? "" : declared.get$ref();
                if (jsonModelRef == null) {
                    jsonModelRef = ref;
                } else if (!equivalentBodySchema(ref, jsonModelRef)) {
                    // One typed body per operation: a second JSON media type
                    // declaring a DIFFERENT schema would be decoded into the
                    // first one's type, silently misreading one valid
                    // representation. Accept only the selected representation
                    // and let the rest answer 415.
                    LOGGER.warn("cpp-boost-beast-server: operation '{}' declares"
                                    + " media type '{}' with a schema that differs"
                                    + " from the selected request representation;"
                                    + " the generated handler accepts only '{}'",
                            op.operationId, mediaType, mediaTypes.isEmpty()
                                    ? "the first JSON media type" : mediaTypes.get(0));
                    continue;
                }
                String normalized = CppBoostBeastServerCodegen.normalizeMediaType(mediaType);
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
        // The field reference is model-namespace qualified when the model is
        // named like this operation's Request type (injected-class-name
        // shadowing) or like a runtime type (ambiguous under the model
        // using-directive).
        facts.put("modelCollides", hasBody && (dataType.equals(pascal + "Request")
                || collidingModels.contains(dataType)));
        // OpenAPI request bodies are optional unless required: true. The
        // handler must not reject an absent optional body as malformed JSON.
        facts.put("required", body != null && Boolean.TRUE.equals(body.getRequired()));
        return facts;
    }

    /**
     * Whether two JSON request media types declare the same body schema: both
     * must be refs to the same component. Inline schemas are never treated as
     * equivalent (structurally different objects read identically by name).
     */
    private static boolean equivalentBodySchema(String ref, String selectedRef) {
        if (ref == null || selectedRef == null) {
            return false;
        }
        String left = ModelUtils.getSimpleRef(ref);
        String right = ModelUtils.getSimpleRef(selectedRef);
        return left != null && left.equals(right);
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
            CodegenOperation op, Operation raw, String pascal, Set<String> collidingModels) {
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
            // Serialize every modeled response as JSON and label it with the
            // media type the document actually declares: prefer an exact JSON
            // type, then a +json suffix, then a wildcard, then the first
            // declared type. Responses are always JSON-serialized, so a
            // non-JSON declaration here is a documented degrade (warned at
            // preprocess time by collectSurfaceWarnings).
            facts.put("contentType", responseContentType(raw, response));
            String dataType = response.dataType == null ? "" : response.dataType;
            if (dataType.startsWith("std::shared_ptr<") && dataType.endsWith(">")) {
                dataType = dataType.substring(
                        "std::shared_ptr<".length(), dataType.length() - 1).trim();
            }
            facts.put("hasModel", !dataType.isEmpty());
            // Qualify model references that would otherwise be ambiguous or
            // shadowed: a model named like this operation's nested Request/
            // Responder type (injected-class-name shadowing) or like a
            // runtime type (ambiguous under the model using-directive).
            boolean pascalCollision = dataType.equals(pascal + "Responder")
                    || dataType.equals(pascal + "Request");
            String rendered = pascalCollision && !modelNamespace.isEmpty()
                    ? modelNamespace + "::" + dataType
                    : qualifyCollidingModels(dataType, collidingModels);
            facts.put("sendType", rendered);
            responses.add(facts);
        }
        return responses;
    }

    /** Rewrites whole identifier tokens that name a colliding model to a
     *  model-namespace-qualified spelling (handles containers such as
     *  std::vector&lt;Problem&gt;). Container keywords never collide with a
     *  model name, so the rewrite is safe inside template arguments. */
    String qualifyCollidingModels(String dataType, Set<String> collidingModels) {
        if (dataType.isEmpty() || collidingModels.isEmpty()) {
            return dataType;
        }
        StringBuilder out = new StringBuilder(dataType.length());
        int i = 0;
        while (i < dataType.length()) {
            char c = dataType.charAt(i);
            if (Character.isJavaIdentifierStart(c)) {
                int j = i + 1;
                while (j < dataType.length() && Character.isJavaIdentifierPart(dataType.charAt(j))) {
                    j++;
                }
                String token = dataType.substring(i, j);
                if (collidingModels.contains(token)) {
                    out.append(modelNamespace.isEmpty() ? "" : modelNamespace + "::").append(token);
                } else {
                    out.append(token);
                }
                i = j;
            } else {
                out.append(c);
                i++;
            }
        }
        return out.toString();
    }

    /** The Content-Type the generated responder labels this response with.
     *  The runtime always serializes the model as JSON, so a declared JSON
     *  family type is honored verbatim (exact application/json wins, then a
     *  {+json} suffix); a wildcard or a non-JSON declaration is served as
     *  application/json, which collectSurfaceWarnings has already reported. */
    private String responseContentType(Operation raw, CodegenResponse response) {
        String code = response.code == null ? "default" : response.code;
        ApiResponse apiResponse = raw != null && raw.getResponses() != null
                ? raw.getResponses().get(code) : null;
        if (apiResponse != null && apiResponse.getContent() != null) {
            String plusJson = "";
            for (String mediaType : apiResponse.getContent().keySet()) {
                String normalized = CppBoostBeastServerCodegen.normalizeMediaType(mediaType);
                if ("application/json".equals(normalized)) {
                    return "application/json";
                }
                if (plusJson.isEmpty() && normalized.endsWith("+json")) {
                    plusJson = normalized;
                }
            }
            if (!plusJson.isEmpty()) {
                return plusJson;
            }
        }
        return "application/json";
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
