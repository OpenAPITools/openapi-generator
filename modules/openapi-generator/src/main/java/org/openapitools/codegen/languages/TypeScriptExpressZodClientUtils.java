package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenMediaType;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.model.ApiInfoMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Client-side post-processing for the {@code express-zod} framework of the unified
 * {@code typescript} generator. Holds the logic that used to live in the standalone
 * {@code typescript-express-zod-client} generator so {@code TypeScriptClientCodegen}
 * only needs thin, framework-guarded delegation calls.
 *
 * <p>Logic shared with the {@code typescript-express-zod-server} generator (Zod
 * expression building, model annotation, shape resolution, topological sorting)
 * lives in {@link TypeScriptExpressZodUtils}.
 */
final class TypeScriptExpressZodClientUtils {
    private TypeScriptExpressZodClientUtils() {
    }

    /**
     * Operation post-processing: URL templates, service names, fetch methods, params
     * types, and per-parameter Zod vendor extensions.
     */
    static OperationsMap postProcessOperations(OperationsMap operations) {
        OperationMap objs = operations.getOperations();
        List<CodegenOperation> ops = objs.getOperation();

        for (CodegenOperation op : ops) {
            // Build URL template for JS template literals:
            // /recipe/{recipeId} -> /recipe/${encodeURIComponent(params.recipeId)}
            String urlTemplate = op.path.replaceAll(
                    "\\{([^}]+)\\}",
                    "\\${encodeURIComponent(String(params.$1))}"
            );
            op.vendorExtensions.put("x-url-template", urlTemplate);

            // Determine the service name from the tag
            if (op.tags != null && !op.tags.isEmpty()) {
                String tag = op.tags.get(0).getName();
                String serviceName = camelize(tag) + "Service";
                op.vendorExtensions.put("x-service-name", serviceName);
            }

            // Set fetch method only for non-GET (GET is fetch default)
            String httpMethodUpper = op.httpMethod.toUpperCase(Locale.ENGLISH);
            if (!"GET".equals(httpMethodUpper)) {
                op.vendorExtensions.put("x-fetch-method", httpMethodUpper);
            }

            // Response Zod schema / wire type / mapper are resolved (array/map/primitive
            // aware) in postProcessSupportingFileData via TypeScriptExpressZodUtils.

            // Build params type name
            String paramsType = camelize(op.operationId) + "Params";
            op.vendorExtensions.put("x-params-type", paramsType);

            // Check if all params are optional
            boolean allParamsOptional = true;
            for (CodegenParameter param : op.allParams) {
                if (param.required) {
                    allParamsOptional = false;
                    break;
                }
            }
            op.vendorExtensions.put("x-all-params-optional", allParamsOptional);

            // Body param info
            boolean hasBodyParam = op.bodyParam != null;
            op.vendorExtensions.put("x-has-body-param", hasBodyParam);
            if (hasBodyParam) {
                String bodyParamName = op.bodyParam.paramName;
                if (bodyParamName == null || bodyParamName.isEmpty()) {
                    bodyParamName = "body";
                }
                op.vendorExtensions.put("x-body-param-name", bodyParamName);
            }

            boolean hasNoParams = op.pathParams.isEmpty() && op.queryParams.isEmpty() && op.bodyParam == null;
            op.vendorExtensions.put("x-has-no-params", hasNoParams);

            // Add vendor extensions to each parameter for Zod schema building
            for (CodegenParameter param : op.allParams) {
                addZodVendorExtensions(param);
            }
        }

        return operations;
    }

    /**
     * Add Zod-specific vendor extensions to a parameter.
     */
    private static void addZodVendorExtensions(CodegenParameter param) {
        String zodType = buildZodType(param);
        param.vendorExtensions.put("x-zod-type", zodType);

        // Track if the query param name differs from the camelCase name
        if (param.isQueryParam && !param.baseName.equals(param.paramName)) {
            param.vendorExtensions.put("x-original-name", param.baseName);
            param.vendorExtensions.put("x-has-different-name", true);
        }
    }

    /**
     * Build a Zod type expression for a parameter. This is the client variant; the
     * server's {@code buildZodType} differs (inline z.enum branch) and unifying the
     * two would change generated output — deferred as a follow-up.
     */
    static String buildZodType(CodegenParameter param) {
        StringBuilder zod = new StringBuilder();

        if (param.isEnum) {
            zod.append(camelize(param.dataType) + "Schema");
        } else if (!param.isPrimitiveType && param.dataType != null && !param.isArray
                && !param.isBodyParam && !param.isModel) {
            zod.append(camelize(param.dataType) + "Schema");
        } else if (param.isInteger || param.isLong) {
            zod.append("z.coerce.number().int()");
            if (param.minimum != null) {
                zod.append(".gte(").append(param.minimum).append(")");
            }
            if (param.maximum != null) {
                zod.append(".lte(").append(param.maximum).append(")");
            }
        } else if (param.isNumber || param.isFloat || param.isDouble) {
            zod.append("z.number()");
            if (param.minimum != null) {
                zod.append(".gte(").append(param.minimum).append(")");
            }
            if (param.maximum != null) {
                zod.append(".lte(").append(param.maximum).append(")");
            }
        } else if (param.isBoolean) {
            zod.append("z.boolean()");
        } else if (param.isString) {
            zod.append("z.string()");
            if (param.maxLength != null) {
                zod.append(".max(").append(param.maxLength).append(")");
            }
            if (param.minLength != null) {
                zod.append(".min(").append(param.minLength).append(")");
            }
        } else if (param.isArray && param.items != null) {
            zod.append(TypeScriptExpressZodUtils.zodExpr(param.items)).append(".array()");
        } else if (param.isMap && param.additionalProperties != null) {
            zod.append("z.record(z.string(), ")
               .append(TypeScriptExpressZodUtils.zodExpr(param.additionalProperties))
               .append(")");
        } else if (param.isModel || param.isBodyParam) {
            // Bodies whose schema dereferences to a bare primitive (e.g. a $ref'd enum
            // handled as string by fromRequestBody) carry a primitive dataType with the
            // primitive flags unset — camelize(dataType)+"Schema" would fabricate an
            // unresolved symbol like StringSchema.
            if ("string".equals(param.dataType)) {
                zod.append("z.string()");
            } else if ("number".equals(param.dataType)) {
                zod.append("z.number()");
            } else if ("boolean".equals(param.dataType)) {
                zod.append("z.boolean()");
            } else {
                zod.append(camelize(param.dataType) + "Schema");
            }
        } else {
            zod.append("z.string()");
            if (param.maxLength != null) {
                zod.append(".max(").append(param.maxLength).append(")");
            }
        }

        if (!param.required) {
            zod.append(".optional()");
        }

        return zod.toString();
    }

    /**
     * Bundle builder for the supporting-file templates: services, flat operation list,
     * resolved response/body shapes, SSE stream operations, ModelError inspection, and
     * topologically sorted models.
     */
    @SuppressWarnings("unchecked")
    static Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        List<OperationsMap> allOps = ((ApiInfoMap) bundle.get("apiInfo")).getApis();

        // Collect all unique services
        Map<String, List<Map<String, Object>>> serviceMap = new LinkedHashMap<>();

        for (OperationsMap opsMap : allOps) {
            OperationMap operationMap = opsMap.getOperations();
            List<CodegenOperation> ops = operationMap.getOperation();

            for (CodegenOperation op : ops) {
                String serviceName = (String) op.vendorExtensions.getOrDefault("x-service-name", "DefaultService");

                serviceMap.computeIfAbsent(serviceName, k -> new ArrayList<>());

                Map<String, Object> opInfo = new HashMap<>();
                String operationIdPascal = camelize(op.operationId);
                // Resolve response/body shapes (array/map/primitive aware). The client converts
                // wire json -> domain (FROM_WIRE) and domain -> wire before sending (TO_WIRE).
                TypeScriptExpressZodUtils.Resolved rr = op.returnType != null
                        ? TypeScriptExpressZodUtils.resolveResponse(
                                op, TypeScriptExpressZodUtils.Direction.FROM_WIRE)
                        : null;
                TypeScriptExpressZodUtils.Resolved rb = op.bodyParam != null
                        ? TypeScriptExpressZodUtils.resolveBody(
                                op.bodyParam, TypeScriptExpressZodUtils.Direction.TO_WIRE,
                                buildZodType(op.bodyParam))
                        : null;
                opInfo.put("operationId", op.operationId);
                opInfo.put("operationIdPascal", operationIdPascal);
                opInfo.put("description", op.summary != null ? op.summary : op.notes);
                opInfo.put("httpMethod", op.httpMethod.toLowerCase(Locale.ENGLISH));
                opInfo.put("httpMethodUpper", op.httpMethod.toUpperCase(Locale.ENGLISH));
                opInfo.put("serviceName", serviceName);
                opInfo.put("returnType", op.returnType);
                opInfo.put("hasParams", !op.allParams.isEmpty());
                opInfo.put("hasNoParams", op.allParams.isEmpty());
                opInfo.put("allParamsOptional", op.vendorExtensions.get("x-all-params-optional"));

                // URL template for JS template literals
                opInfo.put("urlTemplate", op.vendorExtensions.get("x-url-template"));

                // Fetch method (only for non-GET)
                opInfo.put("fetchMethod", op.vendorExtensions.get("x-fetch-method"));
                opInfo.put("hasFetchMethod", op.vendorExtensions.containsKey("x-fetch-method"));

                // Params info
                List<Map<String, Object>> pathParams = new ArrayList<>();
                for (CodegenParameter p : op.pathParams) {
                    Map<String, Object> pm = new HashMap<>();
                    pm.put("name", p.paramName);
                    pm.put("baseName", p.baseName);
                    pm.put("zodType", buildZodType(p));
                    pm.put("dataType", p.dataType);
                    pathParams.add(pm);
                }
                opInfo.put("pathParams", pathParams);
                opInfo.put("hasPathParams", !pathParams.isEmpty());

                List<Map<String, Object>> queryParams = new ArrayList<>();
                for (CodegenParameter p : op.queryParams) {
                    Map<String, Object> qm = new HashMap<>();
                    qm.put("name", p.paramName);
                    qm.put("baseName", p.baseName);
                    qm.put("hasDifferentName", !p.baseName.equals(p.paramName));
                    qm.put("zodType", buildZodType(p));
                    qm.put("dataType", p.dataType);
                    qm.put("required", p.required);
                    qm.put("isEnum", p.isEnum);
                    queryParams.add(qm);
                }
                opInfo.put("queryParams", queryParams);
                opInfo.put("hasQueryParams", !queryParams.isEmpty());

                if (op.bodyParam != null) {
                    Map<String, Object> body = new HashMap<>();
                    body.put("name", op.vendorExtensions.get("x-body-param-name"));
                    body.put("dataType", op.bodyParam.dataType);
                    body.put("required", op.bodyParam.required);
                    opInfo.put("bodyParam", body);
                    opInfo.put("hasBodyParam", true);
                    // Convert the domain body -> wire before JSON.stringify. Identity (x) => x
                    // for bare primitives so the template can always call it as a function.
                    // When the body can be undefined (optional requestBody, or params itself
                    // optional), guard the mapper so absent bodies keep the old
                    // JSON.stringify(undefined) -> no-body behavior instead of crashing.
                    String bodyMapFn = rb.mapFn != null ? rb.mapFn : "(x) => x";
                    boolean bodyMayBeUndefined = !op.bodyParam.required
                            || Boolean.TRUE.equals(op.vendorExtensions.get("x-all-params-optional"));
                    if (rb.mapFn != null && bodyMayBeUndefined) {
                        bodyMapFn = "(b) => b === undefined ? undefined : (" + rb.mapFn + ")(b)";
                    }
                    opInfo.put("bodyMapFn", bodyMapFn);
                } else {
                    opInfo.put("hasBodyParam", false);
                }

                // Response info. The response Zod schema is emitted as a per-operation const
                // in schemas.ts, and the parsed wire payload is converted to domain via mapFn
                // (identity (x) => x for bare primitives, so it can always be called as a function).
                opInfo.put("responseType", op.returnType);
                if (rr != null) {
                    opInfo.put("responseSchema", operationIdPascal + "ResponseSchema");
                    opInfo.put("responseZodExpr", rr.zod);
                    opInfo.put("responseDtoMapper", rr.mapFn != null ? rr.mapFn : "(x) => x");
                    opInfo.put("responseDtoGeneric", rr.dtoGeneric);
                    // Domain type qualified for http-client.ts (which imports types); e.g.
                    // types.Pet[], { [key: string]: number }, string.
                    opInfo.put("responseDomainGeneric", rr.domainGeneric);
                }

                // SSE detection
                boolean hasSSE = false;
                for (CodegenResponse response : op.responses) {
                    if (response.is2xx && response.getContent() != null) {
                        CodegenMediaType sseMt = response.getContent().get("text/event-stream");
                        if (sseMt != null && sseMt.getSchema() != null) {
                            hasSSE = true;
                            String sseType = sseMt.getSchema().getComplexType();
                            if (sseType == null) sseType = sseMt.getSchema().getDataType();
                            opInfo.put("sseType", sseType);
                            opInfo.put("sseDtoType", sseType + "Dto");
                            opInfo.put("sseDtoMapper", "mapFrom" + sseType + "Dto");
                            opInfo.put("sseSchema", sseType + "Schema");
                            opInfo.put("streamOperationId", op.operationId + "Stream");
                            break;
                        }
                    }
                }
                opInfo.put("hasSSE", hasSSE);

                serviceMap.get(serviceName).add(opInfo);
            }
        }

        // Convert service map to list for Mustache iteration
        List<Map<String, Object>> services = new ArrayList<>();
        for (Map.Entry<String, List<Map<String, Object>>> entry : serviceMap.entrySet()) {
            Map<String, Object> svc = new HashMap<>();
            svc.put("name", entry.getKey());
            svc.put("operations", entry.getValue());
            boolean serviceHasSSE = entry.getValue().stream()
                    .anyMatch(op -> Boolean.TRUE.equals(op.get("hasSSE")));
            svc.put("hasSSE", serviceHasSSE);
            services.add(svc);
        }
        bundle.put("services", services);

        // Collect all operations flat
        List<Map<String, Object>> allOperations = new ArrayList<>();
        for (Map<String, Object> svc : services) {
            List<Map<String, Object>> ops = (List<Map<String, Object>>) svc.get("operations");
            allOperations.addAll(ops);
        }
        allOperations.sort(Comparator.comparing(o -> (String) o.get("operationId")));
        bundle.put("allOperations", allOperations);

        boolean anySSE = allOperations.stream()
                .anyMatch(op -> Boolean.TRUE.equals(op.get("hasSSE")));
        bundle.put("hasAnySSE", anySSE);

        // Topologically sort models
        List<Object> models = (List<Object>) bundle.get("models");
        if (models != null) {
            bundle.put("models", TypeScriptExpressZodUtils.topologicallySortModels(models));
        }

        // Inspect ModelError schema and expose error-shape template variables
        if (models != null) {
            for (Object modelEntry : models) {
                Map<String, Object> entry = (Map<String, Object>) modelEntry;
                CodegenModel cm = (CodegenModel) entry.get("model");
                if (cm != null && "ModelError".equals(cm.classname)) {
                    // Determine the message field name: prefer "message", then "title",
                    // then first required string property, fallback to "message"
                    String messageField = null;
                    String firstRequiredString = null;
                    boolean hasCode = false;
                    boolean hasSource = false;

                    for (CodegenProperty prop : cm.vars) {
                        if ("code".equals(prop.name)) {
                            hasCode = true;
                        }
                        if ("source".equals(prop.name)) {
                            hasSource = true;
                        }
                        if (prop.required && prop.isString) {
                            if ("message".equals(prop.name)) {
                                messageField = "message";
                            } else if ("title".equals(prop.name) && messageField == null) {
                                messageField = "title";
                            }
                            if (firstRequiredString == null) {
                                firstRequiredString = prop.name;
                            }
                        }
                    }

                    if (messageField == null) {
                        messageField = firstRequiredString != null ? firstRequiredString : "message";
                    }

                    bundle.put("errorMessageField", messageField);
                    bundle.put("errorHasCode", hasCode);
                    bundle.put("errorHasSource", hasSource);
                    break;
                }
            }
        }

        // Flag whether any model is a discriminated union, so the DTO mappers can
        // conditionally import ZodError (thrown from mapFrom*Dto default branches).
        boolean hasDiscriminatedUnion = false;
        if (models != null) {
            for (Object modelEntry : models) {
                Map<String, Object> entry = (Map<String, Object>) modelEntry;
                CodegenModel cm = (CodegenModel) entry.get("model");
                if (cm != null && cm.vendorExtensions.containsKey("x-is-discriminated-union")) {
                    hasDiscriminatedUnion = true;
                    break;
                }
            }
        }
        bundle.put("hasDiscriminatedUnion", hasDiscriminatedUnion);

        return bundle;
    }
}
