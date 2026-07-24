package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class TypeScriptExpressZodServerCodegen extends AbstractTypeScriptClientCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(TypeScriptExpressZodServerCodegen.class);

    public TypeScriptExpressZodServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        outputFolder = "generated-code" + File.separator + "typescript-express-zod-server";
        embeddedTemplateDir = templateDir = "typescript-express-zod-server";

        // We generate everything via supporting files, not per-model/per-api
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");

        apiPackage = "";
        modelPackage = "";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "typescript-express-zod-server";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript Express server with Zod validation.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        supportingFiles.clear();
        modelTemplateFiles.clear();
        apiTemplateFiles.clear();

        supportingFiles.add(new SupportingFile("types.mustache", "", "types.ts"));
        supportingFiles.add(new SupportingFile("schemas.mustache", "", "schemas.ts"));
        supportingFiles.add(new SupportingFile("dtos" + File.separator + "types.mustache", "dtos", "types.ts"));
        supportingFiles.add(new SupportingFile("dtos" + File.separator + "mappers.mustache", "dtos", "mappers.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "handlers.mustache", "express", "handlers.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "types.mustache", "express", "types.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "router-factory.mustache", "express", "router-factory.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "errors.mustache", "express", "errors.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "auth-middleware.mustache", "express", "auth-middleware.ts"));
        supportingFiles.add(new SupportingFile("express" + File.separator + "index.mustache", "express", "index.ts"));
        supportingFiles.add(new SupportingFile("dtos" + File.separator + "index.mustache", "dtos", "index.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> allModels) {
        OperationMap objs = operations.getOperations();
        List<CodegenOperation> ops = objs.getOperation();

        for (CodegenOperation op : ops) {
            // Convert {param} to :param for Express routing
            op.path = op.path.replaceAll("\\{([^}]+)\\}", ":$1");
            // Escape reserved path-to-regexp v8 characters for Express v5 compatibility
            op.path = escapeExpressReservedChars(op.path);

            // Add vendor extensions for template use
            op.vendorExtensions.put("x-express-path", op.path);
            op.vendorExtensions.put("x-http-method-lower", op.httpMethod.toLowerCase(Locale.ENGLISH));

            // Build the handler name: handle + PascalCase(operationId)
            String handlerName = "handle" + camelize(op.operationId);
            op.vendorExtensions.put("x-handler-name", handlerName);

            // Determine the service name from the tag
            if (op.tags != null && !op.tags.isEmpty()) {
                String tag = op.tags.get(0).getName();
                String serviceName = camelize(tag) + "Service";
                op.vendorExtensions.put("x-service-name", serviceName);
                op.vendorExtensions.put("x-service-getter", "get" + serviceName);
            }

            // Response Zod schema / wire type / mapper are resolved (array/map/primitive
            // aware) in postProcessSupportingFileData via TypeScriptExpressZodUtils.

            // Mark whether this operation has only path params, only query params, etc.
            boolean hasOnlyPathParams = !op.pathParams.isEmpty() && op.queryParams.isEmpty() && op.bodyParam == null;
            boolean hasOnlyQueryParams = op.pathParams.isEmpty() && !op.queryParams.isEmpty() && op.bodyParam == null;
            boolean hasBodyParam = op.bodyParam != null;
            boolean hasNoParams = op.pathParams.isEmpty() && op.queryParams.isEmpty() && op.bodyParam == null;

            op.vendorExtensions.put("x-has-no-params", hasNoParams);
            op.vendorExtensions.put("x-has-body-param", hasBodyParam);

            // Build params schema name
            String paramsType = camelize(op.operationId) + "Params";
            op.vendorExtensions.put("x-params-type", paramsType);
            op.vendorExtensions.put("x-params-schema", paramsType + "Schema");

            // Check if all params are optional (for optional parse)
            boolean allParamsOptional = true;
            for (CodegenParameter param : op.allParams) {
                if (param.required) {
                    allParamsOptional = false;
                    break;
                }
            }
            op.vendorExtensions.put("x-all-params-optional", allParamsOptional);

            // The body Zod schema / wire type / mapper are resolved (array/map/primitive
            // aware) in postProcessSupportingFileData; here we only fix the body param name.
            if (op.bodyParam != null) {
                // Get the request body name from x-codegen-request-body-name or default to "body"
                String bodyParamName = op.bodyParam.paramName;
                if (bodyParamName == null || bodyParamName.isEmpty()) {
                    bodyParamName = "body";
                }
                op.vendorExtensions.put("x-body-param-name", bodyParamName);
            }

            // Add vendor extensions to each parameter for Zod schema building
            for (CodegenParameter param : op.allParams) {
                addZodVendorExtensions(param);
            }

            // Build RequestHandler type generics
            buildRequestHandlerType(op);
        }

        return operations;
    }

    /**
     * Add Zod-specific vendor extensions to a parameter.
     */
    private void addZodVendorExtensions(CodegenParameter param) {
        String zodType = buildZodType(param);
        param.vendorExtensions.put("x-zod-type", zodType);

        // Track if the query param name differs from the camelCase name
        if (param.isQueryParam && !param.baseName.equals(param.paramName)) {
            param.vendorExtensions.put("x-original-name", param.baseName);
            param.vendorExtensions.put("x-has-different-name", true);
        }
    }

    /**
     * Build a Zod type expression for a parameter.
     */
    private String buildZodType(CodegenParameter param) {
        StringBuilder zod = new StringBuilder();

        if (param.isEnum && param.allowableValues != null) {
            List<?> values = (List<?>) param.allowableValues.get("values");
            if (values != null && !values.isEmpty()) {
                StringBuilder sb = new StringBuilder("z.enum([");
                for (int i = 0; i < values.size(); i++) {
                    if (i > 0) sb.append(", ");
                    sb.append("'").append(values.get(i)).append("'");
                }
                sb.append("])");
                zod.append(sb);
                // Array-of-enum params (e.g. a repeated query param) — the param itself is
                // flagged isEnum, so wrap the enum in .array() to match the Array<Enum> type.
                if (param.isArray) {
                    zod.append(".array()");
                }
            } else {
                zod.append(camelize(param.dataType) + "Schema");
            }
        } else if (!param.isPrimitiveType && param.dataType != null && !param.isArray
                && !param.isBodyParam && !param.isModel) {
            // Reference to a non-primitive type (e.g., enum via $ref)
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
                // Reference the model schema
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
     * Build the Express RequestHandler type generics for an operation.
     */
    private void buildRequestHandlerType(CodegenOperation op) {
        // Params type (path params)
        StringBuilder paramsType = new StringBuilder("{");
        if (!op.pathParams.isEmpty()) {
            paramsType = new StringBuilder("{\n");
            for (int i = 0; i < op.pathParams.size(); i++) {
                CodegenParameter p = op.pathParams.get(i);
                paramsType.append("    ").append(p.paramName).append(": string");
                if (i < op.pathParams.size() - 1) paramsType.append(",");
                paramsType.append("\n");
            }
            paramsType.append("  }");
        } else {
            paramsType.append("}");
        }
        op.vendorExtensions.put("x-params-generic", paramsType.toString());

        // Query type
        StringBuilder queryType = new StringBuilder("{");
        if (!op.queryParams.isEmpty()) {
            queryType = new StringBuilder("{\n");
            for (int i = 0; i < op.queryParams.size(); i++) {
                CodegenParameter p = op.queryParams.get(i);
                String qName = p.baseName.contains("-") ? "\"" + p.baseName + "\"" : p.baseName;
                queryType.append("    ").append(qName).append("?: string");
                if (i < op.queryParams.size() - 1) queryType.append(",");
                queryType.append("\n");
            }
            queryType.append("  }");
        } else {
            queryType.append("}");
        }
        op.vendorExtensions.put("x-query-generic", queryType.toString());

        // The body and response wire (DTO) generics for the RequestHandler type are resolved
        // (array/map/primitive aware) in postProcessSupportingFileData; see resolveBody/resolveResponse.
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);

        // Zod/union/intersection/ref annotations shared with the express-zod client
        // (the `express-zod` framework of the `typescript` generator).
        TypeScriptExpressZodUtils.annotateModels(result, this.openAPI, this::toModelName);

        // Second pass: Make discriminator properties required in Zod schemas.
        // Discriminator properties must not be .optional() so that the inferred
        // type from z.discriminatedUnion matches the generated TypeScript types.
        for (ModelsMap entry : result.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm.vendorExtensions.containsKey("x-is-discriminated-union") && cm.discriminator != null) {
                    String discPropName = cm.discriminator.getPropertyBaseName();
                    if (cm.oneOf != null) {
                        for (String memberName : cm.oneOf) {
                            ModelsMap memberModelsMap = result.get(memberName);
                            if (memberModelsMap != null) {
                                for (ModelMap mm : memberModelsMap.getModels()) {
                                    for (CodegenProperty prop : mm.getModel().vars) {
                                        if (prop.baseName.equals(discPropName) || prop.name.equals(discPropName)) {
                                            String zodFull = (String) prop.vendorExtensions.get("x-zod-full");
                                            if (zodFull != null && zodFull.endsWith(".optional()")) {
                                                prop.vendorExtensions.put("x-zod-full",
                                                    zodFull.substring(0, zodFull.length() - ".optional()".length()));
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Escape characters that are reserved in path-to-regexp v8 (used by Express v5).
     * Reserved chars: ( ) [ ] ? + !
     * These are escaped with backslash in the generated TypeScript source.
     * Double-backslash is used so that Mustache renders \\( in TS, which JS evaluates to \( at runtime.
     * Parameter segments (:paramName) are left untouched.
     * This is backward-compatible with Express v4 (path-to-regexp 0.1.x also treats \( as literal).
     */
    private String escapeExpressReservedChars(String path) {
        StringBuilder result = new StringBuilder();
        int i = 0;
        while (i < path.length()) {
            char c = path.charAt(i);
            if (c == ':') {
                // Skip over :paramName segment
                result.append(c);
                i++;
                while (i < path.length() && (Character.isLetterOrDigit(path.charAt(i)) || path.charAt(i) == '_')) {
                    result.append(path.charAt(i));
                    i++;
                }
            } else if ("()[]?+!".indexOf(c) >= 0) {
                result.append("\\\\").append(c);
                i++;
            } else {
                result.append(c);
                i++;
            }
        }
        return result.toString();
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        bundle = super.postProcessSupportingFileData(bundle);

        // Build service map from operations for the types template
        List<OperationsMap> allOps = ((org.openapitools.codegen.model.ApiInfoMap) bundle.get("apiInfo")).getApis();

        // Collect all unique services
        Map<String, List<Map<String, Object>>> serviceMap = new LinkedHashMap<>();
        // Collect all unique paths for router
        Map<String, List<Map<String, Object>>> pathMap = new LinkedHashMap<>();

        for (OperationsMap opsMap : allOps) {
            OperationMap operationMap = opsMap.getOperations();
            List<CodegenOperation> ops = operationMap.getOperation();

            for (CodegenOperation op : ops) {
                String serviceName = (String) op.vendorExtensions.getOrDefault("x-service-name", "DefaultService");
                String serviceGetter = (String) op.vendorExtensions.getOrDefault("x-service-getter", "getDefaultService");

                serviceMap.computeIfAbsent(serviceName, k -> new ArrayList<>());

                Map<String, Object> opInfo = new HashMap<>();
                String operationIdPascal = camelize(op.operationId);
                // Resolve response/body shapes (array/map/primitive aware). The server maps
                // domain results -> wire (TO_WIRE) and req.body wire -> domain (FROM_WIRE).
                TypeScriptExpressZodUtils.Resolved rr = op.returnType != null
                        ? TypeScriptExpressZodUtils.resolveResponse(
                                op, TypeScriptExpressZodUtils.Direction.TO_WIRE)
                        : null;
                TypeScriptExpressZodUtils.Resolved rb = op.bodyParam != null
                        ? TypeScriptExpressZodUtils.resolveBody(
                                op.bodyParam, TypeScriptExpressZodUtils.Direction.FROM_WIRE,
                                buildZodType(op.bodyParam))
                        : null;
                opInfo.put("operationId", op.operationId);
                opInfo.put("operationIdPascal", operationIdPascal);
                opInfo.put("description", op.summary != null ? op.summary : op.notes);
                opInfo.put("path", op.path);
                opInfo.put("httpMethod", op.httpMethod.toLowerCase(Locale.ENGLISH));
                opInfo.put("httpMethodUpper", op.httpMethod.toUpperCase(Locale.ENGLISH));
                opInfo.put("handlerName", op.vendorExtensions.get("x-handler-name"));
                opInfo.put("serviceName", serviceName);
                opInfo.put("serviceGetter", serviceGetter);
                opInfo.put("returnType", op.returnType);
                opInfo.put("hasParams", !op.allParams.isEmpty());
                opInfo.put("hasNoParams", op.allParams.isEmpty());
                opInfo.put("allParamsOptional", op.vendorExtensions.get("x-all-params-optional"));

                // Build params info
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
                    // Zod schema for the body field (array/map/primitive aware) and the
                    // wire->domain mapper function applied to req.body. mapFn is null for
                    // bare primitives (identity), in which case the template passes req.body through.
                    body.put("zodExpr", rb.zod);
                    // The mapper runs on RAW req.body before Zod validation, so guard by
                    // shape: a malformed body must fall through to the schema (ZodError ->
                    // 400) instead of crashing in the mapper (TypeError -> 500).
                    String bodyMapFn = rb.mapFn;
                    if (bodyMapFn != null) {
                        if (op.bodyParam.isArray) {
                            bodyMapFn = "(b) => Array.isArray(b) ? (" + rb.mapFn + ")(b) : b";
                        } else if (op.bodyParam.isMap) {
                            bodyMapFn = "(b) => b !== null && typeof b === \"object\" && !Array.isArray(b) ? (" + rb.mapFn + ")(b) : b";
                        } else {
                            bodyMapFn = "(b) => b == null ? b : (" + rb.mapFn + ")(b)";
                        }
                    }
                    body.put("mapFn", bodyMapFn);
                    opInfo.put("bodyParam", body);
                    opInfo.put("hasBodyParam", true);
                } else {
                    opInfo.put("hasBodyParam", false);
                }

                // Auth permission from x-auth-permission vendor extension
                opInfo.put("permission", op.vendorExtensions.get("x-auth-permission"));

                // Response info. The response Zod schema is emitted as a per-operation
                // const in schemas.ts (so array/map/primitive shapes have a real symbol to
                // reference), and the payload is converted via mapFn (null => passthrough).
                opInfo.put("responseType", op.returnType);
                if (rr != null) {
                    opInfo.put("responseSchema", operationIdPascal + "ResponseSchema");
                    opInfo.put("responseZodExpr", rr.zod);
                    opInfo.put("responseDtoMapper", rr.mapFn);
                    // Domain type qualified for use outside types.ts (e.g. the SSE branch's
                    // result cast in handlers.ts): types.Pet[], { [key: string]: number }, string.
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
                            opInfo.put("sseDtoMapper", "mapTo" + sseType + "Dto");
                            opInfo.put("sseSchema", sseType + "Schema");
                            break;
                        }
                    }
                }
                opInfo.put("hasSSE", hasSSE);

                // RequestHandler type generics
                opInfo.put("paramsGeneric", op.vendorExtensions.get("x-params-generic"));
                opInfo.put("queryGeneric", op.vendorExtensions.get("x-query-generic"));
                // Wire (DTO) generics for the express RequestHandler type, resolved for
                // array/map/primitive shapes (e.g. dtos.PetDto[], { [key: string]: number }).
                opInfo.put("bodyGeneric", rb != null ? rb.dtoGeneric : "unknown");
                opInfo.put("responseGeneric", rr != null ? rr.dtoGeneric : "any");

                serviceMap.get(serviceName).add(opInfo);

                // Group by path for router
                String expressPath = op.path;
                pathMap.computeIfAbsent(expressPath, k -> new ArrayList<>());
                pathMap.get(expressPath).add(opInfo);
            }
        }

        // Convert service map to list for Mustache iteration
        List<Map<String, Object>> services = new ArrayList<>();
        for (Map.Entry<String, List<Map<String, Object>>> entry : serviceMap.entrySet()) {
            Map<String, Object> svc = new HashMap<>();
            svc.put("name", entry.getKey());
            svc.put("getter", "get" + entry.getKey());
            svc.put("operations", entry.getValue());
            services.add(svc);
        }
        bundle.put("services", services);

        // Convert path map to list for router template
        List<Map<String, Object>> routes = new ArrayList<>();
        for (Map.Entry<String, List<Map<String, Object>>> entry : pathMap.entrySet()) {
            Map<String, Object> route = new HashMap<>();
            route.put("path", entry.getKey());
            route.put("operations", entry.getValue());

            // Build the allowed methods list
            Set<String> methods = new TreeSet<>();
            methods.add("OPTIONS");
            for (Map<String, Object> opInfo : entry.getValue()) {
                String method = ((String) opInfo.get("httpMethodUpper"));
                methods.add(method);
                if ("GET".equals(method)) methods.add("HEAD");
            }
            route.put("allowedMethods", String.join(", ", methods));
            routes.add(route);
        }

        // Sort routes: most specific (longer, more segments) first to avoid Express routing conflicts
        routes.sort((a, b) -> {
            String pathA = (String) a.get("path");
            String pathB = (String) b.get("path");
            // Count segments
            long segA = pathA.chars().filter(c -> c == '/').count();
            long segB = pathB.chars().filter(c -> c == '/').count();
            if (segA != segB) return Long.compare(segB, segA); // more segments first
            // Prefer static segments over parameterized ones
            boolean aHasParam = pathA.contains(":");
            boolean bHasParam = pathB.contains(":");
            if (aHasParam != bHasParam) return aHasParam ? 1 : -1; // static first at same depth
            return pathA.compareTo(pathB);
        });

        bundle.put("routes", routes);

        // Collect all operations flat for handlers template
        List<Map<String, Object>> allOperations = new ArrayList<>();
        for (Map<String, Object> svc : services) {
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> ops = (List<Map<String, Object>>) svc.get("operations");
            allOperations.addAll(ops);
        }
        // Sort operations alphabetically by operationId
        allOperations.sort(Comparator.comparing(o -> (String) o.get("operationId")));
        bundle.put("allOperations", allOperations);

        // Build auth operations list for auth-middleware template
        List<Map<String, Object>> authOperations = allOperations.stream()
                .filter(op -> op.get("permission") != null)
                .collect(Collectors.toList());
        bundle.put("authOperations", authOperations);
        bundle.put("hasAuthOperations", !authOperations.isEmpty());

        // Topologically sort models so that referenced schemas are declared before they are used
        @SuppressWarnings("unchecked")
        List<Object> models = (List<Object>) bundle.get("models");
        if (models != null) {
            bundle.put("models", TypeScriptExpressZodUtils.topologicallySortModels(models));
        }

        // Flag whether any model is a discriminated union, so templates can
        // conditionally import ZodError (thrown from mapFrom*Dto default branches).
        boolean hasDiscriminatedUnion = false;
        if (models != null) {
            for (Object modelEntry : models) {
                @SuppressWarnings("unchecked")
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
