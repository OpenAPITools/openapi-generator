package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ApiInfoMap;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class TypescriptExpressZodClientCodegen extends AbstractTypeScriptClientCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(TypescriptExpressZodClientCodegen.class);

    public TypescriptExpressZodClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        outputFolder = "generated-code" + File.separator + "typescript-express-zod-client";
        embeddedTemplateDir = templateDir = "typescript-express-zod-client";

        // We generate everything via supporting files, not per-model/per-api
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");

        apiPackage = "";
        modelPackage = "";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "typescript-express-zod-client";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript HTTP client with Zod response validation for an Express API.";
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
        supportingFiles.add(new SupportingFile("dtos" + File.separator + "index.mustache", "dtos", "index.ts"));
        supportingFiles.add(new SupportingFile("http-client.mustache", "", "http-client.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> allModels) {
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

            // Determine the response type and schema names
            if (op.returnType != null) {
                op.vendorExtensions.put("x-response-type", op.returnType);
                op.vendorExtensions.put("x-response-schema", op.returnType + "Schema");
            }

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
        } else if (param.isArray) {
            zod.append("z.array(z.string())"); // simplified
        } else if (param.isModel || param.isBodyParam) {
            zod.append(camelize(param.dataType) + "Schema");
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

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);

        // Collect enum model names
        Set<String> enumModelNames = new HashSet<>();
        for (ModelsMap modEntry : result.values()) {
            for (ModelMap innerMo : modEntry.getModels()) {
                CodegenModel innerCm = innerMo.getModel();
                if (innerCm.isEnum) {
                    enumModelNames.add(innerCm.classname);
                }
            }
        }

        for (ModelsMap entry : result.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();

                // Add Zod vendor extensions to each property
                for (CodegenProperty prop : cm.vars) {
                    addZodVendorExtensions(prop, cm);
                }

                // Look up original schema from OAS spec (shared by oneOf and allOf detection)
                Map<String, Schema> allSchemas = ModelUtils.getSchemas(this.openAPI);
                Schema origSchema = null;
                for (Map.Entry<String, Schema> schemaEntry : allSchemas.entrySet()) {
                    if (toModelName(schemaEntry.getKey()).equals(cm.classname)) {
                        origSchema = schemaEntry.getValue();
                        break;
                    }
                }

                // Determine if this is a discriminated union
                if (cm.discriminator != null && cm.oneOf != null && !cm.oneOf.isEmpty()) {
                    cm.vendorExtensions.put("x-is-discriminated-union", true);
                    cm.vendorExtensions.put("x-discriminator-property", cm.discriminator.getPropertyBaseName());

                    List<Map<String, String>> mappedTypes = new ArrayList<>();

                    if (cm.discriminator.getMappedModels() != null && !cm.discriminator.getMappedModels().isEmpty()) {
                        for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                            Map<String, String> mapped = new HashMap<>();
                            mapped.put("modelName", mm.getModelName());
                            mapped.put("mappingName", mm.getMappingName());
                            mappedTypes.add(mapped);
                        }
                    } else {
                        for (String memberName : cm.oneOf) {
                            Map<String, String> mapped = new HashMap<>();
                            mapped.put("modelName", memberName);
                            String discValue = findDiscriminatorValue(memberName, cm.discriminator.getPropertyBaseName(), result);
                            mapped.put("mappingName", discValue != null ? discValue : memberName);
                            mappedTypes.add(mapped);
                        }
                    }
                    cm.vendorExtensions.put("x-mapped-models", mappedTypes);
                }

                // Detect non-discriminated oneOf unions from the original OAS spec
                if (cm.discriminator == null && !cm.vendorExtensions.containsKey("x-is-discriminated-union")) {
                    if (origSchema != null && origSchema.getOneOf() != null && !origSchema.getOneOf().isEmpty()) {
                        cm.vendorExtensions.put("x-is-union", true);

                        // Collect member classnames
                        List<String> memberNames = new ArrayList<>();
                        for (Object memberObj : origSchema.getOneOf()) {
                            Schema memberSchema = (Schema) memberObj;
                            String ref = memberSchema.get$ref();
                            if (ref != null) {
                                memberNames.add(toModelName(ModelUtils.getSimpleRef(ref)));
                            }
                        }

                        // Collect property sets per member to find distinguishing properties
                        Map<String, Set<String>> memberPropSets = new LinkedHashMap<>();
                        for (String memberName : memberNames) {
                            Set<String> props = new LinkedHashSet<>();
                            ModelsMap memberModels = result.get(memberName);
                            if (memberModels != null) {
                                for (ModelMap mm : memberModels.getModels()) {
                                    for (CodegenProperty prop : mm.getModel().vars) {
                                        props.add(prop.name);
                                    }
                                }
                            }
                            memberPropSets.put(memberName, props);
                        }

                        // Build x-union-members list with distinguishing properties
                        List<Map<String, Object>> members = new ArrayList<>();
                        for (int i = 0; i < memberNames.size(); i++) {
                            String memberName = memberNames.get(i);
                            Map<String, Object> info = new HashMap<>();
                            info.put("modelName", memberName);

                            if (i < memberNames.size() - 1) {
                                // Find a property unique to this member
                                Set<String> myProps = memberPropSets.getOrDefault(memberName, Collections.emptySet());
                                for (String prop : myProps) {
                                    boolean unique = true;
                                    for (Map.Entry<String, Set<String>> other : memberPropSets.entrySet()) {
                                        if (!other.getKey().equals(memberName) && other.getValue().contains(prop)) {
                                            unique = false;
                                            break;
                                        }
                                    }
                                    if (unique) {
                                        info.put("hasDistinguishingProperty", true);
                                        info.put("distinguishingProperty", prop);
                                        break;
                                    }
                                }
                            }
                            // Last member is always the fallback (no distinguishing property needed)
                            members.add(info);
                        }

                        cm.vendorExtensions.put("x-union-members", members);
                    }
                }

                // Detect allOf intersection types
                if (origSchema != null && origSchema.getAllOf() != null && !origSchema.getAllOf().isEmpty()) {
                    List<String> parentNames = new ArrayList<>();
                    for (Object member : origSchema.getAllOf()) {
                        Schema memberSchema = (Schema) member;
                        if (memberSchema.get$ref() != null) {
                            parentNames.add(toModelName(ModelUtils.getSimpleRef(memberSchema.get$ref())));
                        }
                    }

                    if (!parentNames.isEmpty()) {
                        cm.vendorExtensions.put("x-is-intersection", true);

                        List<Map<String, String>> parents = new ArrayList<>();
                        Set<String> parentPropertyNames = new HashSet<>();
                        for (String parentName : parentNames) {
                            Map<String, String> parentInfo = new HashMap<>();
                            parentInfo.put("modelName", parentName);
                            parents.add(parentInfo);

                            ModelsMap parentModels = result.get(parentName);
                            if (parentModels != null) {
                                for (ModelMap pmm : parentModels.getModels()) {
                                    for (CodegenProperty prop : pmm.getModel().vars) {
                                        parentPropertyNames.add(prop.name);
                                    }
                                }
                            }
                        }
                        cm.vendorExtensions.put("x-intersection-parents", parents);

                        boolean hasOwnProperties = false;
                        for (CodegenProperty prop : cm.vars) {
                            if (!parentPropertyNames.contains(prop.name)) {
                                prop.vendorExtensions.put("x-is-own-property", true);
                                hasOwnProperties = true;
                            }
                        }
                        cm.vendorExtensions.put("x-has-own-properties", hasOwnProperties);
                    }
                }

                // Check if model has properties that reference other object models or enums
                for (CodegenProperty prop : cm.vars) {
                    if (prop.complexType != null && !prop.isArray) {
                        if (enumModelNames.contains(prop.complexType)) {
                            prop.vendorExtensions.put("x-is-enum-ref", true);
                            prop.vendorExtensions.put("x-enum-type", prop.complexType);
                        } else {
                            prop.vendorExtensions.put("x-is-ref", true);
                            prop.vendorExtensions.put("x-ref-type", prop.complexType);
                        }
                    } else if (prop.isArray && prop.items != null && prop.items.complexType != null) {
                        if (enumModelNames.contains(prop.items.complexType)) {
                            prop.vendorExtensions.put("x-is-enum-ref", true);
                            prop.vendorExtensions.put("x-enum-type", prop.items.complexType);
                        } else {
                            prop.vendorExtensions.put("x-is-ref", true);
                            prop.vendorExtensions.put("x-ref-type", prop.items.complexType);
                        }
                    }
                }

                // Schema and DTO names
                cm.vendorExtensions.put("x-schema-name", cm.classname + "Schema");
                cm.vendorExtensions.put("x-dto-name", cm.classname + "Dto");

                // Check if has single-value enum properties (literal types)
                for (CodegenProperty prop : cm.vars) {
                    if (prop.isEnum && prop.allowableValues != null) {
                        List<?> values = (List<?>) prop.allowableValues.get("values");
                        if (values != null && values.size() == 1) {
                            prop.vendorExtensions.put("x-is-literal", true);
                            prop.vendorExtensions.put("x-literal-value", values.get(0).toString());
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Find the discriminator property's literal value in a member model.
     */
    private String findDiscriminatorValue(String modelName, String discriminatorProperty,
                                           Map<String, ModelsMap> allModels) {
        ModelsMap modelsMap = allModels.get(modelName);
        if (modelsMap == null) return null;

        for (ModelMap mo : modelsMap.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty prop : cm.vars) {
                if (prop.baseName.equals(discriminatorProperty) || prop.name.equals(discriminatorProperty)) {
                    if (prop.isEnum && prop.allowableValues != null) {
                        List<?> values = (List<?>) prop.allowableValues.get("values");
                        if (values != null && values.size() == 1) {
                            return values.get(0).toString();
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Add Zod-specific vendor extensions to a model property.
     */
    private void addZodVendorExtensions(CodegenProperty prop, CodegenModel model) {
        String zodType = buildZodPropertyType(prop);
        prop.vendorExtensions.put("x-zod-type", zodType);

        String fullZod = zodType;
        if (!prop.required) {
            fullZod = zodType + ".optional()";
        }
        prop.vendorExtensions.put("x-zod-full", fullZod);
    }

    /**
     * Build a Zod type expression for a model property.
     */
    private String buildZodPropertyType(CodegenProperty prop) {
        // Handle enum properties with allowable values
        if (prop.isEnum && prop.allowableValues != null) {
            List<?> values = (List<?>) prop.allowableValues.get("values");
            if (values != null && values.size() == 1) {
                return "z.literal(\"" + values.get(0) + "\")";
            }
            if (prop.complexType != null) {
                return camelize(prop.complexType) + "Schema";
            }
            if (values != null) {
                StringBuilder sb = new StringBuilder("z.enum([");
                for (int i = 0; i < values.size(); i++) {
                    if (i > 0) sb.append(", ");
                    sb.append("\"").append(values.get(i)).append("\"");
                }
                sb.append("])");
                return sb.toString();
            }
        }

        // Handle arrays (before complexType check)
        if (prop.isArray && prop.items != null) {
            String itemZod = buildZodPropertyType(prop.items);
            return itemZod + ".array()";
        }

        // Handle $ref to another model or enum type
        if (prop.complexType != null) {
            return camelize(prop.complexType) + "Schema";
        }

        // Handle primitive types
        if (prop.isInteger || prop.isLong) {
            StringBuilder sb = new StringBuilder("z.coerce.number().int()");
            if (prop.minimum != null) {
                sb.append(".gte(").append(prop.minimum).append(")");
            }
            if (prop.maximum != null) {
                sb.append(".lte(").append(prop.maximum).append(")");
            }
            return sb.toString();
        }

        if (prop.isNumber || prop.isFloat || prop.isDouble) {
            StringBuilder sb = new StringBuilder("z.number()");
            if (prop.minimum != null) {
                sb.append(".gte(").append(prop.minimum).append(")");
            }
            if (prop.maximum != null) {
                sb.append(".lte(").append(prop.maximum).append(")");
            }
            return sb.toString();
        }

        if (prop.isBoolean) {
            return "z.boolean()";
        }

        if (prop.isString) {
            StringBuilder sb = new StringBuilder("z.string()");
            if (prop.maxLength != null) {
                sb.append(".max(").append(prop.maxLength).append(")");
            }
            if (prop.minLength != null) {
                sb.append(".min(").append(prop.minLength).append(")");
            }
            return sb.toString();
        }

        return "z.any()";
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        bundle = super.postProcessSupportingFileData(bundle);

        List<OperationsMap> allOps = ((org.openapitools.codegen.model.ApiInfoMap) bundle.get("apiInfo")).getApis();

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
                    opInfo.put("bodyParam", body);
                    opInfo.put("hasBodyParam", true);
                } else {
                    opInfo.put("hasBodyParam", false);
                }

                // Response info
                opInfo.put("responseType", op.returnType);
                opInfo.put("responseSchema", op.vendorExtensions.get("x-response-schema"));
                if (op.returnType != null) {
                    opInfo.put("responseDtoMapper", "mapFrom" + op.returnType + "Dto");
                    opInfo.put("responseDtoType", op.returnType + "Dto");
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

                // Body DTO mapper
                if (op.bodyParam != null) {
                    opInfo.put("bodyDtoMapper", "mapTo" + op.bodyParam.dataType + "Dto");
                }

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
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> ops = (List<Map<String, Object>>) svc.get("operations");
            allOperations.addAll(ops);
        }
        allOperations.sort(Comparator.comparing(o -> (String) o.get("operationId")));
        bundle.put("allOperations", allOperations);

        boolean anySSE = allOperations.stream()
                .anyMatch(op -> Boolean.TRUE.equals(op.get("hasSSE")));
        bundle.put("hasAnySSE", anySSE);

        // Topologically sort models
        @SuppressWarnings("unchecked")
        List<Object> models = (List<Object>) bundle.get("models");
        if (models != null) {
            bundle.put("models", topologicallySortModels(models));
        }

        return bundle;
    }

    /**
     * Topologically sort models so that leaf types come before models that reference them.
     */
    @SuppressWarnings("unchecked")
    private List<Object> topologicallySortModels(List<Object> models) {
        Map<String, Object> modelByName = new LinkedHashMap<>();
        Map<String, Set<String>> dependencies = new LinkedHashMap<>();

        for (Object modelEntry : models) {
            Map<String, Object> entry = (Map<String, Object>) modelEntry;
            CodegenModel cm = (CodegenModel) entry.get("model");
            if (cm == null) continue;

            modelByName.put(cm.classname, modelEntry);

            Set<String> deps = new LinkedHashSet<>();
            for (CodegenProperty prop : cm.vars) {
                if (prop.complexType != null) {
                    deps.add(prop.complexType);
                }
                if (prop.isArray && prop.items != null && prop.items.complexType != null) {
                    deps.add(prop.items.complexType);
                }
            }
            if (cm.oneOf != null) {
                deps.addAll(cm.oneOf);
            }
            // Also include non-discriminated union member dependencies
            if (cm.vendorExtensions.containsKey("x-union-members")) {
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> unionMembers =
                    (List<Map<String, Object>>) cm.vendorExtensions.get("x-union-members");
                for (Map<String, Object> member : unionMembers) {
                    deps.add((String) member.get("modelName"));
                }
            }
            // Also include allOf intersection parent dependencies
            if (cm.vendorExtensions.containsKey("x-intersection-parents")) {
                @SuppressWarnings("unchecked")
                List<Map<String, String>> intersectionParents =
                    (List<Map<String, String>>) cm.vendorExtensions.get("x-intersection-parents");
                for (Map<String, String> parent : intersectionParents) {
                    deps.add(parent.get("modelName"));
                }
            }
            deps.remove(cm.classname);
            dependencies.put(cm.classname, deps);
        }

        List<Object> sorted = new ArrayList<>();
        Set<String> visited = new LinkedHashSet<>();
        Set<String> visiting = new LinkedHashSet<>();

        for (String name : modelByName.keySet()) {
            if (!visited.contains(name)) {
                topoVisit(name, modelByName, dependencies, visited, visiting, sorted);
            }
        }

        return sorted;
    }

    private void topoVisit(String name, Map<String, Object> modelByName,
                            Map<String, Set<String>> dependencies,
                            Set<String> visited, Set<String> visiting, List<Object> sorted) {
        if (visited.contains(name)) return;
        if (visiting.contains(name)) return; // cycle detected, break it
        visiting.add(name);

        Set<String> deps = dependencies.getOrDefault(name, Collections.emptySet());
        for (String dep : deps) {
            if (modelByName.containsKey(dep)) {
                topoVisit(dep, modelByName, dependencies, visited, visiting, sorted);
            }
        }

        visiting.remove(name);
        visited.add(name);
        if (modelByName.containsKey(name)) {
            sorted.add(modelByName.get(name));
        }
    }
}
