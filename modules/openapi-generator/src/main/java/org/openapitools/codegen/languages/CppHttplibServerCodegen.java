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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.CamelizeOption;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;

/**
 * C++ HTTP Library Server Code Generator.
 * This code generator creates C++ server stubs using the httplib library for handling HTTP requests.
 * It generates:
 * - Model classes with proper C++ typing (std::string, std::vector, std::optional, etc.)
 * - API handler interfaces with type-safe request/response handling
 * - CMake build configuration
 * - JSON serialization/deserialization using nlohmann::json
 * Key features:
 * - Supports nullable types using std::optional
 * - Proper namespace organization for models and APIs
 * - Status code to error type mapping for responses
 * - Standard C++ container types (vector, map, etc.)
 * - Automatic include generation for dependencies
 *
 * @author OpenAPI Generator Contributors
 */
public class CppHttplibServerCodegen extends AbstractCppCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CppHttplibServerCodegen.class);

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        // Check if any security is defined in the spec
        boolean hasAnySecurity = (openAPI.getComponents() != null
                && openAPI.getComponents().getSecuritySchemes() != null
                && !openAPI.getComponents().getSecuritySchemes().isEmpty());
        additionalProperties.put("hasAuthMethods", hasAnySecurity);

        // Process all paths to enhance inline schemas with meaningful titles
        if (openAPI.getPaths() != null) {
            for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
                PathItem pathItem = pathEntry.getValue();
                if (pathItem.readOperations() != null) {
                    for (Operation operation : pathItem.readOperations()) {
                        String operationId = operation.getOperationId() != null ? operation.getOperationId() : pathEntry.getKey();

                        // Process request body schema
                        if (operation.getRequestBody() != null && operation.getRequestBody().getContent() != null) {
                            for (MediaType mediaType : operation.getRequestBody().getContent().values()) {
                                if (mediaType.getSchema() != null && mediaType.getSchema().getTitle() == null) {
                                    mediaType.getSchema().setTitle(toPascalCase(operationId) + "Request");
                                    processNestedSchemas(mediaType.getSchema(), toPascalCase(operationId) + "Request");
                                }
                            }
                        }

                        // Process response schemas
                        if (operation.getResponses() != null) {
                            for (Map.Entry<String, ApiResponse> respEntry : operation.getResponses().entrySet()) {
                                ApiResponse response = respEntry.getValue();
                                if (response.getContent() != null) {
                                    for (MediaType mediaType : response.getContent().values()) {
                                        if (mediaType.getSchema() != null && mediaType.getSchema().getTitle() == null) {
                                            // Format: {OperationId}{StatusCode}Response (e.g., TestHeaderParameters200Response)
                                            String responseTitle = toPascalCase(operationId) + respEntry.getKey() + "Response";
                                            mediaType.getSchema().setTitle(responseTitle);
                                            processNestedSchemas(mediaType.getSchema(), responseTitle);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static final String PROJECT_NAME = "projectName";
    private static final String DEFAULT_PROJECT_NAME = "cpp-httplib-server";

    public static final String MODEL_NAMESPACE = "modelNamespace";
    public static final String API_NAMESPACE = "apiNamespace";
    public static final String ENUM_NAMESPACE = "enumNamespace";
    public static final String MODEL_SUFFIX = "models";
    public static final String API_SUFFIX = "api";
    public static final String ENUM_SUFFIX = "enum";
    public static final String INCLUDE_VARIANT = "#include <variant>";
    public static final String INCLUDE_OPTIONAL = "#include <optional>";
    public static final String HTTP_RESPONSE_PREFIX = "HTTP_RESPONSE_CODE_";
    public static final String ADD_API_IMPL_STUBS = "addApiImplStubs";
    public static final String ENUM_FROM_STRING = "EnumFromString";
    public static final String ENUM_TO_STRING = "EnumToString";
    public static final String PRIMITIVE_FROM_STRING = "PrimitiveFromString";
    public static final String PRIMITIVE_TO_STRING = "PrimitiveToString";

    // Standard includes for mapped C++ types
    private static final Map<String, String> standardIncludes = Map.ofEntries(
            Map.entry("std::string", "#include <string>"),
            Map.entry("std::vector", "#include <vector>"),
            Map.entry("std::map", "#include <map>"),
            Map.entry("std::set", "#include <set>"),
            Map.entry("std::list", "#include <list>"),
            Map.entry("std::unordered_map", "#include <unordered_map>"),
            Map.entry("std::unordered_set", "#include <unordered_set>"),
            Map.entry("std::deque", "#include <deque>"),
            Map.entry("std::queue", "#include <queue>"),
            Map.entry("std::stack", "#include <stack>"),
            Map.entry("std::pair", "#include <utility>"),
            Map.entry("std::tuple", "#include <tuple>"),
            Map.entry("std::optional", "#include <optional>"),
            Map.entry("std::any", "#include <any>"),
            Map.entry("std::variant", "#include <variant>")
    );

    // Numeric types for <cstdint> include
    private static final Set<String> cstdintTypes = Set.of(
            "byte", "int", "integer", "long", "short", "unsigned int", "unsigned long",
            "unsigned short", "size_t", "ssize_t", "ptrdiff_t", "double", "float",
            "boolean", "bool", "char", "wchar_t", "char16_t", "char32_t"
    );

    /**
     * Constructor for CppHttplibServerCodegen.
     * Initializes the code generator with C++ httplib server specific configurations,
     * type mappings, and template files.
     */
    public CppHttplibServerCodegen() {
        super();
        embeddedTemplateDir = templateDir = "cpp-httplib-server";

        // Enable inline schema options to prevent schema reuse
        // This ensures each operation gets its own uniquely named response model
        inlineSchemaOption.put("SKIP_SCHEMA_REUSE", "true");

        // Map OpenAPI types/formats to C++ types (always use std:: prefix)
        typeMapping.put("integer", "int");
        typeMapping.put("long", "long");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        // Default: OpenAPI 'number' → 'double', unless format is 'float'
        typeMapping.put("number", "double");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "std::string");
        typeMapping.put("byte", "unsigned char");
        typeMapping.put("ByteArray", "std::vector<uint8_t>");
        typeMapping.put("binary", "std::string");
        typeMapping.put("date", "std::string");
        typeMapping.put("date-time", "std::string");
        typeMapping.put("password", "std::string");
        typeMapping.put("object", "nlohmann::json");
        typeMapping.put("array", "std::vector");
        typeMapping.put("file", "std::string");
        typeMapping.put("oas_any_type_not_mapped", "nlohmann::json");

        // Only mapped C++ types as primitives
        languageSpecificPrimitives.addAll(Arrays.asList(
                "int", "long", "float", "double", "bool", "char", "unsigned int", "unsigned long", "unsigned char", "size_t", "void",
                "std::string", "std::vector", "std::map", "std::set", "std::list", "std::unordered_map", "std::unordered_set", "std::deque", "std::queue", "std::stack", "std::pair", "std::tuple", "std::optional", "std::any", "std::variant"
        ));

        // Configure template files for code generation
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        // Add CLI options
        cliOptions.add(org.openapitools.codegen.CliOption.newBoolean(ADD_API_IMPL_STUBS,
                "Generate API implementation stubs and a sample main.cpp for quick start").defaultValue("false"));

        // Add supporting files for project structure
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("License.mustache", "", "LICENSE"));

        // Initialize feature set for httplib server
        // Note: Polymorphism is partially supported:
        //   ✅ allOf (inheritance/composition)
        //   ✅ anyOf/oneOf (std::variant)
        //   ❌ discriminator (not implemented)
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
        );

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Returns the name identifier for this code generator.
     *
     * @return the generator name
     */
    @Override
    public String getName() {
        return DEFAULT_PROJECT_NAME;
    }

    /**
     * Returns a brief description of what this code generator does.
     *
     * @return help text describing the generator's purpose
     */
    @Override
    public String getHelp() {
        return "Generates a C++ server using the httplib library.";
    }

    /**
     * Converts a model name to its corresponding import statement.
     * Handles both standard C++ types and custom model types.
     *
     * @param name the model name to import
     * @return the appropriate #include directive or empty string
     */
    @Override
    public String toModelImport(String name) {
        // Skip if already formatted as an include
        String includeFile;
        if (name.startsWith("#include")) {
            includeFile = null;
        }
        // Use import mapping if available
        if (importMapping.containsKey(name)) {
            includeFile = importMapping.get(name);
        }
        // Map OpenAPI type to C++ type if possible
        String mappedType = typeMapping.getOrDefault(name, name);
        if (languageSpecificPrimitives.contains(mappedType)) {
            String include = getStandardIncludeForType(mappedType);
            includeFile = include != null ? include : "";
        } else {
            Map<String, String> pathFromClassName = stripPathFromClassName(mappedType);
            includeFile = "#include \"" + pathFromClassName.get("className") + ".h\"";
        }
        return includeFile;
    }

    /**
     * Returns the appropriate #include directive for a given C++ standard type.
     *
     * @param typeName the C++ type name
     * @return the include directive or null if no include is needed
     */
    private String getStandardIncludeForType(String typeName) {
        String mappedType = typeMapping.getOrDefault(typeName, typeName);
        if (standardIncludes.containsKey(mappedType)) {
            return standardIncludes.get(mappedType);
        }
        if (cstdintTypes.contains(typeName)) {
            return "#include <cstdint>";
        } else if (cstdintTypes.contains(mappedType)) {
            return "#include <cstdint>";
        }
        return null;
    }

    /**
     * Post-processes operations after model processing to add C++ specific configurations.
     * This method:
     * - Sets up class names and namespaces for API classes
     * - Processes request and response models for each operation
     * - Maps response status codes to their corresponding types
     * - Collects all used models for proper include generation
     *
     * @param objs      the operations map to process
     * @param allModels list of all models in the API
     * @return the processed operations map
     */

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {

        // No allParams logic; only expose requestModel, queryParams, headerParams as vendorExtensions
        if (objs == null || objs.getOperations() == null) {
            LOGGER.warn("Operations or operations map is null");
            return objs;
        }
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();
        // Per-operation unique status code constants
        // Set classname and compute apiNamespace ONCE per API class
        String classname = operations.getClassname();
        if ("DefaultApi".equals(classname) || classname == null || classname.isEmpty()) {
            classname = addProjectOrDefaultName(classname, "DefaultApi");
            operations.setClassname(classname);
        }
        objs.put("apiHeaderFileName", toPascalCase(classname) + toPascalCase(API_SUFFIX) + ".h");
        objs.put("apiClassnameInPascalCase", toPascalCase(classname));

        // Compute API namespace ONCE, append classname only if not already present as last segment
        String apiNamespace = (String) additionalProperties.get(API_NAMESPACE);
        if (apiNamespace == null || apiNamespace.isEmpty()) {
            apiNamespace = API_SUFFIX.toLowerCase(Locale.ROOT);
        }

        objs.put("apiNamespace", apiNamespace);

        // Build a lookup map: PascalCase className -> modelNamespace
        Map<String, String> modelNamespaceMap = new HashMap<>();
        for (ModelMap modelMap : allModels) {
            CodegenModel model = modelMap.getModel();
            if (model != null && model.vendorExtensions.containsKey("modelNamespace")) {
                modelNamespaceMap.put(model.classname, (String) model.vendorExtensions.get("modelNamespace"));
            }
        }

        // Track all models used for includes
        Set<String> modelsUsed = new HashSet<>();
        List<Map<String, String>> opStatusCodeConsts = new ArrayList<>();
        boolean includeOptionalHeader = false;
        for (CodegenOperation op : operationList) {
            boolean hasPrimitiveParams = false;
            Set<String> seenSuccessTypes = new HashSet<>();
            if (op.vendorExtensions == null) {
                op.vendorExtensions = new HashMap<>();
            }
            // Add modelNamespace to vendorExtensions
            String modelNamespace = (String) additionalProperties.get(MODEL_NAMESPACE);
            final String apiNamespacetoFilter = apiNamespace;
            //Filter common words in api and model namespaces
            String namespaceFiltered = Arrays.stream(modelNamespace.toLowerCase(Locale.ROOT).split("::"))
                    .filter(word -> !Arrays.asList(apiNamespacetoFilter.toLowerCase(Locale.ROOT).split("::")).contains(word))
                    .collect(Collectors.joining("::"));
            // Handle request model
            if (op.bodyParam != null && op.bodyParam.baseType != null) {
                String className = op.bodyParam.baseType;
                String requestModel = toPascalCase(className);
                op.vendorExtensions.put("requestModel", requestModel);

                // Set bodyParam dataType with namespace prefix
                if (!namespaceFiltered.isEmpty()) {
                    op.vendorExtensions.put("requestModelNamespace", namespaceFiltered);
                    op.bodyParam.dataType = namespaceFiltered + "::" + requestModel;
                } else {
                    op.bodyParam.dataType = requestModel;
                }

                // Wrap in std::optional if not required
                if (!op.bodyParam.required) {
                    op.bodyParam.dataType = "std::optional<" + op.bodyParam.dataType + ">";
                }

                includeOptionalHeader = true;
                modelsUsed.add(requestModel);
            }
            // Add type flags for query and header params for template type conversion
            if (op.queryParams != null) {
                for (CodegenParameter qp : op.queryParams) {
//                    setCppTypeFlags(qp);
                    if (!hasPrimitiveParams && Boolean.TRUE.equals(qp.vendorExtensions.get("isPrimitive"))) {
                        hasPrimitiveParams = true;
                    }
                }
            }
            if (op.headerParams != null) {
                for (CodegenParameter hp : op.headerParams) {
//                    setCppTypeFlags(hp);
                    if (!hasPrimitiveParams && Boolean.TRUE.equals(hp.vendorExtensions.get("isPrimitive"))) {
                        hasPrimitiveParams = true;
                    }
                }
            }
            op.vendorExtensions.put("hasPrimitiveParams", hasPrimitiveParams);
            op.vendorExtensions.put("operationIdPascalCase", toHandlerFunctionName(op.httpMethod, op.path.toString(), false));
            op.vendorExtensions.put("httpMethod", op.httpMethod != null ? toPascalCase(op.httpMethod) : "");
            op.vendorExtensions.put("handlerFunctionName", toHandlerFunctionName(op.httpMethod, op.path.toString(), true));
            op.vendorExtensions.put("requestType", toHandlerFunctionRequest(op.path.toString(), op.httpMethod));
            op.vendorExtensions.put("responseType", toHandlerFunctionResponse(op.path.toString(), op.httpMethod));
            if (op.path != null) {
                op.vendorExtensions.put("path", op.path);
            }

            // Expose query, header, path, and cookie parameters for mustache
            if (op.queryParams == null) op.queryParams = new ArrayList<>();
            if (op.headerParams == null) op.headerParams = new ArrayList<>();
            if (op.pathParams == null) op.pathParams = new ArrayList<>();
            if (op.cookieParams == null) op.cookieParams = new ArrayList<>();

            // Track path parameter indices for regex matching in C++
            int pathParamIndex = 1; // Start at 1 for regex match groups

            // Add style/explode/allowReserved to each param
            for (CodegenParameter param : op.queryParams) {
                param.vendorExtensions.put("style", param.style);
                param.vendorExtensions.put("explode", param.isExplode);
                // Set parameter type flags
                setParameterTypeFlags(param);

                // Track models used in parameters (for includes and namespace prefix)
                if (param.baseType != null && !param.isPrimitiveType && !languageSpecificPrimitives.contains(param.baseType)) {
                    String className = toPascalCase(param.baseType);
                    modelsUsed.add(className);
                    // Add namespace prefix to parameter dataType if it's a model and doesn't already have namespace
                    // Extract inner type from containers like std::optional<T> and std::vector<T>
                    String innerType = param.dataType;
                    if (param.dataType.contains("<")) {
                        int startIdx = param.dataType.indexOf('<') + 1;
                        int endIdx = param.dataType.lastIndexOf('>');
                        if (startIdx > 0 && endIdx > startIdx) {
                            innerType = param.dataType.substring(startIdx, endIdx).trim();
                        }
                    }
                    if (!innerType.contains("::") && !languageSpecificPrimitives.contains(innerType)) {
                        String prefixedType = namespaceFiltered + "::" + innerType;
                        param.dataType = param.dataType.replace(innerType, prefixedType);
                    }
                } else if (param.dataType != null && param.dataType.contains("<")) {
                    // Additional handling: extract model from dataType when baseType might not be set
                    // This handles cases like std::optional<SomeModel> where baseType might be incorrectly set
                    String innerType = param.dataType;
                    int startIdx = param.dataType.indexOf('<') + 1;
                    int endIdx = param.dataType.lastIndexOf('>');
                    if (startIdx > 0 && endIdx > startIdx) {
                        innerType = param.dataType.substring(startIdx, endIdx).trim();
                        // Check if this looks like a model class (PascalCase)
                        if (!languageSpecificPrimitives.contains(innerType) &&
                                !innerType.startsWith("std::") &&
                                !innerType.contains("::") &&
                                innerType.length() > 0 &&
                                Character.isUpperCase(innerType.charAt(0))) {
                            modelsUsed.add(innerType);
                            // Add namespace prefix
                            String prefixedType = namespaceFiltered + "::" + innerType;
                            param.dataType = param.dataType.replace(innerType, prefixedType);
                        }
                    }
                }

                // Extract unwrapped type for JSON deserialization AFTER namespace has been added
                // For std::optional<T>, we need T for the .get<T>() call
                String unwrappedType = param.dataType;
                if (param.dataType != null && param.dataType.contains("<")) {
                    int startIdx = param.dataType.indexOf('<') + 1;
                    int endIdx = param.dataType.lastIndexOf('>');
                    if (startIdx > 0 && endIdx > startIdx) {
                        unwrappedType = param.dataType.substring(startIdx, endIdx).trim();
                    }
                }
                param.vendorExtensions.put("unwrappedDataType", unwrappedType);
            }
            for (CodegenParameter param : op.headerParams) {
                param.vendorExtensions.put("style", param.style);
                param.vendorExtensions.put("explode", param.isExplode);
                setParameterTypeFlags(param);
                // Track models used in parameters
                if (param.baseType != null && !param.isPrimitiveType && !languageSpecificPrimitives.contains(param.baseType)) {
                    String className = toPascalCase(param.baseType);
                    modelsUsed.add(className);
                    // Extract inner type from containers
                    String innerType = param.dataType;
                    if (param.dataType.contains("<")) {
                        int startIdx = param.dataType.indexOf('<') + 1;
                        int endIdx = param.dataType.lastIndexOf('>');
                        if (startIdx > 0 && endIdx > startIdx) {
                            innerType = param.dataType.substring(startIdx, endIdx).trim();
                        }
                    }
                    if (!innerType.contains("::") && !languageSpecificPrimitives.contains(innerType)) {
                        String prefixedType = namespaceFiltered + "::" + innerType;
                        param.dataType = param.dataType.replace(innerType, prefixedType);
                    }
                }
            }
            for (CodegenParameter param : op.pathParams) {
                param.vendorExtensions.put("style", param.style);
                param.vendorExtensions.put("explode", param.isExplode);
                param.vendorExtensions.put("pathIndex", pathParamIndex);
                pathParamIndex++;
                setParameterTypeFlags(param);
                // Track models used in parameters
                if (param.baseType != null && !param.isPrimitiveType && !languageSpecificPrimitives.contains(param.baseType)) {
                    String className = toPascalCase(param.baseType);
                    modelsUsed.add(className);
                    // Extract inner type from containers
                    String innerType = param.dataType;
                    if (param.dataType.contains("<")) {
                        int startIdx = param.dataType.indexOf('<') + 1;
                        int endIdx = param.dataType.lastIndexOf('>');
                        if (startIdx > 0 && endIdx > startIdx) {
                            innerType = param.dataType.substring(startIdx, endIdx).trim();
                        }
                    }
                    if (!innerType.contains("::") && !languageSpecificPrimitives.contains(innerType)) {
                        String prefixedType = namespaceFiltered + "::" + innerType;
                        param.dataType = param.dataType.replace(innerType, prefixedType);
                    }
                }
            }
            for (CodegenParameter param : op.cookieParams) {
                param.vendorExtensions.put("style", param.style);
                param.vendorExtensions.put("explode", param.isExplode);
                setParameterTypeFlags(param);
                // Track models used in parameters
                if (param.baseType != null && !param.isPrimitiveType && !languageSpecificPrimitives.contains(param.baseType)) {
                    String className = toPascalCase(param.baseType);
                    modelsUsed.add(className);
                    // Extract inner type from containers
                    String innerType = param.dataType;
                    if (param.dataType.contains("<")) {
                        int startIdx = param.dataType.indexOf('<') + 1;
                        int endIdx = param.dataType.lastIndexOf('>');
                        if (startIdx > 0 && endIdx > startIdx) {
                            innerType = param.dataType.substring(startIdx, endIdx).trim();
                        }
                    }
                    if (!innerType.contains("::") && !languageSpecificPrimitives.contains(innerType)) {
                        String prefixedType = namespaceFiltered + "::" + innerType;
                        param.dataType = param.dataType.replace(innerType, prefixedType);
                    }
                }
            }
            op.vendorExtensions.put("queryParams", op.queryParams);
            op.vendorExtensions.put("headerParams", op.headerParams);
            op.vendorExtensions.put("pathParams", op.pathParams);
            op.vendorExtensions.put("cookieParams", op.cookieParams);

            // Process security requirements
            processSecurityRequirements(op);

            // Process responses
            List<String> errorTypes = new ArrayList<>();
            Set<String> successTypesSet = new LinkedHashSet<>();
            Set<String> allResponseTypesSet = new LinkedHashSet<>();
            List<Map<String, Object>> successCodeToTypes = new ArrayList<>();
            List<Map<String, Object>> errorCodeToTypes = new ArrayList<>();
            boolean isSuccessResponseType = false;
            boolean isSuccessResponsePrimitive = false;
            boolean isErrorResponsePrimitive = false;
            boolean hasVoidResponse = false;

            for (CodegenResponse resp : op.responses) {
                boolean hasAnyResponseSchema = false;

                if (resp.code != null) {
                    // Check if response has no schema (void response)
                    if (resp.baseType == null || resp.baseType.isEmpty()) {
                        hasVoidResponse = true;
                        continue;
                    }

                    // Collect all 2xx response types as successTypes
                    if (resp.code.startsWith("2") && resp.baseType != null) {
                        hasAnyResponseSchema = true;
                        String successType;
                        String successConstName;
                        if (!typeMapping.containsKey(resp.baseType)) {
                            String className = toPascalCase(resp.baseType);
                            successType = namespaceFiltered + "::" + className;
                            modelsUsed.add(className);
                            successConstName = HTTP_RESPONSE_PREFIX + StringUtils.underscore(className).toUpperCase(Locale.ROOT);
                        } else {
                            successType = typeMapping.get(resp.baseType);
                            successConstName = HTTP_RESPONSE_PREFIX + "PRIMITIVE_" + StringUtils.underscore(resp.baseType).toUpperCase(Locale.ROOT);
                            isSuccessResponsePrimitive = true;
                        }
                        // Only add if not already in set (deduplication across all responses)
                        successTypesSet.add(successType);
                        if (allResponseTypesSet.add(successType)) {
                            if (successConstName != null && !successConstName.isEmpty() && successType != null && !successType.isEmpty()) {
                                final String finalSuccessConstName = successConstName;
                                boolean successConstExists = opStatusCodeConsts.stream()
                                        .anyMatch(listitem -> finalSuccessConstName.equals(listitem.get("constName")));
                                if (!successConstExists) {
                                    Map<String, String> item = new HashMap<>();
                                    item.put("constName", successConstName);
                                    item.put("statusCode", resp.code);
                                    opStatusCodeConsts.add(item);
                                }
                                Map<String, Object> successItem = new HashMap<>();
                                successItem.put("successType", successType);
                                successItem.put("successConstName", successConstName);
                                successCodeToTypes.add(successItem);
                            }
                        }
                    } else {
                        String errorBaseType = resp.baseType;
                        String errorConstName = "";
                        if (errorBaseType != null && !errorBaseType.isEmpty()) {
                            hasAnyResponseSchema = true;
                            String errorType = "";
                            if (!typeMapping.containsKey(errorBaseType)) {
                                String className = toPascalCase(errorBaseType);
                                errorType = namespaceFiltered + "::" + className;
                                modelsUsed.add(className);
                                errorConstName = HTTP_RESPONSE_PREFIX + StringUtils.underscore(className).toUpperCase(Locale.ROOT);
                                // errorTypeIsEnum = Boolean.TRUE.equals(classNameIsEnumMap.get(toPascalCase(className)));
                            } else {
                                isErrorResponsePrimitive = true;
                                errorType = typeMapping.get(resp.baseType);
                                errorConstName = HTTP_RESPONSE_PREFIX + "PRIMITIVE_" + StringUtils.underscore(resp.baseType).toUpperCase(Locale.ROOT);
                            }

                            if (errorConstName != null && !errorConstName.isEmpty()) {
                                errorTypes.add(errorType);
                                op.vendorExtensions.put("errorTypes", errorTypes);
                                // Only add to errorCodeToTypes if not already used in any response
                                if (allResponseTypesSet.add(errorType)) {
                                    final String finalErrorConstName = errorConstName;
                                    boolean errorConstExists = opStatusCodeConsts.stream()
                                            .anyMatch(listitem -> finalErrorConstName.equals(listitem.get("constName")));
                                    if (!errorConstName.isEmpty() && !errorConstExists) {
                                        Map<String, String> item = new HashMap<>();
                                        item.put("constName", errorConstName);
                                        item.put("statusCode", resp.code);
                                        opStatusCodeConsts.add(item);
                                    }
                                    Map<String, Object> errorItem = new HashMap<>();
                                    errorItem.put("errorType", errorType);
                                    errorItem.put("errorConstName", errorConstName);
                                    // errorItem.put("errorTypeIsEnum", errorTypeIsEnum);
                                    errorCodeToTypes.add(errorItem);
                                }
                            }
                        }
                    }
                    if (hasAnyResponseSchema) {
                        op.vendorExtensions.put("hasAnyResponseSchema", true);
                    }
                }
            }

            op.vendorExtensions.put("hasVoidResponse", hasVoidResponse);
            op.vendorExtensions.put("isSuccessResponseType", isSuccessResponseType);
            op.vendorExtensions.put("isSuccessResponsePrimitive", isSuccessResponsePrimitive);
            op.vendorExtensions.put("isErrorResponsePrimitive", isErrorResponsePrimitive);

            // Convert Set to List for template
            if (!successTypesSet.isEmpty()) {
                op.vendorExtensions.put("successTypes", new ArrayList<>(successTypesSet));
            }
            if (successCodeToTypes != null && !successCodeToTypes.isEmpty()) {
                op.vendorExtensions.put("successCodeToTypes", successCodeToTypes);
            }
            if (errorCodeToTypes != null && !errorCodeToTypes.isEmpty()) {
                op.vendorExtensions.put("errorCodeToTypes", errorCodeToTypes);
            }

            // Check if we have only one response type (to avoid unnecessary std::variant)
            int totalResponseTypes = successCodeToTypes.size() + errorCodeToTypes.size();
            if (totalResponseTypes == 1) {
                op.vendorExtensions.put("hasSingleResponseType", true);
                // Store the single response type
                if (!successCodeToTypes.isEmpty()) {
                    op.vendorExtensions.put("singleResponseType", successCodeToTypes.get(0).get("successType"));
                } else if (!errorCodeToTypes.isEmpty()) {
                    op.vendorExtensions.put("singleResponseType", errorCodeToTypes.get(0).get("errorType"));
                }
            } else {
                op.vendorExtensions.put("hasSingleResponseType", false);
            }

            boolean hasRequestSchema = (op.queryParams != null && !op.queryParams.isEmpty()) ||
                    (op.headerParams != null && !op.headerParams.isEmpty()) ||
                    (op.cookieParams != null && !op.cookieParams.isEmpty()) ||
                    (op.pathParams != null && !op.pathParams.isEmpty()) ||
                    (op.bodyParams != null && !op.bodyParams.isEmpty());
            op.vendorExtensions.put("hasAnyRequestSchema", hasRequestSchema);

            // Check if operation has any array parameters (for sstream include)
            boolean hasArrayParams = false;
            for (CodegenParameter param : op.allParams) {
                if (param.isArray) {
                    hasArrayParams = true;
                    break;
                }
            }
            op.vendorExtensions.put("hasArrayParams", hasArrayParams);
        }

        // Aggregate security settings from all operations in this API
        // Also check if ANY operation actually uses auth (hasAuth flag)
        boolean hasApiKeyAuth = false;
        boolean hasBearerAuth = false;
        boolean hasBasicAuth = false;
        boolean hasOAuth2 = false;
        boolean anyOperationUsesAuth = false;
        List<Map<String, Object>> apiKeyAuthList = new ArrayList<>();
        List<Map<String, Object>> bearerAuthList = new ArrayList<>();
        List<Map<String, Object>> basicAuthList = new ArrayList<>();
        List<Map<String, Object>> oauth2AuthList = new ArrayList<>();
        Set<String> seenApiKeys = new HashSet<>();
        Set<String> seenBearers = new HashSet<>();
        Set<String> seenBasics = new HashSet<>();
        Set<String> seenOAuth2s = new HashSet<>();

        for (CodegenOperation op : operationList) {
            // Check if this operation uses auth
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasAuth"))) {
                anyOperationUsesAuth = true;
            }

            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasApiKeyAuth"))) {
                hasApiKeyAuth = true;
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> opApiKeys = (List<Map<String, Object>>) op.vendorExtensions.get("apiKeyAuth");
                if (opApiKeys != null) {
                    for (Map<String, Object> auth : opApiKeys) {
                        String name = (String) auth.get("name");
                        if (name != null && seenApiKeys.add(name)) {
                            apiKeyAuthList.add(auth);
                        }
                    }
                }
            }
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasBearerAuth"))) {
                hasBearerAuth = true;
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> opBearers = (List<Map<String, Object>>) op.vendorExtensions.get("bearerAuth");
                if (opBearers != null) {
                    for (Map<String, Object> auth : opBearers) {
                        String name = (String) auth.get("name");
                        if (name != null && seenBearers.add(name)) {
                            bearerAuthList.add(auth);
                        }
                    }
                }
            }
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasBasicAuth"))) {
                hasBasicAuth = true;
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> opBasics = (List<Map<String, Object>>) op.vendorExtensions.get("basicAuth");
                if (opBasics != null) {
                    for (Map<String, Object> auth : opBasics) {
                        String name = (String) auth.get("name");
                        if (name != null && seenBasics.add(name)) {
                            basicAuthList.add(auth);
                        }
                    }
                }
            }
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasOAuth2"))) {
                hasOAuth2 = true;
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> opOAuth2s = (List<Map<String, Object>>) op.vendorExtensions.get("oauth2Auth");
                if (opOAuth2s != null) {
                    for (Map<String, Object> auth : opOAuth2s) {
                        String name = (String) auth.get("name");
                        if (name != null && seenOAuth2s.add(name)) {
                            oauth2AuthList.add(auth);
                        }
                    }
                }
            }
        }

        // Add aggregated security settings to objs for the API class
        // Only add them if at least one operation actually uses authentication
        boolean hasAnyAuth = false;
        if (anyOperationUsesAuth) {
            hasAnyAuth = true;
            if (hasApiKeyAuth) {
                objs.put("hasApiKeyAuth", true);
                objs.put("apiKeyAuth", apiKeyAuthList);
            }
            if (hasBearerAuth) {
                objs.put("hasBearerAuth", true);
                objs.put("bearerAuth", bearerAuthList);
            }
            if (hasBasicAuth) {
                objs.put("hasBasicAuth", true);
                objs.put("basicAuth", basicAuthList);
            }
            if (hasOAuth2) {
                objs.put("hasOAuth2", true);
                objs.put("oauth2Auth", oauth2AuthList);
            }
        }
        // Only set hasAnyAuth if at least one operation actually uses auth
        if (hasAnyAuth) {
            objs.put("hasAnyAuth", true);
        }

        Object objApiStubs = additionalProperties.get(ADD_API_IMPL_STUBS);
        boolean addApiImplStubs = false;
        if (objApiStubs instanceof Boolean) {
            addApiImplStubs = (Boolean) objApiStubs;
        }
        if (objApiStubs instanceof String) {
            addApiImplStubs = Boolean.parseBoolean((String) objApiStubs);
        }
        LOGGER.debug("Generating Api stubs in the source file:{}.", addApiImplStubs);
        if (addApiImplStubs == true) {
            objs.put("addApiImplStubs", true);
        }

        // Create a map from status code to constant name for easy lookup in templates
        Map<String, String> statusCodeToConstMap = new HashMap<>();
        for (Map<String, String> item : opStatusCodeConsts) {
            String code = item.get("statusCode");
            String constName = item.get("constName");
            // Store the first constant found for each code (prefer spec-defined ones)
            if (!statusCodeToConstMap.containsKey(code)) {
                statusCodeToConstMap.put(code, constName);
            }
        }

        // Determine which common status codes are actually needed
        boolean needsNoContent = false;      // 204 - used for void responses
        boolean needsBadRequest = false;     // 400 - used for parameter validation
        boolean needsUnauthorized = false;   // 401 - used for authentication failures
        boolean needsInternalError = false;  // 500 - used for JSON parsing errors and response handler fallback

        for (CodegenOperation op : operationList) {
            // 204 NO_CONTENT: needed if any operation has void response
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasVoidResponse"))) {
                needsNoContent = true;
            }
            // 400 BAD_REQUEST: needed if any operation has request parameters or request body
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasAnyRequestSchema"))) {
                needsBadRequest = true;
            }
            // 401 UNAUTHORIZED: needed if any operation has authentication
            if (Boolean.TRUE.equals(op.vendorExtensions.get("hasAuth"))) {
                needsUnauthorized = true;
            }
            // 500 INTERNAL_SERVER_ERROR: needed if any operation has request body (for JSON parsing errors)
            // or if any operation has response schema (for response handler fallback)
            if (op.vendorExtensions.get("requestModel") != null ||
                    Boolean.TRUE.equals(op.vendorExtensions.get("hasAnyResponseSchema"))) {
                needsInternalError = true;
            }
        }

        // Only add common status codes if they're actually needed
        if (needsNoContent && !statusCodeToConstMap.containsKey("204")) {
            String constName = HTTP_RESPONSE_PREFIX + "NO_CONTENT";
            statusCodeToConstMap.put("204", constName);
            Map<String, String> item = new HashMap<>();
            item.put("constName", constName);
            item.put("statusCode", "204");
            opStatusCodeConsts.add(item);
        }

        if (needsBadRequest && !statusCodeToConstMap.containsKey("400")) {
            String constName = HTTP_RESPONSE_PREFIX + "BAD_REQUEST";
            statusCodeToConstMap.put("400", constName);
            Map<String, String> item = new HashMap<>();
            item.put("constName", constName);
            item.put("statusCode", "400");
            opStatusCodeConsts.add(item);
        }

        if (needsUnauthorized && !statusCodeToConstMap.containsKey("401")) {
            String constName = HTTP_RESPONSE_PREFIX + "UNAUTHORIZED";
            statusCodeToConstMap.put("401", constName);
            Map<String, String> item = new HashMap<>();
            item.put("constName", constName);
            item.put("statusCode", "401");
            opStatusCodeConsts.add(item);
        }

        if (needsInternalError && !statusCodeToConstMap.containsKey("500")) {
            String constName = HTTP_RESPONSE_PREFIX + "INTERNAL_SERVER_ERROR";
            statusCodeToConstMap.put("500", constName);
            Map<String, String> item = new HashMap<>();
            item.put("constName", constName);
            item.put("statusCode", "500");
            opStatusCodeConsts.add(item);
        }

        // Pass the map to templates
        objs.put("statusCodeToConst", statusCodeToConstMap);

        if (!opStatusCodeConsts.isEmpty()) {
            objs.put("statusCodeConsts", opStatusCodeConsts);
        }
        // Add modelsUsed to objs for header includes
        if (modelsUsed != null && !modelsUsed.isEmpty()) {
            ArrayList<String> sortedModels = new ArrayList<>(modelsUsed);
            Collections.sort(sortedModels);
            objs.put("modelsUsed", new ArrayList<>(sortedModels));
            objs.put("includeVariantHeader", INCLUDE_VARIANT);
        }
        if (includeOptionalHeader) {
            objs.put("includeOptionalHeader", INCLUDE_OPTIONAL);
        }
        return objs;
    }

    /**
     * Preprocesses the OpenAPI specification to enhance inline schemas with proper titles.
     * This method ensures that request and response schemas without explicit titles
     * get automatically generated names based on the operation ID.
     *
     * @param openAPI the OpenAPI specification to preprocess
     */

    /**
     * Recursively processes nested schemas within a parent schema to set meaningful titles.
     * This handles cases like component responses that have nested inline object schemas.
     *
     * @param schema     the parent schema to process
     * @param parentName the name of the parent (used for generating child names)
     */
    @SuppressWarnings("rawtypes")
    private void processNestedSchemas(Schema schema, String parentName) {
        processNestedSchemas(schema, parentName, 0);
    }

    private void processNestedSchemas(Schema schema, String parentName, int depth) {
        if (schema == null || depth > 10) {
            return;
        }

        // Process properties of object schemas
        if (schema.getProperties() != null) {
            for (Map.Entry<String, Schema> propEntry : ((Map<String, Schema>) schema.getProperties()).entrySet()) {
                Schema propSchema = propEntry.getValue();
                String propertyName = propEntry.getKey();

                // Set title for nested inline schemas
                if (propSchema.getTitle() == null && propSchema.get$ref() == null) {
                    // For nested objects, create a meaningful name
                    if ("object".equals(propSchema.getType()) || (propSchema.getType() == null && propSchema.getProperties() != null)) {
                        String title = toPascalCase(parentName + "_" + propertyName);
                        propSchema.setTitle(title);

                        // Recursively process nested properties
                        processNestedSchemas(propSchema, title, depth + 1);
                    }
                }
            }
        }

        // Process array items
        if (schema.getItems() != null) {
            Schema itemSchema = schema.getItems();
            if (itemSchema.getTitle() == null && itemSchema.get$ref() == null) {
                if ("object".equals(itemSchema.getType()) || (itemSchema.getType() == null && itemSchema.getProperties() != null)) {
                    String title = toPascalCase(parentName + "_item");
                    itemSchema.setTitle(title);
                    processNestedSchemas(itemSchema, title, depth + 1);
                }
            }
        }

        // Process additionalProperties
        if (schema.getAdditionalProperties() instanceof Schema) {
            Schema additionalSchema = (Schema) schema.getAdditionalProperties();
            if (additionalSchema.getTitle() == null && additionalSchema.get$ref() == null) {
                if ("object".equals(additionalSchema.getType()) || (additionalSchema.getType() == null && additionalSchema.getProperties() != null)) {
                    String title = toPascalCase(parentName + "_additional");
                    additionalSchema.setTitle(title);
                    processNestedSchemas(additionalSchema, title, depth + 1);
                }
            }
        }
    }

    /**
     * Post-processes all models to add C++ specific configurations and namespace handling.
     * This method:
     * - Computes model namespaces based on class names
     * - Filters imports to exclude primitives and standard types
     * - Processes model variables for proper C++ typing
     * - Sets up proper getter/setter configurations
     *
     * @param objs map of all models to process
     * @return the processed models map
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {

        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        // Compute modelNamespace ONCE and append className
        String modelNamespaceBase = (String) additionalProperties.get(MODEL_NAMESPACE);
        if (modelNamespaceBase == null || modelNamespaceBase.isEmpty()) {
            modelNamespaceBase = MODEL_SUFFIX.toLowerCase(Locale.ROOT);
            additionalProperties.put(MODEL_NAMESPACE, modelNamespaceBase);
        }

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            ModelsMap modelsMap = entry.getValue();

            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (model != null) {
                    String modelClassName = model.classname;
                    // Convert model classname to proper PascalCase
                    String fixedClassName = toPascalCase(modelClassName);
                    LOGGER.debug("Model classname: {} -> {}", modelClassName, fixedClassName);
                    model.classname = fixedClassName;
                    // Set in vendorExtensions for backward compatibility
                    model.vendorExtensions.put("modelNamespace", modelNamespaceBase);
                    model.vendorExtensions.put("modelClassName", model.classname);

                    // --- Filter and set imports for mustache ---
                    Set<String> systemHeaders = new LinkedHashSet<>();
                    Set<String> projectHeaders = new LinkedHashSet<>();

                    if (model.imports != null) {
                        for (String imp : model.imports) {
                            // Only add if not a primitive, not a mapped type, and not "object", "nlohmann::json", or "Map"
                            if (!languageSpecificPrimitives.contains(imp)
                                    && !typeMapping.containsKey(imp)
                                    && !"object".equals(imp)
                                    && !"nlohmann::json".equals(imp)
                                    && !"Map".equals(imp)) {

                                String headerName = toPascalCase(imp);
                                projectHeaders.add("#include \"" + headerName + ".h\"");
                            } else {
                                String standardInclude = getStandardIncludeForType(imp);
                                if (standardInclude != null) {
                                    systemHeaders.add(standardInclude);
                                }
                            }
                        }
                    }

                    // Combine system headers first, then project headers (both sorted)
                    List<String> filteredImports = new ArrayList<>();
                    List<String> sortedSystemHeaders = new ArrayList<>(systemHeaders);
                    Collections.sort(sortedSystemHeaders);
                    filteredImports.addAll(sortedSystemHeaders);

                    List<String> sortedProjectHeaders = new ArrayList<>(projectHeaders);
                    Collections.sort(sortedProjectHeaders);
                    filteredImports.addAll(sortedProjectHeaders);

                    // Set the imports for mustache template
                    model.vendorExtensions.put("filteredImports", filteredImports);
                    // --- End filter and set imports ---

                    if (model.allVars != null) {
                        for (CodegenProperty var : model.allVars) {
                            processModelVariable(var, model);
                        }
                    }
                    if (model.vars != null) {
                        for (CodegenProperty var : model.vars) {
                            processModelVariable(var, model);
                        }
                    }
                    // Ensure vars is populated from allVars for template access
                    if ((model.vars == null || model.vars.isEmpty()) && model.allVars != null && !model.allVars.isEmpty()) {
                        model.vars = new ArrayList<>(model.allVars);
                    }

                    // After processing variables, check if any use optional or variant
                    // (processModelVariable may wrap types in std::optional)
                    boolean hasOptional = false;
                    boolean needsVariant = Boolean.TRUE.equals(model.vendorExtensions.get("hasVariant"));

                    List<CodegenProperty> allProperties = new ArrayList<>();
                    if (model.vars != null) allProperties.addAll(model.vars);
                    if (model.allVars != null) {
                        for (CodegenProperty v : model.allVars) {
                            if (!allProperties.contains(v)) allProperties.add(v);
                        }
                    }

                    for (CodegenProperty var : allProperties) {
                        if (var.dataType != null) {
                            if (!hasOptional && var.dataType.contains("std::optional<")) {
                                hasOptional = true;
                            }
                            if (!needsVariant && var.dataType.contains("std::variant<")) {
                                needsVariant = true;
                            }
                        }
                    }

                    // Update filtered imports if needed
                    if (hasOptional && !filteredImports.contains("#include <optional>")) {
                        filteredImports.add(0, "#include <optional>");
                    }
                    if (needsVariant && !filteredImports.contains("#include <variant>")) {
                        filteredImports.add(0, "#include <variant>");
                    }

                    model.vendorExtensions.put("filteredImports", filteredImports);
                }
            }
        }

        return processed;
    }

    /**
     * Processes individual model variables to configure C++ specific attributes.
     * Handles nullable types, container types, and default values according to C++ conventions.
     *
     * @param var   the model variable to process
     * @param model the parent model containing this variable
     */
    private void processModelVariable(CodegenProperty var, CodegenModel model) {
        // Ensure baseName is set for JSON serialization
        if (var.baseName == null || var.baseName.isEmpty()) {
            var.baseName = var.name;
        }
        String modelClassName = model.vendorExtensions.get("modelClassName").toString();
        String varName = toPascalCase(var.name);

        // Convert inline model names in dataType to PascalCase
        if (var.dataType != null && !var.isPrimitiveType && !var.isContainer) {
            // Check if dataType contains underscore or hyphen (likely inline model)
            if (var.dataType.contains("_") || var.dataType.contains("-")) {
                // Don't process if it's a C++ standard library type
                if (!var.dataType.startsWith("std::")) {
                    var.dataType = toPascalCase(var.dataType);
                }
            }
        }
        if (var.datatypeWithEnum != null && !var.isPrimitiveType && !var.isContainer) {
            if (var.datatypeWithEnum.contains("_") || var.datatypeWithEnum.contains("-")) {
                if (!var.datatypeWithEnum.startsWith("std::")) {
                    var.datatypeWithEnum = toPascalCase(var.datatypeWithEnum);
                }
            }
        }

        // Replace oas_any_type_not_mapped placeholder with nlohmann::json
        if ("oas_any_type_not_mapped".equals(var.dataType)) {
            var.dataType = "nlohmann::json";
        }
        if ("oas_any_type_not_mapped".equals(var.datatypeWithEnum)) {
            var.datatypeWithEnum = "nlohmann::json";
        }
        if (var.items != null && "oas_any_type_not_mapped".equals(var.items.dataType)) {
            var.items.dataType = "nlohmann::json";
        }
        if (var.items != null && "oas_any_type_not_mapped".equals(var.items.datatypeWithEnum)) {
            var.items.datatypeWithEnum = "nlohmann::json";
        }

        // Handle anyOf/oneOf union types (std::variant)
        if (var.dataType != null && var.dataType.startsWith("std::variant<")) {
            var.vendorExtensions.put("isUnionType", true);

            // Generate union type alias name from property name
            String unionTypeName = toPascalCase(var.name);
            var.vendorExtensions.put("unionTypeName", unionTypeName);

            // Extract individual types from the variant declaration
            String variantContent = var.dataType.substring(13, var.dataType.length() - 1);
            List<String> unionTypes = Arrays.asList(variantContent.split(",\\s*")); // Split by comma and optional whitespace
            var.vendorExtensions.put("unionTypes", unionTypes);

            // Mark that model needs variant include
            model.vendorExtensions.put("hasVariant", true);
        } else {
            var.vendorExtensions.put("isUnionType", false);
        }

        // Handle nullable types
        if (var.isNullable) {
            var.vendorExtensions.put("isOptional", true);
            // Don't wrap again if already wrapped
            if (!var.dataType.startsWith("std::optional<")) {
                // Store the inner type for the template
                var.vendorExtensions.put("innerType", var.dataType);
                var.dataType = "std::optional<" + var.dataType + ">";
            }
        } else {
            var.vendorExtensions.put("isOptional", false);
        }

        // Remove namespace prefixes from enum names in array items
        // Enums are defined within the class scope and don't need prefixes
        if (var.isArray && var.items != null && var.items.isEnum) {
            if (var.items.dataType != null && var.items.dataType.contains("::")) {
                String[] parts = var.items.dataType.split("::");
                var.items.dataType = parts[parts.length - 1];
            }
            if (var.items.datatypeWithEnum != null && var.items.datatypeWithEnum.contains("::")) {
                String[] parts = var.items.datatypeWithEnum.split("::");
                var.items.datatypeWithEnum = parts[parts.length - 1];
            }
        }

        // Handle container types
        if (var.isArray && var.dataType.startsWith("std::vector<")) {
            // Use datatypeWithEnum for enums to preserve enum type names before they're stripped
            String itemType = var.items != null ?
                    (var.items.datatypeWithEnum != null ? var.items.datatypeWithEnum : var.items.dataType) :
                    "std::string";

            // For inline enums in arrays, qualify with the model class name if not already qualified
            if (var.items != null && var.items.isEnum && !itemType.contains("::")) {
                itemType = modelClassName + "::" + itemType;
            }

            // Use property name and item type for better naming
            String arrayTypeName = toPascalCase(var.name) + "VectorOf" + toPascalCase(itemType);
            var.vendorExtensions.put("arrayTypeName", arrayTypeName);
            var.dataType = "std::vector<" + itemType + ">";
            var.datatypeWithEnum = var.dataType; // Keep them in sync
        }

        if (var.isMap && var.dataType.startsWith("std::map<")) {
            String itemType = var.items != null ? var.items.dataType : "std::string";
            var.dataType = "std::map<std::string, " + itemType + ">";
        }

        if (var.items != null && "string".equals(var.items.dataType)) {
            var.items.dataType = "std::string";
            var.items.isPrimitiveType = true;
        }

        // Handle arrays
        if (var.isArray) {
            model.vendorExtensions.put("hasArrays", true);
            setArrayVendorExtensions(var, varName, modelClassName, model);

            // Set default value for arrays - use datatypeWithEnum for correct enum type
            if (var.defaultValue == null) {
                // Use datatypeWithEnum which has the correct type (including enum types)
                var.defaultValue = var.datatypeWithEnum + "()";
            }
        } else {
            //Handle enums
            if (var.isEnum) {
                setEnumVendorExtensions(var, model);

                // Check for explicit default values in schema
                // Base generator may auto-generate defaults which should be ignored for enums
                boolean hasExplicitDefault = var.defaultValue != null
                        && !var.defaultValue.equals("\"\"")
                        && !var.defaultValue.equals("null")
                        && !var.defaultValue.equals("0");  // Base generator default for integer types

                // Handle enum default values based on required status and schema default
                if (!hasExplicitDefault) {
                    if (!var.required) {
                        // Optional enum: wrap in std::optional and default to std::nullopt
                        if (!var.dataType.startsWith("std::optional<")) {
                            var.dataType = "std::optional<" + var.datatypeWithEnum + ">";
                            var.datatypeWithEnum = var.dataType;
                        }
                        var.defaultValue = "std::nullopt";
                        // Mark that this enum is wrapped in std::optional (for template conditionals)
                        var.vendorExtensions.put("isOptionalEnum", true);
                    } else {
                        // Required enum: Use first value which is always UNSPECIFIED (added by setEnumVendorExtensions)
                        if (var.allowableValues != null && var.allowableValues.containsKey("values")) {
                            @SuppressWarnings("unchecked")
                            List<Object> values = (List<Object>) var.allowableValues.get("values");
                            if (values != null && !values.isEmpty()) {
                                // First value is always safe (UNSPECIFIED) after setEnumVendorExtensions
                                String enumIdentifier = values.get(0).toString();
                                var.defaultValue = var.datatypeWithEnum + "::" + enumIdentifier;
                            }
                        }
                    }
                } else {
                    // Explicit default value from schema - use qualified enum name
                    String defaultVal = var.defaultValue.replaceAll("\"", "");
                    // Convert numeric enum values (e.g., "100" -> "_100")
                    if (defaultVal.matches("^[0-9]+$")) {
                        defaultVal = "_" + defaultVal;
                    }
                    // Convert to UPPERCASE to match enum definition
                    defaultVal = defaultVal.toUpperCase(Locale.ROOT);
                    var.defaultValue = var.datatypeWithEnum + "::" + defaultVal;
                }
            }
            // Handle Date types
            if ("Date".equals(var.baseType) || "DateTime".equals(var.baseType)) {
                var.dataType = "std::string";
                var.datatypeWithEnum = "std::string";
            }
            // Set default values
            if (var.defaultValue == null) {
                var.defaultValue = getDefaultValueForType(var.dataType, var.isNullable);
            }
            // Set primitive flags for mustache
            if (var.dataType.equals("int") || var.dataType.equals("int32_t") || var.dataType.equals("int64_t")
                    || var.dataType.equals("integer")) {
                var.vendorExtensions.put("isInt", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.dataType.equals("long")) {
                var.vendorExtensions.put("isLong", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.dataType.equals("float")) {
                var.vendorExtensions.put("isFloat", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.dataType.equals("double") || var.dataType.equals("number")) {
                var.vendorExtensions.put("isDouble", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.dataType.equals("bool") || var.dataType.equals("boolean")) {
                var.vendorExtensions.put("isBool", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.dataType.equals("std::string") || var.dataType.equals("string")) {
                var.vendorExtensions.put("isString", true);
                var.vendorExtensions.put("isPrimitive", true);
            }
            if (var.isModel) {
                var.vendorExtensions.put("isModel", true);
                if (var.defaultValue != null && var.defaultValue.startsWith("std::make_shared<")) {
                    var.defaultValue = var.datatypeWithEnum + "()";
                }
            }
        }
        //Handle getters and setters
        if (var.getter != null) {
            var.vendorExtensions.put("getter", var.getter);
            var.vendorExtensions.put("getterType", var.datatypeWithEnum);
        }
        if (var.setter != null) {
            var.vendorExtensions.put("setter", var.setter);
            var.vendorExtensions.put("setterType", var.datatypeWithEnum);
        }
    }

    /**
     * Generates appropriate default values for C++ types based on the data type and nullability.
     *
     * @param dataType   the C++ data type
     * @param isNullable whether the type is nullable
     * @return the appropriate default value string
     */
    private String getDefaultValueForType(String dataType, boolean isNullable) {
        if (isNullable) {
            return "std::nullopt";
        }

        if (dataType.equals("std::string")) {
            return "\"\"";
        } else if (dataType.equals("bool")) {
            return "false";
        } else if (dataType.equals("int") || dataType.equals("int32_t") || dataType.equals("int64_t")) {
            return "0";
        } else if (dataType.equals("long")) {
            return "0L";
        } else if (dataType.equals("float")) {
            return "0.0f";
        } else if (dataType.equals("double")) {
            return "0.0";
        } else if (dataType.startsWith("std::vector<")) {
            return dataType + "()";  // Explicit construction
        } else if (dataType.startsWith("std::map<")) {
            return dataType + "()";  // Explicit construction
        } else if (dataType.startsWith("std::optional<")) {
            return "std::nullopt";
        } else if (dataType.startsWith("std::shared_ptr<")) {
            return "nullptr";  // Shared pointers can be null
        }

        // For model types, don't set default - let default constructor handle it
        // Return null so the template skips this in the initializer list
        return null;
    }

    /**
     * Converts an OpenAPI model name to a C++ class name.
     * Handles inline model prefixes and converts to PascalCase.
     *
     * @param name the original model name
     * @return the C++ class name
     */
    @Override
    public String toModelName(String name) {
        if (name == null) {
            return "Model";
        }

        // Handle generic object names that need better naming
        if (name.equals("object") || name.startsWith("object_") || name.contains("inline_object")) {
            // Try to extract a number suffix for uniqueness
            if (name.contains("_")) {
                String[] parts = name.split("_");
                if (parts.length > 1 && parts[parts.length - 1].matches("\\d+")) {
                    return "InlineModel" + parts[parts.length - 1];
                }
            }
            return "InlineModel";
        }

        // Handle inline model names starting with underscore
        if (name.startsWith("_")) {
            name = name.substring(1);
        }

        // Handle inline model names starting with "inline_"
        if (name.startsWith("inline_")) {
            name = name.substring(7); // Remove "inline_" prefix
        }

        // Convert to PascalCase
        return toPascalCase(sanitizeName(name));
    }

    /**
     * Converts an API name to a C++ API class name.
     * Handles default naming and path-based names.
     *
     * @param name the original API name
     * @return the C++ API class name
     */
    @Override
    public String toApiName(String name) {
        String apiName = name;
        if ("Default".equals(apiName) || apiName == null || apiName.isEmpty()) {
            apiName = addProjectOrDefaultName(apiName, "Default");
        } else {
            String[] parts = apiName.split("[/]");
            if (parts.length > 1) {
                apiName = toPascalCase(parts[1]);
            } else {
                apiName = toPascalCase(apiName);
            }
        }
        return apiName;
    }

    /**
     * Converts an API name to its corresponding filename.
     *
     * @param name the API name
     * @return the filename without extension
     */
    @Override
    public String toApiFilename(String name) {
        String apiFileName = name;
        if ("Default".equals(apiFileName) || apiFileName == null || apiFileName.isEmpty()) {
            apiFileName = addProjectOrDefaultName(apiFileName, "Default");
        } else {
            apiFileName = toPascalCase(name);
        }
        return apiFileName + toPascalCase(API_SUFFIX);
    }

    @Override
    @SuppressWarnings({"rawtypes", "unchecked"})
    public String getTypeDeclaration(Schema p) {
        if (p.getOneOf() != null && !p.getOneOf().isEmpty()) {
            // For oneOf, map to std::variant of possible types
            StringBuilder variant = new StringBuilder("std::variant<");
            List<Schema> schemas = p.getOneOf();
            for (int i = 0; i < schemas.size(); i++) {
                if (i > 0) variant.append(", ");
                variant.append(getTypeDeclaration(schemas.get(i)));
            }
            variant.append(">");
            return variant.toString();
        } else if (p.getAnyOf() != null && !p.getAnyOf().isEmpty()) {
            // For anyOf, also use std::variant to handle multiple possible types
            StringBuilder variant = new StringBuilder("std::variant<");
            List<Schema> schemas = p.getAnyOf();
            for (int i = 0; i < schemas.size(); i++) {
                if (i > 0) variant.append(", ");
                variant.append(getTypeDeclaration(schemas.get(i)));
            }
            variant.append(">");
            return variant.toString();
        } else if (ModelUtils.isArraySchema(p)) {
            // Handle arrays with full type declaration
            String inner = getTypeDeclaration(Objects.requireNonNull(ModelUtils.getSchemaItems(p)));
            return "std::vector<" + inner + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            // Handle maps with full type declaration
            String inner = getTypeDeclaration(ModelUtils.getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">";
        } else if (ModelUtils.isComposedSchema(p) && p.getAllOf() != null) {
            // allOf is handled by composition in fromModel
            return super.getTypeDeclaration(p);
        }
        String typeDecl = super.getTypeDeclaration(p);

        if (typeDecl != null && typeDecl.contains("oas_any_type_not_mapped")) {
            typeDecl = typeDecl.replace("oas_any_type_not_mapped", "nlohmann::json");
        }
        return typeDecl;
    }

    @Override
    @SuppressWarnings({"rawtypes", "unchecked"})
    public CodegenModel fromModel(String name, Schema schema) {
        // Use schema title if available for better naming
        String modelName = name;
        if (schema.getTitle() != null && !schema.getTitle().isEmpty()) {
            modelName = schema.getTitle();
        } else if (name != null && (name.startsWith("object") || name.contains("inline_object"))) {
            // Try to generate a better name for inline objects
        }

        CodegenModel model = super.fromModel(modelName, schema);

        // Ensure the model name is properly set
        if (model != null) {
            model.name = toModelName(modelName);
            model.classname = model.name;

            if (schema.getAllOf() != null && !schema.getAllOf().isEmpty() && this.openAPI != null) {
                int refCount = 0;
                String parentRef = null;
                Set<String> parentPropertyNames = new HashSet<>();
                List<CodegenProperty> mergedVars = new ArrayList<>();

                for (Schema allOfSchema : (List<Schema>) schema.getAllOf()) {
                    if (allOfSchema.get$ref() != null) {
                        refCount++;
                        parentRef = ModelUtils.getSimpleRef(allOfSchema.get$ref());

                        // Get parent properties to filter them out from child
                        Schema parentSchema = ModelUtils.getReferencedSchema(openAPI, allOfSchema);
                        CodegenModel parentModel = super.fromModel(parentRef, parentSchema);
                        if (parentModel != null && parentModel.vars != null) {
                            for (CodegenProperty var : parentModel.vars) {
                                parentPropertyNames.add(var.name);
                            }
                            // Only merge if we have multiple parents
                            if (schema.getAllOf().size() > 1) {
                                mergedVars.addAll(parentModel.vars);
                            }
                        }
                    } else {
                        // Inline schema: merge its properties
                        CodegenModel inlineModel = super.fromModel(modelName + "_inline", allOfSchema);
                        if (inlineModel != null && inlineModel.vars != null) {
                            mergedVars.addAll(inlineModel.vars);
                        }
                    }
                }

                // Case 1: Single parent with possible inline schema (inheritance)
                if (refCount == 1 && parentRef != null) {
                    model.parent = toModelName(parentRef);
                    model.parentModel = null; // Don't need to store parent model

                    // Filter out inherited properties from child
                    List<CodegenProperty> childOnlyVars = new ArrayList<>();
                    for (CodegenProperty var : model.vars) {
                        if (!parentPropertyNames.contains(var.name)) {
                            childOnlyVars.add(var);
                        }
                    }
                    model.vars = childOnlyVars;
                }
                // Case 2: Multiple parents or complex composition (merge all)
                else if (refCount > 1 || mergedVars.size() > 0) {
                    // Avoid duplicates
                    Map<String, CodegenProperty> uniqueVars = new LinkedHashMap<>();
                    for (CodegenProperty var : mergedVars) {
                        uniqueVars.put(var.name, var);
                    }
                    for (CodegenProperty var : model.vars) {
                        uniqueVars.put(var.name, var);
                    }
                    model.vars = new ArrayList<>(uniqueVars.values());
                    model.parent = null; // No inheritance for multiple parents
                }
            }

            // Handle oneOf/anyOf schemas - generate std::variant type alias instead of class
            if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
                model.vendorExtensions.put("isOneOfSchema", true);
                model.vendorExtensions.put("hasVariant", true);
                List<String> variantTypes = new ArrayList<>();
                for (Schema oneOfSchema : (List<Schema>) schema.getOneOf()) {
                    String typeName = getTypeDeclaration(oneOfSchema);
                    variantTypes.add(typeName);
                }
                model.vendorExtensions.put("oneOfTypes", variantTypes);

                // Extract discriminator if present
                if (schema.getDiscriminator() != null) {
                    model.vendorExtensions.put("discriminatorProperty", schema.getDiscriminator().getPropertyName());
                    if (schema.getDiscriminator().getMapping() != null) {
                        // Convert Map to List for Mustache iteration
                        List<Map<String, String>> mappingList = new ArrayList<>();
                        for (Map.Entry<String, String> entry : schema.getDiscriminator().getMapping().entrySet()) {
                            Map<String, String> mappingEntry = new HashMap<>();
                            mappingEntry.put("value", entry.getKey());
                            // Extract type name from schema reference
                            String schemaRef = entry.getValue();
                            String typeName = schemaRef.substring(schemaRef.lastIndexOf('/') + 1);
                            mappingEntry.put("key", typeName);
                            mappingList.add(mappingEntry);
                        }
                        model.vendorExtensions.put("discriminatorMapping", mappingList);
                    }
                }
            } else if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
                model.vendorExtensions.put("isAnyOfSchema", true);
                model.vendorExtensions.put("hasVariant", true);
                List<String> variantTypes = new ArrayList<>();
                for (Schema anyOfSchema : (List<Schema>) schema.getAnyOf()) {
                    String typeName = getTypeDeclaration(anyOfSchema);
                    variantTypes.add(typeName);
                }
                model.vendorExtensions.put("anyOfTypes", variantTypes);

                // Extract discriminator if present
                if (schema.getDiscriminator() != null) {
                    model.vendorExtensions.put("discriminatorProperty", schema.getDiscriminator().getPropertyName());
                    if (schema.getDiscriminator().getMapping() != null) {
                        // Convert Map to List for Mustache iteration
                        List<Map<String, String>> mappingList = new ArrayList<>();
                        for (Map.Entry<String, String> entry : schema.getDiscriminator().getMapping().entrySet()) {
                            Map<String, String> mappingEntry = new HashMap<>();
                            mappingEntry.put("value", entry.getKey());
                            // Extract type name from schema reference
                            String schemaRef = entry.getValue();
                            String typeName = schemaRef.substring(schemaRef.lastIndexOf('/') + 1);
                            mappingEntry.put("key", typeName);
                            mappingList.add(mappingEntry);
                        }
                        model.vendorExtensions.put("discriminatorMapping", mappingList);
                    }
                }
            }
        }
        return model;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // Convert Date/DateTime types to std::string
        if ("Date".equals(property.baseType) || "DateTime".equals(property.baseType)) {
            property.dataType = "std::string";
            property.datatypeWithEnum = "std::string";
        }

        // Set vendor extensions for array properties
        if (property.isArray) {
            property.vendorExtensions.put("isArray", true);

            // Check if it's an array of enums
            if (property.items != null && property.items.isEnum) {
                property.vendorExtensions.put("isArrayOfEnum", true);

                if (property.datatypeWithEnum != null && property.datatypeWithEnum.contains("std::")) {
                    String enumName = property.items.datatypeWithEnum;
                    if (enumName != null && !enumName.contains("::")) {
                        // Replace std::EnumName with ClassName::EnumName
                        property.datatypeWithEnum = property.datatypeWithEnum.replace(
                                "std::" + enumName,
                                model.classname + "::" + enumName
                        );
                        property.dataType = property.datatypeWithEnum;
                    }
                }
            } else {
                property.vendorExtensions.put("isArrayOfEnum", false);
            }
        } else {
            property.vendorExtensions.put("isArray", false);
            property.vendorExtensions.put("isArrayOfEnum", false);
        }

        // Set vendor extensions for map properties
        if (property.isMap) {
            property.vendorExtensions.put("isContainer", true);
        } else if (property.isArray) {
            property.vendorExtensions.put("isContainer", true);
        } else {
            property.vendorExtensions.put("isContainer", false);
        }

        // Set vendor extensions for enum properties
        if (property.isEnum) {
            property.vendorExtensions.put("isEnum", true);
            // Convert numeric enum values to have underscore prefix
            convertNumericEnumValues(property);
        } else {
            property.vendorExtensions.put("isEnum", false);
        }

        // Set vendor extension for nullable/optional properties
        if (property.isNullable) {
            property.vendorExtensions.put("isOptional", true);
        } else {
            property.vendorExtensions.put("isOptional", false);
        }
    }

    @Override
    @SuppressWarnings("rawtypes")
    public String toDefaultValue(Schema p) {
        // Handle byte arrays separately - they are std::vector<uint8_t>, not string
        if (ModelUtils.isByteArraySchema(p)) {
            return null; // Will be initialized as empty vector in constructor
        }

        if (ModelUtils.isStringSchema(p) || ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
            // For enum types, don't return empty string - it will be handled in processModelVariable
            if (p.getEnum() != null && !p.getEnum().isEmpty()) {
                return null; // Let processModelVariable handle enum defaults
            }
            return p.getDefault() != null ? "\"" + p.getDefault().toString() + "\"" : "\"\"";
        }
        if (ModelUtils.isBooleanSchema(p)) {
            return p.getDefault() != null ? p.getDefault().toString() : "false";
        }
        if (ModelUtils.isIntegerSchema(p)) {
            return p.getDefault() != null ? p.getDefault().toString() : "0";
        }
        if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) {
                return p.getDefault() != null ? p.getDefault().toString() + "f" : "0.0f";
            }
            return p.getDefault() != null ? p.getDefault().toString() : "0.0";
        }
        if (ModelUtils.isArraySchema(p)) {
            // For arrays, don't generate default value here - let postProcessModelProperty handle it
            // This avoids incorrect enum type qualification issues
            return null;
        }
        if (ModelUtils.isMapSchema(p)) {
            String inner = getTypeDeclaration(ModelUtils.getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">()";
        }
        if (p.get$ref() != null && !p.get$ref().isEmpty()) {
            // Model references as shared pointers - create instance with make_shared
            return "std::make_shared<" + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + ">()";
        }
        // Unknown type - skip initialization, let default constructor handle it
        return null;
    }

    /**
     * Returns the directory path for generated model files.
     *
     * @return the model file directory path
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/models").replace("/", File.separator);
    }

    /**
     * Returns the directory path for generated API files.
     *
     * @return the API file directory path
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }


    private Map<String, String> stripPathFromClassName(String path) {
        Object stripPathValue = additionalProperties.get("stripPathFromClassName");
        Boolean isStripPath = false;
        if (stripPathValue instanceof Boolean) {
            isStripPath = (Boolean) stripPathValue;
        } else if (stripPathValue instanceof String) {
            isStripPath = Boolean.parseBoolean((String) stripPathValue);
        }
        Map<String, String> result = new HashMap<>();
        if (isStripPath != null && isStripPath) {
            if (path == null || path.isEmpty()) {
                result.put("namespace", "");
                result.put("className", "DefaultClass");
                return result;
            }
            if (path.contains("_error") || path.contains("_Error") || path.contains("error_")
                    || path.contains("Error_")) {
                path = path.replace("_error", "").replace("_Error", "").replace("error_", "").replace("Error_", "");
            }
            // Remove leading underscores
            String clean = path.replaceAll("^_+", "");

            // If the path is now empty or whitespace only, return default
            if (clean.isEmpty() || clean.trim().isEmpty()) {
                result.put("namespace", "");
                result.put("className", "DefaultClass");
                return result;
            }

            // Split by "/", "_", "-", " ", camelCase boundary
            String[] segments = clean.split("(?<=[a-z0-9])(?=[A-Z])|[^a-zA-Z0-9]");
            List<String> parts = new ArrayList<>();
            for (String seg : segments) {
                if (seg != null && !seg.trim().isEmpty()) {
                    parts.add(seg.trim());
                }
            }

            if (parts.size() == 0) {
                result.put("namespace", "");
                result.put("className", "DefaultClass");
                return result;
            }
            if (clean.toLowerCase(Locale.ROOT).contains("error")) {
                result.put("namespace", "error");
                result.put("className", toPascalCase(clean));
                return result;
            }

            // ClassName is the rest, joined and PascalCased
            StringBuilder classNameBuilder = new StringBuilder();
            String namespace = "";
            if (parts.size() > 1) {
                // Use first part as namespace only for API class names, not models
                namespace = parts.get(0).toLowerCase(Locale.ROOT);
                for (int i = 1; i < parts.size(); i++) {
                    String pascalPart = toPascalCase(parts.get(i));
                    if (pascalPart != null && !pascalPart.isEmpty()) {
                        classNameBuilder.append(pascalPart);
                    }
                }
            } else {
                // Single part: use as class name
                String pascalPart = toPascalCase(parts.get(0));
                if (pascalPart != null && !pascalPart.isEmpty()) {
                    classNameBuilder.append(pascalPart);
                }
            }

            // Ensure we have a valid class name
            String finalClassName = classNameBuilder.toString();
            if (finalClassName.isEmpty()) {
                finalClassName = "DefaultClass";
            }

            result.put("namespace", namespace);
            result.put("className", toPascalCase(finalClassName));
            return result;
        } else {
            result.put("namespace", "");
            result.put("className", toPascalCase(path));
            return result;
        }

    }

    /**
     * Adds project name to a class name or returns default name if project name is not set.
     *
     * @param name        the base name
     * @param defaultName the default name to use if input is null/empty
     * @return the project-prefixed name or default name
     */
    public String addProjectOrDefaultName(String name, String defaultName) {
        String apiName = name;
        if (name == null || name.isEmpty()) {
            apiName = defaultName;
        }
        String projectName = (String) additionalProperties.get(PROJECT_NAME);
        if (projectName != null && !projectName.isEmpty()) {
            apiName = toPascalCase(projectName) + API_SUFFIX;
        }
        return apiName;
    }

    /**
     * Converts a model namespace string to proper format.
     *
     * @param modelNamespace the namespace to convert
     * @return the formatted model namespace
     */
    public String toModelNamespace(String modelNamespace) {
        if (modelNamespace == null || modelNamespace.isEmpty()) {
            return toPascalCase(MODEL_SUFFIX);
        }
        return modelNamespace;
    }

    /**
     * Converts a string to PascalCase formatting with consistent handling.
     * Handles hyphens, underscores, and camelCase boundaries.
     * Used for class names, type names, and enum names.
     *
     * @param name the string to convert
     * @return the PascalCase formatted string
     */
    public String toPascalCase(String name) {
        if (name == null || name.isEmpty()) {
            return "DefaultClass";
        }

        // First, insert spaces before uppercase letters to split camelCase
        String normalized = name.replaceAll("([a-z])([A-Z])", "$1 $2");
        // Split on delimiters (hyphens, underscores, spaces)
        String[] parts = normalized.split("[_\\-\\s]+");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            if (part != null && !part.isEmpty()) {
                // Check if the part starts with a digit
                if (Character.isDigit(part.charAt(0))) {
                    // For parts starting with digit (e.g., "200response"), find where letters start
                    int firstLetterIndex = 0;
                    while (firstLetterIndex < part.length() && Character.isDigit(part.charAt(firstLetterIndex))) {
                        firstLetterIndex++;
                    }
                    // Keep digits as-is
                    result.append(part.substring(0, firstLetterIndex));
                    // Capitalize first letter after digits, lowercase the rest
                    if (firstLetterIndex < part.length()) {
                        result.append(part.substring(firstLetterIndex, firstLetterIndex + 1).toUpperCase(Locale.ROOT));
                        if (firstLetterIndex + 1 < part.length()) {
                            result.append(part.substring(firstLetterIndex + 1).toLowerCase(Locale.ROOT));
                        }
                    }
                } else {
                    // Check if part is already in PascalCase or camelCase (mixed case)
                    boolean hasMixedCase = !part.equals(part.toLowerCase(Locale.ROOT)) &&
                            !part.equals(part.toUpperCase(Locale.ROOT));

                    if (hasMixedCase && Character.isUpperCase(part.charAt(0))) {
                        // Already PascalCase, keep as-is
                        result.append(part);
                    } else {
                        // Convert to PascalCase: Capitalize first letter, lowercase the rest
                        result.append(part.substring(0, 1).toUpperCase(Locale.ROOT));
                        if (part.length() > 1) {
                            result.append(part.substring(1).toLowerCase(Locale.ROOT));
                        }
                    }
                }
            }
        }
        return result.length() > 0 ? result.toString() : "DefaultClass";
    }

    /**
     * Converts a string to camelCase formatting.
     * Used for variable names and method names.
     *
     * @param name the string to convert
     * @return the camelCase formatted string
     */
    public String toCamelCase(String name) {
        if (name == null || name.isEmpty()) {
            return "defaultVariable";
        }
        String result = camelize(name, true);
        return result.isEmpty() ? "defaultVariable" : result;
    }

    /**
     * Converts a string to camelCase or PascalCase with proper case handling.
     * Handles uppercase words like "POST" -> "Post" and "GET" -> "Get".
     *
     * @param name                 the string to convert
     * @param lowercaseFirstLetter whether to lowercase the first letter (camelCase)
     * @return the camelized string
     */
    public String camelize(String name, boolean lowercaseFirstLetter) {
        if (name == null || name.isEmpty()) {
            return "";
        }

        // First, try the framework utility
        String result;
        if (lowercaseFirstLetter) {
            result = org.openapitools.codegen.utils.StringUtils.camelize(name, CamelizeOption.LOWERCASE_FIRST_LETTER);
        } else {
            result = org.openapitools.codegen.utils.StringUtils.camelize(name);
        }

        // If the result is the same as input and input is all uppercase,
        if (result.equals(name) && name.equals(name.toUpperCase(Locale.ROOT)) && name.matches("[A-Z]+")) {
            // Handle all-caps words manually
            result = name.substring(0, 1).toUpperCase(Locale.ROOT) + name.substring(1).toLowerCase(Locale.ROOT);
            if (lowercaseFirstLetter) {
                result = result.substring(0, 1).toLowerCase(Locale.ROOT) + result.substring(1);
            }
        }

        return result;
    }

    /**
     * Generates a handler function name from HTTP method and path.
     *
     * @param httpMethod the HTTP method (GET, POST, etc.)
     * @param path       the API path
     * @return the generated handler function name
     */
    public String toHandlerFunctionName(String httpMethod, String path, Boolean prefix) {
        String method = toPascalCase(httpMethod);
        String className = stripPathFromClassName(path).get("className");
        className = className.replaceAll("[^A-Za-z0-9]", "");
        if (prefix) {
            return "handle" + method + "For" + toPascalCase(className);
        } else {
            return toPascalCase(className) + method;
        }
    }

    /**
     * Generates a handler function request type from the API Path
     *
     * @param path the API path
     * @return the generated handler function request
     */
    public String toHandlerFunctionRequest(String path, String method) {
        String className = stripPathFromClassName(path).get("className");
        className = className.replaceAll("[^A-Za-z0-9]", "");
        return toPascalCase(className + toPascalCase(method) + "Request");
    }


    /**
     * Generates a handler function response type from the API path.
     *
     * @param path the API path
     * @return the generated handler function response
     */
    public String toHandlerFunctionResponse(String path, String method) {
        String className = stripPathFromClassName(path).get("className");
        className = className.replaceAll("[^A-Za-z0-9]", "");
        return toPascalCase(className + toPascalCase(method) + "Response");
    }

    /**
     * Processes command line options and additional properties.
     * Sets up default values for project configuration.
     */
    @Override
    public void processOpts() {
        super.processOpts();

        // Set default model package if not provided
        if (modelPackage == null || modelPackage.isEmpty()) {
            modelPackage = DEFAULT_PROJECT_NAME;
        }

        // Get project name from additional properties
        String projectName = DEFAULT_PROJECT_NAME;
        if (additionalProperties.containsKey(PROJECT_NAME)) {
            projectName = (String) additionalProperties.get(PROJECT_NAME);
        }

        // Make package name available to templates
        additionalProperties.put("packageName", modelPackage);
        additionalProperties.put("projectName", projectName);
        String cmakeProjectName = (String) additionalProperties.get("cmakeProjectName");
        if (cmakeProjectName == null || cmakeProjectName.isEmpty()) {
            cmakeProjectName = (String) projectName;
        }
        additionalProperties.put("cmakeProjectName", StringUtils.underscore(cmakeProjectName));


        // Set Enum namespace
        String enumNamespace = (String) additionalProperties.get(ENUM_NAMESPACE);
        if (enumNamespace == null || enumNamespace.isEmpty()) {
            enumNamespace = toPascalCase(ENUM_SUFFIX);
        }
        additionalProperties.put(ENUM_NAMESPACE, enumNamespace);

        // Set API namespace for templates (needed for AuthenticationManager and other supporting files)
        String apiNamespaceValue = (String) additionalProperties.get(API_NAMESPACE);
        if (apiNamespaceValue == null || apiNamespaceValue.isEmpty()) {
            apiNamespaceValue = toPascalCase(API_SUFFIX);
        }
        additionalProperties.put(API_NAMESPACE, apiNamespaceValue);

        // Handle addApiImplStubs option
        if (additionalProperties.containsKey(ADD_API_IMPL_STUBS)) {
            boolean addApiImplStubs = Boolean.parseBoolean(additionalProperties.get(ADD_API_IMPL_STUBS).toString());
            if (addApiImplStubs) {
                // Add API implementation stub templates
                apiTemplateFiles.put("api-impl-header.mustache", "Impl.h");
                apiTemplateFiles.put("api-impl.mustache", "Impl.cpp");
                // Add main.cpp for executable
                supportingFiles.add(new SupportingFile("main.mustache", "", "main.cpp"));
                LOGGER.debug("API implementation stubs will be generated");
            }
        }

    }

    /**
     * Post-processes parameters to set up C++ specific configurations.
     * Handles the required flag and parameter type conversions.
     *
     * @param parameter the parameter to process
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        // Set vendor extensions for template usage
        parameter.vendorExtensions.put("isRequired", parameter.required);
        parameter.vendorExtensions.put("isOptional", !parameter.required);

        // Handle non-required parameters - wrap in std::optional
        if (!parameter.required) {
            if (!parameter.dataType.startsWith("std::optional<")) {
                parameter.dataType = "std::optional<" + parameter.dataType + ">";
            }
        }

        // Extract default value from schema if available
        if (parameter.defaultValue != null && !parameter.defaultValue.isEmpty()) {
            // Correct nullptr default value to std::nullopt for std::optional types
            if ("nullptr".equals(parameter.defaultValue) && parameter.dataType.startsWith("std::optional<")) {
                parameter.defaultValue = "std::nullopt";
            }
            parameter.vendorExtensions.put("hasDefaultValue", true);
            parameter.vendorExtensions.put("defaultValue", parameter.defaultValue);
        } else {
            parameter.vendorExtensions.put("hasDefaultValue", false);
        }
    }

    /**
     * Sets C++ type flags for a parameter for use in Mustache templates.
     * Handles arrays, enums, and primitives with proper vendor extensions.
     */
    private void setParameterTypeFlags(CodegenParameter param) {
        if (param == null) {
            return;
        }

        // Set parameter style flags for serialization
        String style = param.style != null ? param.style : "form";
        param.vendorExtensions.put("isStyleSimple", "simple".equals(style));
        param.vendorExtensions.put("isStyleForm", "form".equals(style));
        param.vendorExtensions.put("isStyleSpaceDelimited", "spaceDelimited".equals(style));
        param.vendorExtensions.put("isStylePipeDelimited", "pipeDelimited".equals(style));
        param.vendorExtensions.put("isStyleDeepObject", "deepObject".equals(style));

        // Set array flags and helper names
        if (param.isArray) {
            setArrayVendorExtensions(param, param.nameInPascalCase, param.baseName, null);
        } else if (param.isEnum) {
            // Handle enum parameters
            param.vendorExtensions.put("isEnum", true);
            param.vendorExtensions.put("enumType", toPascalCase(param.baseType) + "Enum");
            param.vendorExtensions.put("enumFromStringHelper", param.nameInPascalCase + ENUM_FROM_STRING);
            param.vendorExtensions.put("enumToStringHelper", param.nameInPascalCase + ENUM_TO_STRING);
        } else {
            // Set primitive type flags
            setPrimitiveTypes(param);
        }
    }

    /**
     * Helper to set vendorExtensions for array properties and parameters.
     */
    private void setArrayVendorExtensions(Object varObj, String varName, String modelBaseName, CodegenModel model) {
        if (varObj == null) return;

        Map<String, Object> vendorExtensions;
        CodegenProperty items;
        boolean isContainer;

        if (varObj instanceof CodegenProperty) {
            CodegenProperty var = (CodegenProperty) varObj;
            vendorExtensions = var.vendorExtensions;
            items = var.items;
            isContainer = var.isContainer;
        } else if (varObj instanceof CodegenParameter) {
            CodegenParameter var = (CodegenParameter) varObj;
            vendorExtensions = var.vendorExtensions;
            items = var.items;
            isContainer = var.isContainer;
        } else {
            return;
        }

        vendorExtensions.put("isArray", true);
        String itemType = (items != null && items.dataType != null) ? items.dataType : "std::string";
        // Improved array item type naming
        String arrayTypeName = toPascalCase(varName) + "VectorOf" + toPascalCase(itemType);
        vendorExtensions.put("arrayTypeName", arrayTypeName);
        vendorExtensions.put("arrayItemType", itemType);
        if (items != null) {
            // ALWAYS set primitive type flags for items (needed for type conversion in templates)
            setPrimitiveTypes(items);

            if (items.isEnum) {
                // Set up enum vendor extensions for items (needed for enumCases and conversion helpers)
                // Only call if model is available (not for parameters)
                if (model != null) {
                    setEnumVendorExtensions(items, model);
                }

                vendorExtensions.put("isArrayOfEnum", true);
                vendorExtensions.put("enumFromStringHelper", varName + "EnumFromString");
                vendorExtensions.put("enumToStringHelper", varName + "EnumToString");
                if (modelBaseName != null && !modelBaseName.isEmpty()) {
                    vendorExtensions.put("enumModelClass", modelBaseName);
                }
                // Remove namespace prefixes from enum datatypes in array items
                // Enums are defined inside the class, so namespace qualifiers are not needed
                if (items.datatypeWithEnum != null && items.datatypeWithEnum.contains("::")) {
                    // Remove any namespace prefix (like "std::" or "ModelClass::")
                    String[] parts = items.datatypeWithEnum.split("::");
                    items.datatypeWithEnum = parts[parts.length - 1];
                }
                // Also fix items.dataType which is used to construct parent's datatypeWithEnum for arrays
                if (items.dataType != null && items.dataType.contains("::")) {
                    String[] parts = items.dataType.split("::");
                    items.dataType = parts[parts.length - 1];
                }
            } else if (items.isModel) {
                vendorExtensions.put("isArrayOfModel", true);
                vendorExtensions.put("vectorFromJsonHelper", varName + "VectorFromJson");
                vendorExtensions.put("vectorToJsonHelper", varName + "VectorToJson");
            } else if (items.isPrimitiveType) {
                vendorExtensions.put("isArrayOfPrimitive", true);
                vendorExtensions.put("vectorFromStringHelper", varName + "VectorFromString");
                vendorExtensions.put("vectorToStringHelper", varName + "VectorToString");
            } else if (items.isContainer) {
                vendorExtensions.put("isArrayOfContainer", true);
                vendorExtensions.put("vectorFromStringHelper", varName + "VectorFromString");
                vendorExtensions.put("vectorToStringHelper", varName + "VectorToString");
            } else {
                // Default to primitive if type is unknown but items are present
                vendorExtensions.put("isArrayOfPrimitive", true);
            }
        } else {
            // If items are not defined, default to a simple array of primitives (strings)
            vendorExtensions.put("isArrayOfPrimitive", true);
        }
    }

    private void setPrimitiveTypes(Object varObj) {
        if (varObj == null) return;

        String type;
        Map<String, Object> vendorExtensions;
        boolean isObject = false;

        if (varObj instanceof CodegenProperty) {
            CodegenProperty prop = (CodegenProperty) varObj;
            type = (prop.dataType != null) ? prop.dataType : "";
            vendorExtensions = prop.vendorExtensions;
            isObject = prop.isModel || (!prop.isPrimitiveType && !prop.isArray && !prop.isEnum);
        } else if (varObj instanceof CodegenParameter) {
            CodegenParameter param = (CodegenParameter) varObj;
            type = (param.dataType != null) ? param.dataType : "";
            vendorExtensions = param.vendorExtensions;
            isObject = param.isModel || (!param.isPrimitiveType && !param.isArray && !param.isEnum);
        } else {
            return; // Unsupported type
        }

        // Extract inner type from containers like std::optional<T>, std::shared_ptr<T>
        String innerType = type;
        if (type.contains("<")) {
            int startIdx = type.indexOf('<') + 1;
            int endIdx = type.lastIndexOf('>');
            if (startIdx > 0 && endIdx > startIdx) {
                innerType = type.substring(startIdx, endIdx).trim();
            }
        }

        // Normalize some aliases
        if ("string".equals(innerType)) innerType = "std::string";
        if ("integer".equals(innerType)) innerType = "int";
        if ("number".equals(innerType)) innerType = "double"; // treat generic number as double

        boolean isLong = innerType.equals("long") || innerType.equals("int64_t");
        boolean isFloat = innerType.equals("float");
        boolean isString = innerType.equals("std::string") || innerType.equals("string");
        boolean isInt = innerType.equals("int") || innerType.equals("int32_t") || innerType.equals("int64_t") || innerType.equals("integer");
        boolean isBool = innerType.equals("bool") || innerType.equals("boolean");
        boolean isDouble = innerType.equals("double") || innerType.equals("number");

        // Handle overlaps: int64_t should be treated as long, not int
        if (innerType.equals("int64_t")) {
            isInt = false;
            isLong = true;
        }

        boolean isPrimitive = isInt || isLong || isFloat || isBool || isDouble || isString;

        vendorExtensions.put("isObject", isObject);
        vendorExtensions.put("isString", isString);
        vendorExtensions.put("isInt", isInt);
        vendorExtensions.put("isLong", isLong);
        vendorExtensions.put("isFloat", isFloat);
        vendorExtensions.put("isBool", isBool);
        vendorExtensions.put("isDouble", isDouble);
        vendorExtensions.put("isPrimitive", isPrimitive);
    }

    /**
     * Converts numeric enum values to valid C++ enum names by prefixing with underscore.
     * This is needed because C++ enum identifiers cannot start with digits.
     * This method processes enum values in a CodegenProperty and prefixes any purely numeric
     * enum values with an underscore. This is necessary because in C++, enum identifiers cannot
     * start with a digit, so numeric enum values must be transformed to valid C++ identifiers.
     * <p>
     * For example:
     * - "200" becomes "_200"
     * - "404" becomes "_404"
     * - "OK" remains "OK"
     * <p>
     * The method retrieves enum values from either the property's {@code _enum} field or from
     * the {@code allowableValues} map, then updates all three locations with the converted values
     * to maintain consistency across the property's internal state.
     *
     * @param property the CodegenProperty containing enum values to convert
     */
    private void convertNumericEnumValues(CodegenProperty property) {
        // Get enum values from property._enum or allowableValues
        List<?> enumValues = property._enum;
        if ((enumValues == null || enumValues.isEmpty()) && property.allowableValues != null) {
            Object valuesObj = property.allowableValues.get("values");
            if (valuesObj instanceof List) {
                enumValues = (List<?>) valuesObj;
            }
        }

        if (enumValues != null && !enumValues.isEmpty()) {
            List<String> convertedValues = new ArrayList<>();
            for (Object enumVal : enumValues) {
                String enumValStr = enumVal != null ? enumVal.toString() : "";
                String convertedVal = enumValStr;
                // Convert numeric values to have underscore prefix
                if (enumValStr.matches("^[0-9]+$")) {
                    convertedVal = "_" + enumValStr;
                }
                // Convert to UPPERCASE for C++ enum standards (clang style)
                convertedVal = convertedVal.toUpperCase(Locale.ROOT);
                convertedValues.add(convertedVal);
            }
            // Update the property with converted values
            property._enum = convertedValues;
            property.vendorExtensions.put("values", convertedValues);
            if (property.allowableValues != null) {
                property.allowableValues.put("values", convertedValues);
            }
        }
    }

    /**
     * Helper to set vendorExtensions for enum properties and parameters.
     * Handles enum value conversion and helper function naming.
     */
    private void setEnumVendorExtensions(Object varObj, CodegenModel model) {
        if (varObj == null) {
            return;
        }

        CodegenProperty var;
        if (varObj instanceof CodegenProperty) {
            var = (CodegenProperty) varObj;
        } else {
            return;
        }

        var.vendorExtensions.put("isEnum", true);
        var.vendorExtensions.put("enumType", toPascalCase(var.baseType) + "Enum");

        // Use SHORT enum name that will be scoped to the class
        // e.g., "CardTypeEnum" not "CreditCardCardTypeEnum"
        // Use centralized toPascalCase for consistent naming
        String shortEnumName = toPascalCase(var.baseName) + "Enum";
        var.vendorExtensions.put("enumName", shortEnumName);
        var.enumName = shortEnumName;  // Also set the direct property
        var.vendorExtensions.put("enumToStringHelper", var.name + ENUM_TO_STRING);

        // Convert numeric enum values to valid C++ enum names
        List<String> convertedValues = new ArrayList<>();

        // Try to get enum values from var._enum, allowableValues, or var.allowableValues
        List<String> enumValues = var._enum;
        if ((enumValues == null || enumValues.isEmpty()) && var.allowableValues != null) {
            enumValues = new ArrayList<>();
            for (Object val : var.allowableValues.values()) {
                enumValues.add(val != null ? val.toString() : "");
            }
        }

        LOGGER.debug("Processing enum for variable {}: isEnum={}, _enum={}, allowableValues={}",
                var.name, var.isEnum, var._enum, var.allowableValues);

        // Check if enum has UNKNOWN/UNSPECIFIED value, if not add it at the beginning
        boolean hasUnknownValue = false;
        if (enumValues != null && !enumValues.isEmpty()) {
            for (String enumVal : enumValues) {
                String upperVal = enumVal.toUpperCase(Locale.ROOT);
                if (upperVal.equals("UNKNOWN") || upperVal.equals("UNSPECIFIED") ||
                        upperVal.equals("NONE") || upperVal.equals("UNDEFINED")) {
                    hasUnknownValue = true;
                    break;
                }
            }
        }

        if (!hasUnknownValue) {
            if (enumValues == null) {
                enumValues = new ArrayList<>();
            }
            enumValues.add(0, "UNSPECIFIED");
            LOGGER.debug("Added UNSPECIFIED value to enum {} (needed for safe initialization)", var.name);
        }

        if (enumValues != null && !enumValues.isEmpty()) {
            for (String enumVal : enumValues) {
                // Convert numeric values to words or keep string values as-is
                String convertedVal = enumVal;
                if (enumVal.matches("^[0-9]+$")) {
                    // Convert number to enum name
                    convertedVal = "_" + enumVal;  // Prefix with underscore for numeric values
                }
                convertedValues.add(convertedVal);
            }
        } else {
            LOGGER.warn("No enum values found for variable {} in model {}", var.name, model.classname);
        }

        var.vendorExtensions.put("values", convertedValues);
        var._enum = convertedValues;  // Also set it on the var itself for Mustache access
        var.allowableValues.put("values", convertedValues);  // Also update allowableValues for template

        // Create enumCases for use in helper functions
        List<Map<String, String>> enumCases = new ArrayList<>();
        // Iterate through original enum values to maintain mapping between C++ identifiers and original JSON values
        if (enumValues != null) {
            for (int i = 0; i < enumValues.size(); i++) {
                String originalVal = enumValues.get(i);  // Original value (e.g., "200")
                String convertedVal = convertedValues.get(i);  // Converted C++ identifier (e.g., "_200")
                Map<String, String> caseMap = new HashMap<>();
                caseMap.put("name", convertedVal);  // C++ enum identifier
                caseMap.put("value", originalVal);  // Original JSON value
                caseMap.put("enumName", shortEnumName);  // Use short name
                enumCases.add(caseMap);
            }
        }
        var.vendorExtensions.put("enumCases", enumCases);

        LOGGER.debug("Set enum values for {}: {}", var.name, convertedValues);
        var.vendorExtensions.put("enumFromStringHelper", var.name + ENUM_FROM_STRING);
        // Use the short enum name (e.g., "CardTypeEnum") for the datatype
        // When used in containers, it will be scoped to the class (e.g., "CreditCard::CardTypeEnum")
        var.vendorExtensions.put("shortEnumName", shortEnumName);
        var.datatypeWithEnum = shortEnumName;
    }

    /**
     * Process security requirements for an operation.
     * Adds vendor extensions for different authentication types.
     */
    private void processSecurityRequirements(CodegenOperation op) {
        if (op.authMethods == null || op.authMethods.isEmpty()) {
            op.vendorExtensions.put("hasAuth", false);
            return;
        }

        op.vendorExtensions.put("hasAuth", true);

        List<Map<String, Object>> apiKeyAuth = new ArrayList<>();
        List<Map<String, Object>> bearerAuth = new ArrayList<>();
        List<Map<String, Object>> basicAuth = new ArrayList<>();
        List<Map<String, Object>> oauth2Auth = new ArrayList<>();

        for (org.openapitools.codegen.CodegenSecurity auth : op.authMethods) {
            Map<String, Object> authInfo = new HashMap<>();
            authInfo.put("name", auth.name);
            authInfo.put("keyParamName", auth.keyParamName);

            if (auth.isApiKey) {
                authInfo.put("isKeyInHeader", auth.isKeyInHeader);
                authInfo.put("isKeyInQuery", auth.isKeyInQuery);
                authInfo.put("isKeyInCookie", auth.isKeyInCookie);
                apiKeyAuth.add(authInfo);
            } else if (auth.isBasicBearer) {
                if (auth.name != null && auth.name.toLowerCase(Locale.ROOT).contains("bearer")) {
                    bearerAuth.add(authInfo);
                } else {
                    basicAuth.add(authInfo);
                }
            } else if (auth.isBasic || auth.isBasicBasic) {
                basicAuth.add(authInfo);
            } else if (auth.isOAuth) {
                authInfo.put("scopes", auth.scopes);
                oauth2Auth.add(authInfo);
            }
        }

        if (!apiKeyAuth.isEmpty()) {
            op.vendorExtensions.put("hasApiKeyAuth", true);
            op.vendorExtensions.put("apiKeyAuth", apiKeyAuth);
        }
        if (!bearerAuth.isEmpty()) {
            op.vendorExtensions.put("hasBearerAuth", true);
            op.vendorExtensions.put("bearerAuth", bearerAuth);
        }
        if (!basicAuth.isEmpty()) {
            op.vendorExtensions.put("hasBasicAuth", true);
            op.vendorExtensions.put("basicAuth", basicAuth);
        }
        if (!oauth2Auth.isEmpty()) {
            op.vendorExtensions.put("hasOAuth2", true);
            op.vendorExtensions.put("oauth2Auth", oauth2Auth);
        }
    }

    /**
     * Post-processes supporting file data to conditionally add files based on API features.
     * This is called after preprocessOpenAPI, so hasAuthMethods is properly set.
     *
     * @param objs the supporting file data
     * @return the processed supporting file data
     */
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        // Conditionally add AuthenticationManager file if security is defined
        if (additionalProperties.containsKey("hasAuthMethods")
                && Boolean.TRUE.equals(additionalProperties.get("hasAuthMethods"))) {
            supportingFiles.add(new SupportingFile("AuthenticationManager.mustache", "api", "AuthenticationManager.h"));
        }
        return super.postProcessSupportingFileData(objs);
    }
}
