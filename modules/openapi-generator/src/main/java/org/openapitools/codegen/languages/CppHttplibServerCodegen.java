package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.utils.*;
import org.openapitools.codegen.utils.CamelizeOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

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

    public static final String PROJECT_NAME = "projectName";
    private static final String DEFAULT_PROJECT_NAME = "cpp-httplib-server";

    //Model Namespace
    public static final String MODEL_NAMESPACE = "modelNamespace";
    //Api Namespace
    public static final String API_NAMESPACE = "apiNamespace";
    //Enum Namespace
    public static final String ENUM_NAMESPACE = "enumNamespace";
    //Model Suffix
    public static final String MODEL_SUFFIX = "models";
    //Api Suffix
    public static final String API_SUFFIX = "api";
    //Enum Suffix
    public static final String ENUM_SUFFIX = "enum";


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

        // Map OpenAPI types/formats to C++ types (always use std:: prefix)
        typeMapping.put("integer", "int");
        typeMapping.put("long", "long");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("number", "double");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "std::string");
        typeMapping.put("byte", "unsigned char");
        typeMapping.put("binary", "std::string");
        typeMapping.put("date", "std::string");
        typeMapping.put("date-time", "std::string");
        typeMapping.put("password", "std::string");
        typeMapping.put("object", "nlohmann::json");
        typeMapping.put("array", "std::vector");
        typeMapping.put("file", "std::string");

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
        
        // Add supporting files for project structure
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("License.mustache", "", "LICENSE"));
        
        // Initialize feature set for httplib server
        modifyFeatureSet(features -> features
            .includeDocumentationFeatures(DocumentationFeature.Readme)
            .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
            .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
            .excludeGlobalFeatures(
                GlobalFeature.XMLStructureDefinitions,
                GlobalFeature.Callbacks,
                GlobalFeature.LinkObjects,
                GlobalFeature.ParameterStyling
            )
            .excludeSchemaSupportFeatures(
                SchemaSupportFeature.Polymorphism
            )
        );

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
        String includeFile = "";
        if (name.startsWith("#include")) {
            includeFile = null;
        }
        // Use import mapping if available
        if (importMapping.containsKey(name)) {
            includeFile = importMapping.get(name);
        }
        // Map OpenAPI type to C++ type if possible
        String mappedType = typeMapping.getOrDefault(name, name);
        if(languageSpecificPrimitives.contains(mappedType)) {
            String include = getStandardIncludeForType(mappedType);
            includeFile = include != null ? include : "";
        }
        else {
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
        if (standardIncludes.containsKey(typeName)) {
            return standardIncludes.get(typeName);
        }
        if (cstdintTypes.contains(typeName)) {
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
     * @param objs the operations map to process
     * @param allModels list of all models in the API
     * @return the processed operations map
     */
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
       if (objs == null || objs.getOperations() == null) {
            LOGGER.warn("Operations or operations map is null");
            return objs;
        }
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();


        // Set classname and compute apiNamespace ONCE per API class
        String classname = operations.getClassname();
        if ("DefaultApi".equals(classname) || classname == null || classname.isEmpty()) {
            classname = addProjectOrDefaultName(classname, "DefaultApi");
            operations.setClassname(classname);
        }
        objs.put("apiHeaderFileName", "#include \"" + toPascalCase(classname) + toPascalCase(API_SUFFIX) + ".h\"");
        objs.put("apiClassnameInPascalCase", toPascalCase(classname));

        // Compute API namespace ONCE, append classname only if not already present as last segment
        String apiNamespace = (String) additionalProperties.get(API_NAMESPACE);
        if (apiNamespace == null || apiNamespace.isEmpty()) {
            apiNamespace = toLowerCase(API_SUFFIX);
        }

        objs.put("apiNamespace", apiNamespace);

        // Track all models used for includes
        Set<String> modelsUsed = new HashSet<>();
        for (CodegenOperation op : operationList) {
            if (op.vendorExtensions == null) {
                op.vendorExtensions = new HashMap<>();
            }
            op.vendorExtensions.put("operationIdCamelCase", toCamelCase(op.operationId));
            op.vendorExtensions.put("httpMethod", op.httpMethod != null ? toPascalCase(op.httpMethod) : "");
            op.vendorExtensions.put("handlerFunctionName", toHandlerFunctionName(op.httpMethod, op.path.toString()));
            op.vendorExtensions.put("handlerFunctionResponse", toHandlerFunctionResponse(op.path.toString()));
            if (op.path != null) {
                op.vendorExtensions.put("path", op.path);
            }
            // Handle request model
            if (op.bodyParam != null && op.bodyParam.baseType != null) {
                String className = stripPathFromClassName(op.bodyParam.baseType).get("className");
                String requestModel = "models::" + toPascalCase(className);
                op.vendorExtensions.put("requestModel", requestModel);
                modelsUsed.add(className);
            }

            // Process responses
            String successType = "";
            List<String> errorTypes = new ArrayList<>();
            List<Map<String, String>> statusCodeToTypes = new ArrayList<>();
            for (CodegenResponse resp : op.responses) {
                Boolean hasAnyResponseSchema = false;
                if (resp.code != null) {
                    if (resp.code.equals("200") && resp.baseType != null) {
                        hasAnyResponseSchema = true;
                        if(!typeMapping.containsKey(resp.baseType)) {
                            String className = stripPathFromClassName(resp.baseType).get("className");
                            successType = "models::" + toPascalCase(className);
                            modelsUsed.add(className);
                        } else {
                            // Primitive type - use the mapped C++ type
                            successType = typeMapping.get(resp.baseType);
                        }
                    } else {
                        String errorBaseType = resp.baseType;
                        String errorType="";

                        if (errorBaseType != null && !errorBaseType.isEmpty()) {
                            hasAnyResponseSchema = true;
                            if(!typeMapping.containsKey(errorBaseType)) {
                                // Custom model type
                                String className = stripPathFromClassName(errorBaseType).get("className");
                                errorType = "models::" + toPascalCase(className);
                                modelsUsed.add(className);
                            }
                            else {
                                errorType = typeMapping.get(resp.baseType);
                            }
                        }
                        if (hasAnyResponseSchema) {
                            errorTypes.add(errorType);
                            Map<String, String> item = new HashMap<>();
                            item.put("typeName", errorType);
                            item.put("statusCode", resp.code);
                            statusCodeToTypes.add(item);
                        }
                    }
                }
            }
            // Store vendor extensions for Mustache templates
            if(successType != null && !successType.isEmpty()) {
                op.vendorExtensions.put("successType", successType);
            }
            if(errorTypes != null && !errorTypes.isEmpty()) {
                op.vendorExtensions.put("errorTypes", errorTypes);
            }
            if(!statusCodeToTypes.isEmpty()) {
                op.vendorExtensions.put("statusCodeToTypes", statusCodeToTypes);
            }
            op.vendorExtensions.put("hasAnyResponseSchema", (successType != null && !successType.isEmpty()) || !errorTypes.isEmpty());
        }
        // Add modelsUsed to objs for header includes
        objs.put("modelsUsed", new ArrayList<>(modelsUsed));
        return objs;
    }    /**
     * Preprocesses the OpenAPI specification to enhance inline schemas with proper titles.
     * This method ensures that request and response schemas without explicit titles
     * get automatically generated names based on the operation ID.
     * 
     * @param openAPI the OpenAPI specification to preprocess
     */
    @Override
    @SuppressWarnings("rawtypes")
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        
        // Process schemas in components section first
        if (openAPI.getComponents() != null && openAPI.getComponents().getSchemas() != null) {
            for (Map.Entry<String, Schema> entry : openAPI.getComponents().getSchemas().entrySet()) {
                Schema schema = entry.getValue();
                if (schema.getTitle() == null) {
                    String title = toPascalCase(entry.getKey());
                    schema.setTitle(title);
                }
            }
        }
        
        // Process response schemas in components section
        if (openAPI.getComponents() != null && openAPI.getComponents().getResponses() != null) {
            for (Map.Entry<String, ApiResponse> entry : openAPI.getComponents().getResponses().entrySet()) {
                ApiResponse apiResponse = entry.getValue();
                String responseName = entry.getKey();

                if (apiResponse.getContent() != null) {
                    MediaType mediaType = apiResponse.getContent().get("application/json");
                    if (mediaType != null && mediaType.getSchema() != null) {
                        Schema<?> schema = mediaType.getSchema();
                        if (schema.getTitle() == null) {
                            String title = toPascalCase(responseName);
                            schema.setTitle(title);
                        }

                        // Also process nested schemas within the response schema
                        processNestedSchemas(schema, responseName);
                    }
                }
            }
        }

        if (openAPI.getPaths() != null) {
            for (Map.Entry<String, PathItem> entry : openAPI.getPaths().entrySet()) {
                PathItem pathItem = entry.getValue();
                for (Map.Entry<PathItem.HttpMethod, Operation> opEntry : pathItem.readOperationsMap().entrySet()) {
                    Operation op = opEntry.getValue();
                    String operationId = op.getOperationId();

                    // Generate a fallback name if operationId is null
                    if (operationId == null || operationId.isEmpty()) {
                        operationId = toPascalCase(opEntry.getKey().name()) + "_" + toPascalCase(entry.getKey().replaceAll("[{}]", ""));
                    }

                    // Set title for inline request schemas
                    if (op.getRequestBody() != null && op.getRequestBody().getContent() != null) {
                        Map<String, MediaType> content = op.getRequestBody().getContent();
                        MediaType mediaType = content.get("application/json");
                        if (mediaType != null && mediaType.getSchema() != null) {
                            Schema<?> schema = mediaType.getSchema();
                            if (schema.getTitle() == null && schema.get$ref() == null) {
                                String title = toPascalCase(operationId) + "Request";
                                schema.setTitle(title);
                            }
                        }
                    }

                    // Set title for inline response schemas
                    if (op.getResponses() != null) {
                        for (Map.Entry<String, ApiResponse> respEntry : op.getResponses().entrySet()) {
                            ApiResponse apiResp = respEntry.getValue();
                            if (apiResp.getContent() != null) {
                                MediaType mediaType = apiResp.getContent().get("application/json");
                                if (mediaType != null && mediaType.getSchema() != null) {
                                    Schema<?> schema = mediaType.getSchema();
                                    if (schema.getTitle() == null && schema.get$ref() == null) {
                                        String title = toPascalCase(operationId) + "Response" + respEntry.getKey();
                                        schema.setTitle(title);
                                    }
                                }
                            }
                        }
                    }

                    // Set title for inline parameter schemas
                    if (op.getParameters() != null) {
                        for (io.swagger.v3.oas.models.parameters.Parameter parameter : op.getParameters()) {
                            if (parameter.getSchema() != null) {
                                Schema<?> schema = parameter.getSchema();
                                if (schema.getTitle() == null && schema.get$ref() == null) {
                                    String title = toPascalCase(operationId) + toPascalCase(parameter.getName()) + "Parameter";
                                    schema.setTitle(title);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Recursively processes nested schemas within a parent schema to set meaningful titles.
     * This handles cases like component responses that have nested inline object schemas.
     *
     * @param schema the parent schema to process
     * @param parentName the name of the parent (used for generating child names)
     */
    @SuppressWarnings("rawtypes")
    private void processNestedSchemas(Schema schema, String parentName) {
        if (schema == null) {
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
                        processNestedSchemas(propSchema, title);
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
                    processNestedSchemas(itemSchema, title);
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
                    processNestedSchemas(additionalSchema, title);
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
            modelNamespaceBase = toLowerCase(MODEL_SUFFIX);
        }

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            ModelsMap modelsMap = entry.getValue();

            // Compute className from entry key
            Map<String, String> pathFromClassName = stripPathFromClassName(entry.getKey());
            String modelClassName = pathFromClassName.getOrDefault("className", toModelName(entry.getKey()));
            String modelNamespace = modelNamespaceBase;
            // Append className to modelNamespace
            if(!modelClassName.isEmpty()) {
                modelNamespace = modelNamespaceBase;// + "::" + toLowerCase(modelClassName);
            }

            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (model != null) {
                    // Set in vendorExtensions for backward compatibility
                    model.vendorExtensions.put("modelNamespace", modelNamespace);
                    model.vendorExtensions.put("modelClassName", toPascalCase(modelClassName));

                    // --- Filter and set imports for mustache ---
                    Set<String> filteredImports = new LinkedHashSet<>();
                    if (model.imports != null) {
                        for (String imp : model.imports) {
                            // Only add if not a primitive, not a mapped type, and not "object" or "nlohmann::json"
                            if (!languageSpecificPrimitives.contains(imp)
                                    && !typeMapping.containsKey(imp)
                                    && !"object".equals(imp)
                                    && !"nlohmann::json".equals(imp)) {

                                String headerName = toPascalCase(imp);
                                filteredImports.add("#include \"" + headerName + ".h\"");
                            }
                        }
                    }
                    // Set the imports for mustache template
                    model.vendorExtensions.put("filteredImports", new ArrayList<>(filteredImports));
                    // --- End filter and set imports ---

                    //   Process model variables
                    if (model.vars != null) {
                        for (CodegenProperty var : model.vars) {
                            processModelVariable(var, model);
                        }
                    }
                    if (model.allVars != null) {
                        for (CodegenProperty var : model.allVars) {
                            processModelVariable(var, model);
                        }
                    }
                    if ((model.vars == null || model.vars.isEmpty()) && model.allVars != null && !model.allVars.isEmpty()) {
                        model.vars = new ArrayList<>(model.allVars);
                    }
                }
            }
        }
    
        return processed;
    }
    
    /**
     * Processes individual model variables to configure C++ specific attributes.
     * Handles nullable types, container types, and default values according to C++ conventions.
     * 
     * @param var the model variable to process
     * @param model the parent model containing this variable
     */
    private void processModelVariable(CodegenProperty var, CodegenModel model) {
        // Ensure baseName is set for JSON serialization
        if (var.baseName == null || var.baseName.isEmpty()) {
            var.baseName = var.name;
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

        // Handle container types
        if (var.isArray && !var.dataType.startsWith("std::vector<")) {
            String itemType = var.items != null ? var.items.dataType : "std::string";
            var.dataType = "std::vector<" + itemType + ">";
        }

        if (var.isMap && !var.dataType.startsWith("std::map<")) {
            String itemType = var.items != null ? var.items.dataType : "std::string";
            var.dataType = "std::map<std::string, " + itemType + ">";
        }

        // Set default values
        if (var.defaultValue == null) {
            var.defaultValue = getDefaultValueForType(var.dataType, var.isNullable);
        }
    }

    /**
     * Generates appropriate default values for C++ types based on the data type and nullability.
     * 
     * @param dataType the C++ data type
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
        } else if (dataType.equals("int") || dataType.equals("long") || dataType.equals("float") || dataType.equals("double")) {
            return "0";
        } else if (dataType.startsWith("std::vector<")) {
            return "{}";
        } else if (dataType.startsWith("std::map<")) {
            return "{}";
        } else if (dataType.startsWith("std::optional<")) {
            return "std::nullopt";
        }
        
        return "{}";
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
     * Converts a model name to its corresponding filename.
     * 
     * @param name the model name
     * @return the filename without extension
     */
    @Override
    public String toModelFilename(String name) {
        String fileName = (String) stripPathFromClassName(name).get("className");
        if (fileName == null || fileName.isEmpty()) {
            fileName = "Model";
        }
        return toPascalCase(fileName);
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
        }
        else {
            String[] parts = apiName.split("[/]");
            if(parts.length > 1)
            {
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
            for (Schema schema : schemas) {
                variant.append(getTypeDeclaration(schema)).append(", ");
            }
            if (variant.length() > 0) {
                variant.setLength(variant.length() - 2); // Remove last comma
            }
            variant.append(">");
            return variant.toString();
        } else if (p.getAnyOf() != null && !p.getAnyOf().isEmpty()) {
            // Similar to oneOf, use std::any for anyOf
            return "std::any";
        } else if (ModelUtils.isArraySchema(p)) {
            // Handle arrays with full type declaration
            String inner = getTypeDeclaration(ModelUtils.getSchemaItems(p));
            return "std::vector<" + inner + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            // Handle maps with full type declaration
            String inner = getTypeDeclaration(ModelUtils.getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">";
        } else if (ModelUtils.isComposedSchema(p) && p.getAllOf() != null) {
            // allOf is handled by composition in fromModel
            return super.getTypeDeclaration(p);
        }
        return super.getTypeDeclaration(p);
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
            
            if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
                // Add composition logic if needed (e.g., inherit from allOf schemas)
                for (Schema allOfSchema : (List<Schema>) schema.getAllOf()) {
                    if (allOfSchema.get$ref() != null) {
                        String parentRef = ModelUtils.getSimpleRef(allOfSchema.get$ref());
                        model.parent = toModelName(parentRef);
                        break;
                    }
                }
            }
        }
        return model;
    }
    @Override
    @SuppressWarnings("rawtypes")
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p) || ModelUtils.isDateSchema(p) || 
            ModelUtils.isDateTimeSchema(p) || ModelUtils.isByteArraySchema(p)) {
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
            String inner = getTypeDeclaration(ModelUtils.getSchemaItems(p));
            return "std::vector<" + inner + ">()";
        }
        if (ModelUtils.isMapSchema(p)) {
            String inner = getTypeDeclaration(ModelUtils.getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">()";
        }
        if (p.get$ref() != null && !p.get$ref().isEmpty()) {
            return "std::make_shared<" + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + ">()";
        }
        return "nullptr";
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
            if (clean.toLowerCase().contains("error")) {
                result.put("namespace", "error");
                result.put("className", toPascalCase(clean));
                return result;
            }

            // ClassName is the rest, joined and PascalCased
            StringBuilder classNameBuilder = new StringBuilder();
            String namespace = "";
            if (parts.size() > 1) {
                // Use first part as namespace only for API class names, not models
                namespace = toLowerCase(parts.get(0));
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
            result.put("className", finalClassName);
            return result;
        } else {
            result.put("namespace","");
            result.put("className", toPascalCase(path));
            return result;
        }
        
    }

    /**
     * Adds project name to a class name or returns default name if project name is not set.
     * 
     * @param name the base name
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
     * Converts a string to PascalCase formatting.
     * Used for class names and type names.
     * 
     * @param name the string to convert
     * @return the PascalCase formatted string
     */
    public String toPascalCase(String name) {
        if(name == null || name.isEmpty()) {
            return "DefaultClass";
        }

        String result = camelize(name, false);
        return result.isEmpty() ? "DefaultClass" : result;
    }

    /**
     * Converts a string to camelCase formatting.
     * Used for variable names and method names.
     * 
     * @param name the string to convert
     * @return the camelCase formatted string
     */
    public String toCamelCase(String name) {
        if(name == null || name.isEmpty()) {
            return "defaultVariable";
        }
        String result = camelize(name, true);
        return result.isEmpty() ? "defaultVariable" : result;
    }

    /**
     * Converts a string to camelCase or PascalCase with proper case handling.
     * Handles uppercase words like "POST" -> "Post" and "GET" -> "Get".
     *
     * @param name the string to convert
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
        if (result.equals(name) && name.equals(name.toUpperCase()) && name.matches("[A-Z]+")) {
            // Handle all-caps words manually
            result = name.substring(0, 1).toUpperCase() + name.substring(1).toLowerCase();
            if (lowercaseFirstLetter) {
                result = result.substring(0, 1).toLowerCase() + result.substring(1);
            }
        }
        
        return result;
    }
    
    /**
     * Generates a handler function name from HTTP method and path.
     * 
     * @param httpMethod the HTTP method (GET, POST, etc.)
     * @param path the API path
     * @return the generated handler function name
     */
    public String toHandlerFunctionName(String httpMethod, String path) {
        String method = toPascalCase(httpMethod);
        String className = stripPathFromClassName(path).get("className");
        if (className == null || className.isEmpty()) {
            className = "Default";
        }
        return "handle" + method + "For" + className;
    }

    /**
     * Generates a handler function response type from the API path.
     * 
     * @param http Method the HTTP method (GET, POST, etc.)
     * @param path the API path
     * @return the generated handler function name
     */
    public String toHandlerFunctionResponse(String path) {
        String className = stripPathFromClassName(path).get("className");
        if (className == null || className.isEmpty()) {
            className = "Default";
        }
        return toPascalCase(className + "Response");
    }

    /**
     * Converts a string to lowercase.
     * Used for namespace names and identifiers.
     * 
     * @param name the string to convert
     * @return the lowercase string
     */
    public String toLowerCase(String name) {
        return name == null? "" : name.toLowerCase(Locale.ROOT);
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
        additionalProperties.put("cmakeProjectName", (String) StringUtils.underscore(projectName));


        // Set Enum namespace  
        String enumNamespace = (String) additionalProperties.get(ENUM_NAMESPACE);
        if (enumNamespace == null || enumNamespace.isEmpty()) {
            enumNamespace = toPascalCase(ENUM_SUFFIX);
        }
        additionalProperties.put(ENUM_NAMESPACE, enumNamespace);

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
        
        // Handle nullable parameters for C++
        if (!parameter.required) {
            // For non-required parameters, we can make them optional in C++
            if (!parameter.dataType.startsWith("std::optional<") && !parameter.isContainer) {
                parameter.vendorExtensions.put("optionalType", "std::optional<" + parameter.dataType + ">");
            }
        }
    }
}