package io.swagger.codegen.languages;


import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.FileSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.NumberSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.oas.models.responses.ApiResponse;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class PistacheServerCodegen extends AbstractCppCodegen {
    protected String implFolder = "impl";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "pistache-server";
    }

    @Override
    public String getHelp() {
        return "Generates a C++ API server (based on Pistache)";
    }

    public PistacheServerCodegen() {
        super();

        apiPackage = "io.swagger.server.api";
        modelPackage = "io.swagger.server.model";

        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");
        apiTemplateFiles.put("api-impl-header.mustache", ".h");
        apiTemplateFiles.put("api-impl-source.mustache", ".cpp");
        apiTemplateFiles.put("main-api-server.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "pistache-server";

        cliOptions.clear();

        reservedWords = new HashSet<>();

        supportingFiles.add(new SupportingFile("modelbase-header.mustache", "model", "ModelBase.h"));
        supportingFiles.add(new SupportingFile("modelbase-source.mustache", "model", "ModelBase.cpp"));
        supportingFiles.add(new SupportingFile("cmake.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "std::string");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");

        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("Object", "#include \"Object.h\"");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name; // add an underscore to the name
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".h\"";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allSchemas) {
        CodegenModel codegenModel = super.fromModel(name, schema, allSchemas);

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }

        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
                                          Map<String, Schema> schemas, OpenAPI openAPI) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, schemas, openAPI);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                Schema schemaResponse = getSchemaFromResponse(methodResponse);
                if (schemaResponse != null) {
                    CodegenProperty cm = fromProperty("response", schemaResponse);
                    codegenOperation.vendorExtensions.put("x-codegen-response", cm);
                    if(cm.datatype == "HttpContent") {
                        codegenOperation.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }

        String pathForPistache = path.replaceAll("\\{(.*?)}", ":$1");
        codegenOperation.vendorExtensions.put("x-codegen-pistache-path", pathForPistache);

        return codegenOperation;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        String classname = (String) operations.get("classname");
        operations.put("classnameSnakeUpperCase", DefaultCodegen.underscore(classname).toUpperCase());
        operations.put("classnameSnakeLowerCase", DefaultCodegen.underscore(classname).toLowerCase());

        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            boolean consumeJson = false;
            boolean isParsingSupported = true;
            if (op.bodyParam != null) {
                if (op.bodyParam.vendorExtensions == null) {
                    op.bodyParam.vendorExtensions = new HashMap<>();
                }

                op.bodyParam.vendorExtensions.put("x-codegen-pistache-isStringOrDate",
                        getBooleanValue(op.bodyParam, CodegenConstants.IS_STRING_EXT_NAME) ||getBooleanValue(op.bodyParam, CodegenConstants.IS_DATE_EXT_NAME));
            }
            if(op.consumes != null) {
                for (Map<String, String> consume : op.consumes) {
                    if (consume.get("mediaType") != null && consume.get("mediaType").equals("application/json")) {
                        consumeJson = true;
                    }
                }
            }

            op.httpMethod = op.httpMethod.substring(0, 1).toUpperCase() + op.httpMethod.substring(1).toLowerCase();

            for(CodegenParameter param : op.allParams){
                if (getBooleanValue(param, CodegenConstants.IS_FORM_PARAM_EXT_NAME)) {
                    isParsingSupported=false;
                }
                if (getBooleanValue(param, CodegenConstants.IS_FILE_EXT_NAME)) {
                    isParsingSupported=false;
                }
                if (getBooleanValue(param, CodegenConstants.IS_COOKIE_PARAM_EXT_NAME)) {
                    isParsingSupported=false;
                }

                //TODO: This changes the info about the real type but it is needed to parse the header params
                if (getBooleanValue(param, CodegenConstants.IS_HEADER_PARAM_EXT_NAME)) {
                    param.dataType = "Optional<Net::Http::Header::Raw>";
                    param.baseType = "Optional<Net::Http::Header::Raw>";
                } else if(getBooleanValue(param, CodegenConstants.IS_QUERY_PARAM_EXT_NAME)){
                    if(getBooleanValue(param, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME)) {
                        param.dataType = "Optional<" + param.dataType + ">";
                    } else {
                        param.dataType = "Optional<" + param.baseType + ">";
                        param.baseType = "Optional<" + param.baseType + ">";
                    }
                }
            }

            if (op.vendorExtensions == null) {
                op.vendorExtensions = new HashMap<>();
            }
            op.vendorExtensions.put("x-codegen-pistache-consumesJson", consumeJson);
            op.vendorExtensions.put("x-codegen-pistache-isParsingSupported", isParsingSupported);
        }

        return objs;
    }

    @Override
    public String toModelFilename(String name) {
        return initialCaps(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if ( templateName.endsWith("impl-header.mustache") ) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + result.substring(ix, result.length() - 2) + "Impl.h";
            result = result.replace(apiFileFolder(), implFileFolder());
        } else if ( templateName.endsWith("impl-source.mustache") ) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + result.substring(ix, result.length() - 4) + "Impl.cpp";
            result = result.replace(apiFileFolder(), implFileFolder());
        } else if ( templateName.endsWith("api-server.mustache") ) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + result.substring(ix, result.length() - 4) + "MainServer.cpp";
            result = result.replace(apiFileFolder(), outputFolder);
        }
        return result;
    }

    @Override
    public String toApiFilename(String name) {
        return initialCaps(name) + "Api";
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     *         `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema propertySchema) {
        String schemaType = getSchemaType(propertySchema);
        if (propertySchema instanceof ArraySchema) {
            Schema inner = ((ArraySchema) propertySchema).getItems();
            return String.format("%s<%s>", schemaType, getTypeDeclaration(inner));
        } else if (propertySchema instanceof MapSchema) {
            Schema inner = propertySchema.getAdditionalProperties();
            return String.format("%s<std::string, %s>", schemaType, getTypeDeclaration(inner));
        }
        if (propertySchema instanceof StringSchema || propertySchema instanceof DateSchema
                || propertySchema instanceof DateTimeSchema || propertySchema instanceof FileSchema
                || languageSpecificPrimitives.contains(schemaType)) {
            return toModelName(schemaType);
        }
        return super.getTypeDeclaration(propertySchema);
    }

    @Override
    public String toDefaultValue(Schema propertySchema) {
        if (propertySchema instanceof StringSchema) {
            return "\"\"";
        } else if (propertySchema instanceof BooleanSchema) {
            return "false";
        } else if (propertySchema instanceof DateSchema) {
            return "\"\"";
        } else if (propertySchema instanceof DateTimeSchema) {
            return "\"\"";
        } else if (propertySchema instanceof NumberSchema) {
            if(SchemaTypeUtil.FLOAT_FORMAT.equals(propertySchema.getFormat())) {
                return "0.0f";
            }
            return "0.0";
        } else if (propertySchema instanceof IntegerSchema) {
            if(SchemaTypeUtil.INTEGER64_FORMAT.equals(propertySchema.getFormat())) {
                return "0L";
            }
            return "0";
        } else if (propertySchema instanceof MapSchema) {
            String inner = getSchemaType(propertySchema.getAdditionalProperties());
            return "std::map<std::string, " + inner + ">()";
        } else if (propertySchema instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) propertySchema;
            String inner = getSchemaType(arraySchema.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner = "std::shared_ptr<" + inner + ">";
            }
            return "std::vector<" + inner + ">()";
        } else if (StringUtils.isNotBlank(propertySchema.get$ref())) {
            return "new " + toModelName(propertySchema.get$ref()) + "()";
        }
        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = getBooleanValue(parameter, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
        boolean isListContainer = getBooleanValue(parameter, CodegenConstants.IS_LIST_CONTAINER_EXT_NAME);
        boolean isString = getBooleanValue(parameter, CodegenConstants.IS_STRING_EXT_NAME);

        if (!isPrimitiveType && !isListContainer && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
            parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
        }
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    public String modelFileFolder() {
        return (outputFolder + "/model").replace("/", File.separator);
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }

    private String implFileFolder() {
        return (outputFolder + "/" + implFolder).replace("/", File.separator);
    }

    /**
     * Optional - swagger type conversion. This is used to map swagger types in
     * a `Property` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.oas.models.media.Schema
     */
    @Override
    public String getSchemaType(Schema propertySchema) {
        String schemaType = super.getSchemaType(propertySchema);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = schemaType;
        }
        return toModelName(type);
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) || typeMapping.values().contains(type)
                || importMapping.values().contains(type) || defaultIncludes.contains(type)
                || languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toApiName(String type) {
        return Character.toUpperCase(type.charAt(0)) + type.substring(1) + "Api";
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }
}
