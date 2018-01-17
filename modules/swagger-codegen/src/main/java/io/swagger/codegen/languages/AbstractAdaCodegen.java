package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenSecurity;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.helpers.ExtensionHelper;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.responses.ApiResponse;

import java.util.*;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {
    protected String packageName = "swagger";
    protected String projectName = "Swagger";
    protected List<Map<String, Object>> orderedModels;
    protected Map<String, List<String>> modelDepends;
    protected Map<String, String> nullableTypeMapping;
    protected HashMap<String, String> operationsScopes;
    protected int scopeIndex = 0;

    public AbstractAdaCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "abort",
                        "abs",
                        "abstract",
                        "accept",
                        "access",
                        "aliased",
                        "all",
                        "and",
                        "array",
                        "at",
                        "begin",
                        "body",
                        "case",
                        "constant",
                        "declare",
                        "delay",
                        "digits",
                        "do",
                        "else",
                        "elsif",
                        "end",
                        "entry",
                        "exception",
                        "exit",
                        "for",
                        "function",
                        "generic",
                        "goto",
                        "if",
                        "in",
                        "interface",
                        "is",
                        "limited",
                        "loop",
                        "mod",
                        "new",
                        "not",
                        "null",
                        "of",
                        "or",
                        "others",
                        "out",
                        "overriding",
                        "package",
                        "pragma",
                        "private",
                        "procedure",
                        "protected",
                        "raise",
                        "range",
                        "record",
                        "rem",
                        "renames",
                        "requeue",
                        "return",
                        "reverse",
                        "select",
                        "separate",
                        "some",
                        "subtype",
                        "synchronized",
                        "tagged",
                        "task",
                        "terminate",
                        "then",
                        "type",
                        "until",
                        "use",
                        "when",
                        "while",
                        "with",
                        "xor")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "Swagger.Date");
        typeMapping.put("DateTime", "Swagger.Datetime");
        typeMapping.put("string", "Swagger.UString");
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Swagger.Long");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("array", "Swagger.Vector");
        typeMapping.put("map", "Swagger.Map");
        typeMapping.put("object", "Swagger.Object");
        typeMapping.put("number", "Swagger.Number");
        typeMapping.put("UUID", "Swagger.UString");
        typeMapping.put("file", "Swagger.Http_Content_Type");
        typeMapping.put("binary", "Swagger.Binary");

        nullableTypeMapping = new HashMap<String, String>();
        nullableTypeMapping.put("date", "Swagger.Nullable_Date");
        nullableTypeMapping.put("DateTime", "Swagger.Nullable_Date");
        nullableTypeMapping.put("string", "Swagger.Nullable_UString");
        nullableTypeMapping.put("integer", "Swagger.Nullable_Integer");
        nullableTypeMapping.put("long", "Swagger.Nullable_Long");
        nullableTypeMapping.put("boolean", "Swagger.Nullable_Boolean");
        nullableTypeMapping.put("object", "Swagger.Object");

        modelDepends = new HashMap<String, List<String>>();
        orderedModels = new ArrayList<Map<String, Object>>();
        operationsScopes = new HashMap<String, String>();
        super.importMapping = new HashMap<String, String>();

        // CLI options
        addOption(CodegenConstants.PROJECT_NAME, "GNAT project name",
                  this.projectName);

        modelNameSuffix = "_Type";
        embeddedTemplateDir = templateDir = "Ada";

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("integer", "boolean", "Integer", "Character", "Boolean", "long", "float", "double"));
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    public String toFilename(String name) {
        return name.replace(".", "-").toLowerCase();
    }

    /**
     * Turn a parameter name, operation name into an Ada identifier.
     *
     * Ada programming standard avoid the camelcase syntax and prefer the underscore
     * notation.  We also have to make sure the identifier is not a reserved keyword.
     * When this happens, we add the configurable prefix.  The function translates:
     *
     * body              - P_Body
     * petId             - Pet_Id
     * updatePetWithForm - Update_Pet_With_Form
     *
     * @param name the parameter name.
     * @param prefix the optional prefix in case the parameter name is a reserved keyword.
     * @return the Ada identifier to be used.
     */
    protected String toAdaIdentifier(String name, String prefix) {
        // We cannot use reserved keywords for identifiers
        if (isReservedWord(name)) {
            LOGGER.warn("Identifier '" + name + "' is a reserved word, renamed to " + prefix + name);
            name = prefix + name;
        }
        StringBuilder result = new StringBuilder();
        boolean needUpperCase = true;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (needUpperCase) {
                needUpperCase = false;
                result.append(Character.toUpperCase(c));

            } else if (Character.isUpperCase((c))) {
                if (!needUpperCase) {
                    result.append('_');
                }
                result.append(c);
                needUpperCase = false;
            } else {
                result.append(c);
                if (c == '_') {
                    needUpperCase = true;
                }
            }
        }
        return result.toString();
    }

    @Override
    public String toOperationId(String operationId) {
        return toAdaIdentifier(sanitizeName(operationId), "Call_");
    }

    @Override
    public String toVarName(String name) {
        return toAdaIdentifier(sanitizeName(name), "P_");
    }

    @Override
    public String toParamName(String name) {
        return toAdaIdentifier(super.toParamName(name), "P_");
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    public String toModelName(final String name) {
        String result = super.toModelName(name);
        if (result.matches("^\\d.*") || result.startsWith("_")) {
            result = "Model_" + result;
        }
        return result.replaceAll("[\\.-]", "_").replaceAll("__+", "_");
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema schema) {
        CodegenProperty property = super.fromProperty(name, schema);
        String nameInCamelCase = property.nameInCamelCase;
        nameInCamelCase = sanitizeName(nameInCamelCase);
        property.nameInCamelCase = nameInCamelCase;
        return property;
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
        return "p_" + name; // add an underscore to the name
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*").replace("-", "_");
    }

    /**
     * Override the Mustache compiler configuration.
     *
     * We don't want to have special characters escaped
     *
     * @param compiler the compiler.
     * @return the compiler to use.
     */
    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        compiler = super.processCompiler(compiler).emptyStringIsFalse(true);

        return compiler.withEscaper(Escapers.NONE);
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
    public String getTypeDeclaration(Schema property) {
        String schemaType = getSchemaType(property);

        if (schemaType != null) {
            schemaType = schemaType.replace("-", "_");
        }

        if (property instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) property;
            Schema inner = arraySchema.getItems();
            return getTypeDeclaration(inner) + "_Vectors.Vector";
        }
        if (property instanceof MapSchema && hasSchemaProperties(property)) {
            MapSchema mapSchema = (MapSchema) property;
            Schema inner = (Schema) mapSchema.getAdditionalProperties();
            String name = getTypeDeclaration(inner) + "_Map";
            if (name.startsWith("Swagger.")) {
                return name;
            } else {
                return "Swagger." + name;
            }
        }
        if (typeMapping.containsKey(schemaType)) {
            /** todo: find out how to check if a schema property is required on oas 3
            if (property.getRequired()) {
                return typeMapping.get(schemaType);
            } else {
                return nullableTypeMapping.get(schemaType);
            }
            */
        }
        //  LOGGER.info("Swagger type " + swaggerType);
        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }
        String modelType = toModelName(schemaType).replace("-", "_");
        if (property instanceof StringSchema || property instanceof DateSchema
                || property instanceof DateTimeSchema || property instanceof FileSchema
                || languageSpecificPrimitives.contains(modelType)) {
            return modelType;
        }

        return modelPackage + ".Models." + modelType;
    }

    /**
     * Overrides postProcessParameter to add a vendor extension "x-is-model-type".
     * This boolean indicates that the parameter comes from the model package.
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        if (parameter.dataType == null) {
            return;
        }
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel
                && !getBooleanValue(parameter, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME)
                && !getBooleanValue(parameter, CodegenConstants.IS_DATE_EXT_NAME)
                && !getBooleanValue(parameter, CodegenConstants.IS_STRING_EXT_NAME)
                && !getBooleanValue(parameter, CodegenConstants.IS_CONTAINER_EXT_NAME)
                && !getBooleanValue(parameter, CodegenConstants.IS_FILE_EXT_NAME)) {
            isModel = true;
        }
        parameter.vendorExtensions.put("x-is-model-type", isModel);
    }

    /**
     * Post process the media types (produces and consumes) for Ada code generator.
     *
     * For each media type, add a adaMediaType member that gives the Ada enum constant
     * for the corresponding type.
     *
     * @param types the list of media types.
     * @return the number of media types.
     */
    protected int postProcessMediaTypes(List<Map<String, String>> types) {
        int count = 0;
        if (types != null) {
            for (Map<String, String> media : types) {
                String mt = media.get("mediaType");
                if (mt != null) {
                    mt = mt.replace('/', '_');
                    media.put("adaMediaType", mt.toUpperCase());
                    count++;
                }
            }
        }
        return count;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Schema> definitions, OpenAPI openAPI) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, definitions, openAPI);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                Schema schema = getSchemaFromResponse(methodResponse);
                if (schema != null) {
                    CodegenProperty codegenProperty = fromProperty("response", schema);
                    codegenOperation.vendorExtensions.put("x-codegen-response", codegenProperty);
                    if(codegenProperty.datatype == "HttpContent") {
                        codegenOperation.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }
        return codegenOperation;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation codegenOperation : operationList) {
            if (codegenOperation.summary != null) {
                codegenOperation.summary = codegenOperation.summary.trim();
            }
            if (codegenOperation.notes != null) {
                codegenOperation.notes = codegenOperation.notes.trim();
            }
            codegenOperation.vendorExtensions.put("x-has-uniq-produces", postProcessMediaTypes(codegenOperation.produces) == 1);
            codegenOperation.vendorExtensions.put("x-has-uniq-consumes", postProcessMediaTypes(codegenOperation.consumes) == 1);
            codegenOperation.vendorExtensions.put("x-has-notes", codegenOperation.notes != null && codegenOperation.notes.length() > 0);

            // Set the file parameter type for both allParams and formParams.
            for (CodegenParameter codegenParameter : codegenOperation.allParams) {
                if (getBooleanValue(codegenParameter, CodegenConstants.IS_FORM_PARAM_EXT_NAME)
                        && getBooleanValue(codegenParameter, CodegenConstants.IS_FILE_EXT_NAME)) {
                    codegenParameter.dataType = "Swagger.File_Part_Type";
                }
            }
            for (CodegenParameter codegenParameter : codegenOperation.formParams) {
                if (getBooleanValue(codegenParameter, CodegenConstants.IS_FILE_EXT_NAME)) {
                    codegenParameter.dataType = "Swagger.File_Part_Type";
                }
            }
            postProcessAuthMethod(codegenOperation.authMethods);

            /*
             * Scan the path parameter to construct a x-path-index that tells the index of
             * the path parameter.
             */
            for (CodegenParameter p : codegenOperation.pathParams) {
                String path = codegenOperation.path;
                int pos = 0;
                int index = 0;
                while (pos >= 0 && pos < path.length()) {
                    int last;
                    pos = path.indexOf('{', pos);
                    if (pos < 0) {
                        break;
                    }
                    pos++;
                    last = path.indexOf('}', pos);
                    index++;
                    if (last < 0) {
                        break;
                    }
                    if (path.substring(pos, last - 1) == p.baseName) {
                        break;
                    }
                    pos = last + 1;
                }
                p.vendorExtensions.put("x-path-index", index);
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // Collect the model dependencies.
        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> modelMap : models) {
            Object model = modelMap.get("model");
            if (model instanceof CodegenModel) {
                CodegenModel codegenModel = (CodegenModel) model;
                List<String> d = new ArrayList<String>();
                for (CodegenProperty codegenProperty : codegenModel.allVars) {
                    boolean isModel = false;
                    CodegenProperty item = codegenProperty;
                    if (getBooleanValue(codegenProperty, CodegenConstants.IS_CONTAINER_EXT_NAME)) {
                        item = codegenProperty.items;
                    }
                    if (item != null
                            && !getBooleanValue(item, CodegenConstants.IS_STRING_EXT_NAME)
                            && !getBooleanValue(item, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME)
                            && !getBooleanValue(item, CodegenConstants.IS_CONTAINER_EXT_NAME)
                            && !getBooleanValue(item, CodegenConstants.IS_INTEGER_EXT_NAME)) {
                        if (!d.contains(item.datatype)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(item.datatype);
                            isModel = true;
                        }
                    }
                    codegenProperty.vendorExtensions.put("x-is-model-type", isModel);
                }
                modelDepends.put(codegenModel.name, d);
                orderedModels.add(modelMap);
            }
        }

        // Sort the models according to dependencies so that model that depend
        // on others appear at end of the list.
        final Map<String, List<String>> deps = modelDepends;
        Collections.sort(orderedModels, new Comparator<Map<String, Object>>() {
            @Override
            public int compare(Map<String, Object> lhs, Map<String, Object> rhs) {
                Object v = lhs.get("model");
                String lhsName = ((CodegenModel) v).name;
                v = rhs.get("model");
                String rhsName = ((CodegenModel) v).name;
                List<String> lhsList = deps.get(lhsName);
                List<String> rhsList = deps.get(rhsName);
                if (lhsList == rhsList) {
                    // LOGGER.info("First compare " + lhsName + "<" + rhsName);
                    return lhsName.compareTo(rhsName);
                }
                // Put models without dependencies first.
                if (lhsList == null) {
                    // LOGGER.info("  Empty " + lhsName + ", no check " + rhsName);
                    return -1;
                }
                if (rhsList == null) {
                    // LOGGER.info("  No check " + lhsName + ", empty " + rhsName);
                    return 1;
                }
                // Put models that depend on another after.
                if (lhsList.contains(rhsName)) {
                    // LOGGER.info("  LSH " + lhsName + " uses " + rhsName);
                    return 1;
                }
                if (rhsList.contains(lhsName)) {
                    // LOGGER.info("  RHS " + rhsName + " uses " + lhsName);
                    return -1;
                }
                // Put models with less dependencies first.
                if (lhsList.size() < rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " < RHS size " + rhsName);
                    return -1;
                }
                if (lhsList.size() > rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " > RHS size " + rhsName);
                    return 1;
                }
                // Sort models on their name.
                // LOGGER.info("Compare " + lhsName + "<" + rhsName);
                return lhsName.compareTo(rhsName);
            }
        });
        /* for (Map<String, Object> model : orderedModels) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                LOGGER.info("Order: " + m.name);
            }
        }*/
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs.put("orderedModels", orderedModels);
        OpenAPI openAPI = (OpenAPI) objs.get("openapi");
        if(openAPI != null) {
            try {
                objs.put("openapi-json", Json.pretty().writeValueAsString(openAPI).replace("\r\n", "\n"));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }

        /**
         * Collect the scopes to generate unique identifiers for each of them.
         */
        List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
        postProcessAuthMethod(authMethods);

        return super.postProcessSupportingFileData(objs);
    }

    /**
     * Collect the scopes to generate a unique identifier for each of them.
     *
     * @param authMethods the auth methods with their scopes.
     */
    private void postProcessAuthMethod(List<CodegenSecurity> authMethods) {
        if (authMethods != null) {
            for (CodegenSecurity authMethod : authMethods) {
                if (authMethod.scopes != null) {
                    String name = authMethod.scopes.get("scope");
                    if (operationsScopes.containsKey(name)) {
                        authMethod.scopes.put("ident", operationsScopes.get(name));
                    } else {
                        String ident;
                        if (name.startsWith("https://")) {
                            int pos = name.lastIndexOf('/');
                            ident = name.substring(pos + 1);
                        } else {
                            ident = name;
                        }
                        scopeIndex++;
                        ident = toAdaIdentifier(sanitizeName(ident.replaceAll(":", "_")), "S_");
                        if (operationsScopes.containsValue(ident)) {
                            ident = ident + "_" + scopeIndex;
                        }
                        operationsScopes.put(name, ident);
                        authMethod.scopes.put("ident", ident);
                    }
                }
                authMethod.name = camelize(sanitizeName(authMethod.name), true);
            }
        }
    }
}
