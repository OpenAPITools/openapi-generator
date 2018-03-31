package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.properties.*;
import io.swagger.util.Json;

import java.util.*;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {
    protected String packageName = "defaultPackage";
    protected String projectName = "defaultProject";
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
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty property = super.fromProperty(name, p);
        if (property != null) {
            String nameInCamelCase = property.nameInCamelCase;
            nameInCamelCase = sanitizeName(nameInCamelCase);
            property.nameInCamelCase = nameInCamelCase;
        }
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
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);

        if (swaggerType != null) {
            swaggerType = swaggerType.replace("-", "_");
        }

        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner) + "_Vectors.Vector";
        }
        if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            String name = getTypeDeclaration(inner) + "_Map";
            if (name.startsWith("Swagger.")) {
                return name;
            } else {
                return "Swagger." + name;
            }
        }
        if (typeMapping.containsKey(swaggerType)) {
            if (p.getRequired()) {
                return typeMapping.get(swaggerType);
            } else {
                return nullableTypeMapping.get(swaggerType);
            }
        }
        //  LOGGER.info("Swagger type " + swaggerType);
        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }
        String modelType = toModelName(swaggerType).replace("-", "_");
        if (p instanceof StringProperty || p instanceof DateProperty
                || p instanceof DateTimeProperty || p instanceof FileProperty
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
    public void postProcessParameter(CodegenParameter parameter){
        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        if (parameter.dataType == null) {
            return;
        }
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isString && !parameter.isContainer && !parameter.isFile) {
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
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
                                          Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            Response methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                if (methodResponse.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", methodResponse.getSchema());
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if(cm.datatype == "HttpContent") {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }
        return op;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op1 : operationList) {
            if (op1.summary != null) {
                op1.summary = op1.summary.trim();
            }
            if (op1.notes != null) {
                op1.notes = op1.notes.trim();
            }
            op1.vendorExtensions.put("x-has-uniq-produces", postProcessMediaTypes(op1.produces) == 1);
            op1.vendorExtensions.put("x-has-uniq-consumes", postProcessMediaTypes(op1.consumes) == 1);
            op1.vendorExtensions.put("x-has-notes", op1.notes != null && op1.notes.length() > 0);

            // Set the file parameter type for both allParams and formParams.
            for (CodegenParameter p : op1.allParams) {
                if (p.isFormParam && p.isFile) {
                    p.dataType = "Swagger.File_Part_Type";
                }
            }
            for (CodegenParameter p : op1.formParams) {
                if (p.isFile) {
                    p.dataType = "Swagger.File_Part_Type";
                }
            }
            postProcessAuthMethod(op1.authMethods);

            /*
             * Scan the path parameter to construct a x-path-index that tells the index of
             * the path parameter.
             */
            for (CodegenParameter p : op1.pathParams) {
                String path = op1.path;
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
        for (Map<String, Object> model : models) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                List<String> d = new ArrayList<String>();
                for (CodegenProperty p : m.allVars) {
                    boolean isModel = false;
                    CodegenProperty item = p;
                    if (p.isContainer) {
                        item = p.items;
                    }
                    if (item != null && !item.isString && !item.isPrimitiveType && !item.isContainer && !item.isInteger) {
                        if (!d.contains(item.datatype)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(item.datatype);
                        }
                        isModel = true;
                    }
                    p.vendorExtensions.put("x-is-model-type", isModel);
                }
                // let us work with fully qualified names only
                modelDepends.put(modelPackage + ".Models." + m.classname, d);
                orderedModels.add(model);
            }
        }

        // Sort models using dependencies:
        //   List revisedOrderedModels <- ()
        //   if you have N model, do N passes. In each pass look for an independent model
        //   cycle over orderedModels
        //     if I find a model that has no dependencies, or all of its dependencies are in revisedOrderedModels, consider it the independentModel
        //   put the independentModel at the end of revisedOrderedModels, and remove it from orderedModels
        //   
        List<Map<String, Object>> revisedOrderedModels = new ArrayList<Map<String, Object>>();
        List<String> collectedModelNames = new ArrayList<String>();
        int sizeOrderedModels = orderedModels.size();
        for (int i=0;i<sizeOrderedModels;i++) {
            Map<String, Object> independentModel = null;
            String independentModelName = null;
            for (Map<String, Object> model : orderedModels) {
                // let us work with fully qualified names only
                String modelName = modelPackage + ".Models." + ((CodegenModel) model.get("model")).classname;
                boolean dependent = false;
                for (String dependency : modelDepends.get(modelName)) {
                    if (!collectedModelNames.contains(dependency)) {
                        dependent = true;
                    }
                }
                if (!dependent) {
                    // this model was independent
                    independentModel = model;
                    independentModelName = modelName;
                }
            }
            if (null != independentModel) {
                // I have find an independentModel. Add it to revisedOrderedModels, and remove from orderedModels
                revisedOrderedModels.add(independentModel);
                collectedModelNames.add(independentModelName);
                orderedModels.remove(independentModel);
            }
        }
        // bookkeeping:
        // if I still have elements in orderedModels:
        //   if it's NOT last time I postProcessModels(), it means there are some dependencies that were not considered yet. That's not a problem
        //   if it's last iteration, there are circular dependencies.
        //  In any case, I add models still in orderedModels to revisedOrderedModels
        revisedOrderedModels.addAll(orderedModels);
        orderedModels = revisedOrderedModels;

        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs.put("orderedModels", orderedModels);
        Swagger swagger = (Swagger)objs.get("swagger");
        if(swagger != null) {
            String host = swagger.getBasePath();
            try {
                swagger.setHost("SWAGGER_HOST");
                objs.put("swagger-json", Json.pretty().writeValueAsString(swagger).replace("\r\n", "\n"));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
            swagger.setHost(host);
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
                    for (Map<String, Object> scope : authMethod.scopes) {
                        String name = (String) scope.get("scope");
                        if (operationsScopes.containsKey(name)) {
                            scope.put("ident", operationsScopes.get(name));
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
                            scope.put("ident", ident);
                        }
                    }
                }
                authMethod.name = camelize(sanitizeName(authMethod.name), true);
            }
        }
    }
}
