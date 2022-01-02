/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractAdaCodegen.class);

    protected String packageName = "defaultPackage";
    protected String projectName = "defaultProject";
    protected List<Map<String, Object>> orderedModels;
    protected final Map<String, List<String>> modelDepends;
    protected final Map<String, String> nullableTypeMapping;
    protected final Map<String, String> operationsScopes;
    protected int scopeIndex = 0;

    public AbstractAdaCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

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

        typeMapping = new HashMap<>();
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
        typeMapping.put("URI", "Swagger.UString");
        typeMapping.put("file", "Swagger.Http_Content_Type");
        typeMapping.put("binary", "Swagger.Binary");

        // Mapping to convert an Ada required type to an optional type (nullable).
        nullableTypeMapping = new HashMap<>();
        nullableTypeMapping.put("Swagger.Date", "Swagger.Nullable_Date");
        nullableTypeMapping.put("Swagger.Datetime", "Swagger.Nullable_Date");
        nullableTypeMapping.put("Swagger.UString", "Swagger.Nullable_UString");
        nullableTypeMapping.put("Integer", "Swagger.Nullable_Integer");
        nullableTypeMapping.put("Swagger.Long", "Swagger.Nullable_Long");
        nullableTypeMapping.put("Boolean", "Swagger.Nullable_Boolean");
        nullableTypeMapping.put("Swagger.Object", "Swagger.Object");

        modelDepends = new HashMap<>();
        orderedModels = new ArrayList<>();
        operationsScopes = new HashMap<>();
        super.importMapping = new HashMap<>();

        // CLI options
        addOption(CodegenConstants.PROJECT_NAME, "GNAT project name",
                this.projectName);

        modelNameSuffix = "Type";
        embeddedTemplateDir = templateDir = "Ada";

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("integer", "boolean", "number", "long", "float",
                        "double", "object", "string", "date", "DateTime", "binary"));
    }

    public String toFilename(String name) {
        return name.replace(".", "-").toLowerCase(Locale.ROOT);
    }

    /**
     * Turn a parameter name, operation name into an Ada identifier.
     * <p>
     * Ada programming standard avoid the camelcase syntax and prefer the underscore
     * notation.  We also have to make sure the identifier is not a reserved keyword.
     * When this happens, we add the configurable prefix.  The function translates:
     * <p>
     * body              - P_Body
     * petId             - Pet_Id
     * updatePetWithForm - Update_Pet_With_Form
     *
     * @param name   the parameter name.
     * @param prefix the optional prefix in case the parameter name is a reserved keyword.
     * @return the Ada identifier to be used.
     */
    protected String toAdaIdentifier(String name, String prefix) {
        // We cannot use reserved keywords for identifiers
        if (isReservedWord(name)) {
            LOGGER.warn("Identifier '{}' is a reserved word, renamed to {}{}", name, prefix, name);
            name = prefix + name;
        }
        StringBuilder result = new StringBuilder();
        boolean needUpperCase = true;
        boolean prevUpperCase = false;
        if (name.isEmpty() || Character.isDigit(name.charAt(0)) || name.charAt(0) == '_') {
            result.append(prefix);
        }
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            boolean isUpperOrDigit = Character.isUpperCase(c) || Character.isDigit(c);
            if (needUpperCase) {
                needUpperCase = false;
                prevUpperCase = isUpperOrDigit;
                result.append(Character.toUpperCase(c));

            } else if (isUpperOrDigit) {
                if (!prevUpperCase) {
                    result.append('_');
                }
                result.append(c);
                needUpperCase = false;
                prevUpperCase = true;
            } else {
                result.append(c);
                prevUpperCase = isUpperOrDigit;
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
    @Override
    public String toModelName(final String name) {
        String result = camelize(sanitizeName(name));

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            result = modelNamePrefix + "_" + result;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model_" + result;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, modelName);
            return modelName;
        }

        // model name starts with number
        if (result.matches("^\\d.*")) {
            String modelName = "Model_" + result; // e.g. 200Response => Model_200Response (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        if (languageSpecificPrimitives.contains(result)) {
            String modelName = "Model_" + result;
            LOGGER.warn("{} (model name matches existing language type) cannot be used as a model name. Renamed to {}",
                    name, modelName);
            return modelName;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            result = result + "_" + modelNameSuffix;
        }

        return result;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String var = null;
        if (value.isEmpty()) {
            var = "EMPTY";
        }

        // for symbol, e.g. $, #
        else if (getSymbolName(value) != null) {
            var = getSymbolName(value).toUpperCase(Locale.ROOT);
        }

        // number
        else if ("Integer".equals(datatype) || "Long".equals(datatype) ||
                "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            var = varName;
        }

        // string
        else {
            var = value.replaceAll("\\W+", "_").toUpperCase(Locale.ROOT);
            if (var.matches("\\d.*")) {
                var = "_" + var;
            } else {
                var = sanitizeName(var);
            }
        }
        return var;
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
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
     * <p>
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
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        String schemaType = getSchemaType(p);

        if (schemaType != null) {
            schemaType = schemaType.replace("-", "_");
        }

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner) + "_Vectors.Vector";
        }
        if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            String name = getTypeDeclaration(inner) + "_Map";
            if (name.startsWith("Swagger.")) {
                return name;
            } else {
                return "Swagger." + name;
            }
        }
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }
        //  LOGGER.info("OpenAPI type " + schemaType);
        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }
        String modelType = toModelName(schemaType).replace("-", "_");
        if (ModelUtils.isStringSchema(p) || ModelUtils.isFileSchema(p)
                || languageSpecificPrimitives.contains(modelType)) {
            return modelType;
        }

        return modelPackage + ".Models." + modelType;
    }

    private boolean isStreamType(CodegenProperty parameter) {
        boolean isStreamType = parameter.isString || parameter.isBoolean || parameter.isDate
                || parameter.isDateTime || parameter.isInteger || parameter.isLong
                || (parameter.isFreeFormObject && !parameter.isMap);

        return isStreamType;
    }

    private boolean isModelType(CodegenProperty parameter) {
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isFreeFormObject
                && !parameter.isString && !parameter.isContainer && !parameter.isFile
                && !parameter.dataType.startsWith("Swagger")) {
            isModel = true;
        }
        return isModel;
    }

    private boolean isStreamType(CodegenParameter parameter) {
        boolean isStreamType = parameter.isString || parameter.isBoolean || parameter.isDate
                || parameter.isDateTime || parameter.isInteger || parameter.isLong
                || (parameter.isFreeFormObject && !parameter.isMap);

        return isStreamType;
    }

    private boolean isModelType(CodegenParameter parameter) {
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isFreeFormObject
                && !parameter.isString && !parameter.isContainer && !parameter.isFile
                && !parameter.dataType.startsWith("Swagger")) {
            isModel = true;
        }
        return isModel;
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
        parameter.vendorExtensions.put("x-is-model-type", isModelType(parameter));
        parameter.vendorExtensions.put("x-is-stream-type", isStreamType(parameter));
    }

    /**
     * Post process the media types (produces and consumes) for Ada code generator.
     * <p>
     * For each media type, add an adaMediaType member that gives the Ada enum constant
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
                    media.put("adaMediaType", mt.toUpperCase(Locale.ROOT));
                    count++;
                }
            }
        }
        return count;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());
            if (methodResponse != null && ModelUtils.getSchemaFromResponse(methodResponse) != null) {
                CodegenProperty cm = fromProperty("response", ModelUtils.getSchemaFromResponse(methodResponse));
                op.vendorExtensions.put("x-codegen-response", cm);
                op.vendorExtensions.put("x-is-model-type", isModelType(cm));
                op.vendorExtensions.put("x-is-stream-type", isStreamType(cm));
                if ("HttpContent".equals(cm.dataType)) {
                    op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                }
            }
        }

        // Add a vendor extension attribute that provides a map of auth methods and the scopes
        // which are expected by the operation.  This map is then used by postProcessOperationsWithModels
        // to build another vendor extension that provides a subset of the auth methods with only
        // the scopes required by the operation.
        final List<SecurityRequirement> securities = operation.getSecurity();
        if (securities != null && securities.size() > 0) {
            final Map<String, SecurityScheme> securitySchemes = this.openAPI.getComponents() != null ? this.openAPI.getComponents().getSecuritySchemes() : null;
            final List<SecurityRequirement> globalSecurities = this.openAPI.getSecurity();

            Map<String, List<String>> scopes = getAuthScopes(securities, securitySchemes);
            if (scopes.isEmpty() && globalSecurities != null) {
                scopes = getAuthScopes(globalSecurities, securitySchemes);
            }
            op.vendorExtensions.put("x-scopes", scopes);
        }
        return op;
    }

    private Map<String, List<String>> getAuthScopes(List<SecurityRequirement> securities, Map<String, SecurityScheme> securitySchemes) {
        final Map<String, List<String>> scopes = new HashMap<>();
        Optional.ofNullable(securitySchemes).ifPresent(_securitySchemes -> {
            for (SecurityRequirement requirement : securities) {
                for (String key : requirement.keySet()) {
                    Optional.ofNullable(securitySchemes.get(key))
                            .ifPresent(securityScheme -> scopes.put(key, requirement.get(key)));
                }
            }
        });
        return scopes;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
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
                // Convert optional parameters to use the Nullable_<T> type.
                if (!p.required && nullableTypeMapping.containsKey(p.dataType)) {
                    p.dataType = nullableTypeMapping.get(p.dataType);
                }
            }
            for (CodegenParameter p : op1.formParams) {
                if (p.isFile) {
                    p.dataType = "Swagger.File_Part_Type";
                }
            }

            // Given the operation scopes and the auth methods, build a list of auth methods that only
            // describe the auth methods and scopes required by the operation.
            final Map<String, List<String>> scopes = (Map<String, List<String>>) op1.vendorExtensions.get("x-scopes");
            List<CodegenSecurity> opScopes = postProcessAuthMethod(op1.authMethods, scopes);
            if (opScopes != null) {
                op1.vendorExtensions.put("x-auth-scopes", opScopes);
            }

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
                    if (path.substring(pos, last).equals(p.baseName)) {
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
                List<String> d = new ArrayList<>();
                for (CodegenProperty p : m.vars) {
                    boolean isModel = false;
                    CodegenProperty item = p;
                    if (p.isContainer) {
                        item = p.items;
                    }
                    if (item != null && !item.isString && !item.isPrimitiveType && !item.isContainer && !item.isInteger) {
                        if (!d.contains(item.dataType)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(item.dataType);
                        }
                        isModel = true;
                    }
                    p.vendorExtensions.put("x-is-model-type", isModel);
                    p.vendorExtensions.put("x-is-stream-type", isStreamType(p));
                    Boolean required = p.getRequired();

                    // Convert optional members to use the Nullable_<T> type.
                    if (!Boolean.TRUE.equals(required) && nullableTypeMapping.containsKey(p.dataType)) {
                        p.dataType = nullableTypeMapping.get(p.dataType);
                    }
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
        List<Map<String, Object>> revisedOrderedModels = new ArrayList<>();
        List<String> collectedModelNames = new ArrayList<>();
        int sizeOrderedModels = orderedModels.size();
        for (int i = 0; i < sizeOrderedModels; i++) {
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
        generateJSONSpecFile(objs);
        /* TODO do we still need the SWAGGER_HOST logic below
        Swagger swagger = (Swagger) objs.get("swagger");
        if (swagger != null) {
            String host = swagger.getBasePath();
            try {
                swagger.setHost("SWAGGER_HOST");
                objs.put("swagger-json", Json.pretty().writeValueAsString(swagger).replace("\r\n", "\n"));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
            swagger.setHost(host);
        }*/

        /**
         * Collect the scopes to generate unique identifiers for each of them.
         */
        List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
        postProcessAuthMethod(authMethods, null);

        return super.postProcessSupportingFileData(objs);
    }

    /**
     * Collect the scopes to generate a unique identifier for each of them.
     *
     * @param authMethods the auth methods with their scopes.
     * @param scopes      the optional auth methods and scopes required by an operation
     * @return the authMethods to be used by the operation with its required scopes.
     */
    private List<CodegenSecurity> postProcessAuthMethod(List<CodegenSecurity> authMethods, Map<String, List<String>> scopes) {
        List<CodegenSecurity> result = (scopes == null) ? null : new ArrayList<>();
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

                // If we have operation scopes, filter the auth method to describe the operation auth
                // method with only the scope that it requires.  We have to create a new auth method
                // instance because the original object must not be modified.
                List<String> opScopes = (scopes == null) ? null : scopes.get(authMethod.name);
                authMethod.name = camelize(sanitizeName(authMethod.name), true);
                if (opScopes != null) {
                    CodegenSecurity opSecurity = new CodegenSecurity();
                    opSecurity.name = authMethod.name;
                    opSecurity.type = authMethod.type;
                    opSecurity.isBasic = authMethod.isBasic;
                    opSecurity.isApiKey = authMethod.isApiKey;
                    opSecurity.isKeyInCookie = authMethod.isKeyInCookie;
                    opSecurity.isKeyInHeader = authMethod.isKeyInHeader;
                    opSecurity.isKeyInQuery = authMethod.isKeyInQuery;
                    opSecurity.flow = authMethod.flow;
                    opSecurity.tokenUrl = authMethod.tokenUrl;
                    List<Map<String, Object>> opAuthScopes = new ArrayList<>();
                    for (String opScopeName : opScopes) {
                        for (Map<String, Object> scope : authMethod.scopes) {
                            String name = (String) scope.get("scope");
                            if (opScopeName.equals(name)) {
                                opAuthScopes.add(scope);
                                break;
                            }
                        }
                    }
                    opSecurity.scopes = opAuthScopes;
                    result.add(opSecurity);
                }
            }
        }
        return result;
    }
}
