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
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
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
import java.io.IOException;
import java.util.*;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractAdaCodegen.class);

    public static final String HTTP_SUPPORT_OPTION = "httpSupport";
    public static final String OPENAPI_PACKAGE_NAME_OPTION = "openApiName";

    // Common media types.
    private static final String APPLICATION_XML = "application/xml";
    private static final String TEXT_XML = "text/xml";
    private static final String APPLICATION_OCTET_STREAM = "application/octet-stream";
    private static final String TEXT_PLAIN = "text/plain";
    private static final String APPLICATION_JSON = "application/json";
    private static final String APPLICATION_X_WWW_FORM_URLENCODED = "application/x-www-form-urlencoded";

    // RFC 7807 Support
    private static final String APPLICATION_PROBLEM_JSON = "application/problem+json";
    private static final String APPLICATION_PROBLEM_XML = "application/problem+xml";

    // RFC 7386 support
    private static final String APPLICATION_MERGE_PATCH_JSON = "application/merge-patch+json";

    // Extension attributes used by the Ada code generator
    // "x-ada-type-name" allows to override the name generated for a type/object.
    // It can be a full qualified Ada type name and the type can be defined in an external package.
    // In that case, we don't generate the Ada type but use its external definition.  Only the
    // Serialize/Deserialize are generated in the model.  If there is an inconsistency, this will
    // be detected at compilation time.
    // "x-ada-no-vector" instructs to not instantiate the Vectors package for the given model type.
    // "x-ada-serialize-op" allows to control the name of the serialize operation for the field.
    private static final String X_ADA_TYPE_NAME = "x-ada-type-name";
    private static final String X_ADA_VECTOR_TYPE_NAME = "x-ada-vector-type-name";
    private static final String X_ADA_NO_VECTOR = "x-ada-no-vector";
    private static final String X_ADA_SERIALIZE_OP = "x-ada-serialize-op";

    protected String packageName = "defaultPackage";
    protected String projectName = "defaultProject";
    protected List<ModelMap> orderedModels;
    protected final Map<String, List<String>> modelDepends;
    protected final Map<String, String> nullableTypeMapping;
    protected final Map<String, String> operationsScopes;
    protected final List<List<String>> mediaGroups;
    protected final List<List<NameBinding>> mediaLists;
    protected final Map<String, String> mediaToVariableName;
    protected final List<NameBinding> mediaVariables;
    protected final List<NameBinding> adaImports;
    protected final Set<String> adaImportSet = new TreeSet<>();
    protected int scopeIndex = 0;
    protected String httpClientPackageName = "Curl";
    protected String openApiPackageName = "Swagger";

    private static final String bytesType = "swagger::ByteArray";

    static class NameBinding {
        public int position;
        public String name;
        public String value;

        NameBinding(int pos, String name, String value) {
            this.position = pos;
            this.name = name;
            this.value = value;
        }
    }

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
        typeMapping.put("integer", "Integer");
        typeMapping.put("boolean", "Boolean");

        typeMapping.put("binary", bytesType);
        typeMapping.put("ByteArray", bytesType);

        // Mapping to convert an Ada required type to an optional type (nullable).
        nullableTypeMapping = new HashMap<>();

        modelDepends = new HashMap<>();
        orderedModels = new ArrayList<>();
        operationsScopes = new HashMap<>();
        mediaGroups = new ArrayList<>();
        mediaLists = new ArrayList<>();
        mediaToVariableName = new HashMap<>();
        mediaVariables = new ArrayList<>();
        adaImports = new ArrayList<>();
        super.importMapping = new HashMap<>();

        // CLI options
        addOption(CodegenConstants.PROJECT_NAME, "GNAT project name",
                this.projectName);

        cliOptions.add(CliOption.newString(HTTP_SUPPORT_OPTION, "The name of the HTTP support library.  Possible values include 'curl' or 'aws'."));
        cliOptions.add(CliOption.newString(OPENAPI_PACKAGE_NAME_OPTION, "The name of the Ada package which provides support for OpenAPI for the generated client and server code.  The default is 'Swagger'."));

        modelNameSuffix = "Type";
        embeddedTemplateDir = templateDir = "Ada";

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("integer", "boolean", "number", "long", "float",
                        "double", "object", "string", "date", "DateTime", "binary"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(HTTP_SUPPORT_OPTION)) {
            String httpSupport = additionalProperties.get(HTTP_SUPPORT_OPTION).toString().toLowerCase(Locale.ROOT);

            if ("aws".equals(httpSupport)) {
                this.httpClientPackageName = "Aws";
            } else if ("curl".equals(httpSupport)) {
                this.httpClientPackageName = "Curl";
            } else {
                LOGGER.error("invalid http support option `{}`", httpSupport);
            }
        }
        if (additionalProperties.containsKey(OPENAPI_PACKAGE_NAME_OPTION)) {
            this.openApiPackageName = additionalProperties.get(OPENAPI_PACKAGE_NAME_OPTION).toString();
        }

        typeMapping.put("date", openApiPackageName + ".Date");
        typeMapping.put("DateTime", openApiPackageName + ".Datetime");
        typeMapping.put("string", openApiPackageName + ".UString");
        typeMapping.put("long", openApiPackageName + ".Long");
        typeMapping.put("array", openApiPackageName + ".Vector");
        typeMapping.put("map", openApiPackageName + ".Map");
        typeMapping.put("object", openApiPackageName + ".Object");
        typeMapping.put("number", openApiPackageName + ".Number");
        typeMapping.put("UUID", openApiPackageName + ".UString");
        typeMapping.put("URI", openApiPackageName + ".UString");
        typeMapping.put("file", openApiPackageName + ".Blob_Ref");
        typeMapping.put("binary", openApiPackageName + ".Blob_Ref");
        typeMapping.put("float", openApiPackageName + ".Number");
        typeMapping.put("double", openApiPackageName + ".Number");
        importMapping.put("File", openApiPackageName + ".File");

        // Mapping to convert an Ada required type to an optional type (nullable).
        nullableTypeMapping.put(openApiPackageName + ".Date", openApiPackageName + ".Nullable_Date");
        nullableTypeMapping.put(openApiPackageName + ".Datetime", openApiPackageName + ".Nullable_Date");
        nullableTypeMapping.put(openApiPackageName + ".UString", openApiPackageName + ".Nullable_UString");
        nullableTypeMapping.put("Integer", openApiPackageName + ".Nullable_Integer");
        nullableTypeMapping.put(openApiPackageName + ".Long", openApiPackageName + ".Nullable_Long");
        nullableTypeMapping.put("Boolean", openApiPackageName + ".Nullable_Boolean");
        nullableTypeMapping.put(openApiPackageName + ".Object", openApiPackageName + ".Object");

        mediaToVariableName.put(TEXT_PLAIN, openApiPackageName + ".Mime_Text");
        mediaToVariableName.put(APPLICATION_JSON, openApiPackageName + ".Mime_Json");
        mediaToVariableName.put(APPLICATION_XML, openApiPackageName + ".Mime_Xml");
        mediaToVariableName.put(APPLICATION_X_WWW_FORM_URLENCODED, openApiPackageName + ".Mime_Form");
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
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

        return toAdaIdentifier(sanitizeName(name), "P_");
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

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
        String var;
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

    @SuppressWarnings("rawtypes")
    @Override
    public CodegenProperty fromProperty(String name, Schema p, boolean required) {
        CodegenProperty property = super.fromProperty(name, p, required);
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
    @SuppressWarnings("rawtypes")
    @Override
    public String getTypeDeclaration(Schema p) {
        String schemaType = getSchemaType(p);

        if (schemaType != null) {
            schemaType = schemaType.replace("-", "_");
        }

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            String itemType = getTypeDeclaration(inner);
            if (itemType.startsWith("OpenAPI.")) {
                return itemType + "_Vector";
            } else {
                return itemType + "_Vectors.Vector";
            }
        }
        if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String name = getTypeDeclaration(inner) + "_Map";
            if (name.startsWith(openApiPackageName)) {
                return name;
            } else {
                return openApiPackageName + "." + name;
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

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

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

        // Determine the types that this operation produces. `getProducesInfo`
        // simply lists all the types, and then we collect the list of media types
        // for the operation and keep the global index assigned to that list in x-produces-media-index.
        Set<String> produces = getProducesInfo(openAPI, operation);
        boolean producesPlainText = false;
        if (produces != null && !produces.isEmpty()) {
            List<String> mediaList = new ArrayList<>();
            List<Map<String, String>> c = new ArrayList<>();
            for (String mimeType : produces) {
                Map<String, String> mediaType = new HashMap<>();

                if (isMimetypePlain(mimeType)) {
                    producesPlainText = true;
                }

                mediaType.put("mediaType", mimeType);
                c.add(mediaType);
                mediaList.add(mimeType);
            }
            op.produces = c;
            op.hasProduces = true;
            op.vendorExtensions.put("x-produces-media-index", collectMediaList(mediaList));
        }

        for (CodegenResponse rsp : op.responses) {

            if (rsp.dataType != null) {

                // Write out the type of data we actually expect this response
                // to make.
                if (producesPlainText) {
                    // Plain text means that there is not structured data in
                    // this response. So it'll either be a UTF-8 encoded string
                    // 'plainText' or some generic 'bytes'.
                    //
                    // Note that we don't yet distinguish between string/binary
                    // and string/bytes - that is we don't auto-detect whether
                    // base64 encoding should be done. They both look like
                    // 'producesBytes'.
                    if (bytesType.equals(rsp.dataType)) {
                        rsp.vendorExtensions.put("x-produces-bytes", true);
                    } else {
                        rsp.vendorExtensions.put("x-produces-plain-text", true);
                    }
                }
            }
            for (CodegenProperty header : rsp.headers) {
                header.nameInCamelCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }
        }

        return op;
    }

    @SuppressWarnings("unchecked")
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        // This is run after the postProcessModels

        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

        for (CodegenOperation op : operationList) {
            postProcessOperationWithModels(op, allModels);
        }

        // Build the adaImports variable to create a list of Ada specific packages that must be imported.
        adaImportSet.remove(openApiPackageName);
        for (String adaImport : adaImportSet) {
            adaImports.add(new NameBinding(0, adaImport, adaImport));
        }
        additionalProperties.put("adaImports", adaImports);

        // Add the media list variables.
        additionalProperties.put("mediaVariables", mediaVariables);
        additionalProperties.put("mediaLists", mediaLists);

        return objs;
    }

    private void postProcessOperationWithModels(CodegenOperation op, List<ModelMap> allModels) {

        if (op.consumes != null) {
            List<String> mediaList = new ArrayList<>();
            for (Map<String, String> consume : op.consumes) {
                String mediaType = consume.get("mediaType");
                if (mediaType != null) {
                    mediaList.add(mediaType.toLowerCase(Locale.ROOT));
                    if (isMimetypeWwwFormUrlEncoded(mediaType)) {
                        additionalProperties.put("usesUrlEncodedForm", true);
                    } else if (isMimetypeMultipartFormData(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart", true);
                        additionalProperties.put("apiUsesMultipartFormData", true);
                        additionalProperties.put("apiUsesMultipart", true);
                    } else if (isMimetypeMultipartRelated(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart-related", true);
                        additionalProperties.put("apiUsesMultipartRelated", true);
                        additionalProperties.put("apiUsesMultipart", true);
                    }
                }
            }
            op.vendorExtensions.put("x-consumes-media-index", collectMediaList(mediaList));
        }

        if (op.summary != null) {
            op.summary = op.summary.trim();
        }
        if (op.notes != null) {
            op.notes = op.notes.trim();
        }
        op.vendorExtensions.put("x-has-notes", op.notes != null && op.notes.length() > 0);
        final String prefix = modelPackage() + ".Models";

        // Set the file parameter type for both allParams and formParams.
        for (CodegenParameter p : op.allParams) {
            if (p.isFormParam && p.isFile) {
                p.dataType = openApiPackageName + ".File_Part_Type";
            }
            // Convert optional parameters to use the Nullable_<T> type.
            if (!p.required && nullableTypeMapping.containsKey(p.dataType)) {
                p.dataType = nullableTypeMapping.get(p.dataType);
            }

            String dataType;
            if (p.vendorExtensions.containsKey(X_ADA_TYPE_NAME)) {
                dataType = (String) p.vendorExtensions.get(X_ADA_TYPE_NAME);
            } else {
                CodegenProperty schema = p.getSchema();
                if (schema != null) {
                    dataType = (String) schema.vendorExtensions.get(X_ADA_TYPE_NAME);
                } else {
                    dataType = p.dataType;
                }
                p.vendorExtensions.put(X_ADA_TYPE_NAME, dataType);
            }
            String pkgName = useType(dataType);
            if (pkgName != null && !pkgName.startsWith(prefix)) {
                adaImportSet.add(pkgName);
            }
            p.vendorExtensions.put("x-is-imported-type", pkgName != null);
            p.vendorExtensions.put("x-is-model-type", isModelType(p));
            p.vendorExtensions.put("x-is-stream-type", isStreamType(p));
        }
        for (CodegenParameter p : op.formParams) {
            if (p.isFile) {
                p.dataType = openApiPackageName + ".File_Part_Type";
            }
        }

        // Given the operation scopes and the auth methods, build a list of auth methods that only
        // describe the auth methods and scopes required by the operation.
        final Map<String, List<String>> scopes = (Map<String, List<String>>) op.vendorExtensions.get("x-scopes");
        List<CodegenSecurity> opScopes = postProcessAuthMethod(op.authMethods, scopes);
        if (opScopes != null) {
            op.vendorExtensions.put("x-auth-scopes", opScopes);
        }

        CodegenProperty returnProperty = op.returnProperty;
        CodegenProperty returnType = null;
        if (returnProperty != null) {
            CodegenProperty itemType = returnProperty.getItems();
            returnType = itemType;
            if (itemType != null) {
                String dataType;
                if (itemType.vendorExtensions.containsKey(X_ADA_VECTOR_TYPE_NAME)) {
                    dataType = (String) itemType.vendorExtensions.get(X_ADA_VECTOR_TYPE_NAME);
                    returnProperty.vendorExtensions.put(X_ADA_TYPE_NAME, dataType);
                }
                returnProperty.vendorExtensions.put("x-is-model-type", isModelType(itemType));
                returnProperty.vendorExtensions.put("x-is-stream-type", isStreamType(itemType));
            } else {
                if (!returnProperty.vendorExtensions.containsKey(X_ADA_TYPE_NAME)) {
                    returnProperty.vendorExtensions.put(X_ADA_TYPE_NAME, returnProperty.dataType);
                }
                returnProperty.vendorExtensions.put("x-is-model-type", isModelType(returnProperty));
                returnProperty.vendorExtensions.put("x-is-stream-type", isStreamType(returnProperty));
            }
        }

        for (CodegenResponse rsp : op.responses) {

            if (rsp.dataType != null) {
                String dataType = rsp.dataType;
                if (returnType != null) {
                    if (returnType.vendorExtensions.containsKey(X_ADA_VECTOR_TYPE_NAME)) {
                        dataType = (String) returnType.vendorExtensions.get(X_ADA_VECTOR_TYPE_NAME);
                        rsp.vendorExtensions.put(X_ADA_TYPE_NAME, dataType);
                    }
                    rsp.vendorExtensions.put("x-is-model-type", isModelType(returnType));
                    rsp.vendorExtensions.put("x-is-stream-type", isStreamType(returnType));
                    rsp.vendorExtensions.put("x-is-nullable", returnType.isNull);

                    // Convert optional members to use the Nullable_<T> type.
                    Boolean required = returnType.getHasRequired();
                    if (!Boolean.TRUE.equals(required) && nullableTypeMapping.containsKey(dataType)) {
                        rsp.dataType = nullableTypeMapping.get(dataType);
                        rsp.vendorExtensions.put("x-is-required", false);
                    } else {
                        rsp.vendorExtensions.put("x-is-required", true);
                    }
                    if (!rsp.vendorExtensions.containsKey(X_ADA_SERIALIZE_OP)) {
                        if (returnType.isLong && !required) {
                            rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                        } else if (rsp.isLong && "int64".equals(returnType.dataFormat)) {
                            rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Long_Entity");
                        } else {
                            rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                        }
                    }
                    rsp.vendorExtensions.put("x-scz-return", true);
                } else {
                    rsp.vendorExtensions.put("x-scz-no-return", true);
                    if (returnProperty != null) {
                        if (!rsp.vendorExtensions.containsKey(X_ADA_TYPE_NAME)) {
                            rsp.vendorExtensions.put(X_ADA_TYPE_NAME, returnProperty.dataType);
                        }
                        rsp.vendorExtensions.put("x-is-model-type", isModelType(returnProperty));
                        rsp.vendorExtensions.put("x-is-stream-type", isStreamType(returnProperty));
                        rsp.vendorExtensions.put("x-is-nullable", returnProperty.isNull);

                        // Convert optional members to use the Nullable_<T> type.
                        Boolean required = returnProperty.getHasRequired();
                        if (!Boolean.TRUE.equals(required) && nullableTypeMapping.containsKey(dataType)) {
                            rsp.dataType = nullableTypeMapping.get(dataType);
                            rsp.vendorExtensions.put("x-is-required", false);
                        } else {
                            rsp.vendorExtensions.put("x-is-required", true);
                        }
                        if (!rsp.vendorExtensions.containsKey(X_ADA_SERIALIZE_OP)) {
                            if (returnProperty.isLong && !required) {
                                rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                            } else if (rsp.isLong && "int64".equals(returnProperty.dataFormat)) {
                                rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Long_Entity");
                            } else {
                                rsp.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                            }
                        }
                    }
                }
            }
            for (CodegenProperty header : rsp.headers) {
                header.nameInCamelCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }
        }

        /*
         * Scan the path parameter to construct a x-path-index that tells the index of
         * the path parameter.
         */
        for (CodegenParameter p : op.pathParams) {
            String path = op.path;
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

    /**
     * Helper class to sort the model according to their dependencies and names.
     */
    static class ModelDepend implements Comparable<ModelDepend> {
        final List<String> depend;
        final ModelMap model;
        final String name;

        ModelDepend(ModelMap model, List<String> depend, String name) {
            this.model = model;
            this.depend = depend;
            this.name = name;
        }

        public int compareTo(ModelDepend second) {

            if (depend != null && depend.contains(second.name)) {
                //LOGGER.debug("Compare " + name + " with " + second.name + "=1");
                return 1;
            }
            if (second.depend != null && second.depend.contains(name)) {
                //LOGGER.debug("Compare " + name + " with " + second.name + "=-1");
                return -1;
            }
            if (depend != null && depend.size() != (second.depend == null ? 0 : second.depend.size())) {
                //LOGGER.debug("Compare " + name + " with " + second.name + "=D"
                //        + (depend.size() - second.depend.size()));
                return depend.size() - second.depend.size();
            }
            //LOGGER.debug("Compare " + name + " with " + second.name + "=<name>");
            return name.compareTo(second.name);
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // This is run first, before the operations.
        // remove model imports to avoid error
        List<Map<String, String>> imports = objs.getImports();
        final String prefix = modelPackage() + ".Models";
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
        }

        // Collect the model dependencies.
        for (ModelMap model : objs.getModels()) {
            CodegenModel m = model.getModel();
            List<String> d = new ArrayList<>();
            for (CodegenProperty p : m.vars) {
                boolean isModel = false;
                CodegenProperty item = p;
                String dataType = null;
                String arrayDataType = null;
                if (p.vendorExtensions.containsKey(X_ADA_TYPE_NAME)) {
                    dataType = (String) p.vendorExtensions.get(X_ADA_TYPE_NAME);
                    LOGGER.info("Data type {} mapped to {}", p.dataType, dataType);
                }
                arrayDataType = (String) p.vendorExtensions.get(X_ADA_VECTOR_TYPE_NAME);
                if (p.isContainer) {
                    item = p.items;
                }
                boolean isStreamType = isStreamType(p);
                if (!isStreamType && item != null && !item.isString && !item.isPrimitiveType && !item.isContainer && !item.isInteger) {
                    if (dataType == null) {
                        dataType = item.dataType;
                        if (dataType.startsWith(modelPackage + ".Models.") || item.isFreeFormObject) {
                            p.vendorExtensions.put(X_ADA_TYPE_NAME, dataType);
                        } else {
                            p.vendorExtensions.put(X_ADA_TYPE_NAME, modelPackage + ".Models." + dataType);
                        }
                        LOGGER.debug("Setting ada-type name {} for datatype {}", modelPackage + ".Models." + dataType,
                                dataType);
                    }
                    if (!d.contains(dataType)) {
                        // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                        d.add(dataType);
                    }
                    isModel = true;
                }
                Boolean noVector = Boolean.FALSE;
                if (p.vendorExtensions.get(X_ADA_NO_VECTOR) instanceof Boolean) {
                    noVector = (Boolean) p.vendorExtensions.get(X_ADA_NO_VECTOR);
                }
                p.vendorExtensions.put(X_ADA_NO_VECTOR, noVector);
                p.vendorExtensions.put("x-is-model-type", isModel);
                p.vendorExtensions.put("x-is-stream-type", isStreamType);
                String pkgImport = useType(dataType);
                p.vendorExtensions.put("x-is-imported-type", pkgImport != null);
                if (pkgImport != null) {
                    adaImportSet.add(pkgImport);
                }
                Boolean required = p.getRequired();
                if (!p.vendorExtensions.containsKey(X_ADA_SERIALIZE_OP)) {
                    if (p.isLong && !required) {
                        p.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                    } else if (p.isLong && "int64".equals(p.dataFormat)) {
                        p.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Long_Entity");
                    } else {
                        p.vendorExtensions.put(X_ADA_SERIALIZE_OP, "Write_Entity");
                    }
                }

                // Convert optional members to use the Nullable_<T> type.
                if (!Boolean.TRUE.equals(required) && nullableTypeMapping.containsKey(p.dataType)) {
                    p.dataType = nullableTypeMapping.get(p.dataType);
                    p.vendorExtensions.put("x-is-required", false);
                } else {
                    p.vendorExtensions.put("x-is-required", true);
                }
                p.vendorExtensions.put("x-is-nullable", p.isNullable);
            }
            String name = (String) m.vendorExtensions.get(X_ADA_TYPE_NAME);
            if (name == null) {
                name = modelPackage + ".Models." + m.classname;
                m.vendorExtensions.put(X_ADA_TYPE_NAME, name);
            }
            String pkgName = useType(name);
            if (pkgName != null) {
                adaImportSet.add(pkgName);
            }
            m.vendorExtensions.put(X_ADA_TYPE_NAME, name);

            m.vendorExtensions.put("x-is-imported-type", pkgName != null);
            // let us work with fully qualified names only
            modelDepends.put(name, d);
            orderedModels.add(model);
        }

        objs.setImports(imports);

        // Sort models using dependencies:
        TreeSet<ModelDepend> sorted = new TreeSet<>();
        for (ModelMap model : orderedModels) {
            String modelName = modelPackage + ".Models." + model.getModel().classname;
            sorted.add(new ModelDepend(model, modelDepends.get(modelName), modelName));
        }

        // The comparison method in ModelDepend does not provide a total order
        // we have to adjust the sorted list to make sure the dependent models are
        // written last.
        ArrayList<ModelDepend> models = new ArrayList<>();
        for (ModelDepend item : sorted) {
            int pos = models.size();
            for (int i = 0; i < models.size(); i++) {
                ModelDepend second = models.get(i);
                if (second.depend != null && second.depend.contains(item.name)) {
                    pos = i;
                    break;
                }
            }
            models.add(pos, item);
        }
        List<ModelMap> revisedOrderedModels = new ArrayList<>();
        for (ModelDepend model : models) {
            revisedOrderedModels.add(model.model);
        }
        orderedModels = revisedOrderedModels;

        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs.put("orderedModels", orderedModels);
        generateJSONSpecFile(objs);

        /**
         * Collect the scopes to generate unique identifiers for each of them.
         */
        List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
        postProcessAuthMethod(authMethods, null);

        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);

        if (file == null) {
            return;
        }

        // only process files with .ads or .adb extension
        String extension = FilenameUtils.getExtension(file.toString());
        if ("ads".equals(extension) || "adb".equals(extension)) {

            String commandPrefix = System.getenv("ADA_POST_PROCESS_FILE");
            if (StringUtils.isEmpty(commandPrefix)) {
                commandPrefix = "gnatpp";
            }

            try {
                Process p = Runtime.getRuntime().exec(new String[]{commandPrefix, "--no-compact", "--quiet", file.toString()});
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({} {}). Exit code: {}", commandPrefix, file, exitValue);
                } else {
                    LOGGER.debug("Successfully executed: {} {}", commandPrefix, file);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({} {}). Exception: {}", commandPrefix, file, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.ADA;
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
                authMethod.name = camelize(sanitizeName(authMethod.name), LOWERCASE_FIRST_LETTER);
                if (opScopes != null) {
                    CodegenSecurity opSecurity = authMethod.filterByScopeNames(opScopes);
                    result.add(opSecurity);
                }
            }
        }
        return result;
    }

    private boolean isStreamType(CodegenProperty parameter) {
        return parameter.isString || parameter.isBoolean || parameter.isDate
                || parameter.isDateTime || parameter.isInteger || parameter.isLong
                || (parameter.isFreeFormObject && !parameter.isMap);
    }

    private boolean isModelType(CodegenProperty parameter) {
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isFreeFormObject
                && !parameter.isString && !parameter.isContainer && !parameter.isFile
                && !parameter.dataType.startsWith(openApiPackageName)) {
            isModel = true;
        }
        return isModel;
    }

    private boolean isStreamType(CodegenParameter parameter) {
        return parameter.isString || parameter.isBoolean || parameter.isDate
                || parameter.isDateTime || parameter.isInteger || parameter.isLong
                || (parameter.isFreeFormObject && !parameter.isMap);
    }

    private boolean isModelType(CodegenParameter parameter) {
        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isFreeFormObject
                && !parameter.isString && !parameter.isContainer && !parameter.isFile
                && !parameter.dataType.startsWith(openApiPackageName)) {
            isModel = true;
        }
        return isModel;
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

    /**
     * Check if the Ada type name is imported from another package.
     *
     * @param name the Ada full qualified type name.
     * @return true if this Ada type is imported.
     */
    private String useType(String name) {
        if (name == null) {
            return null;
        }
        int pos = name.lastIndexOf('.');
        if (pos <= 0) {
            return null;
        }

        String pkg = name.substring(0, pos);
        if (pkg.equals(modelPackage + ".Models")) {
            return null;
        }

        return pkg;
    }

    private boolean isMimetypeXml(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_XML) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_PROBLEM_XML) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(TEXT_XML);
    }

    private boolean isMimetypeJson(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_JSON) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_MERGE_PATCH_JSON) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_PROBLEM_JSON);
    }

    private boolean isMimetypeWwwFormUrlEncoded(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_X_WWW_FORM_URLENCODED);
    }

    private boolean isMimetypeMultipartFormData(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/form-data");
    }

    private boolean isMimetypeOctetStream(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(APPLICATION_OCTET_STREAM);
    }

    private boolean isMimetypeMultipartRelated(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/related");
    }

    private boolean isMimetypeUnknown(String mimetype) {
        return "*/*".equals(mimetype);
    }

    /**
     * Do we have any special handling for this mimetype?
     */
    private boolean isMimetypePlain(String mimetype) {
        return !(isMimetypeUnknown(mimetype) ||
                isMimetypeXml(mimetype) ||
                isMimetypeJson(mimetype) ||
                isMimetypeWwwFormUrlEncoded(mimetype) ||
                isMimetypeMultipartFormData(mimetype) ||
                isMimetypeMultipartRelated(mimetype));
    }

    /**
     * Collect the list of media types to emit unique arrays of media types.
     * An array represents a list of media types and it can be referenced by several operations.
     * These arrays are emitted at the top of the client/server body packages.
     *
     * @param mediaList the list of media types.
     * @return the unique index assigned to that media list.
     */
    private int collectMediaList(List<String> mediaList) {
        for (int i = 0; i < mediaGroups.size(); i++) {
            if (mediaList.equals(mediaGroups.get(i))) {
                return i + 1;
            }
        }

        mediaGroups.add(mediaList);
        List<NameBinding> varList = new ArrayList<>();
        int pos = 0;
        for (String media : mediaList) {
            String varName = mediaToVariableName.get(media);
            if (varName == null) {
                varName = "Mime_" + (mediaVariables.size() + 1);
                mediaVariables.add(new NameBinding(mediaVariables.size() + 1, varName, media));
                varName = varName + "'Access";
            }
            pos++;
            varList.add(new NameBinding(pos, varName, media));
        }
        mediaLists.add(varList);
        return mediaGroups.size();
    }
}
