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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

/**
 * Swift (2.x) generator is no longer actively maintained. Please use
 * 'swift3' or 'swift4' generator instead.
 */

public class SwiftClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(SwiftClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String RESPONSE_AS = "responseAs";
    public static final String UNWRAP_REQUIRED = "unwrapRequired";
    public static final String POD_SOURCE = "podSource";
    public static final String POD_AUTHORS = "podAuthors";
    public static final String POD_SOCIAL_MEDIA_URL = "podSocialMediaURL";
    public static final String POD_DOCSET_URL = "podDocsetURL";
    public static final String POD_LICENSE = "podLicense";
    public static final String POD_HOMEPAGE = "podHomepage";
    public static final String POD_SUMMARY = "podSummary";
    public static final String POD_DESCRIPTION = "podDescription";
    public static final String POD_SCREENSHOTS = "podScreenshots";
    public static final String POD_DOCUMENTATION_URL = "podDocumentationURL";
    public static final String SWIFT_USE_API_NAMESPACE = "swiftUseApiNamespace";
    public static final String DEFAULT_POD_AUTHORS = "OpenAPI Generator";
    protected static final String LIBRARY_PROMISE_KIT = "PromiseKit";
    protected static final String LIBRARY_RX_SWIFT = "RxSwift";
    protected static final String[] RESPONSE_LIBRARIES = {LIBRARY_PROMISE_KIT, LIBRARY_RX_SWIFT};
    protected String projectName = "OpenAPIClient";
    protected boolean unwrapRequired;
    protected boolean swiftUseApiNamespace;
    protected String[] responseAs = new String[0];
    protected String sourceFolder = "Classes" + File.separator + "OpenAPIs";
    private static final Pattern PATH_PARAM_PATTERN = Pattern.compile("\\{[a-zA-Z_]+\\}");

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "swift2-deprecated";
    }

    @Override
    public String getHelp() {
        return "Generates a Swift (2.x) client library. IMPORTANT NOTE: this generator (swfit 2.x)  is no longer actively maintained so please use 'swift4' generator instead.";
    }

    public SwiftClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.DEPRECATED)
                .build();

        modifyFeatureSet(features -> features
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
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "swift";
        modelTemplateFiles.put("model.mustache", ".swift");
        apiTemplateFiles.put("api.mustache", ".swift");
        embeddedTemplateDir = templateDir = "swift";
        apiPackage = File.separator + "APIs";
        modelPackage = File.separator + "Models";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "Int",
                        "Int32",
                        "Int64",
                        "Float",
                        "Double",
                        "Bool",
                        "Void",
                        "String",
                        "Character",
                        "AnyObject")
        );
        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "NSData",
                        "NSDate",
                        "NSURL", // for file
                        "NSUUID",
                        "Array",
                        "Dictionary",
                        "Set",
                        "Any",
                        "Empty",
                        "AnyObject")
        );
        reservedWords = new HashSet<String>(
                Arrays.asList(
                        // name used by swift client
                        "ErrorResponse",

                        // swift keywords
                        // https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html Section: Keywords and Punctuation
                        "Int", "Int32", "Int64", "Int64", "Float", "Double", "Bool", "Void", "String", "Character", "AnyObject",
                        "class", "Class", "break", "as", "associativity", "deinit", "case", "dynamicType", "convenience", "enum", "continue",
                        "false", "dynamic", "extension", "default", "is", "didSet", "func", "do", "nil", "final", "import", "else",
                        "self", "get", "init", "fallthrough", "Self", "infix", "internal", "for", "super", "inout", "let", "if",
                        "true", "lazy", "operator", "in", "COLUMN", "left", "private", "return", "FILE", "mutating", "protocol",
                        "switch", "FUNCTION", "none", "public", "where", "LINE", "nonmutating", "static", "while", "optional",
                        "struct", "override", "subscript", "postfix", "typealias", "precedence", "var", "prefix", "Protocol",
                        "required", "right", "set", "Type", "unowned", "weak", "Data","fileprivate", "open", "rethrows", "defer",
                        "guard", "repeat", "Any", "catch", "throw", "throws", "try", "indirect", "willSet")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("date", "ISOFullDate");
        typeMapping.put("Date", "ISOFullDate");
        typeMapping.put("DateTime", "NSDate");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "Character");
        typeMapping.put("short", "Int");
        typeMapping.put("int", "Int32");
        typeMapping.put("long", "Int64");
        typeMapping.put("integer", "Int32");
        typeMapping.put("Integer", "Int32");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "Double");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "AnyObject");
        typeMapping.put("file", "NSURL");
        typeMapping.put("binary", "NSURL");
        typeMapping.put("ByteArray", "NSData");
        typeMapping.put("UUID", "NSUUID");
        typeMapping.put("URI", "String");

        importMapping = new HashMap<String, String>();

        cliOptions.add(new CliOption(PROJECT_NAME, "Project name in Xcode"));
        cliOptions.add(new CliOption(RESPONSE_AS, "Optionally use libraries to manage response.  Currently " +
                StringUtils.join(RESPONSE_LIBRARIES, ", ") + " are available."));
        cliOptions.add(new CliOption(UNWRAP_REQUIRED, "Treat 'required' properties in response as non-optional " +
                "(which would crash the app if api returns null as opposed to required option specified in json schema"));
        cliOptions.add(new CliOption(POD_SOURCE, "Source information used for Podspec"));
        cliOptions.add(new CliOption(CodegenConstants.POD_VERSION, "Version used for Podspec"));
        cliOptions.add(new CliOption(POD_AUTHORS, "Authors used for Podspec"));
        cliOptions.add(new CliOption(POD_SOCIAL_MEDIA_URL, "Social Media URL used for Podspec"));
        cliOptions.add(new CliOption(POD_DOCSET_URL, "Docset URL used for Podspec"));
        cliOptions.add(new CliOption(POD_LICENSE, "License used for Podspec"));
        cliOptions.add(new CliOption(POD_HOMEPAGE, "Homepage used for Podspec"));
        cliOptions.add(new CliOption(POD_SUMMARY, "Summary used for Podspec"));
        cliOptions.add(new CliOption(POD_DESCRIPTION, "Description used for Podspec"));
        cliOptions.add(new CliOption(POD_SCREENSHOTS, "Screenshots used for Podspec"));
        cliOptions.add(new CliOption(POD_DOCUMENTATION_URL, "Documentation URL used for Podspec"));
        cliOptions.add(new CliOption(SWIFT_USE_API_NAMESPACE, "Flag to make all the API classes inner-class of {{projectName}}API"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Setup project name
        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(PROJECT_NAME));
        } else {
            additionalProperties.put(PROJECT_NAME, projectName);
        }
        sourceFolder = projectName + File.separator + sourceFolder;

        // Setup unwrapRequired option, which makes all the properties with "required" non-optional
        if (additionalProperties.containsKey(UNWRAP_REQUIRED)) {
            setUnwrapRequired(convertPropertyToBooleanAndWriteBack(UNWRAP_REQUIRED));
        }

        // Setup unwrapRequired option, which makes all the properties with "required" non-optional
        if (additionalProperties.containsKey(RESPONSE_AS)) {
            Object responseAsObject = additionalProperties.get(RESPONSE_AS);
            if (responseAsObject instanceof String) {
                setResponseAs(((String) responseAsObject).split(","));
            } else {
                setResponseAs((String[]) responseAsObject);
            }
        }
        additionalProperties.put(RESPONSE_AS, responseAs);
        if (ArrayUtils.contains(responseAs, LIBRARY_PROMISE_KIT)) {
            additionalProperties.put("usePromiseKit", true);
        }
        if (ArrayUtils.contains(responseAs, LIBRARY_RX_SWIFT)) {
            additionalProperties.put("useRxSwift", true);
        }

        // Setup swiftUseApiNamespace option, which makes all the API classes inner-class of {{projectName}}API
        if (additionalProperties.containsKey(SWIFT_USE_API_NAMESPACE)) {
            setSwiftUseApiNamespace(convertPropertyToBooleanAndWriteBack(SWIFT_USE_API_NAMESPACE));
        }

        if (!additionalProperties.containsKey(POD_AUTHORS)) {
            additionalProperties.put(POD_AUTHORS, DEFAULT_POD_AUTHORS);
        }

        supportingFiles.add(new SupportingFile("Podspec.mustache", "", projectName + ".podspec"));
        supportingFiles.add(new SupportingFile("Cartfile.mustache", "", "Cartfile"));
        supportingFiles.add(new SupportingFile("APIHelper.mustache", sourceFolder, "APIHelper.swift"));
        supportingFiles.add(new SupportingFile("AlamofireImplementations.mustache", sourceFolder,
                "AlamofireImplementations.swift"));
        supportingFiles.add(new SupportingFile("Extensions.mustache", sourceFolder, "Extensions.swift"));
        supportingFiles.add(new SupportingFile("Models.mustache", sourceFolder, "Models.swift"));
        supportingFiles.add(new SupportingFile("APIs.mustache", sourceFolder, "APIs.swift"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

    }

    @Override
    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word); //don't lowercase as super does
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name; // add an underscore to the name
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return "[String:" + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type) || defaultIncludes.contains(type))
                return type;
        } else
            type = schemaType;
        return toModelName(type);
    }

    @Override
    public boolean isDataTypeFile(String dataType) {
        return dataType != null && dataType.equals("NSURL");
    }

    @Override
    public boolean isDataTypeBinary(final String dataType) {
        return dataType != null && dataType.equals("NSData");
    }

    /**
     * Output the proper model name (capitalized)
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        name = sanitizeName(name);  // FIXME parameter should not be assigned. Also declare it as "final"

        if (!StringUtils.isEmpty(modelNameSuffix)) { // set model suffix
            name = name + "_" + modelNameSuffix;
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) { // set model prefix
            name = modelNamePrefix + "_" + name;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            String modelName = "Model" + name; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return name;
    }

    /**
     * Return the capitalized file name of the model
     *
     * @param name the model name
     * @return the file name of the model
     */
    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toDefaultValue(Schema p) {
        // nil
        return null;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(ModelUtils.getAdditionalProperties(p));
            return "[String:" + inner + "]";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return "[" + inner + "]";
        }
        return null;
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty codegenProperty = super.fromProperty(name, p);
        // TODO skip array/map of enum for the time being,
        // we need to add logic here to handle array/map of enum for any
        // dimensions
        if (Boolean.TRUE.equals(codegenProperty.isContainer)) {
            return codegenProperty;
        }

        if (codegenProperty.isEnum) {
            List<Map<String, String>> swiftEnums = new ArrayList<Map<String, String>>();
            List<String> values = (List<String>) codegenProperty.allowableValues.get("values");

            for (Object value : values) {
                Map<String, String> map = new HashMap<String, String>();
                map.put("enum", toSwiftyEnumName(String.valueOf(value)));
                map.put("raw", String.valueOf(value));
                swiftEnums.add(map);
            }
            codegenProperty.allowableValues.put("values", swiftEnums);
            codegenProperty.datatypeWithEnum = toEnumName(codegenProperty);
            //codegenProperty.datatypeWithEnum =
            //    StringUtils.left(codegenProperty.datatypeWithEnum, codegenProperty.datatypeWithEnum.length() - "Enum".length());

            // Ensure that the enum type doesn't match a reserved word or
            // the variable name doesn't match the generated enum type or the
            // Swift compiler will generate an error
            if (isReservedWord(codegenProperty.datatypeWithEnum) || toVarName(name).equals(codegenProperty.datatypeWithEnum)) {
                codegenProperty.datatypeWithEnum = codegenProperty.datatypeWithEnum + "Enum";
            }
        }
        return codegenProperty;
    }

    @SuppressWarnings("static-method")
    public String toSwiftyEnumName(String value) {
        if (value.length() == 0) {
            return "Empty";
        }

        if (value.matches("^-?\\d*\\.{0,1}\\d+.*")) { // starts with number
            value = "Number" + value;
            value = value.replaceAll("-", "Minus");
            value = value.replaceAll("\\+", "Plus");
            value = value.replaceAll("\\.", "Dot");
        }

        // Prevent from breaking properly cased identifier
        if (value.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            return value;
        }

        char[] separators = {'-', '_', ' ', ':', '/'};
        return WordUtils.capitalizeFully(StringUtils.lowerCase(value), separators).replaceAll("[-_  :/]", "");
    }


    @Override
    public String toApiName(String name) {
        if (name.length() == 0)
            return "DefaultAPI";
        return camelize(name) + "API";
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = camelize(sanitizeName(operationId), true);

        // throw exception if method name is empty. This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("call_" + operationId), true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId), true));
            operationId = camelize(sanitizeName("call_" + operationId), true);
        }

        return operationId;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        path = normalizePath(path); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // issue 3914 - removed logic designed to remove any parameter of type HeaderParameter
        return super.fromOperation(path, httpMethod, operation, servers);
    }

    private static String normalizePath(String path) {
        StringBuilder builder = new StringBuilder();

        int cursor = 0;
        Matcher matcher = PATH_PARAM_PATTERN.matcher(path);
        boolean found = matcher.find();
        while (found) {
            String stringBeforeMatch = path.substring(cursor, matcher.start());
            builder.append(stringBeforeMatch);

            String group = matcher.group().substring(1, matcher.group().length() - 1);
            group = camelize(group, true);
            builder
                    .append("{")
                    .append(group)
                    .append("}");

            cursor = matcher.end();
            found = matcher.find();
        }

        String stringAfterMatch = path.substring(cursor);
        builder.append(stringAfterMatch);

        return builder.toString();
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setUnwrapRequired(boolean unwrapRequired) {
        this.unwrapRequired = unwrapRequired;
    }

    public void setResponseAs(String[] responseAs) {
        this.responseAs = responseAs;
    }

    public void setSwiftUseApiNamespace(boolean swiftUseApiNamespace) {
        this.swiftUseApiNamespace = swiftUseApiNamespace;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        // TODO: this code is probably useless, because the var name is computed from the value in map.put("enum", toSwiftyEnumName(value));
        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // TODO: toModelName already does something for names starting with number, so this code is probably never called
        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
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
