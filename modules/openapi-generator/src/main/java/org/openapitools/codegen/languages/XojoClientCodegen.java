/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;
import java.util.function.Consumer;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class XojoClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String CODEGEN_MODULE_NAME = "xojo-client";
    public static final String LIBRARY_HTTPSOCKET = "httpsocket";

    public static final String SERIALIZATION_LIBRARY_DESC = "What serialization library to use: 'xoson' (default).";
    public static final String PROJECT_NAME_DESC = "Project name in Xojo";

    public enum SERIALIZATION_LIBRARY_TYPE {xoson}

    protected String projectName = "OpenAPIClient";
    protected boolean nonPublicApi = false;
    protected boolean supportsAsync = true;
    protected SERIALIZATION_LIBRARY_TYPE serializationLibrary = SERIALIZATION_LIBRARY_TYPE.xoson;

    // Number for each object that appears in the xojo_project file.
    // We start with a relatively high value, to have space for other static objects.
    protected int projectObjectNumber = 100042;

    private final Logger LOGGER = LoggerFactory.getLogger(XojoClientCodegen.class);

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setNonPublicApi(boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
    }

    public void setSupportsAsync(Boolean supportsAsync) {
        this.supportsAsync = supportsAsync;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + projectName + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + projectName + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    public XojoClientCodegen() {
        super();
        this.useOneOfInterfaces = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + CODEGEN_MODULE_NAME;
        modelTemplateFiles.put("model.mustache", ".xojo_code");
        modelTemplateFiles.put("modelOptionalEnum.mustache", "Optional.xojo_code");
        apiTemplateFiles.put("api.mustache", ".xojo_code");
        embeddedTemplateDir = templateDir = CODEGEN_MODULE_NAME;
        apiPackage = "APIs";
        modelPackage = "Models";

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile("App.mustache",
            "",
            "App.xojo_code"));

        supportingFiles.add(new SupportingFile("BuildAutomation.mustache",
            "",
            "Build Automation.xojo_code"));

        supportingFiles.add(new SupportingFile("api_mock.mustache",
            "",
            "Mock.xojo_code"));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Integer",
                        "Int8",
                        "Int16",
                        "Int32",
                        "Int64",
                        "UInteger",
                        "UInt8",
                        "UInt16",
                        "UInt32",
                        "UInt64",
                        "Single",
                        "Double",
                        "Boolean",
                        "String",
                        "Color",
                        "Currency")
        );

        setReservedWordsLowerCase(
            Arrays.asList(
                    // name used by Xojo client
                    "Models", "APIs",

                    // Xojo keywords. This list is taken from here:
                    // https://documentation.xojo.com/getting_started/using_the_xojo_language/reserved_words.html
                    //
                    "#Bad", "#Else", "#Elseif", "#Endif", "#If", "#Pragma", "#Tag", "AddHandler", "AddressOf", "Aggregates", "And", "Array", "As", "Assigns", "Async", "Attributes", "Await", "Break", "ByRef", "ByVal", "Call", "Case", "Catch", "Class", "Const", "Continue", "CType", "Declare", "Delegate", "Dim", "Do", "DownTo", "Each", "Else", "ElseIf", "End", "Enum", "Event", "Exception", "Exit", "Extends", "False", "Finally", "For", "Function", "Global", "GoTo", "Handles", "If", "Implements", "In", "Inherits", "Interface", "Is", "IsA", "Lib", "Loop", "Me", "Mod", "Module", "Namespace", "New", "Next", "Nil", "Not", /*"Object",*/ "Of", "Optional", "Or", "ParamArray", "Private", "Property", "Protected", "Public", "Raise", "RaiseEvent", "Redim", "Rem", "RemoveHandler", "Return", "Select", "Self", "Shared", "Soft", "Static", "Step", "Structure", "Sub", "Super", "Then", "To", "True", "Try", "Until", "Using", "Var", "WeakAddressOf", "Wend", "While", "With", "Xor",

                    // The following are other words we want to reserve
                    "Void", "COLUMN", "FILE", "FUNCTION", "LINE"
            )
        );

        typeMapping = new HashMap<>();
        typeMapping.put("map", "Dictionary");
        typeMapping.put("set", "Array");
        typeMapping.put("Date", "Date");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("char", "Byte");
        typeMapping.put("short", "Int16");
        typeMapping.put("int", "Integer");
        typeMapping.put("long", "Int64");
        typeMapping.put("integer", "Integer");
        typeMapping.put("Integer", "Integer");
        typeMapping.put("float", "Single");
        typeMapping.put("number", "Double");
        typeMapping.put("double", "Double");
        typeMapping.put("file", "FolderItem");
        typeMapping.put("binary", "FolderItem");
        typeMapping.put("ByteArray", "MemoryBlock");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");
        typeMapping.put("decimal", "Currency");
        typeMapping.put("object", "Object");
        typeMapping.put("AnyType", "Variant");
        typeMapping.put("null", "nil");

        // in the future we should also support URLConnection
        supportedLibraries.put(LIBRARY_HTTPSOCKET, "[DEFAULT] HTTP client: HTTPSocket");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC);
        libraryOption.setEnum(supportedLibraries);
        libraryOption.setDefault(LIBRARY_HTTPSOCKET);
        cliOptions.add(libraryOption);
        setLibrary(LIBRARY_HTTPSOCKET);

        CliOption serializationLibraryOpt = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_DESC);
        cliOptions.add(serializationLibraryOpt.defaultValue(serializationLibrary.name()));

        cliOptions.add(new CliOption(CodegenConstants.API_NAME_PREFIX, "Prefix that will be appended to all API classes. Default: empty string."));
        cliOptions.add(new CliOption(CodegenConstants.PROJECT_NAME, PROJECT_NAME_DESC));
        cliOptions.add(new CliOption(CodegenConstants.NON_PUBLIC_API, CodegenConstants.NON_PUBLIC_API_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SUPPORTS_ASYNC, CodegenConstants.SUPPORTS_ASYNC_DESC));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
    }

    private static CodegenModel reconcileProperties(CodegenModel codegenModel,
                                                    CodegenModel parentCodegenModel) {
        // To support inheritance in this generator, we will analyze
        // the parent and child models, look for properties that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the properties
        // will be available via the parent.

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;
        codegenModel.allVars = new ArrayList<CodegenProperty>(codegenProperties);
        codegenModel.parentVars = parentCodegenModel.allVars;

        // Iterate over all of the parent model properties
        boolean removedChildProperty = false;

        for (CodegenProperty parentModelCodegenProperty : parentModelCodegenProperties) {
            // Now that we have found a prop in the parent class,
            // and search the child class for the same prop.
            Iterator<CodegenProperty> iterator = codegenProperties.iterator();
            while (iterator.hasNext()) {
                CodegenProperty codegenProperty = iterator.next();
                if (codegenProperty.baseName.equals(parentModelCodegenProperty.baseName)) {
                    // We found a property in the child class that is
                    // a duplicate of the one in the parent, so remove it.
                    iterator.remove();
                    removedChildProperty = true;
                }
            }
        }

        if (removedChildProperty) {
            codegenModel.vars = codegenProperties;
        }

        return codegenModel;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return super.getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type) || defaultIncludes.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String escapeReservedWord(String name) {
        LOGGER.warn("Escaping word '" + name + "' to 'Escaped" + name + "'. This will likely cause issues at runtime!");
        return "Escaped" + name;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return CODEGEN_MODULE_NAME;
    }

    @Override
    public String getHelp() {
        return "Generates a Xojo client module.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // mandatory settings to ensure compilation
        setSortParamsByRequiredFlag(true);
        setEnsureUniqueParams(true);
        setAllowUnicodeIdentifiers(false);

        // Setup nonPublicApi option, which generates code with reduced access
        // modifiers; allows embedding elsewhere without exposing non-public API calls
        // to consumers
        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            setNonPublicApi(convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API));
        }
        additionalProperties.put(CodegenConstants.NON_PUBLIC_API, nonPublicApi);

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary((String) additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY));
            additionalProperties.put(this.serializationLibrary.name(), true);
        } else {
            additionalProperties.put(this.serializationLibrary.name(), true);
        }

        syncBooleanProperty(additionalProperties, CodegenConstants.SUPPORTS_ASYNC, this::setSupportsAsync, this.supportsAsync);
        syncStringProperty(additionalProperties, CodegenConstants.PROJECT_NAME, this::setProjectName, this.projectName);
        syncStringProperty(additionalProperties, CodegenConstants.LIBRARY, this::setLibrary, this.library);

        if (supportsAsync) {
            apiTemplateFiles.put("CallbackHandler.mustache", "CallbackHandler.xojo_code");
        }

        supportingFiles.add(new SupportingFile("MainModule.mustache",
            "",
            projectName + ".xojo_code"));

        supportingFiles.add(new SupportingFile("APIsModule.mustache",
            projectName,
            "APIs.xojo_code"));

        supportingFiles.add(new SupportingFile("ModelModule.mustache",
            projectName,
            "Models.xojo_code"));

        supportingFiles.add(new SupportingFile("Project.mustache",
            "",
            projectName + ".xojo_project"));

        supportingFiles.add(new SupportingFile("Exception.mustache",
            projectName,
            projectName + "Exception.xojo_code"));

        supportingFiles.add(new SupportingFile("Resources.mustache",
            "",
            projectName + ".xojo_resources"));
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel codegenModel = super.fromModel(name, model);

        codegenModel.vendorExtensions.put("x-xojo-project-id", String.format(Locale.ROOT, "%014XFF", projectObjectNumber));
        projectObjectNumber = projectObjectNumber + 16;

        if (allDefinitions != null) {
            String parentSchema = codegenModel.parentSchema;

            // multilevel inheritance: reconcile properties of all the parents
            while (parentSchema != null) {
                final Schema parentModel = allDefinitions.get(parentSchema);
                final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent,
                        parentModel);
                codegenModel = XojoClientCodegen.reconcileProperties(codegenModel, parentCodegenModel);

                // get the next parent
                parentSchema = parentCodegenModel.parentSchema;
            }
        }
        
        return codegenModel;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // escape " in strings to avoid code injection
        return input.replace("\"", "\"\"");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // if a backslash occurs in a property note for instance, Xojo crashes.
        return input.replace("*/", "*_/").replace("/*", "/_*").replace("\\", "");
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        ModelsMap postProcessedModelsEnum = postProcessModelsEnum(objs);
        return postProcessedModelsEnum;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, Object> vendorExtensions = (Map<String, Object>)objs.get("vendorExtensions");
        if (null == vendorExtensions) {
            vendorExtensions = new HashMap<>();
            objs.put("vendorExtensions", vendorExtensions);
        }

        vendorExtensions.put("x-xojo-project-id", String.format(Locale.ROOT, "%014XFF", projectObjectNumber));
        projectObjectNumber = projectObjectNumber + 16;

        return objs;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# xojo-client contributed by Christopher Kobusch (https://github.com/Topheee). #");
        System.out.println("################################################################################");
    }

    // escape api key name
    @Override
    @SuppressWarnings("static-method")
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        List<CodegenSecurity> securities = super.fromSecurity(securitySchemeMap);

        for (CodegenSecurity sec : securities) {
            if (sec.isApiKey) sec.name = toVarName(sec.name);
        }

        return securities;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.XOJO; }

    @Override
    public String toOperationId(String operationId) {
        operationId = camelize(sanitizeName(operationId));

        // Throw exception if method name is empty.
        // This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("Call_" + operationId));
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), LOWERCASE_FIRST_LETTER));
            operationId = camelize(sanitizeName("Call_" + operationId));
        }

        return operationId;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        return "\"" + value + "\"";
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "empty";
        }

        if (enumUnknownDefaultCase) {
            if (name.equals(enumUnknownDefaultCaseName)) {
                return camelize(name);
            }
        }

        // Reserved Name
        String nameLowercase = StringUtils.lowerCase(name);
        if (isReservedWord(nameLowercase)) {
            return escapeReservedWord(nameLowercase);
        }

        // Prefix with underscore if name starts with number
        if (name.matches("[+-]?\\d.*")) {
            LOGGER.warn("Escaping enum var name '" + name + "' to 'Escaped" + replaceSpecialCharacters(camelize(name)) + "'. This will likely cause issues at runtime!");
            return "Escaped" + replaceSpecialCharacters(camelize(name));
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(WordUtils.capitalizeFully(getSymbolName(name).toUpperCase(Locale.ROOT)));
        }

        // Camelize only when we have a structure defined below
        boolean camelized = false;
        if (name.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            name = camelize(name);
            camelized = true;
        }

        // Check for numerical conversions
        if ("Int".equals(datatype) || "Int32".equals(datatype) || "Int64".equals(datatype)
                || "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "number" + camelize(name);
            return replaceSpecialCharacters(varName);
        }

        // If we have already camelized the word, don't progress
        // any further
        if (camelized) {
            return replaceSpecialCharacters(name);
        }

        char[] separators = {'-', '_', ' ', ':', '(', ')'};
        return camelize(replaceSpecialCharacters(WordUtils.capitalizeFully(StringUtils.lowerCase(name), separators)
                        .replaceAll("[-_ :\\(\\)]", "")));
    }

    private String replaceSpecialCharacters(String name) {
        for (Map.Entry<String, String> specialCharacters : specialCharReplacements.entrySet()) {
            String specialChar = specialCharacters.getKey();
            String replacement = specialCharacters.getValue();
            // Underscore is the only special character we'll allow
            if (!specialChar.equals("_") && name.contains(specialChar)) {
                name = replaceCharacters(name, specialChar, replacement);
            }
        }

        // Fallback, replace unknowns with underscore.
        name = name.replaceAll("\\W+", "_");

        return name;
    }

    private String replaceCharacters(String word, String oldValue, String newValue) {
        if (!word.contains(oldValue)) {
            return word;
        }
        if (word.equals(oldValue)) {
            return newValue;
        }
        int i = word.indexOf(oldValue);
        String start = word.substring(0, i);
        String end = recurseOnEndOfWord(word, oldValue, newValue, i);
        return start + newValue + end;
    }

    private String recurseOnEndOfWord(String word, String oldValue, String newValue, int lastReplacedValue) {
        String end = word.substring(lastReplacedValue + 1);
        if (!end.isEmpty()) {
            end = titleCase(end);
            end = replaceCharacters(end, oldValue, newValue);
        }
        return end;
    }

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase(Locale.ROOT) + input.substring(1);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

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

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        // for reserved words prepend with 'Escaped'
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, prepend _
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public String toModelFilename(String name) {
        name = super.toModelFilename(name);
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, prepend A
        if (name.matches("^\\d.*")) {
            name = "A" + name;
        }

        if (!name.equals(toModelName(name))) {
            LOGGER.error("Model file name '" + name + "' differs from model name '" + toModelName(name) + "'. This will result in compilation errors.");
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name);

        if (!StringUtils.isEmpty(modelNameSuffix) && !isLanguageSpecificType(name)) { // set model suffix
            name = name + "_" + modelNameSuffix;
        }

        if (!StringUtils.isEmpty(modelNamePrefix) && !isLanguageSpecificType(name)) { // set model prefix
            name = modelNamePrefix + "_" + name;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            // e.g. 200Response => Model200Response (after camelize)
            String modelName = "Model" + name;
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return name;
    }

    @Override
    public String toExampleValue(Schema schema) {
        if (schema.getExample() != null) {
            return schema.getExample().toString();
        }

        return getDefaultPropertyValue(schema);
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        }

        return getDefaultPropertyValue(schema);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter) {
        super.setParameterExampleValue(codegenParameter);
        if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            codegenParameter.example = "GetTemporaryFolderItem";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            codegenParameter.example = "New MemoryBlock(8)";
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) {
            codegenParameter.example = "GetTemporaryFolderItem";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) {
            codegenParameter.example = "New Date";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) {
            codegenParameter.example = "New Date";
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.example = codegenParameter.paramName + "_example";
        } else if (Boolean.TRUE.equals(codegenParameter.isFreeFormObject)) {
            codegenParameter.example = "New Dictionary";
        }
    }

    public SERIALIZATION_LIBRARY_TYPE getSerializationLibrary() {
        return this.serializationLibrary;
    }

    /**
     * Sets the serialization engine for Xojo
     *
     * @param enumSerializationLibrary The string representation of the serialization library as defined by
     *                                 {@link org.openapitools.codegen.languages.XojoClientCodegen.SERIALIZATION_LIBRARY_TYPE}
     */
    public void setSerializationLibrary(final String enumSerializationLibrary) {
        try {
            this.serializationLibrary = SERIALIZATION_LIBRARY_TYPE.valueOf(enumSerializationLibrary);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumSerializationLibrary + " is an invalid enum property naming option. Please choose from:");
            for (SERIALIZATION_LIBRARY_TYPE t : SERIALIZATION_LIBRARY_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }

    private String getDefaultPropertyValue(Schema schema) {
        if (ModelUtils.isBooleanSchema(schema)) {
            return "False";
        } else if (ModelUtils.isDateSchema(schema)) {
            return "Nil";
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "Nil";
        } else if (ModelUtils.isNumberSchema(schema)) {
            return "0";
        } else if (ModelUtils.isIntegerSchema(schema)) {
            return "0";
        } else if (ModelUtils.isStringSchema(schema)) {
            return "Sample";
        } else if (ModelUtils.isObjectSchema(schema)) {
            return "Nil";
        } else {
            return "Nil";
        }
    }

    private void syncBooleanProperty(final Map<String, Object> additionalProperties, final String key, final Consumer<Boolean> setter, final Boolean defaultValue) {
        if (additionalProperties.containsKey(key)) {
            setter.accept(convertPropertyToBooleanAndWriteBack(key));
        } else {
            additionalProperties.put(key, defaultValue);
            setter.accept(defaultValue);
        }
    }

    private void syncStringProperty(final Map<String, Object> additionalProperties, final String key, final Consumer<String> setter, final String defaultValue) {
        if (additionalProperties.containsKey(key)) {
            setter.accept((String) additionalProperties.get(key));
        } else {
            additionalProperties.put(key, defaultValue);
            setter.accept(defaultValue);
        }
    }

    private Boolean isLanguageSpecificType(String name) {
        return languageSpecificPrimitives.contains(name);
    }
}
