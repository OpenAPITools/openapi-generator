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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractGoCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractGoCodegen.class);
    private static final String NUMERIC_ENUM_PREFIX = "_";

    protected boolean withGoCodegenComment = false;
    protected boolean withAWSV4Signature = false;
    protected boolean withXml = false;
    protected boolean enumClassPrefix = false;
    protected boolean structPrefix = false;
    protected boolean generateInterfaces = false;

    protected String packageName = "openapi";
    protected Set<String> numberTypes;

    protected boolean usesOptionals = true;

    public AbstractGoCodegen() {
        super();

        supportsInheritance = true;
        hideGenerationTimestamp = Boolean.FALSE;

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array")
        );

        setReservedWordsLowerCase(
                Arrays.asList(
                        // data type
                        "string", "bool", "uint", "uint8", "uint16", "uint32", "uint64",
                        "int", "int8", "int16", "int32", "int64", "float32", "float64",
                        "complex64", "complex128", "rune", "byte", "uintptr",

                        "break", "default", "func", "interface", "select",
                        "case", "defer", "go", "map", "struct",
                        "chan", "else", "goto", "package", "switch",
                        "const", "fallthrough", "if", "range", "type",
                        "continue", "for", "import", "return", "var", "error", "nil")
                // Added "error" as it's used so frequently that it may as well be a keyword
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "string",
                        "bool",
                        "uint",
                        "uint32",
                        "uint64",
                        "int",
                        "int32",
                        "int64",
                        "float32",
                        "float64",
                        "complex64",
                        "complex128",
                        "rune",
                        "byte",
                        "map[string]interface{}",
                        "interface{}"
                )
        );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float32");
        typeMapping.put("float", "float32");
        typeMapping.put("double", "float64");
        typeMapping.put("decimal", "float64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "time.Time");
        typeMapping.put("password", "string");
        typeMapping.put("File", "*os.File");
        typeMapping.put("file", "*os.File");
        typeMapping.put("binary", "*os.File");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("null", "nil");
        // A 'type: object' OAS schema without any declared property is
        // (per JSON schema specification) "an unordered set of properties
        // mapping a string to an instance".
        // Hence map[string]interface{} is the proper implementation in golang.
        // Note: OpenAPITools uses the same token 'object' for free-form objects
        // and arbitrary types. A free form object is implemented in golang as
        // map[string]interface{}, whereas an arbitrary type is implemented
        // in golang as interface{}.
        // See issue #5387 for more details.
        typeMapping.put("object", "map[string]interface{}");
        typeMapping.put("AnyType", "interface{}");

        numberTypes = new HashSet<>(
                Arrays.asList(
                        "uint", "uint8", "uint16", "uint32", "uint64",
                        "int", "int8", "int16", "int32", "int64",
                        "float32", "float64")
        );

        importMapping = new HashMap<>();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Go package name (convention: lowercase).")
                .defaultValue("openapi"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Go package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("GO_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable GO_POST_PROCESS_FILE not defined so Go code may not be properly formatted. To define it, try `export GO_POST_PROCESS_FILE=\"/usr/local/bin/gofmt -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return camelize(name) + '_';
    }

    @Override
    public String toVarName(String name) {

        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name);

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize (lower first character) the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word append _
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as variable name. Renamed to {}", name, escapeReservedWord(name));
            name = escapeReservedWord(name);
        }

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = "Var" + name;

        if ("AdditionalProperties".equals(name)) {
            // AdditionalProperties is a reserved field (additionalProperties: true), use AdditionalPropertiesField instead
            return "AdditionalPropertiesField";
        }

        return name;
    }

    @Override
    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word);
    }

    @Override
    public String toParamName(String name) {
        // params should be lowerCamelCase. E.g. "person Person", instead of
        // "Person Person".
        //
        name = camelize(toVarName(name), true);

        // REVISIT: Actually, for idiomatic go, the param name should
        // really should just be a letter, e.g. "p Person"), but we'll get
        // around to that some other time... Maybe.
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as parameter name. Renamed to {}_", name, name);
            name = name + "_";
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(toModel(name));
    }

    protected boolean isReservedFilename(String name) {
        String[] parts = name.split("_");
        String suffix = parts[parts.length - 1];

        Set<String> reservedSuffixes = new HashSet<>(Arrays.asList(
                // Test
                "test",
                // $GOOS
                "aix", "android", "darwin", "dragonfly", "freebsd", "illumos", "js", "linux", "netbsd", "openbsd",
                "plan9", "solaris", "windows",
                // $GOARCH
                "386", "amd64", "arm", "arm64", "mips", "mips64", "mips64le", "mipsle", "ppc64", "ppc64le", "s390x",
                "wasm"));
        return reservedSuffixes.contains(suffix);
    }

    @Override
    public String toModelFilename(String name) {
        name = toModel("model_" + name);

        if (isReservedFilename(name)) {
            LOGGER.warn("{}.go with suffix (reserved word) cannot be used as filename. Renamed to {}_.go", name,
                    name);
            name += "_";
        }
        return name;
    }

    public String toModel(String name) {
        return toModel(name, true);
    }

    public String toModel(String name, boolean doUnderscore) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, "model_" + name);
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    "model_" + name);
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        if (doUnderscore) {
            return underscore(name);
        }
        return name;
    }

    @Override
    public String toApiFilename(String name) {
        final String apiName;
        // replace - with _ e.g. created-at => created_at
        String api = name.replaceAll("-", "_");
        // e.g. PetApi.go => pet_api.go
        api = "api_" + underscore(api);
        if (isReservedFilename(api)) {
            LOGGER.warn("{}.go with suffix (reserved word) cannot be used as filename. Renamed to {}_.go", name,
                    api);
            api += "_";
        }
        apiName = api;
        return apiName;
    }

    /**
     * Return the golang implementation type for the specified property.
     *
     * @param p the OAS property.
     * @return the golang implementation type.
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            // In OAS 3.0.x, the array "items" attribute is required.
            // In OAS >= 3.1, the array "items" attribute is optional such that the OAS
            // specification is aligned with the JSON schema specification.
            // When "items" is not specified, the elements of the array may be anything at all.
            if (inner != null) {
                inner = ModelUtils.unaliasSchema(this.openAPI, inner);
            }
            String typDecl;
            if (inner != null) {
                typDecl = getTypeDeclaration(inner);
            } else {
                typDecl = "interface{}";
            }
            if (inner != null && Boolean.TRUE.equals(inner.getNullable())) {
                typDecl = "*" + typDecl;
            }
            return "[]" + typDecl;
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "[string]" + getTypeDeclaration(ModelUtils.unaliasSchema(this.openAPI, inner));
        }
        //return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String openAPIType = getSchemaType(p);
        String ref = p.get$ref();
        if (ref != null && !ref.isEmpty()) {
            String tryRefV2 = "#/definitions/" + openAPIType;
            String tryRefV3 = "#/components/schemas/" + openAPIType;
            if (ref.equals(tryRefV2) || ref.equals(tryRefV3)) {
                return toModelName(openAPIType);
            }
        }

        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (typeMapping.containsValue(openAPIType)) {
            return openAPIType;
        }

        if (languageSpecificPrimitives.contains(openAPIType)) {
            return openAPIType;
        }

        return toModelName(openAPIType);
    }

    /**
     * Return the OpenAPI type for the property.
     *
     * @param p the OAS property.
     * @return the OpenAPI type.
     */
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String ref = p.get$ref();
        String type = null;

        if (ref != null && !ref.isEmpty()) {
            type = toModelName(openAPIType);
        } else if ("object".equals(openAPIType) && ModelUtils.isAnyType(p)) {
            // Arbitrary type. Note this is not the same thing as free-form object.
            type = "interface{}";
        } else if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = openAPIType;
        return type;
    }

    /**
     * Determines the golang instantiation type of the specified schema.
     * <p>
     * This function is called when the input schema is a map, and specifically
     * when the 'additionalProperties' attribute is present in the OAS specification.
     * Codegen invokes this function to resolve the "parent" association to
     * 'additionalProperties'.
     * <p>
     * Note the 'parent' attribute in the codegen model is used in the following scenarios:
     * - Indicate a polymorphic association with some other type (e.g. class inheritance).
     * - If the specification has a discriminator, codegen create a “parent” based on the discriminator.
     * - Use of the 'additionalProperties' attribute in the OAS specification.
     * This is the specific scenario when codegen invokes this function.
     *
     * @param property the input schema
     * @return the golang instantiation type of the specified property.
     */
    @Override
    public String toInstantiationType(Schema property) {
        if (ModelUtils.isMapSchema(property)) {
            return getTypeDeclaration(property);
        } else if (ModelUtils.isArraySchema(property)) {
            return getTypeDeclaration(property);
        }
        return super.toInstantiationType(property);
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, camelize("call_" + sanitizedOperationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        // operationId starts with a number
        if (sanitizedOperationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize("call_" + sanitizedOperationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return camelize(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");

        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }

        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(apiPackage()))
                iterator.remove();
        }

        // this will only import "fmt" and "strings" if there are items in pathParams
        for (CodegenOperation operation : operations) {
            if (operation.pathParams != null && operation.pathParams.size() > 0) {
                imports.add(createMapping("import", "strings"));
                break; //just need to import once
            }
        }

        boolean addedOptionalImport = false;
        boolean addedTimeImport = false;
        boolean addedOSImport = false;
        boolean addedReflectImport = false;
        for (CodegenOperation operation : operations) {
            // import "os" if the operation uses files
            if (!addedOSImport && "*os.File".equals(operation.returnType)) {
                imports.add(createMapping("import", "os"));
                addedOSImport = true;
            }
            for (CodegenParameter param : operation.allParams) {
                // import "os" if the operation uses files
                if (!addedOSImport && "*os.File".equals(param.dataType)) {
                    imports.add(createMapping("import", "os"));
                    addedOSImport = true;
                }

                // import "time" if the operation has a required time parameter.
                if (param.required || !usesOptionals) {
                    if (!addedTimeImport && "time.Time".equals(param.dataType)) {
                        imports.add(createMapping("import", "time"));
                        addedTimeImport = true;
                    }
                }

                // import "reflect" package if the parameter is collectionFormat=multi
                if (!addedReflectImport && param.isCollectionFormatMulti) {
                    imports.add(createMapping("import", "reflect"));
                    addedReflectImport = true;
                }

                // import "optionals" package if the parameter is optional
                if (!param.required && usesOptionals) {
                    if (!addedOptionalImport) {
                        imports.add(createMapping("import", "github.com/antihax/optional"));
                        addedOptionalImport = true;
                    }

                    // We need to specially map Time type to the optionals package
                    if ("time.Time".equals(param.dataType)) {
                        param.vendorExtensions.put("x-optional-data-type", "Time");
                    } else {
                        // Map optional type to dataType
                        String optionalType = param.dataType.substring(0, 1).toUpperCase(Locale.ROOT) + param.dataType.substring(1);
                        param.vendorExtensions.put("x-optional-data-type", optionalType);
                    }
                }

                // set x-exportParamName
                char nameFirstChar = param.paramName.charAt(0);
                if (Character.isUpperCase(nameFirstChar)) {
                    // First char is already uppercase, just use paramName.
                    param.vendorExtensions.put("x-export-param-name", param.paramName);
                } else {
                    // It's a lowercase first char, let's convert it to uppercase
                    StringBuilder sb = new StringBuilder(param.paramName);
                    sb.setCharAt(0, Character.toUpperCase(nameFirstChar));
                    param.vendorExtensions.put("x-export-param-name", sb.toString());
                }
            }

            setExportParameterName(operation.queryParams);
            setExportParameterName(operation.formParams);
            setExportParameterName(operation.headerParams);
            setExportParameterName(operation.bodyParams);
            setExportParameterName(operation.cookieParams);
            setExportParameterName(operation.optionalParams);
            setExportParameterName(operation.requiredParams);

        }

        // recursively add import for mapping one type to multiple imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                listIterator.add(createMapping("import", importMapping.get(_import)));
            }
        }

        return objs;
    }

    private void setExportParameterName(List<CodegenParameter> codegenParameters) {
        for (CodegenParameter param : codegenParameters) {
            char nameFirstChar = param.paramName.charAt(0);
            if (Character.isUpperCase(nameFirstChar)) {
                // First char is already uppercase, just use paramName.
                param.vendorExtensions.put("x-export-param-name", param.paramName);
            } else {
                // It's a lowercase first char, let's convert it to uppercase
                StringBuilder sb = new StringBuilder(param.paramName);
                sb.setCharAt(0, Character.toUpperCase(nameFirstChar));
                param.vendorExtensions.put("x-export-param-name", sb.toString());
            }
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        // The 'go-experimental/model.mustache' template conditionally generates accessor methods.
        // For primitive types and custom types (e.g. interface{}, map[string]interface{}...),
        // the generated code has a wrapper type and a Get() function to access the underlying type.
        // For containers (e.g. Array, Map), the generated code returns the type directly.
        if (property.isContainer || property.isFreeFormObject || property.isAnyType) {
            property.vendorExtensions.put("x-golang-is-container", true);
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage();
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
        }

        boolean addedTimeImport = false;
        boolean addedOSImport = false;
        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> m : models) {
            Object v = m.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel model = (CodegenModel) v;
                for (CodegenProperty param : model.vars) {
                    if (!addedTimeImport
                            && ("time.Time".equals(param.dataType) || ("[]time.Time".equals(param.dataType)))) {
                        imports.add(createMapping("import", "time"));
                        addedTimeImport = true;
                    }
                    if (!addedOSImport && "*os.File".equals(param.baseType)) {
                        imports.add(createMapping("import", "os"));
                        addedOSImport = true;
                    }
                }

                if (this instanceof GoClientCodegen && model.isEnum) {
                    imports.add(createMapping("import", "fmt"));
                }

                // if oneOf contains "null" type
                if (model.oneOf != null && !model.oneOf.isEmpty() && model.oneOf.contains("nil")) {
                    model.isNullable = true;
                    model.oneOf.remove("nil");
                }

                // if anyOf contains "null" type
                if (model.anyOf != null && !model.anyOf.isEmpty() && model.anyOf.contains("nil")) {
                    model.isNullable = true;
                    model.anyOf.remove("nil");
                }
            }
        }
        // recursively add import for mapping one type to multiple imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                listIterator.add(createMapping("import", importMapping.get(_import)));
            }
        }

        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
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

    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<>();
        customImport.put(key, value);

        return customImport;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if (isNumberType(datatype) || "bool".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if (isNumberType(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return NUMERIC_ENUM_PREFIX + varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name).toUpperCase(Locale.ROOT);
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName)) { // reserved word
            return escapeReservedWord(enumName);
        } else if (enumName.matches("\\d.*")) { // starts with a number
            return NUMERIC_ENUM_PREFIX + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = underscore(toModelName(property.name)).toUpperCase(Locale.ROOT);

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return NUMERIC_ENUM_PREFIX + enumName;
        } else {
            return enumName;
        }
    }

    public void setWithGoCodegenComment(boolean withGoCodegenComment) {
        this.withGoCodegenComment = withGoCodegenComment;
    }

    public void setWithAWSV4Signature(boolean withAWSV4Signature) {
        this.withAWSV4Signature = withAWSV4Signature;
    }

    public void setWithXml(boolean withXml) {
        this.withXml = withXml;
    }

    public void setEnumClassPrefix(boolean enumClassPrefix) {
        this.enumClassPrefix = enumClassPrefix;
    }

    public void setStructPrefix(boolean structPrefix) {
        this.structPrefix = structPrefix;
    }

    public void setGenerateInterfaces(boolean generateInterfaces) {
        this.generateInterfaces = generateInterfaces;
    }

    @Override
    public String toDefaultValue(Schema schema) {
        schema = ModelUtils.unaliasSchema(this.openAPI, schema);
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        } else {
            return null;
        }
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String goPostProcessFile = System.getenv("GO_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(goPostProcessFile)) {
            return; // skip if GO_POST_PROCESS_FILE env variable is not defined
        }

        // only process the following type (or we can simply rely on the file extension to check if it's a Go file)
        Set<String> supportedFileType = new HashSet<>(
                Arrays.asList(
                        "supporting-mustache",
                        "model-test",
                        "model",
                        "api-test",
                        "api"));
        if (!supportedFileType.contains(fileType)) {
            return;
        }

        // only process files with go extension
        if ("go".equals(FilenameUtils.getExtension(file.toString()))) {
            // e.g. "gofmt -w yourcode.go"
            // e.g. "go fmt path/to/your/package"
            String command = goPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    protected boolean isNumberType(String datatype) {
        return numberTypes.contains(datatype);
    }
}
