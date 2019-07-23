/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.base.Strings;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class OCamlClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(OCamlClientCodegen.class);
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";

    static final String X_MODEL_MODULE = "x-modelModule";

    public static final String CO_HTTP = "cohttp";

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiFolder = "src/apis";
    protected String modelFolder = "src/models";

    private Map<String, List<String>> enumNames = new HashMap<>();
    private Map<String, Schema> enumHash = new HashMap<>();
    private Map<String, String> enumUniqNames;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "ocaml-client";
    }

    public String getHelp() {
        return "Generates an OCaml client library (beta).";
    }

    public OCamlClientCodegen() {
        super();
        outputFolder = "generated-code/ocaml-client";
        modelTemplateFiles.put("model.mustache", ".ml");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        embeddedTemplateDir = templateDir = "ocaml-client";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "and", "as", "assert", "asr", "begin", "class",
                        "constraint", "do", "done", "downto", "else", "end",
                        "exception", "external", "false", "for ", "fun", "function",
                        "functor", "if", "in", "include", "inherit", "initializer",
                        "land", "lazy", "let", "lor", "lsl", "lsr",
                        "lxor", "match", "method", "mod", "module", "mutable",
                        "new", "nonrec", "object", "of", "open", "or",
                        "private", "rec", "sig", "struct", "then", "to",
                        "true", "try", "type", "val", "virtual", "when",
                        "while", "with"
                )
        );

        supportingFiles.add(new SupportingFile("dune.mustache", "", "dune"));

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "int",
                        "int32",
                        "int64",
                        "float",
                        "bool",
                        "char",
                        "string",
                        "list"
                        )
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "int",
                        "int32",
                        "int64",
                        "float",
                        "bool",
                        "char",
                        "string",
                        "list",
                        "Yojson.Safe.t"
                )
        );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("boolean", "bool");
        typeMapping.put("int", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("short", "int");
        typeMapping.put("char", "char");
        typeMapping.put("float", "float");
        typeMapping.put("double", "float");
        typeMapping.put("integer", "int32");
        typeMapping.put("number", "float");
        typeMapping.put("date", "string");
        typeMapping.put("object", "Yojson.Safe.t");
        typeMapping.put("any", "Yojson.Safe.t");
        typeMapping.put("file", "string");
        // lib
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("set", "`Set");
        typeMapping.put("passsword", "string");
        typeMapping.put("DateTime", "string");

//        supportedLibraries.put(CO_HTTP, "HTTP client: CoHttp.");
//
//        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use.");
//        libraryOption.setEnum(supportedLibraries);
//        // set hyper as the default
//        libraryOption.setDefault(CO_HTTP);
//        cliOptions.add(libraryOption);
//        setLibrary(CO_HTTP);
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> superobjs) {
        List<String> toRemove = new ArrayList<>();

        for (Map.Entry<String, Object> modelEntry : superobjs.entrySet()) {
            Map<String, Object> objs = (Map<String, Object>) modelEntry.getValue();
            // process enum in models
            List<Object> models = (List<Object>) objs.get("models");
            for (Object _mo : models) {
                Map<String, Object> mo = (Map<String, Object>) _mo;
                CodegenModel cm = (CodegenModel) mo.get("model");

                // for enum model
                if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                    toRemove.add(modelEntry.getKey());
                }
            }
        }

        for (String keyToRemove : toRemove) {
            superobjs.remove(keyToRemove);
        }

        return superobjs;

    }

    @Override
    protected void updateDataTypeWithEnumForMap(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }

        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(", " + baseItem.baseType, ", " + toEnumName(baseItem));

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(", " + property.items.baseType, ", " + toEnumName(property.items));
            }

            updateCodegenPropertyEnum(property);
        }
    }

    @Override
    protected void updateDataTypeWithEnumForArray(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(baseItem.baseType, toEnumName(baseItem));

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(baseItem.baseType, toEnumName(baseItem));
            }

            updateCodegenPropertyEnum(property);
        }
    }

    @SuppressWarnings("unchecked")
    private String hashEnum(Schema schema) {
        return ((List<Object>) schema.getEnum()).stream().map(String::valueOf).collect(Collectors.joining(","));
    }

    private boolean isEnumSchema(Schema schema) {
        return schema != null && schema.getEnum() != null && !schema.getEnum().isEmpty();
    }

    private void collectEnumSchemas(String parentName, String sName, Schema schema) {
        if (isEnumSchema(schema)) {
            String h = hashEnum(schema);
            if (!enumHash.containsKey(h)) {
                enumHash.put(h, schema);
                enumNames.computeIfAbsent(h, k -> new ArrayList<>()).add(sName.toLowerCase());
                if (parentName != null) {
                    enumNames.get(h).add((parentName + "_" + sName).toLowerCase());
                }
            }
        }
    }

    private void collectEnumSchemas(String sName, Schema schema) {
        collectEnumSchemas(null, sName, schema);
    }

    @SuppressWarnings("unchecked")
    private void collectEnumSchemas(String parentName, Map<String, Schema> schemas) {
        for (String sName : schemas.keySet()) {
            Schema schema = schemas.get(sName);

            collectEnumSchemas(parentName, sName, schema);

            if (schema.getProperties() != null) {
                String pName = parentName != null ? parentName + "_" + sName : sName;
                collectEnumSchemas(pName, schema.getProperties());
            }
        }
    }

    private void collectEnumSchemas(Operation operation) {
        if (operation != null) {
            if (operation.getParameters() != null) {
                for (Parameter parameter : operation.getParameters()) {
                    collectEnumSchemas(parameter.getName(), parameter.getSchema());
                }
            }
            if (operation.getRequestBody() != null && operation.getRequestBody().getContent() != null) {
                Content content = operation.getRequestBody().getContent();
                for (String p : content.keySet()) {
                    collectEnumSchemas(p, content.get(p).getSchema());
                }
            }
            if (operation.getResponses() != null) {
                for (String s : operation.getResponses().keySet()) {
                    ApiResponse apiResponse = operation.getResponses().get(s);
                    if (apiResponse.getContent() != null) {
                        Content content = apiResponse.getContent();
                        for (String p : content.keySet()) {
                            collectEnumSchemas(p, content.get(p).getSchema());
                        }
                    }
                }
            }
        }
    }

    private String sanitizeOCamlTypeName(String name) {
        String typeName = name.replace("-", "_").replace(" ", "_").trim();
        int i = 0;
        char c;
        while (i < typeName.length() && (Character.isDigit(c = typeName.charAt(i)) || c == '_')) {
            i++;
        }

        return typeName.substring(i);
    }

    private void computeEnumUniqNames() {
        Map<String, String> definitiveNames = new HashMap<>();
        for (String h : enumNames.keySet()) {
            boolean hasDefName = false;
            List<String> nameCandidates = enumNames.get(h);
            for (String name : nameCandidates) {
                String candidate = sanitizeOCamlTypeName(name);
                if (!definitiveNames.containsKey(candidate) && !reservedWords.contains(candidate)) {
                    definitiveNames.put(candidate, h);
                    hasDefName = true;
                    break;
                }
            }
            if (!hasDefName) {
                int i = 0;
                String candidate;;
                while (definitiveNames.containsKey(candidate = sanitizeOCamlTypeName(nameCandidates.get(0) + "_" + i))) {
                    i++;
                }
                definitiveNames.put(candidate, h);
            }
        }

        enumUniqNames = definitiveNames.entrySet().stream().collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
    }

    private void collectEnumSchemas(OpenAPI openAPI) {
        Components components = openAPI.getComponents();
        if (components != null && components.getSchemas() != null  && !components.getSchemas().isEmpty()) {
            collectEnumSchemas(null, components.getSchemas());
        }

        Paths paths = openAPI.getPaths();
        if (paths != null && !paths.isEmpty()) {
            for (String path : paths.keySet()) {
                PathItem item = paths.get(path);
                collectEnumSchemas(item.getGet());
                collectEnumSchemas(item.getPost());
                collectEnumSchemas(item.getPut());
                collectEnumSchemas(item.getDelete());
                collectEnumSchemas(item.getPatch());
                collectEnumSchemas(item.getOptions());
                collectEnumSchemas(item.getHead());
                collectEnumSchemas(item.getTrace());
            }
        }

        computeEnumUniqNames();
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        collectEnumSchemas(openAPI);

        supportingFiles.add(new SupportingFile("lib.mustache", "", packageName + ".opam"));
        supportingFiles.add(new SupportingFile("support.mustache", "src/support", "request.ml"));
        supportingFiles.add(new SupportingFile("json.mustache", "src/support", "jsonSupport.ml"));
        supportingFiles.add(new SupportingFile("enums.mustache", "src/support", "enums.ml"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        apiTemplateFiles.put("api-impl.mustache", ".ml");
        apiTemplateFiles.put("api-intf.mustache", ".mli");

        modelPackage = packageName;
        apiPackage = packageName;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return '_' + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + apiFolder).replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separator + modelFolder).replace("/", File.separator);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // snake_case, e.g. PetId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = "var_" + name;

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        return capitalize(toModelFilename(name)) + ".t";
    }

    @Override
    public String toModelFilename(String name) {

        if (!Strings.isNullOrEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!Strings.isNullOrEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(final String name) {
        // replace - with _ e.g. created-at => created_at
        final String _name = name.replaceAll("-", "_");

        // e.g. PetApi.ml => pet_api.ml
        return underscore(_name) + "_api";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                LOGGER.warn(ap.getName() + "(array property) does not have a proper inner type defined.Default to string");
                inner = new StringSchema().description("TODO default missing array inner type to string");
            }
            return getTypeDeclaration(inner) + " list";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            if (inner == null) {
                LOGGER.warn(p.getName() + "(map property) does not have a proper inner type defined. Default to string");
                inner = new StringSchema().description("TODO default missing map inner type to string");
            }
            return "(string * " + getTypeDeclaration(inner) + ") list";
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (typeMapping.containsValue(schemaType)) {
            return schemaType;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, p);
        if (referencedSchema != null && referencedSchema.getEnum() != null) {
            String h = hashEnum(referencedSchema);
            return "Enums." + enumUniqNames.get(h);
        }

        return toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            String type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        }
        return capitalize(toModelFilename(schemaType));
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + StringUtils.underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return StringUtils.underscore(sanitizedOperationId);
    }

    private Map<String, Object> allowableValues(String valueString) {
        Map<String, Object> result = new HashMap<>();
        result.put("values", buildEnumValues(valueString));
        return result;
    }

    private List<Map<String, Object>> buildEnumValues(String valueString) {
        List<Map<String, Object>> result = new ArrayList<>();

        for (String v : valueString.split(",")) {
            Map<String, Object> m = new HashMap<>();
            String value = v.isEmpty() ? "empty" : v;
            m.put("name", value);
            m.put("camlEnumValueName", capitalize(value.replace(".", "_").replace(" ", "_")));
            result.add(m);
        }

        return result;
    }

    private CodegenModel buildEnumModel(String enumName, String values) {
        CodegenModel m = new CodegenModel();
        m.setAllowableValues(allowableValues(values));
        m.setName(enumName);
        m.setClassname(enumName);
        m.setDataType(enumName);
        m.isEnum = true;

        return m;
    }

    private Map<String, Object> buildEnumModelWrapper(String enumName, String values) {
        Map<String, Object> m = new HashMap<>();
        m.put("importPath", packageName + "." + enumName);
        m.put("model", buildEnumModel(enumName, values));
        return m;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion, depending on client library (e.g. Hyper: PUT => Put, Reqwest: PUT => put)
            //if (CO_HTTP.equals(getLibrary())) {
            operation.httpMethod = operation.httpMethod.toLowerCase(Locale.ROOT);
            for (CodegenParameter param : operation.bodyParams) {
                if (param.isModel && param.dataType.endsWith(".t")) {
                    param.vendorExtensions.put(X_MODEL_MODULE, param.dataType.substring(0, param.dataType.lastIndexOf('.')));
                }
            }
        }

        for (Map.Entry<String, String> e : enumUniqNames.entrySet()) {
            allModels.add(buildEnumModelWrapper(e.getValue(), e.getKey()));
        }

        enumUniqNames.clear();

        return objs;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
                && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
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

    @Override
    public String toEnumName(CodegenProperty property) {
        String hash = String.join(",", property.get_enum());

        if (enumUniqNames.containsKey(hash)) {
            return enumUniqNames.get(hash);
        }

        throw new IllegalArgumentException("Unreferenced enum " + hash);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() != null) {
            return p.getDefault().toString();
        } else {
            return null;
        }
    }
}
