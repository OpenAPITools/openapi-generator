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

import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.openapitools.codegen.utils.StringUtils.escape;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class OCamlClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(OCamlClientCodegen.class);
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";

    static final String X_MODEL_MODULE = "x-model-module";

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
        return "ocaml";
    }

    public String getHelp() {
        return "Generates an OCaml client library (beta).";
    }

    public OCamlClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey
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


        outputFolder = "generated-code/ocaml";
        modelTemplateFiles.put("model.mustache", ".ml");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        embeddedTemplateDir = templateDir = "ocaml";

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
                        "while", "with",

                        "result"
                )
        );

        importMapping.remove("File");

        supportingFiles.add(new SupportingFile("dune.mustache", "", "dune"));
        supportingFiles.add(new SupportingFile("dune-project.mustache", "", "dune-project"));
        supportingFiles.add(new SupportingFile("readme.mustache", "", "README.md"));

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
                        "bytes",
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
        typeMapping.put("ByteArray", "string");
        // lib
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("set", "`Set");
        typeMapping.put("password", "string");
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
                } else {
                    enrichPropertiesWithEnumDefaultValues(cm.getAllVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getReadOnlyVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getReadWriteVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getRequiredVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getOptionalVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getVars());
                    enrichPropertiesWithEnumDefaultValues(cm.getParentVars());
                }
            }
        }

        for (String keyToRemove : toRemove) {
            superobjs.remove(keyToRemove);
        }

        return superobjs;

    }

    private void enrichPropertiesWithEnumDefaultValues(List<CodegenProperty> properties) {
        for (CodegenProperty property : properties) {
            if (property.get_enum() != null && property.get_enum().size() == 1) {
                String value = property.get_enum().get(0);
                property.defaultValue = ocamlizeEnumValue(value);
            }
        }
    }

    @Override
    protected void updateDataTypeWithEnumForMap(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMap)
                || Boolean.TRUE.equals(baseItem.isArray))) {
            baseItem = baseItem.items;
        }

        if (baseItem != null) {
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
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMap)
                || Boolean.TRUE.equals(baseItem.isArray))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {
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
        if (schema instanceof ArraySchema) {
            collectEnumSchemas(parentName, sName, ((ArraySchema) schema).getItems());
        } else if (schema instanceof MapSchema && schema.getAdditionalProperties() instanceof Schema) {
            collectEnumSchemas(parentName, sName, (Schema) schema.getAdditionalProperties());
        } else if (isEnumSchema(schema)) {
            String h = hashEnum(schema);
            if (!enumHash.containsKey(h)) {
                enumHash.put(h, schema);
                enumNames.computeIfAbsent(h, k -> new ArrayList<>()).add(sName.toLowerCase(Locale.ROOT));
                if (parentName != null) {
                    enumNames.get(h).add((parentName + "_" + sName).toLowerCase(Locale.ROOT));
                }
            }
        }
    }

    private void collectEnumSchemas(String sName, Schema schema) {
        collectEnumSchemas(null, sName, schema);
    }

    @SuppressWarnings("unchecked")
    private void collectEnumSchemas(String parentName, Map<String, Schema> schemas) {
        for (Map.Entry<String, Schema> schemasEntry : schemas.entrySet()) {
            String sName = schemasEntry.getKey();
            Schema schema = schemasEntry.getValue();

            collectEnumSchemas(parentName, sName, schema);

            if (schema.getProperties() != null) {
                String pName = parentName != null ? parentName + "_" + sName : sName;
                collectEnumSchemas(pName, schema.getProperties());
            }

            if (schema.getAdditionalProperties() != null && schema.getAdditionalProperties() instanceof Schema) {
                String pName = parentName != null ? parentName + "_" + sName : sName;
                collectEnumSchemas(pName, (Schema) schema.getAdditionalProperties());
            }

            if (schema instanceof ArraySchema) {
                ArraySchema s = (ArraySchema) schema;
                if (s.getItems() != null) {
                    String pName = parentName != null ? parentName + "_" + sName : sName;
                    collectEnumSchemas(pName, s.getItems());
                }
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
                for (Map.Entry<String, ApiResponse> operationGetResponsesEntry : operation.getResponses().entrySet()) {
                    String s = operationGetResponsesEntry.getKey();
                    ApiResponse apiResponse = operationGetResponsesEntry.getValue();
                    if (apiResponse.getContent() != null) {
                        Content content = apiResponse.getContent();
                        for (String p : content.keySet()) {
                            collectEnumSchemas(p, content.get(p).getSchema());
                        }
                    }
                    if (apiResponse.getHeaders() != null) {
                        Map<String, Header> headers = apiResponse.getHeaders();
                        for (Map.Entry<String, Header> headersEntry : headers.entrySet()) {
                            String h = headersEntry.getKey();
                            Header header = headersEntry.getValue();
                            collectEnumSchemas(h, header.getSchema());
                        }
                    }
                }
            }
        }
    }

    private String sanitizeOCamlTypeName(String name) {
        String typeName = name.replace("-", "_").replace(" ", "_").replace('.', '_').trim();
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
                String candidate;
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
        if (components != null && components.getSchemas() != null && !components.getSchemas().isEmpty()) {
            collectEnumSchemas(null, components.getSchemas());
        }

        Paths paths = openAPI.getPaths();
        if (paths != null && !paths.isEmpty()) {
            for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
                String path = pathsEntry.getKey();
                PathItem item = pathsEntry.getValue();
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

        if (StringUtils.isEmpty(System.getenv("OCAML_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable 'OCAML_POST_PROCESS_FILE' (optional) not defined. E.g. to format the source code, please try 'export OCAML_POST_PROCESS_FILE=\"ocamlformat -i --enable-outside-detected-project\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'OCAML_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

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

        if (!StringUtils.isBlank(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isBlank(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, "model_" + name);
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number or _
        if (name.matches("^\\d.*|^_.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    "model_" + name);
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
                LOGGER.warn("{}(array property) does not have a proper inner type defined.Default to string",
                        ap.getName());
                inner = new StringSchema().description("TODO default missing array inner type to string");
            }
            return getTypeDeclaration(inner) + " list";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            if (inner == null) {
                LOGGER.warn("{}(map property) does not have a proper inner type defined. Default to string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
            }
            String prefix = inner.getEnum() != null ? "Enums." : "";
            return "(string * " + prefix + getTypeDeclaration(inner) + ") list";
        } else if (p.getEnum() != null) {
            String h = hashEnum(p);
            return enumUniqNames.get(h);
        }

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, p);
        if (referencedSchema != null && referencedSchema.getEnum() != null) {
            String h = hashEnum(referencedSchema);
            return "Enums." + enumUniqNames.get(h);
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
        if (isReservedWord(sanitizedOperationId) || sanitizedOperationId.matches("^[0-9].*")) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return underscore(sanitizedOperationId);
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
            m.put("camlEnumValueName", ocamlizeEnumValue(value));
            result.add(m);
        }

        return result;
    }

    public String toEnumValueName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character)))) {
            return escape(name, specialCharReplacements, Collections.singletonList("_"), null);
        } else {
            return name;
        }
    }

    private String ocamlizeEnumValue(String value) {
        String sanitizedValue =
                toEnumValueName(value.isEmpty() ? "empty" : value)
                        .replace(" ", "_");

        if (!sanitizedValue.matches("^[a-zA-Z_].*")) {
            sanitizedValue = "_" + sanitizedValue;
        }
        return "`" + capitalize(sanitizedValue);
    }

    private CodegenModel buildEnumModel(String enumName, String values) {
        CodegenModel m = new CodegenModel();
        m.setAllowableValues(allowableValues(values));
        m.setName(enumName);
        m.setClassname(enumName);
        m.setDataType(enumName);
        String[] vals = values.split(",");
        if (vals.length == 1) {
            m.setDefaultValue(ocamlizeEnumValue(vals[0]));
        }
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
            for (CodegenParameter param : operation.bodyParams) {
                if (param.isModel && param.dataType.endsWith(".t")) {
                    param.vendorExtensions.put(X_MODEL_MODULE, param.dataType.substring(0, param.dataType.lastIndexOf('.')));
                }
            }

            if ("Yojson.Safe.t".equals(operation.returnBaseType)) {
                operation.vendorExtensions.put("x-return-free-form-object", true);
            }

            if (operation.returnType != null && operation.returnType.startsWith("Enums.")) {
                String returnTypeEnum = operation.returnType.replaceAll(" list$", "");
                operation.vendorExtensions.put("x-returntype-enum", returnTypeEnum);
                operation.vendorExtensions.put("x-returntype-is-enum", true);
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
        return input
                .replace("*)", "*_)")
                .replace("(*", "(_*")
                .replace("\"", "''");
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
            if (p.getEnum() != null) {
                return ocamlizeEnumValue(p.getDefault().toString());
            }
            return p.getDefault().toString();
        } else {
            return null;
        }
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);

        if (file == null) {
            return;
        }
        String ocamlPostProcessFile = System.getenv("OCAML_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(ocamlPostProcessFile)) {
            return; // skip if OCAML_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with ml or mli extension
        if ("ml".equals(FilenameUtils.getExtension(file.toString())) || "mli".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = ocamlPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
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

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.OCAML; }
}
