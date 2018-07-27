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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.ByteArraySchema;
import io.swagger.v3.oas.models.media.EmailSchema;
import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.PasswordSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class ApexClientCodegen extends AbstractJavaCodegen {

    private static final String CLASS_PREFIX = "classPrefix";
    private static final String API_VERSION = "apiVersion";
    private static final String BUILD_METHOD = "buildMethod";
    private static final String NAMED_CREDENTIAL = "namedCredential";
    private static final Logger LOGGER = LoggerFactory.getLogger(ApexClientCodegen.class);
    private String classPrefix = "Swag";
    private String apiVersion = "39.0";
    private String buildMethod = "sfdx";
    private String namedCredential = classPrefix;
    private String srcPath = "force-app/main/default/";

    public ApexClientCodegen() {
        super();

        importMapping.clear();

        testFolder = sourceFolder = srcPath;

        embeddedTemplateDir = templateDir = "apex";
        outputFolder = "generated-code" + File.separator + "apex";
        apiPackage = "classes";
        modelPackage = "classes";
        testPackage = "force-app.main.default.classes";
        modelNamePrefix = classPrefix;
        dateLibrary = "";

        apiTemplateFiles.put("api.mustache", ".cls");
        apiTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        apiTestTemplateFiles.put("api_test.mustache", ".cls");
        apiTestTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        modelTemplateFiles.put("model.mustache", ".cls");
        modelTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        modelTestTemplateFiles.put("model_test.mustache", ".cls");
        modelTestTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");

        cliOptions.add(CliOption.newString(CLASS_PREFIX, "Prefix for generated classes. Set this to avoid overwriting existing classes in your org."));
        cliOptions.add(CliOption.newString(API_VERSION, "The Metadata API version number to use for components in this package."));
        cliOptions.add(CliOption.newString(BUILD_METHOD, "The build method for this package."));
        cliOptions.add(CliOption.newString(NAMED_CREDENTIAL, "The named credential name for the HTTP callouts"));

        supportingFiles.add(new SupportingFile("Swagger.cls", srcPath + "classes", "Swagger.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "Swagger.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("SwaggerTest.cls", srcPath + "classes", "SwaggerTest.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "SwaggerTest.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("SwaggerResponseMock.cls", srcPath + "classes", "SwaggerResponseMock.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "SwaggerResponseMock.cls-meta.xml"));

        typeMapping.put("BigDecimal", "Double");
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "Blob");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Datetime");
        typeMapping.put("file", "Blob");
        typeMapping.put("float", "Double");
        typeMapping.put("number", "Double");
        typeMapping.put("short", "Integer");
        typeMapping.put("UUID", "String");

        setReservedWordsLowerCase(
                Arrays.asList("abstract", "activate", "and", "any", "array", "as", "asc", "autonomous",
                        "begin", "bigdecimal", "blob", "break", "bulk", "by", "byte", "case", "cast",
                        "catch", "char", "class", "collect", "commit", "const", "continue",
                        "convertcurrency", "date", "decimal", "default", "delete", "desc", "do", "else",
                        "end", "enum", "exception", "exit", "export", "extends", "false", "final",
                        "finally", "float", "for", "from", "future", "global", "goto", "group", "having",
                        "hint", "if", "implements", "import", "inner", "insert", "instanceof", "int",
                        "interface", "into", "join", "last_90_days", "last_month", "last_n_days",
                        "last_week", "like", "limit", "list", "long", "loop", "map", "merge", "new",
                        "next_90_days", "next_month", "next_n_days", "next_week", "not", "null", "nulls",
                        "number", "object", "of", "on", "or", "outer", "override", "package", "parallel",
                        "pragma", "private", "protected", "public", "retrieve", "return", "returning",
                        "rollback", "savepoint", "search", "select", "set", "short", "sort", "stat",
                        "static", "super", "switch", "synchronized", "system", "testmethod", "then", "this",
                        "this_month", "this_week", "throw", "today", "tolabel", "tomorrow", "transaction",
                        "trigger", "true", "try", "type", "undelete", "update", "upsert", "using",
                        "virtual", "webservice", "when", "where", "while", "yesterday"
                ));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("Blob", "Boolean", "Date", "Datetime", "Decimal", "Double", "ID",
                        "Integer", "Long", "Object", "String", "Time"
                ));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CLASS_PREFIX)) {
            setClassPrefix((String) additionalProperties.get(CLASS_PREFIX));
        }
        additionalProperties.put(CLASS_PREFIX, classPrefix);

        if (additionalProperties.containsKey(API_VERSION)) {
            setApiVersion(toApiVersion((String) additionalProperties.get(API_VERSION)));
        }
        additionalProperties.put(API_VERSION, apiVersion);

        if (additionalProperties.containsKey(BUILD_METHOD)) {
            setBuildMethod((String) additionalProperties.get(BUILD_METHOD));
        }
        additionalProperties.put(BUILD_METHOD, buildMethod);

        if (additionalProperties.containsKey(NAMED_CREDENTIAL)) {
            setNamedCredential((String) additionalProperties.get(NAMED_CREDENTIAL));
        }
        additionalProperties.put(NAMED_CREDENTIAL, namedCredential);

        postProcessOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        // Identifiers must start with a letter
        return "r" + super.escapeReservedWord(name);
    }

    @Override
    public String toModelName(String name) {
        String modelName = super.toModelName(name);

        // Max length is 40; save the last 4 for "Test"
        if (modelName.length() > 36) {
            modelName = modelName.substring(0, 36);
        }
        return modelName;
    }

    @Override
    public String toDefaultValue(Schema p) {
        String out = null;
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ((ArraySchema) p).getItems();
            out = String.format(Locale.ROOT,
                    "new List<%s>()",
                    inner == null ? "Object" : getTypeDeclaration(inner)
            );
        } else if (ModelUtils.isBooleanSchema(p)) {
            // true => "true", false => "false", null => "null"
            out = String.valueOf(((BooleanSchema) p).getDefault());
        } else if (ModelUtils.isLongSchema(p)) { // long
            out = p.getDefault() == null ? out : p.getDefault().toString() + "L";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            String s = inner == null ? "Object" : getTypeDeclaration(inner);
            out = String.format(Locale.ROOT, "new Map<String, %s>()", s);
        } else if (ModelUtils.isStringSchema(p)) {
            String def = (String) p.getDefault();
            if (def != null) {
                out = p.getEnum() == null ? String.format(Locale.ROOT, "'%s'", escapeText(def)) : def;
            }
        } else {
            out = super.toDefaultValue(p);
        }

        // we'll skip over null defaults in the model template to avoid redundant initialization
        return "null".equals(out) ? null : out;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if (Boolean.TRUE.equals(p.isInteger)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(p.isLong)) {
            if (example == null) {
                example = "2147483648L";
            }
        } else if (Boolean.TRUE.equals(p.isDouble)
                || Boolean.TRUE.equals(p.isFloat)
                || Boolean.TRUE.equals(p.isNumber)) {
            if (example == null) {
                example = "3.4";
            }
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            if (Boolean.parseBoolean(p.example)) {
                p.example = "1";
            } else {
                p.example = "0";
            }
        } else if (Boolean.TRUE.equals(p.isFile) || Boolean.TRUE.equals(p.isBinary)) {
            example = "Blob.valueOf('Sample text file\\nContents')";
        } else if (Boolean.TRUE.equals(p.isByteArray)) {
            if (example == null) {
                example = "YmFzZSA2NCBkYXRh";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            if (example == null) {
                example = "1960, 2, 17";
            }
            example = "Date.newInstance(" + escapeText(p.example) + ")";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            if (example == null) {
                example = "2013, 11, 12, 3, 3, 3";
            }
            example = "Datetime.newInstanceGmt(" + escapeText(p.example) + ")";
        } else if (Boolean.TRUE.equals(p.isString)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\'" + escapeText(example) + "\'";

        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = type + ".getExample()";
        }

        // container
        if (Boolean.TRUE.equals(p.isListContainer)) {
            example = setPropertyExampleValue(p.items);
            example = "new " + p.dataType + "{" + example + "}";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = setPropertyExampleValue(p.items);
            example = "new " + p.dataType + "{" + example + "}";
        } else if (example == null) {
            example = "null";
        }

        p.example = example;
    }

    protected String setPropertyExampleValue(CodegenProperty p) {
        String example;

        if (p == null) {
            return "null";
        }

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if (Boolean.TRUE.equals(p.isInteger)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(p.isLong)) {
            if (example == null) {
                example = "2147483648L";
            }
        } else if (Boolean.TRUE.equals(p.isDouble)
                || Boolean.TRUE.equals(p.isFloat)
                || Boolean.TRUE.equals(p.isNumber)) {
            if (example == null) {
                example = "3.4";
            }
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            if (example == null) {
                example = "true";
            }
        } else if (Boolean.TRUE.equals(p.isFile) || Boolean.TRUE.equals(p.isBinary)) {
            if (example == null) {
                example = "Blob.valueOf('Sample text file\\nContents')";
            }
            example = escapeText(example);
        } else if (Boolean.TRUE.equals(p.isDate)) {
            if (example == null) {
                example = "1960, 2, 17";
            }
            example = "Date.newInstance(" + escapeText(p.example) + ")";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            if (example == null) {
                example = "2013, 11, 12, 3, 3, 3";
            }
            example = "Datetime.newInstanceGmt(" + escapeText(p.example) + ")";
        } else if (Boolean.TRUE.equals(p.isString)) {
            if (example == null) {
                example = p.name + "_example";
            }
            example = "\'" + escapeText(example) + "\'";

        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = type + ".getExample()";
        }

        return example;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model, Map<String, Schema> allDefinitions) {
        CodegenModel cm = super.fromModel(name, model, allDefinitions);
        if (cm.interfaces == null) {
            cm.interfaces = new ArrayList<String>();
        }

        Boolean hasDefaultValues = false;

        // for (de)serializing properties renamed for Apex (e.g. reserved words)
        List<Map<String, String>> propertyMappings = new ArrayList<>();
        for (CodegenProperty p : cm.allVars) {
            hasDefaultValues |= p.defaultValue != null;
            if (!p.baseName.equals(p.name)) {
                Map<String, String> mapping = new HashMap<>();
                mapping.put("externalName", p.baseName);
                mapping.put("internalName", p.name);
                propertyMappings.add(mapping);
            }
        }

        cm.vendorExtensions.put("hasPropertyMappings", !propertyMappings.isEmpty());
        cm.vendorExtensions.put("hasDefaultValues", hasDefaultValues);
        cm.vendorExtensions.put("propertyMappings", propertyMappings);

        if (!propertyMappings.isEmpty()) {
            cm.interfaces.add("Swagger.MappedProperties");
        }
        return cm;
    }

    /* the following workaround is no longer needed
    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isBodyParam && parameter.isListContainer) {
            // items of array bodyParams are being nested an extra level too deep for some reason
            parameter.items = parameter.items.items;
            setParameterExampleValue(parameter);
        }
    }
    */

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();
        String calloutLabel = info.getTitle();
        additionalProperties.put("calloutLabel", calloutLabel);
        String sanitized = sanitizeName(calloutLabel);
        additionalProperties.put("calloutName", sanitized);
        supportingFiles.add(new SupportingFile("namedCredential.mustache", srcPath + "/namedCredentials",
                sanitized + ".namedCredential"
        ));

        if (additionalProperties.get(BUILD_METHOD).equals("sfdx")) {
            generateSfdxSupportingFiles();
        } else if (additionalProperties.get(BUILD_METHOD).equals("ant")) {
            generateAntSupportingFiles();
        }

    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          Map<String, Schema> definitions,
                                          OpenAPI openAPI) {
        Boolean hasFormParams = false;
        // comment out the following as there's no consume/produce in OAS3.0
        // we can move the logic below to postProcessOperations if needed
        /*
        // only support serialization into JSON and urlencoded forms for now
        operation.setConsumes(
            Collections.singletonList(hasFormParameter(operation)
                ? "application/x-www-form-urlencoded"
                : "application/json"));

        // only support deserialization from JSON for now
        operation.setProduces(Collections.singletonList("application/json"));
        */

        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, openAPI);

        if (op.getHasExamples()) {
            // prepare examples for Apex test classes
            ApiResponse responseProperty = findMethodResponse(operation.getResponses());
            String deserializedExample = toExampleValue(ModelUtils.getSchemaFromResponse(responseProperty));
            for (Map<String, String> example : op.examples) {
                example.put("example", escapeText(example.get("example")));
                example.put("deserializedExample", deserializedExample);
            }
        }

        return op;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("'", "\\'");
    }

    public void setBuildMethod(String buildMethod) {
        if (buildMethod.equals("ant")) {
            this.srcPath = "deploy/";
        } else {
            this.srcPath = "src/";
        }
        testFolder = sourceFolder = srcPath;
        this.buildMethod = buildMethod;
    }

    public void setNamedCredential(String namedCredential) {
        this.namedCredential = namedCredential;
    }

    public void setClassPrefix(String classPrefix) {
        // the best thing we can do without namespacing in Apex
        modelNamePrefix = classPrefix;
        this.classPrefix = classPrefix;
    }

    public void setApiVersion(String apiVersion) {
        this.apiVersion = apiVersion;
    }

    private String toApiVersion(String apiVersion) {
        if (apiVersion.matches("^\\d{2}(\\.0)?$")) {
            return apiVersion.substring(0, 2) + ".0";
        } else {
            LOGGER.warn(String.format(Locale.ROOT, "specified API version is invalid: %s - defaulting to %s", apiVersion, this.apiVersion));
            return this.apiVersion;
        }
    }

    private void postProcessOpts() {
        supportingFiles.add(
                new SupportingFile("client.mustache", srcPath + "classes", classPrefix + "Client.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes",
                classPrefix + "Client.cls-meta.xml"
        ));
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        return input.replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r");
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Test";
    }

    @Override
    public String toExampleValue(Schema p) {
        if (p == null) {
            return "";
        }
        Object obj = p.getExample();
        String example = obj == null ? "" : obj.toString();
        if (ModelUtils.isArraySchema(p)) { // array
            example = "new " + getTypeDeclaration(p) + "{" + toExampleValue(
                    ((ArraySchema) p).getItems()) + "}";
        } else if (ModelUtils.isBooleanSchema(p)) {
            example = String.valueOf(!"false".equals(example));
        } else if (ModelUtils.isByteArraySchema(p)) { // byte array
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
            }
            ((ByteArraySchema) p).setExample(example);
            example = "EncodingUtil.base64Decode('" + example + "')";
        } else if (ModelUtils.isDateSchema(p)) { // date
            if (example.matches("^\\d{4}(-\\d{2}){2}")) {
                example = example.substring(0, 10).replaceAll("-0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23";
            } else {
                LOGGER.warn(String.format(Locale.ROOT, "The example provided for property '%s' is not a valid RFC3339 date. Defaulting to '2000-01-23'. [%s]", p
                        .getName(), example));
                example = "2000, 1, 23";
            }
            example = "Date.newInstance(" + example + ")";
        } else if (ModelUtils.isDateTimeSchema(p)) { // datetime
            if (example.matches("^\\d{4}([-T:]\\d{2}){5}.+")) {
                example = example.substring(0, 19).replaceAll("[-T:]0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23, 4, 56, 7";
            } else {
                LOGGER.warn(String.format(Locale.ROOT, "The example provided for property '%s' is not a valid RFC3339 datetime. Defaulting to '2000-01-23T04-56-07Z'. [%s]", p
                        .getName(), example));
                example = "2000, 1, 23, 4, 56, 7";
            }
            example = "Datetime.newInstanceGmt(" + example + ")";
        } else if (ModelUtils.isNumberSchema(p)) { // number
            example = example.replaceAll("[^-0-9.]", "");
            example = example.isEmpty() ? "1.3579" : example;
        } else if (ModelUtils.isFileSchema(p)) { // file
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
                ((FileSchema) p).setExample(example);
            }
            example = "EncodingUtil.base64Decode(" + example + ")";
        } else if (ModelUtils.isEmailSchema(p)) { // email
            if (example.isEmpty()) {
                example = "example@example.com";
                ((EmailSchema) p).setExample(example);
            }
            example = "'" + example + "'";
        } else if (ModelUtils.isLongSchema(p)) { // long
            example = example.isEmpty() ? "123456789L" : example + "L";
        } else if (ModelUtils.isMapSchema(p)) { // map
            example = "new " + getTypeDeclaration(p) + "{'key'=>" + toExampleValue(
                    (Schema) p.getAdditionalProperties()) + "}";
        } else if (ModelUtils.isObjectSchema(p)) { // object
            example = example.isEmpty() ? "null" : example;
        } else if (ModelUtils.isPasswordSchema(p)) { // password
            example = example.isEmpty() ? "password123" : escapeText(example);
            ((PasswordSchema) p).setExample(example);
            example = "'" + example + "'";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            example = getTypeDeclaration(p) + ".getExample()";
        } else if (ModelUtils.isUUIDSchema(p)) {
            example = example.isEmpty()
                    ? "'046b6c7f-0b8a-43b9-b35d-6489e6daee91'"
                    : "'" + escapeText(example) + "'";
        } else if (ModelUtils.isStringSchema(p)) { // string
            List<String> enums = p.getEnum();
            if (enums != null && example.isEmpty()) {
                example = enums.get(0);
                p.setExample(example);
            } else if (example.isEmpty()) {
                example = "aeiou";
            } else {
                example = escapeText(example);
                p.setExample(example);
            }
            example = "'" + example + "'";
        }

        return example;
    }

    @Override
    public String toApiName(String name) {
        return camelize(classPrefix + super.toApiName(name));
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        super.updateCodegenPropertyEnum(var);
        if (var.isEnum && var.example != null) {
            String example = var.example.replace("'", "");
            example = toEnumVarName(example, var.dataType);
            var.example = toEnumDefaultValue(example, var.datatypeWithEnum);
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "apex";
    }

    @Override
    public String getHelp() {
        return "Generates an Apex API client library (beta).";
    }

    private void generateAntSupportingFiles() {

        supportingFiles.add(new SupportingFile("package.mustache", "deploy", "package.xml"));
        supportingFiles.add(new SupportingFile("package.mustache", "undeploy", "destructiveChanges.xml"));
        supportingFiles.add(new SupportingFile("build.mustache", "build.xml"));
        supportingFiles.add(new SupportingFile("build.properties", "build.properties"));
        supportingFiles.add(new SupportingFile("remove.package.mustache", "undeploy", "package.xml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        writeOptional(outputFolder, new SupportingFile("README_ant.mustache", "README.md"));
    }

    private void generateSfdxSupportingFiles() {
        supportingFiles.add(new SupportingFile("sfdx.mustache", "", "sfdx-oss-manifest.json"));
        writeOptional(outputFolder, new SupportingFile("README_sfdx.mustache", "README.md"));
    }

}
