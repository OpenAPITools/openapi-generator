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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ApexClientCodegen extends AbstractApexCodegen {

    private static final String CLASS_PREFIX = "classPrefix";
    private static final String API_VERSION = "apiVersion";
    private static final String BUILD_METHOD = "buildMethod";
    private static final String NAMED_CREDENTIAL = "namedCredential";
    private final Logger LOGGER = LoggerFactory.getLogger(ApexClientCodegen.class);
    private String classPrefix = "OAS";
    private String apiVersion = "42.0";
    private String buildMethod = "sfdx";
    private String namedCredential;
    private String srcPath = "force-app/main/default/";
    private String sfdxConfigPath = "config/";
    private HashMap<String, Object> primitiveDefaults = new HashMap<>();

    public ApexClientCodegen() {
        super();

        importMapping.clear();

        embeddedTemplateDir = templateDir = "apex";
        outputFolder = "generated-code" + File.separator + "apex";
        modelPackage = apiPackage = srcPath + "classes";
        testPackage = "force-app.main.default.classes";
        modelNamePrefix = classPrefix;

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

        supportingFiles.add(new SupportingFile("OAS.cls", srcPath + "classes", "OAS.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "OAS.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("OASTest.cls", srcPath + "classes", "OASTest.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "OASTest.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("OASResponseMock.cls", srcPath + "classes", "OASResponseMock.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "OASResponseMock.cls-meta.xml"));

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
        typeMapping.put("URI", "String");

        // https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_reserved_words.htm
        setReservedWordsLowerCase(
                Arrays.asList("abstract", "activate", "and", "any", "array", "as", "asc", "autonomous",
                        "begin", "bigdecimal", "blob", "boolean", "break", "bulk", "by", "byte", "case", "cast",
                        "catch", "char", "class", "collect", "commit", "const", "continue",
                        "currency", "date", "datetime", "decimal", "default", "delete", "desc", "do", "double", "else",
                        "end", "enum", "exception", "exit", "export", "extends", "false", "final",
                        "finally", "float", "for", "from", "global", "goto", "group", "having",
                        "hint", "if", "implements", "import", "in", "inner", "insert", "instanceof", "int", "integer",
                        "interface", "into", "join", "like", "limit", "list", "long", "loop", "map", "merge", "new",
                        "not", "null", "nulls", "number", "object", "of", "on", "or", "outer", "override", "package", "parallel",
                        "pragma", "private", "protected", "public", "retrieve", "return",
                        "rollback", "select", "set", "short", "sObject", "sort", "static", "string",
                        "super", "switch", "synchronized", "system", "testmethod", "then", "this",
                        "throw", "time", "transaction", "trigger", "true", "try", "undelete", "update", "upsert", "using",
                        "virtual", "void", "webservice", "when", "where", "while"
                ));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("Blob", "Boolean", "Date", "Datetime", "Decimal", "Double", "ID",
                        "Integer", "Long", "Object", "String", "Time"
                ));

        primitiveDefaults.put("Boolean", true);
        primitiveDefaults.put("Decimal", 1);
        primitiveDefaults.put("Double", 1);
        primitiveDefaults.put("Integer", 1);
        primitiveDefaults.put("Long", 1);
        primitiveDefaults.put("String", "");

        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Map");

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
    public void preprocessOpenAPI(OpenAPI openAPI) {
        String calloutLabel = openAPI.getInfo().getTitle();
        if (StringUtils.isNotBlank(namedCredential)) {
            calloutLabel = namedCredential;
        }
        additionalProperties.put("calloutLabel", calloutLabel);
        String sanitized = sanitizeName(calloutLabel);
        additionalProperties.put("calloutName", sanitized);
        supportingFiles.add(new SupportingFile("namedCredential.mustache", srcPath + "/namedCredentials",
                sanitized + ".namedCredential-meta.xml"
        ));

        if (additionalProperties.get(BUILD_METHOD).equals("sfdx")) {
            generateSfdxSupportingFiles();
        } else if (additionalProperties.get(BUILD_METHOD).equals("ant")) {
            generateAntSupportingFiles();
        }
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("'", "\\'");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        return input.replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r").replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String toApiName(String name) {
        return camelize(classPrefix + super.toApiName(name));
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
            out = String.format(
                    Locale.ROOT,
                    "new List<%s>()",
                    inner == null ? "Object" : getTypeDeclaration(inner)
            );
        } else if (ModelUtils.isBooleanSchema(p)) {
            // true => "true", false => "false", null => "null"
            out = String.valueOf(p.getDefault());
        } else if (ModelUtils.isLongSchema(p)) {
            Long def = (Long) p.getDefault();
            out = def == null ? out : def + "L";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            String s = inner == null ? "Object" : getTypeDeclaration(inner);
            out = String.format(Locale.ROOT, "new Map<String, %s>()", s);
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String def = p.getDefault().toString();
                if (def != null) {
                    out = p.getEnum() == null ? String.format(Locale.ROOT, "'%s'", escapeText(def)) : def;
                }
            }
        } else {
            out = super.toDefaultValue(p);
        }

        // we'll skip over null defaults in the model template to avoid redundant initialization
        return "null".equals(out) ? null : out;
    }

    public void setBuildMethod(String buildMethod) {
        if (buildMethod.equals("ant")) {
            this.srcPath = "deploy/";
        }
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
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        super.updateCodegenPropertyEnum(var);
        if (var.isEnum && var.example != null) {
            String example = var.example.replace("'", "");
            example = toEnumVarName(example, var.dataType);
            var.example = toEnumDefaultValue(example, var.datatypeWithEnum);
        }
    }

    private void generateAntSupportingFiles() {

        supportingFiles.add(new SupportingFile("package.mustache", "deploy", "package.xml"));
        supportingFiles.add(new SupportingFile("package.mustache", "undeploy", "destructiveChanges.xml"));
        supportingFiles.add(new SupportingFile("build.mustache", "build.xml"));
        supportingFiles.add(new SupportingFile("build.properties", "build.properties"));
        supportingFiles.add(new SupportingFile("remove.package.mustache", "undeploy", "package.xml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        supportingFiles.add(new SupportingFile("README_ant.mustache", "README.md")
            .doNotOverwrite());

    }

    private void generateSfdxSupportingFiles() {

        supportingFiles.add(new SupportingFile("sfdx-project-scratch-def.json", sfdxConfigPath, "project-scratch-def.json"));
        supportingFiles.add(new SupportingFile("sfdx-project.json.mustache", "sfdx-project.json"));

        supportingFiles.add(new SupportingFile("README_sfdx.mustache", "README.md")
            .doNotOverwrite());

    }


}
