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

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.templating.mustache.JoinWithCommaLambda;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class ErlangClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(ErlangClientCodegen.class);

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";
    protected String sourceFolder = "src";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "erlang-client";
    }

    public String getHelp() {
        return "Generates an Erlang client library (beta).";
    }

    public ErlangClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(SecurityFeature.ApiKey))
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
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        outputFolder = "generated-code/erlang";
        modelTemplateFiles.put("model.mustache", ".erl");
        apiTemplateFiles.put("api.mustache", ".erl");

        embeddedTemplateDir = templateDir = "erlang-client";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case",
                        "catch", "cond", "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "receive",
                        "rem", "try", "when", "xor"
                )
        );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("enum", "binary()");
        typeMapping.put("date", "calendar:date()");
        typeMapping.put("datetime", "calendar:datetime()");
        typeMapping.put("date-time", "calendar:datetime()");
        typeMapping.put("boolean", "boolean()");
        typeMapping.put("string", "binary()");
        typeMapping.put("integer", "integer()");
        typeMapping.put("int", "integer()");
        typeMapping.put("float", "integer()");
        typeMapping.put("long", "integer()");
        typeMapping.put("double", "float()");
        typeMapping.put("array", "list()");
        typeMapping.put("map", "maps:map()");
        typeMapping.put("number", "integer()");
        typeMapping.put("decimal", "float()");
        typeMapping.put("List", "list()");
        typeMapping.put("object", "maps:map()");
        typeMapping.put("file", "binary()");
        typeMapping.put("binary", "binary()");
        typeMapping.put("bytearray", "binary()");
        typeMapping.put("byte", "binary()");
        typeMapping.put("uuid", "binary()");
        typeMapping.put("uri", "binary()");
        typeMapping.put("password", "binary()");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Erlang application name (convention: lowercase).")
                .defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Erlang application version")
                .defaultValue(this.packageVersion));
    }

    @Override
    public String getTypeDeclaration(String name) {
        return name + ":" + name + "()";
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }
        return schemaType;
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = getTypeDeclaration(toModelName(lowerCamelCase(schemaType)));
        return type;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        additionalProperties.put("joinWithComma", new JoinWithCommaLambda());

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

        additionalProperties.put("length", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(length(fragment.context()));
            }
        });

        additionalProperties.put("qsEncode", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(qsEncode(fragment.context()));
            }
        });

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("rebar.config.mustache", "", "rebar.config"));
        supportingFiles.add(new SupportingFile("app.src.mustache", "", "src" + File.separator + this.packageName + ".app.src"));
        supportingFiles.add(new SupportingFile("utils.mustache", "", "src" + File.separator + this.packageName + "_utils.erl"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    public String qsEncode(Object o) {
        String r = "";
        CodegenParameter q = (CodegenParameter) o;
        if (q.required) {
            if (q.isArray) {
                r += "[{<<\"" + q.baseName + "\">>, X} || X <- " + q.paramName + "]";
            } else {
                r += "{<<\"" + q.baseName + "\">>, " + q.paramName + "}";
            }
        }
        return r;
    }

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
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));
        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        return camelize(toVarName(name));
    }

    @Override
    public String toArrayModelParamName(String name) {
        if (name == null) {
            LOGGER.warn("parameter name for array model is null. Default to 'array_model'.");
            name = "array_model";
        }

        if (name.indexOf(":") > 0) {
            name = name.substring(0, name.indexOf(":")) + "_array";
        }

        return toParamName(name);
    }

    @Override
    public String toModelName(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("-", "_").replaceAll("\\.", "_"));
    }

    @Override
    public String toApiName(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("-", "_").replaceAll("\\.", "_"));
    }

    @Override
    public String toModelFilename(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("\\.", "_"));
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        name = name.replaceAll("-", "_").replaceAll("\\.", "_");

        // e.g. PetApi.erl => pet_api.erl
        return this.packageName + "_" + underscore(name) + "_api";
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. if
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)).replaceAll("\\.", "_"));
            operationId = "call_" + operationId;
        }

        return underscore(operationId.replaceAll("\\.", "_"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> os = (List<CodegenOperation>) operations.get("operation");
        List<ExtendedCodegenOperation> newOs = new ArrayList<ExtendedCodegenOperation>();
        Pattern pattern = Pattern.compile("\\{([^\\}]+)\\}");
        for (CodegenOperation o : os) {
            // force http method to lower case
            o.httpMethod = o.httpMethod.toLowerCase(Locale.ROOT);

            if (o.isArray) {
                o.returnType = "[" + o.returnBaseType + "]";
            }

            ArrayList<String> pathTemplateNames = new ArrayList<String>();
            Matcher matcher = pattern.matcher(o.path);
            StringBuffer buffer = new StringBuffer();
            while (matcher.find()) {
                String pathTemplateName = matcher.group(1);
                matcher.appendReplacement(buffer, "\", " + camelize(pathTemplateName) + ", \"");
                pathTemplateNames.add(pathTemplateName);
            }
            matcher.appendTail(buffer);

            ExtendedCodegenOperation eco = new ExtendedCodegenOperation(o);
            if (buffer.toString().isEmpty()) {
                eco.setReplacedPathName(o.path);
            } else {
                eco.setReplacedPathName(buffer.toString());
            }
            eco.setPathTemplateNames(pathTemplateNames);
            newOs.add(eco);
        }
        operations.put("operation", newOs);
        return objs;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    /**
     * Returns the number of required parameters plus 1.
     *
     * @param os List of Codegen Parameters
     * @return the string representation of the number of required parameters plus 1
     */
    String length(Object os) {
        int l = 1;
        for (CodegenParameter o : ((ExtendedCodegenOperation) os).allParams) {
            if (o.required)
                l++;
        }
        return Integer.toString(l);
    }

    /**
     * Returns the number of required parameters or body parameters.
     *
     * @param os List of Codegen Parameters
     * @return the number of required parameters or body parameters
     */
    int lengthRequired(List<CodegenParameter> allParams) {
        int l = 0;
        for (CodegenParameter o : allParams) {
            if (o.required || o.isBodyParam)
                l++;
        }
        return l;
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

    class ExtendedCodegenOperation extends CodegenOperation {
        private List<String> pathTemplateNames = new ArrayList<String>();
        private String replacedPathName;
        String arityRequired;
        String arityOptional;

        public ExtendedCodegenOperation(CodegenOperation o) {
            super();

            // Copy all fields of CodegenOperation
            this.responseHeaders.addAll(o.responseHeaders);
            this.hasAuthMethods = o.hasAuthMethods;
            this.hasConsumes = o.hasConsumes;
            this.hasProduces = o.hasProduces;
            this.hasParams = o.hasParams;
            this.hasOptionalParams = o.hasOptionalParams;
            this.returnTypeIsPrimitive = o.returnTypeIsPrimitive;
            this.returnSimpleType = o.returnSimpleType;
            this.subresourceOperation = o.subresourceOperation;
            this.isMap = o.isMap;
            this.isArray = o.isArray;
            this.isMultipart = o.isMultipart;
            this.isResponseBinary = o.isResponseBinary;
            this.hasReference = o.hasReference;
            this.isRestfulIndex = o.isRestfulIndex;
            this.isRestfulShow = o.isRestfulShow;
            this.isRestfulCreate = o.isRestfulCreate;
            this.isRestfulUpdate = o.isRestfulUpdate;
            this.isRestfulDestroy = o.isRestfulDestroy;
            this.isRestful = o.isRestful;
            this.path = o.path;
            this.operationId = o.operationId;
            this.returnType = o.returnType;
            this.httpMethod = o.httpMethod;
            this.returnBaseType = o.returnBaseType;
            this.returnContainer = o.returnContainer;
            this.summary = o.summary;
            this.unescapedNotes = o.unescapedNotes;
            this.notes = o.notes;
            this.baseName = o.baseName;
            this.defaultResponse = o.defaultResponse;
            this.discriminator = o.discriminator;
            this.consumes = o.consumes;
            this.produces = o.produces;
            this.bodyParam = o.bodyParam;
            this.allParams = o.allParams;
            this.arityRequired = Integer.toString(lengthRequired(o.allParams) + 1);
            this.arityOptional = Integer.toString(lengthRequired(o.allParams) + 2);
            this.bodyParams = o.bodyParams;
            this.pathParams = o.pathParams;
            this.queryParams = o.queryParams;
            this.headerParams = o.headerParams;
            this.formParams = o.formParams;
            this.authMethods = o.authMethods;
            this.tags = o.tags;
            this.responses = o.responses;
            this.imports = o.imports;
            this.examples = o.examples;
            this.externalDocs = o.externalDocs;
            this.vendorExtensions = o.vendorExtensions;
            this.nickname = o.nickname;
            this.operationIdLowerCase = o.operationIdLowerCase;
            this.operationIdCamelCase = o.operationIdCamelCase;
        }

        public List<String> getPathTemplateNames() {
            return pathTemplateNames;
        }

        public void setPathTemplateNames(List<String> pathTemplateNames) {
            this.pathTemplateNames = pathTemplateNames;
        }

        public String getReplacedPathName() {
            return replacedPathName;
        }

        public void setReplacedPathName(String replacedPathName) {
            this.replacedPathName = replacedPathName;
        }
    }

    @Override
    public String addRegularExpressionDelimiter(String pattern) {
        return pattern;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.ERLANG; }
}
