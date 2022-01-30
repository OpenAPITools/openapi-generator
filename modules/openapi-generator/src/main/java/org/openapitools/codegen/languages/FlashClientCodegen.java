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

import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Locale;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class FlashClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(FlashClientCodegen.class);

    protected String packageName = "org.openapitools";
    protected String packageVersion;

    protected String invokerPackage = "org.openapitools";
    protected String sourceFolder = "flash";

    public FlashClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.DEPRECATED).build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
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
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        modelPackage = "org.openapitools.client.model";
        apiPackage = "org.openapitools.client.api";
        outputFolder = "generated-code" + File.separatorChar + "flash";
        modelTemplateFiles.put("model.mustache", ".as");
        modelTemplateFiles.put("modelList.mustache", "List.as");
        apiTemplateFiles.put("api.mustache", ".as");
        embeddedTemplateDir = templateDir = "flash";

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("Number");
        languageSpecificPrimitives.add("Boolean");
        languageSpecificPrimitives.add("String");
        languageSpecificPrimitives.add("Date");
        languageSpecificPrimitives.add("Array");
        languageSpecificPrimitives.add("Dictionary");

        typeMapping.clear();
        typeMapping.put("integer", "Number");
        typeMapping.put("float", "Number");
        typeMapping.put("long", "Number");
        typeMapping.put("double", "Number");
        typeMapping.put("array", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("object", "Object");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");
        typeMapping.put("binary", "File");

        importMapping = new HashMap<String, String>();
        importMapping.put("File", "flash.filesystem.File");

        // from
        setReservedWordsLowerCase(Arrays.asList("add", "for", "lt", "tellTarget", "and",
                "function", "ne", "this", "break", "ge", "new", "typeof", "continue", "gt", "not",
                "var", "delete", "if", "on", "void", "do", "ifFrameLoaded", "onClipEvent", "while",
                "else", "in", "or", "with", "eq", "le", "return"));

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "flash package name (convention:" +
                " package.name)").defaultValue("org.openapitools"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "flash package version")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, "source folder for generated " +
                "code. e.g. flash"));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        } else {
            //not set, use default to be passed to template
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            apiPackage = packageName + ".client.api";
            modelPackage = packageName + ".client.model";
        } else {
            setPackageName("org.openapitools");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        //modelPackage = invokerPackage + File.separatorChar + "client" + File.separatorChar + "model";
        //apiPackage = invokerPackage + File.separatorChar + "client" + File.separatorChar + "api";

        final String invokerFolder = (sourceFolder + File.separator + "src" + File.separator + invokerPackage + File.separator).replace(".", File.separator).replace('.', File.separatorChar);

        supportingFiles.add(new SupportingFile("ApiInvoker.as", invokerFolder + "common", "ApiInvoker.as"));
        supportingFiles.add(new SupportingFile("ApiUrlHelper.as", invokerFolder + "common", "ApiUrlHelper.as"));
        supportingFiles.add(new SupportingFile("ApiUserCredentials.as", invokerFolder + "common", "ApiUserCredentials.as"));
        supportingFiles.add(new SupportingFile("ListWrapper.as", invokerFolder + "common", "ListWrapper.as"));
        supportingFiles.add(new SupportingFile("OpenApi.as", invokerFolder + "common", "OpenApi.as"));
        supportingFiles.add(new SupportingFile("XMLWriter.as", invokerFolder + "common", "XMLWriter.as"));
        supportingFiles.add(new SupportingFile("ApiError.as", invokerFolder + "exception", "ApiError.as"));
        supportingFiles.add(new SupportingFile("ApiErrorCodes.as", invokerFolder + "exception", "ApiErrorCodes.as"));
        supportingFiles.add(new SupportingFile("ApiClientEvent.as", invokerFolder + "event", "ApiClientEvent.as"));
        supportingFiles.add(new SupportingFile("Response.as", invokerFolder + "event", "Response.as"));
        supportingFiles.add(new SupportingFile("build.properties", sourceFolder, "build.properties"));
        supportingFiles.add(new SupportingFile("build.xml", sourceFolder, "build.xml"));
        supportingFiles.add(new SupportingFile("README.txt", sourceFolder, "README.txt"));
        //supportingFiles.add(new SupportingFile("AirExecutorApp-app.xml", sourceFolder + File.separatorChar
        //        + "bin", "AirExecutorApp-app.xml"));
        supportingFiles.add(new SupportingFile("ASAXB-0.1.1.swc", sourceFolder + File.separatorChar
                + "lib", "ASAXB-0.1.1.swc"));
        supportingFiles.add(new SupportingFile("as3corelib.swc", sourceFolder + File.separatorChar
                + "lib", "as3corelib.swc"));
        supportingFiles.add(new SupportingFile("flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "flash-deprecated";
    }

    @Override
    public String getHelp() {
        return "Generates a Flash (ActionScript) client library (beta). IMPORTANT: this generator has been deprecated in v5.x";
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + sourceFolder + File.separatorChar + ("src/"
                + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + sourceFolder + File.separatorChar + ("src/"
                + modelPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p) || ModelUtils.isMapSchema(p)) {
            return getSchemaType(p);
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = toModelName(schemaType);
        }
        return type;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        } else if (ModelUtils.isDateSchema(p)) {
            return "null";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "null";
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return "0.0";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return "0";
        } else if (ModelUtils.isMapSchema(p)) {
            return "new Dictionary()";
        } else if (ModelUtils.isArraySchema(p)) {
            return "new Array()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "null";
        } else {
            return "NaN";
        }
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all upper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        // underscore the variable name
        // petId => pet_id
        name = camelize(dropDots(name), true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // leverage toModelName
        return dropDots(toModelName(name));
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return camelize(name) + "Api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return camelize(name) + "Api";
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(operationId);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
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
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.FLASH; }
}
