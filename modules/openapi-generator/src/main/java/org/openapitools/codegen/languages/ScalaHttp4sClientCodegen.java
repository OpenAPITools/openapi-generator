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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import lombok.Getter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScalaHttp4sClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(ScalaHttp4sClientCodegen.class);

    protected String packageName = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "scala-http4s-client";
    protected String artifactVersion = "1.0.0";
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean removeOAuthSecurities = true;
    @Getter
    protected boolean excludeSbt = false;
    @Getter
    protected boolean excludeApi = false;
    protected static final String EXCLUDE_SBT = "excludeSbt";
    protected static final String EXCLUDE_API = "excludeApi";
    protected String sourceFolder = "src" + File.separator + "main" + File.separator + "scala";

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "scala-http4s";
    }

    public String getHelp() {
        return "Generates a scala-http4s client.";
    }

    public ScalaHttp4sClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BearerToken
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.not
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie,
                        ParameterFeature.FormMultipart
                )
        );

        useOneOfInterfaces = true;
        supportsMultipleInheritance = true;
        supportsInheritance = true;
        supportsMixins = true;
        addOneOfInterfaceImports = true;

        embeddedTemplateDir = templateDir = "scala-http4s";

        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");

        setApiPackage(packageName + ".apis");
        setModelPackage(packageName + ".models");

        setReservedWordsLowerCase(
                Arrays.asList(
                        // Scala
                        "abstract", "case", "catch", "class", "def",
                        "do", "else", "extends", "false", "final",
                        "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null",
                        "object", "override", "package", "private", "protected",
                        "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val",
                        "var", "while", "with", "yield",
                        // Scala-interop languages keywords
                        "abstract", "continue", "switch", "assert",
                        "default", "synchronized", "goto",
                        "break", "double", "implements", "byte",
                        "public", "throws", "enum", "instanceof", "transient",
                        "int", "short", "char", "interface", "static",
                        "void", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native")
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList("double",
                        "Int",
                        "Long",
                        "Float",
                        "Double",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "List",
                        "Set",
                        "Map")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "String",
                        "Boolean",
                        "Double",
                        "Int",
                        "Integer",
                        "Long",
                        "Float",
                        "Any",
                        "AnyVal",
                        "AnyRef",
                        "Object",
                        "BigDecimal"
                )
        );

        typeMapping = new HashMap<>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("date-time", "Instant");
        typeMapping.put("offset-date-time", "OffsetDateTime");
        typeMapping.put("file", "File");
        typeMapping.put("array", "Seq");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Json");
        typeMapping.put("binary", "File");
        typeMapping.put("Date", "LocalDate");
        typeMapping.put("DateTime", "Instant");
        typeMapping.put("OffsetDateTime", "OffsetDateTime");
        typeMapping.put("uuid", "UUID");

        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put("infoUrl", "http://org.openapitools");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "Apache 2.0");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");

        importMapping = new HashMap<>();
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Json", "io.circe.Json");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "scala.collection.immutable.Map");
        importMapping.put("HashMap", "scala.collection.immutable.HashMap");
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("ArrayBuffer", "scala.collection.mutable.ArrayBuffer");
        importMapping.put("Instant", "java.time.Instant");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("ZonedDateTime", "java.time.ZonedDateTime");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");

        instantiationTypes.put("array", "Seq");
        instantiationTypes.put("seq", "Seq");
        instantiationTypes.put("list", "List");
        instantiationTypes.put("map", "Map");

        //this option allows inline enums to be separate own models
        inlineSchemaOption.put("RESOLVE_INLINE_ENUMS", "true");

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (DateLibraries.java8.name().equals(dateLibrary)) {
            typeMapping.put("date", "LocalDate");
            typeMapping.put("date-time", "Instant");
            typeMapping.put("offset-date-time", "OffsetDateTime");
            typeMapping.put("Date", "LocalDate");
            typeMapping.put("DateTime", "Instant");
            typeMapping.put("OffsetDateTime", "OffsetDateTime");
            this.importMapping.put("LocalDate", "java.time.LocalDate");
            this.importMapping.put("Instant", "java.time.Instant");
            this.importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
            additionalProperties.put("java8", "true");
        } else {
            String error = "DateLibrary " + dateLibrary + " is not supported. Please use java8";
            LOGGER.error(error);
            throw new RuntimeException(error);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);
            setApiPackage(packageName + ".apis");
            setModelPackage(packageName + ".models");
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            //you can set your own source folder, i.e. target/scala-3.3.3/src_managed/main
            this.sourceFolder = (String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER);
        }
        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.artifactId = (String) additionalProperties.get(CodegenConstants.ARTIFACT_ID);
        }

        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());

        supportingFiles.add(new SupportingFile("failedRequest.mustache", modelFileFolderRelative(), "_FailedRequest.scala"));
        supportingFiles.add(new SupportingFile("authModel.mustache", modelFileFolderRelative(), "_Authorization.scala"));
        supportingFiles.add(new SupportingFile("modelsPackage.mustache", modelFileFolderRelative(), "package.scala"));

        if (additionalProperties.containsKey(EXCLUDE_SBT)) {
            this.excludeSbt = convertPropertyToBoolean(EXCLUDE_SBT);
        }
        if (!excludeSbt) {
            supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
            supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        } else {
            supportingFiles.remove(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
            supportingFiles.remove(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        }
        if (additionalProperties.containsKey(EXCLUDE_API)) {
            this.excludeApi = convertPropertyToBoolean(EXCLUDE_API);
        }
        if (!excludeApi) {
            supportingFiles.add(new SupportingFile("baseClient.mustache", apisFileFolderRelative(), "BaseClient.scala"));
            supportingFiles.add(new SupportingFile("jsonSupports.mustache", apisFileFolderRelative(), "JsonSupports.scala"));
            apiTemplateFiles.put("api.mustache", ".scala");
        } else {
            supportingFiles.remove(new SupportingFile("baseClient.mustache", apisFileFolderRelative(), "BaseClient.scala"));
            supportingFiles.remove(new SupportingFile("jsonSupports.mustache", apisFileFolderRelative(), "JsonSupports.scala"));
            apiTemplateFiles.remove("api.mustache");
        }
    }


    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiFileFolderRelative();
    }

    private String apiFileFolderRelative() {
        return sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + modelFileFolderRelative();
    }

    private String modelFileFolderRelative() {
        return sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    private String apisFileFolderRelative() {
        return sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "`" + name + "`";
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        if (registerNonStandardStatusCodes) {
            try {
                OperationMap opsMap = objs.getOperations();
                HashSet<Integer> unknownCodes = new HashSet<>();
                for (CodegenOperation operation : opsMap.getOperation()) {
                    for (CodegenResponse response : operation.responses) {
                        if ("default".equals(response.code)) {
                            continue;
                        }
                        try {
                            int code = Integer.parseInt(response.code);
                            if (code >= 600) {
                                unknownCodes.add(code);
                            }
                        } catch (NumberFormatException e) {
                            LOGGER.error("Status code is not an integer : response.code", e);
                        }
                    }
                }
                if (!unknownCodes.isEmpty()) {
                    additionalProperties.put("unknownStatusCodes", unknownCodes);
                }
            } catch (Exception e) {
                LOGGER.error("Unable to find operations List", e);
            }
        }
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> schemes) {
        final List<CodegenSecurity> codegenSecurities = super.fromSecurity(schemes);
        if (!removeOAuthSecurities) {
            return codegenSecurities;
        }

        // Remove OAuth securities
        codegenSecurities.removeIf(security -> security.isOAuth);
        if (codegenSecurities.isEmpty()) {
            return null;
        }
        return codegenSecurities;
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        return formatIdentifier(name, false);
    }

    @Override
    public String encodePath(String input) {
        String path = super.encodePath(input);

        // The parameter names in the URI must be converted to the same case as
        // the method parameter.
        StringBuilder buf = new StringBuilder(path.length());
        Matcher matcher = Pattern.compile("[{](.*?)[}]").matcher(path);
        while (matcher.find()) {
            matcher.appendReplacement(buf, "\\${" + toParamName(matcher.group(0)) + "}");
        }
        matcher.appendTail(buf);
        return buf.toString();
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.path = encodePath(path);
        return op;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(property.baseName, true);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getRequired() != null && p.getRequired().contains(p.getName())) {
            return "None";
        }

        if (ModelUtils.isBooleanSchema(p)) {
            return null;
        } else if (ModelUtils.isDateSchema(p)) {
            return null;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return null;
        } else if (ModelUtils.isNumberSchema(p)) {
            return null;
        } else if (ModelUtils.isIntegerSchema(p)) {
            return null;
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(ModelUtils.getAdditionalProperties(p));
            return "Map[String, " + inner + "].empty ";
        } else if (ModelUtils.isArraySchema(p)) {
            String inner = getSchemaType(ModelUtils.getSchemaItems(p));
            if (ModelUtils.isSet(p)) {
                return "Set[" + inner + "].empty ";
            }
            return "Seq[" + inner + "].empty ";
        } else if (ModelUtils.isStringSchema(p)) {
            return null;
        } else {
            return null;
        }
    }


    private class EnumEntryLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return formatIdentifier(fragment, true);
        }
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

}
