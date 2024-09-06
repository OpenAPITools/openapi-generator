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
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
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

public class ScalaHttp4sScala3ClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    protected String mainPackage = "org.openapitools.client"; //TODO: configuable??
    protected String groupId = "org.openapitools";  //TODO: configuable??
    protected String artifactId = "http4s-client-scala3"; //TODO: configuable??
    protected String sourceFolder = "scala";
    protected String sourceSubFolder = "main";
    protected String artifactVersion = "1.0.0";
    protected String resourcesFolder = "src/main/resources";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String configKey = "apiRequest";
    protected int defaultTimeoutInMs = 5000;
    protected String configKeyPath = mainPackage;
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean renderJavadoc = true;
    protected boolean removeOAuthSecurities = true;
    public static final String EXCLUDE_SBT = "excludeSbt"; // generate as whole project
    public static final String SOURCE_SUBFOLDER = "sourceSubfolder"; // generate as whole project

    private String packageName = mainPackage;

    @SuppressWarnings("hiding")
    protected final Logger LOGGER = LoggerFactory.getLogger(ScalaHttp4sScala3ClientCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "scala-http4s-scala3";
    }

    public String getHelp() {
        return "Generates a scala-http4s-scala3 client.";
    }

    public ScalaHttp4sScala3ClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
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

        useOneOfInterfaces = true;
        supportsMultipleInheritance = true;
        supportsInheritance = true;
        supportsMixins = true;
        addOneOfInterfaceImports = true;

        outputFolder = "generated-code" + File.separator + "scala-http4s-scala3";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-http4s-scala3";
        apiPackage = mainPackage;
        modelPackage = mainPackage + ".models";

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

        typeMapping = new HashMap<>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("date-time", "ZonedDateTime");
        typeMapping.put("offset-date-time", "OffsetDateTime");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("file", "File");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("Date", "LocalDate");
        typeMapping.put("DateTime", "ZonedDateTime");
        typeMapping.put("OffsetDateTime", "OffsetDateTime");
        typeMapping.put("uuid", "UUID");

        additionalProperties.put("modelPackage", modelPackage());
        additionalProperties.put("apiPackage", apiPackage());
        additionalProperties.put("infoUrl", "http://org.openapitools");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "Apache 2.0");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");


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
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");

        importMapping = new HashMap<>();
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "scala.collection.immutable.Map");
        importMapping.put("HashMap", "scala.collection.immutable.HashMap");
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("ArrayBuffer", "scala.collection.mutable.ArrayBuffer");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("ZonedDateTime", "java.time.ZonedDateTime");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        //refined
        importMapping.put("Refined", "eu.timepit.refined.api.Refined");
        importMapping.put("And", "eu.timepit.refined.boolean.And");
        importMapping.put("MinSize", "eu.timepit.refined.collection.MinSize");
        importMapping.put("MaxSize", "eu.timepit.refined.collection.MaxSize");
        importMapping.put("MatchesRegex", "eu.timepit.refined.string.MatchesRegex");
        importMapping.put("Greater", "eu.timepit.refined.numeric.Greater");
        importMapping.put("GreaterEqual", "eu.timepit.refined.numeric.GreaterEqual");
        importMapping.put("Less", "eu.timepit.refined.numeric.Less");
        importMapping.put("LessEqual", "eu.timepit.refined.numeric.LessEqual");

        cliOptions.add(new CliOption(EXCLUDE_SBT, "exclude sbt from generation"));
        cliOptions.add(new CliOption(SOURCE_SUBFOLDER, "name of subfolder, for example to generate code in src/scala/generated"));

        //inlineSchemaOption.put("SKIP_SCHEMA_REUSE", "true");
        //inlineSchemaOption.put("REFACTOR_ALLOF_INLINE_SCHEMAS", "true");

        cliOptions.add(new CliOption("mainPackage", "Main package name").defaultValue("org.openapitools.client"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);

            setApiPackage(packageName + ".apis");
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());

            setModelPackage(packageName + ".models");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, modelPackage());
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(SOURCE_SUBFOLDER)) {
            sourceSubFolder = (String) additionalProperties.get(SOURCE_SUBFOLDER);
        }
        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());
        additionalProperties.put("fnCapitalize", new CapitalizeLambda());
        additionalProperties.put("fnCamelize", new CamelizeLambda(true));

        sourceFolder = "src" + File.separator + sourceSubFolder + File.separator + sourceFolder;

        //supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("baseClient.mustache", packageFileFolderRelative(), "BaseClient.scala"));
        supportingFiles.add(new SupportingFile("jsonEntityEncoderDecoder.mustache", packageFileFolderRelative(), "JsonEntityEncoderDecoder.scala"));
        //supportingFiles.add(new SupportingFile("model.mustache", modelFileFolderRelative(), "Models.scala"));
        supportingFiles.add(new SupportingFile("failedRequest.mustache", modelFileFolderRelative(), "FailedRequest.scala"));
        supportingFiles.add(new SupportingFile("auth_model.mustache", modelFileFolderRelative(), "Authorization.scala"));
        supportingFiles.add(new SupportingFile("models_package.mustache", modelFileFolderRelative(), "package.scala"));
        supportingFiles.add(new SupportingFile("api.mustache", packageFileFolderRelative(), "Api.scala"));

        apiTemplateFiles.put("api.mustache", ".scala");

        if (!additionalProperties.containsKey(EXCLUDE_SBT) && !Boolean.parseBoolean((String) additionalProperties.get(EXCLUDE_SBT))) {
            supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
            supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        }
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);


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

    private String packageFileFolderRelative() {
        return sourceFolder + File.separator + packageName.replace('.', File.separatorChar);
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


    private static class JavadocLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            final String[] lines = fragment.split("\\r?\\n");
            final StringBuilder sb = new StringBuilder();
            sb.append("  /**\n");
            for (String line : lines) {
                sb.append("   * ").append(line).append("\n");
            }
            sb.append("   */\n");
            return sb.toString();
        }
    }

    private static class CapitalizeLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return StringUtils.capitalize(fragment);
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

    public void setMainPackage(String mainPackage) {
        this.configKeyPath = this.mainPackage = mainPackage;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + File.separator + modelDocPath).replace('/', File.separatorChar);
    }
}
