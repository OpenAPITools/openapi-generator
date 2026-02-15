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
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.CodegenConstants.X_IMPLEMENTS;

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
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> modelsMap = super.postProcessAllModels(objs);

        // First pass: Count how many oneOf parents reference each child model
        Map<String, Integer> oneOfMemberCount = new HashMap<>();
        Map<String, CodegenModel> allModels = new HashMap<>();

        for (ModelsMap mm : modelsMap.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();
                allModels.put(cModel.classname, cModel);

                if (!cModel.oneOf.isEmpty()) {
                    for (String childName : cModel.oneOf) {
                        oneOfMemberCount.put(childName, oneOfMemberCount.getOrDefault(childName, 0) + 1);
                    }
                }
            }
        }

        // Second pass: Mark and configure models
        for (ModelsMap mm : modelsMap.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();

                // Mark models with oneOf as sealed traits (or regular traits for edge cases)
                if (!cModel.oneOf.isEmpty()) {
                    // Collect oneOf members for inlining
                    List<CodegenModel> oneOfMembers = new ArrayList<>();
                    Set<String> additionalImports = new HashSet<>();
                    for (String childName : cModel.oneOf) {
                        CodegenModel childModel = allModels.get(childName);
                        if (childModel != null && oneOfMemberCount.getOrDefault(childName, 0) == 1) {
                            // Mark for inlining (only used by this one parent)
                            childModel.getVendorExtensions().put("x-isOneOfMember", true);
                            childModel.getVendorExtensions().put("x-oneOfParent", cModel.classname);
                            // Store parent's discriminator info for use in template
                            if (cModel.discriminator != null) {
                                childModel.getVendorExtensions().put("x-parentDiscriminatorName", cModel.discriminator.getPropertyName());
                            }
                            oneOfMembers.add(childModel);

                            // Collect imports from inlined members
                            if (childModel.imports != null) {
                                additionalImports.addAll(childModel.imports);
                            }
                        }
                    }

                    // Decide between sealed trait (with inlined members) vs regular trait (edge cases)
                    // Use sealed trait ONLY if ALL oneOf members can be inlined
                    // If some are inlined and some aren't (mixed case), use regular trait
                    boolean allMembersInlined = oneOfMembers.size() == cModel.oneOf.size();

                    if (!oneOfMembers.isEmpty() && allMembersInlined) {
                        // Normal case: can inline ALL members, use sealed trait
                        cModel.getVendorExtensions().put("x-isSealedTrait", true);
                        cModel.getVendorExtensions().put("x-oneOfMembers", oneOfMembers);

                        // Add child imports to parent (excluding already present imports)
                        if (!additionalImports.isEmpty()) {
                            Set<String> parentImports = cModel.imports != null ? new HashSet<>(cModel.imports) : new HashSet<>();
                            additionalImports.removeAll(parentImports);
                            if (!additionalImports.isEmpty()) {
                                if (cModel.imports == null) {
                                    cModel.imports = new HashSet<>();
                                }
                                cModel.imports.addAll(additionalImports);
                            }
                        }
                    } else {
                        // Edge case: nested oneOf, shared members, or mixed case - use regular trait
                        // Implementations will be in separate files
                        cModel.getVendorExtensions().put("x-isRegularTrait", true);

                        // For mixed cases, unmark members for inlining - they need to be separate files
                        for (CodegenModel member : oneOfMembers) {
                            member.getVendorExtensions().remove("x-isOneOfMember");
                            member.getVendorExtensions().remove("x-oneOfParent");
                            member.getVendorExtensions().remove("x-parentDiscriminatorName");
                        }

                        if (oneOfMembers.isEmpty()) {
                            LOGGER.warn("Model '{}' has oneOf with no inlineable members (likely nested oneOf). " +
                                        "Generating as regular trait instead of sealed trait.", cModel.classname);
                        } else {
                            LOGGER.warn("Model '{}' has mixed oneOf (some inlineable, some not). " +
                                        "Generating as regular trait instead of sealed trait.", cModel.classname);
                        }
                    }
                } else if (cModel.isEnum) {
                    cModel.getVendorExtensions().put("x-isEnum", true);
                } else {
                    cModel.getVendorExtensions().put("x-another", true);
                }

                // Handle discriminator
                if (cModel.discriminator != null) {
                    cModel.getVendorExtensions().put("x-use-discr", true);

                    if (cModel.discriminator.getMapping() != null) {
                        cModel.getVendorExtensions().put("x-use-discr-mapping", true);
                    }
                }

                // Handle X_IMPLEMENTS extension (for extends/with separation)
                try {
                    List<String> exts = (List<String>) cModel.getVendorExtensions().get(X_IMPLEMENTS);
                    if (exts != null) {
                        cModel.getVendorExtensions().put("x-extends", exts.subList(0, 1));
                        cModel.getVendorExtensions().put("x-extendsWith", exts.subList(1, exts.size()));
                    }
                } catch (IndexOutOfBoundsException ignored) {
                }
            }
        }

        // Third pass: Clear X_IMPLEMENTS for models extending multiple SEALED traits
        // (Regular traits can be extended from separate files, but sealed traits cannot)
        for (ModelsMap mm : modelsMap.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();

                // Check if this model extends multiple sealed traits
                List<String> exts = (List<String>) cModel.getVendorExtensions().get(X_IMPLEMENTS);
                if (exts != null && exts.size() > 1) {
                    // Count how many of the parents are sealed traits
                    int sealedParentCount = 0;
                    for (String parentName : exts) {
                        CodegenModel parentModel = allModels.get(parentName);
                        if (parentModel != null && parentModel.getVendorExtensions().containsKey("x-isSealedTrait")) {
                            sealedParentCount++;
                        }
                    }

                    // If extending multiple sealed traits, clear all extends (impossible in Scala)
                    if (sealedParentCount > 1) {
                        cModel.getVendorExtensions().remove(X_IMPLEMENTS);
                        LOGGER.warn("Model '{}' cannot extend multiple sealed traits. Generating as standalone class.",
                                    cModel.classname);
                    }
                }
            }
        }

        // Fourth pass: Remove inlined members from output (no separate file generation)
        modelsMap.entrySet().removeIf(entry -> {
            ModelsMap mm = entry.getValue();
            return mm.getModels().stream()
                .anyMatch(model -> model.getModel().getVendorExtensions().containsKey("x-isOneOfMember"));
        });

        return modelsMap;
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
