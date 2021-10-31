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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.rightPad;
import static org.openapitools.codegen.languages.AbstractJavaCodegen.DATE_LIBRARY;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ScalaPlayFrameworkServerCodegen extends AbstractScalaCodegen implements CodegenConfig {
    public static final String TITLE = "title";
    public static final String SKIP_STUBS = "skipStubs";
    public static final String SUPPORT_ASYNC = "supportAsync";
    public static final String GENERATE_CUSTOM_EXCEPTIONS = "generateCustomExceptions";
    public static final String USE_SWAGGER_UI = "useSwaggerUI";
    public static final String ROUTES_FILE_NAME = "routesFileName";
    public static final String BASE_PACKAGE = "basePackage";

     final Logger LOGGER = LoggerFactory.getLogger(ScalaPlayFrameworkServerCodegen.class);

    protected boolean skipStubs = false;
    protected boolean supportAsync = false;
    protected boolean generateCustomExceptions = true;
    protected boolean useSwaggerUI = true;
    protected String routesFileName = "routes";
    protected String basePackage = "org.openapitools";

    public ScalaPlayFrameworkServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "scala-play-server";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-play-server";
        hideGenerationTimestamp = false;
        sourceFolder = "app";
        apiPackage = "api";
        modelPackage = "model";

        instantiationTypes.put("map", "Map");
        instantiationTypes.put("array", "List");

        typeMapping.put("DateTime", "OffsetDateTime");
        typeMapping.put("Date", "LocalDate");
        typeMapping.put("Integer", "Int");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("ByteArray", "Array[Byte]");
        typeMapping.put("object", "JsObject");
        typeMapping.put("file", "TemporaryFile");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("decimal", "BigDecimal");

        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("TemporaryFile", "play.api.libs.Files.TemporaryFile");

        cliOptions.removeIf(opt -> DATE_LIBRARY.equals(opt.getOpt()));
        cliOptions.add(new CliOption(ROUTES_FILE_NAME, "Name of the routes file to generate.").defaultValue(routesFileName));
        cliOptions.add(new CliOption(BASE_PACKAGE, "Base package in which supporting classes are generated.").defaultValue(basePackage));

        addCliOptionWithDefault(SKIP_STUBS, "If set, skips generation of stub classes.", skipStubs);
        addCliOptionWithDefault(SUPPORT_ASYNC, "If set, wraps API return types with Futures and generates async actions.", supportAsync);
        addCliOptionWithDefault(GENERATE_CUSTOM_EXCEPTIONS, "If set, generates custom exception types.", generateCustomExceptions);
        addCliOptionWithDefault(USE_SWAGGER_UI, "Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies", useSwaggerUI);
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "scala-play-server";
    }

    public String getHelp() {
        return "Generates a Scala server application (beta) with Play Framework.";
    }

    public void setSupportAsync(boolean supportAsync) {
        this.supportAsync = supportAsync;
    }

    public void setSkipStubs(boolean skipStubs) {
        this.skipStubs = skipStubs;
    }

    public void setGenerateCustomExceptions(boolean generateCustomExceptions) {
        this.generateCustomExceptions = generateCustomExceptions;
    }

    public void setRoutesFileName(String routesFileName) {
        this.routesFileName = routesFileName;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public void setUseSwaggerUI(boolean useSwaggerUI) {
        this.useSwaggerUI = useSwaggerUI;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(SKIP_STUBS)) {
            this.setSkipStubs(convertPropertyToBoolean(SKIP_STUBS));
        }
        writePropertyBack(SKIP_STUBS, skipStubs);

        if (additionalProperties.containsKey(SUPPORT_ASYNC)) {
            this.setSupportAsync(convertPropertyToBoolean(SUPPORT_ASYNC));
        }
        writePropertyBack(SUPPORT_ASYNC, supportAsync);

        if (additionalProperties.containsKey(GENERATE_CUSTOM_EXCEPTIONS)) {
            this.setGenerateCustomExceptions(convertPropertyToBoolean(GENERATE_CUSTOM_EXCEPTIONS));
        }
        writePropertyBack(GENERATE_CUSTOM_EXCEPTIONS, generateCustomExceptions);

        if (additionalProperties.containsKey(USE_SWAGGER_UI)) {
            this.setUseSwaggerUI(convertPropertyToBoolean(USE_SWAGGER_UI));
        }
        writePropertyBack(USE_SWAGGER_UI, useSwaggerUI);

        if (additionalProperties.containsKey(ROUTES_FILE_NAME)) {
            this.setRoutesFileName((String) additionalProperties.get(ROUTES_FILE_NAME));
        } else {
            additionalProperties.put(ROUTES_FILE_NAME, routesFileName);
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        } else {
            additionalProperties.put(BASE_PACKAGE, basePackage);
        }

        apiTemplateFiles.remove("api.mustache");

        if (!skipStubs) {
            apiTemplateFiles.put("app/apiImplStubs.scala.mustache", "Impl.scala");
        }

        apiTemplateFiles.put("app/apiTrait.scala.mustache", ".scala");
        apiTemplateFiles.put("app/apiController.scala.mustache", "Controller.scala");

        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("conf/application.conf.mustache", "conf", "application.conf"));
        supportingFiles.add(new SupportingFile("conf/logback.xml.mustache", "conf", "logback.xml"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt.mustache", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("conf/routes.mustache", "conf", routesFileName));
        supportingFiles.add(new SupportingFile("app/module.scala.mustache", getBasePackagePath(), "Module.scala"));
        supportingFiles.add(new SupportingFile("app/errorHandler.scala.mustache", getBasePackagePath(), "ErrorHandler.scala"));

        if (generateCustomExceptions) {
            supportingFiles.add(new SupportingFile("app/exceptions.scala.mustache", getBasePackagePath(), "OpenApiExceptions.scala"));
        }

        if (this.useSwaggerUI) {
            //App/Controllers
            supportingFiles.add(new SupportingFile("public/openapi.json.mustache", "public", "openapi.json"));
            supportingFiles.add(new SupportingFile("app/apiDocController.scala.mustache", String.format(Locale.ROOT, "app/%s", apiPackage.replace(".", File.separator)), "ApiDocController.scala"));
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("indented_4", new IndentedLambda(4, " "));
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, CodegenModel> models = new HashMap<>();

        for (Object _mo : allModels) {
            CodegenModel model = (CodegenModel) ((Map<String, Object>) _mo).get("model");
            models.put(model.classname, model);
        }

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                Pattern pathVariableMatcher = Pattern.compile("\\{([^}]+)}");
                Matcher match = pathVariableMatcher.matcher(operation.path);
                while (match.find()) {
                    String completeMatch = match.group();
                    String replacement = ":" + camelize(match.group(1), true);
                    operation.path = operation.path.replace(completeMatch, replacement);
                }

                if ("null".equals(operation.defaultResponse) && models.containsKey(operation.returnType)) {
                    operation.defaultResponse = models.get(operation.returnType).defaultValue;
                }
            }
        }

        return objs;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);
        Map<String, CodegenModel> modelsByClassName = new HashMap<>();

        for (Object _outer : objs.values()) {
            Map<String, Object> outer = (Map<String, Object>) _outer;
            List<Map<String, Object>> models = (List<Map<String, Object>>) outer.get("models");

            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                postProcessModelsEnum(outer);
                cm.classVarName = camelize(cm.classVarName, true);
                modelsByClassName.put(cm.classname, cm);
                boolean hasFiles = cm.vars.stream().anyMatch(var -> var.isFile);
                cm.vendorExtensions.put("x-has-files", hasFiles);
            }
        }

        for (CodegenModel model : modelsByClassName.values()) {
            model.defaultValue = generateModelDefaultValue(model, modelsByClassName);
        }

        return objs;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = super.postProcessSupportingFileData(objs);
        generateJSONSpecFile(objs);

        // Prettify routes file
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");
        List<CodegenOperation> ops = apis.stream()
                .map(api -> (Map<String, Object>) api.get("operations"))
                .flatMap(operations -> ((List<CodegenOperation>) operations.get("operation")).stream())
                .collect(Collectors.toList());
        int maxPathLength = ops.stream()
                .mapToInt(op -> op.httpMethod.length() + op.path.length())
                .reduce(0, Integer::max);
        ops.forEach(op -> {
            String paddedPath = rightPad(op.path, maxPathLength - op.httpMethod.length());
            op.vendorExtensions.put("x-padded-path", paddedPath);
        });
        ops.forEach(op -> {
            op.vendorExtensions.put("x-has-path-params", op.getHasPathParams());
        });

        return objs;
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        openAPIType = getAlias(openAPIType);

        // don't apply renaming on types from the typeMapping
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (null == openAPIType) {
            LOGGER.error("No Type defined for Schema {}", p);
        }
        return toModelName(openAPIType);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getRequired() != null && p.getRequired().contains(p.getName())) {
            return "None";
        }

        if (p.getDefault() != null) {
            return p.getDefault().toString();
        }

        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        }

        if (ModelUtils.isDateSchema(p)) {
            return "LocalDate.now";
        }

        if (ModelUtils.isDateTimeSchema(p)) {
            return "OffsetDateTime.now";
        }

        if (ModelUtils.isDoubleSchema(p)) {
            return "0.0";
        }

        if (ModelUtils.isFloatSchema(p)) {
            return "0.0F";
        }

        if (ModelUtils.isIntegerSchema(p)) {
            return "0";
        }

        if (ModelUtils.isLongSchema(p)) {
            return "0L";
        }

        if (ModelUtils.isStringSchema(p)) {
            return "\"\"";
        }

        if (ModelUtils.isMapSchema(p)) {
            Schema ap = getAdditionalProperties(p);
            String inner = getSchemaType(ap);
            return "Map.empty[String, " + inner + "]";
        }

        if (ModelUtils.isArraySchema(p)) {
            Schema items = ((ArraySchema) p).getItems();
            String inner = getSchemaType(items);
            if (ModelUtils.isSet(p)) {
                return "Set.empty[" + inner + "]";
            }
            return "List.empty[" + inner + "]";
        }

        return "null";
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return camelizeStripReservedEscape(property.name);
    }

    public String camelizeStripReservedEscape(String str) {
        if (str.startsWith("`") && str.endsWith("`")) {
            str = str.substring(1, str.length() - 1);
        }

        return camelize(str);
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = camelize(value.replaceAll("\\W+", "_"));
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    private void addCliOptionWithDefault(String name, String description, boolean defaultValue) {
        cliOptions.add(CliOption.newBoolean(name, description).defaultValue(Boolean.toString(defaultValue)));
    }

    private String getBasePackagePath() {
        return String.format(Locale.ROOT, "%s/%s", sourceFolder, basePackage.replace(".", File.separator));
    }

    private String generateModelDefaultValue(CodegenModel cm, Map<String, CodegenModel> models) {
        StringBuilder defaultValue = new StringBuilder();
        defaultValue.append(cm.classname).append('(');

        for(int i = 0; i < cm.vars.size(); i++) {
            CodegenProperty var = cm.vars.get(i);
            if (!var.required) {
                defaultValue.append("None");
            } else if (models.containsKey(var.dataType)) {
                defaultValue.append(generateModelDefaultValue(models.get(var.dataType), models));
            } else if (var.defaultValue != null) {
                defaultValue.append(var.defaultValue);
            } else if (var.isEnum) {
                defaultValue.append(cm.classname).append('.').append(var.enumName).append(".values.head");
            } else {
                LOGGER.warn("Unknown default value for var {0} in class {1}", var.name, cm.classname);
                defaultValue.append("null");
            }

            if (i < cm.vars.size()-1) {
                defaultValue.append(", ");
            }
        }

        if (cm.isMap) {
            defaultValue.append(", Map.empty");
        }

        defaultValue.append(')');

        return defaultValue.toString();
    }
}
