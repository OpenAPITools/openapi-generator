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
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PhpFlightServerCodegen extends AbstractPhpCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(PhpFlightServerCodegen.class);

    // Type-hintable primitive types
    // ref: http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration
    protected HashSet<String> typeHintable = new HashSet<>(
            Arrays.asList(
                    "array",
                    "bool",
                    "float",
                    "int",
                    "string"
            )
    );

    public PhpFlightServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit))
                .excludeDataTypeFeatures(
                        DataTypeFeature.MapOfCollectionOfEnum,
                        DataTypeFeature.MapOfEnum,
                        DataTypeFeature.MapOfCollectionOfModel,
                        DataTypeFeature.MapOfModel)
                .excludeParameterFeatures(
                        ParameterFeature.FormMultipart,
                        ParameterFeature.FormUnencoded,
                        ParameterFeature.Cookie)
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        embeddedTemplateDir = templateDir = "php-flight";

        // clear import mapping (from default generator) as slim does not use it
        // at the moment
        importMapping.clear();

        srcBasePath = "";

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "\\DateTime"
                )
        );

        variableNamingConvention = "camelCase";
        artifactVersion = "1.0.0";
        setInvokerPackage("OpenAPIServer");
        testPackage = invokerPackage + "\\Test";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;
        outputFolder = "generated-code" + File.separator + "php-flight";

        // no doc files
        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        apiTestTemplateFiles.clear();

        embeddedTemplateDir = templateDir = "php-flight";

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.stream().filter(o -> Objects.equals(o.getOpt(), VARIABLE_NAMING_CONVENTION)).findFirst().ifPresent(o -> o.defaultValue("camelCase"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-flight";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Flight Framework server library.";
    }

    @Override
    public String apiFileFolder() {
        if (apiPackage.startsWith(invokerPackage + "\\")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(StringUtils.removeStart(apiPackage, invokerPackage + "\\"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(apiPackage, srcBasePath));
    }

    @Override
    public String modelFileFolder() {
        if (modelPackage.startsWith(invokerPackage + "\\")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(StringUtils.removeStart(modelPackage, invokerPackage + "\\"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(modelPackage, srcBasePath));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        inlineSchemaOption.put("RESOLVE_INLINE_ENUMS", "true");

        // add trailing slash for mustache templates
        additionalProperties.put("relativeSrcBasePath", srcBasePath.isEmpty() ? "" : srcBasePath + "/");
        additionalProperties.put("modelSrcPath", "." + "/" + toSrcPath(modelPackage, srcBasePath));
        additionalProperties.put("apiSrcPath", "." + "/" + toSrcPath(apiPackage, srcBasePath));
        additionalProperties.put("testSrcPath", "." + "/" + toSrcPath(testPackage, srcBasePath));
        additionalProperties.put("escapedModelPackage", modelPackage.replace("\\", "\\\\"));

        if (additionalProperties.containsKey("testPackage")) {
            // Update model package to contain the specified model package name and the invoker package
            testPackage = invokerPackage + "\\" + (String) additionalProperties.get("testPackage");
        }
        additionalProperties.put("testPackage", testPackage);

        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("phpunit.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("register_routes.mustache", toSrcPath(invokerPackage, srcBasePath), "RegisterRoutes.php"));
        supportingFiles.add(new SupportingFile("register_routes_test.mustache", toSrcPath(testPackage, srcBasePath), "RegisterRoutesTest.php"));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();
        operationList.forEach(operation -> {
            operation.vendorExtensions.put("x-path", mapToFlightPath(operation.path));
            CodegenResponse defaultResponse = operation.responses.stream().filter(r -> r.is2xx && r.dataType != null && !this.getTypeHint(r.dataType, false, false).isEmpty()).findFirst().orElse(null);
            String returnType = defaultResponse != null ? this.getTypeHint(defaultResponse.dataType, false, false) + "|null" : "void";
            operation.vendorExtensions.put("x-return-type", returnType);
            operation.vendorExtensions.put("x-return-type-is-void", returnType.equals("void"));
            operation.vendorExtensions.put("x-return-type-comment", defaultResponse != null ? this.getTypeHint(defaultResponse.dataType, true, false) + "|null" : "void");
            operation.vendorExtensions.put("x-default-media-type", defaultResponse != null ? (
                    defaultResponse.getContent().containsKey("application/json") ? "application/json" : defaultResponse.getContent().keySet().stream().findFirst().orElse(null)) : null);
            operation.vendorExtensions.put("x-default-status-code", defaultResponse != null ? defaultResponse.code : operation.responses.stream().filter(r -> !r.isDefault).findFirst().map(r -> r.code).orElse("200"));
            operation.vendorExtensions.put("x-nonFormParams", operation.allParams.stream().filter(p -> !p.isFormParam).toArray());

            operation.allParams.forEach(param -> {
                param.vendorExtensions.put("x-parameter-type", param.required ? getTypeHint(param.dataType, false, false) : getTypeHintNullable(param.dataType, false));
                String commentType = param.required ? getTypeHint(param.dataType, true, false) : getTypeHintNullable(param.dataType, false);
                param.vendorExtensions.put("x-comment-type", commentType);
                param.vendorExtensions.put("x-comment-type-escaped", commentType.replace("\\", "\\\\"));
            });
        });
        escapeMediaType(operationList);
        return objs;
    }

    private String mapToFlightPath(String path) {
        return path.replaceAll("\\{([^}]+)}", "@$1");
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        ModelMap models = objs.getModels().get(0);
        CodegenModel model = models.getModel();

        // Simplify model var type
        for (CodegenProperty var : model.vars) {
            if (var.dataType != null) {
                // Determine if the parameter type is supported as a type hint and make it available
                // to the templating engine
                var.vendorExtensions.put("x-parameter-type", var.required ? getTypeHint(var.dataType, false, true) : getTypeHintNullable(var.dataType, true));
                var.vendorExtensions.put("x-comment-type", var.required ? getTypeHint(var.dataType, true, true) : getTypeHintNullableForComments(var.dataType, true));
            }
        }

        return objs;
    }

    protected String getTypeHintNullable(String type, boolean modelContext) {
        String typeHint = getTypeHint(type, false, modelContext);
        if (!typeHint.equals("")) {
            return "?" + typeHint;
        }

        return typeHint;
    }

    protected String getTypeHintNullableForComments(String type, boolean modelContext) {
        String typeHint = getTypeHint(type, true, modelContext);
        if (!typeHint.equals("")) {
            return typeHint + "|null";
        }

        return typeHint;
    }

    protected String getTypeHint(String type, boolean forComments, boolean modelContext) {
        // Type hint array types
        if (type.endsWith("[]")) {
            if (forComments) {
                //Make type hints for array in comments. Call getTypeHint recursive for extractSimpleName for models
                String typeWithoutArray = type.substring(0, type.length() - 2);
                return this.getTypeHint(typeWithoutArray, true, modelContext) + "[]";
            } else {
                return "array";
            }
        }

        // Check if the type is a native type that is type hintable in PHP
        if (typeHintable.contains(type)) {
            return type;
        }

        // Default includes are referenced by their fully-qualified class name (including namespace)
        if (defaultIncludes.contains(type)) {
            return type;
        }

        // Model classes are assumed to be imported and we reference them by their class name
        if (isModelClass(type)) {
            // This parameter is an instance of a model
            return modelContext ? extractSimpleName(type) : type;
        }

        // PHP does not support type hinting for this parameter data type
        return "";
    }

    protected Boolean isModelClass(String type) {
        return Boolean.valueOf(type.contains(modelPackage()));
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return toAbstractName("DefaultApi");
        }
        return toAbstractName(camelize(name) + "Api");
    }

    @Override
    public String toApiTestFilename(String name) {
        if (name.length() == 0) {
            return "DefaultApiTest";
        }
        return camelize(name) + "ApiTest";
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
}
