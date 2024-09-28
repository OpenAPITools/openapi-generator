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

import static org.openapitools.codegen.utils.ModelUtils.getSchemaItems;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JavaVertXWebServerCodegen extends AbstractJavaCodegen {

    protected String resourceFolder = "src/main/resources";
    protected String apiVersion = "1.0.0-SNAPSHOT";
    private final Logger LOGGER = LoggerFactory.getLogger(JavaVertXWebServerCodegen.class);

    public JavaVertXWebServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // set the output folder here
        outputFolder = "generated-code" + File.separator + "java-vertx-web";

        modelTemplateFiles.clear();
        modelTemplateFiles.put("model.mustache", ".java");

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
        apiTemplateFiles.put("apiHandler.mustache", "Handler.java");

        embeddedTemplateDir = templateDir = "JavaVertXWebServer";

        invokerPackage = "org.openapitools.vertxweb.server";
        apiPackage = invokerPackage + ".api";
        modelPackage = invokerPackage + ".model";
        artifactId = "openapi-java-vertx-web-server";
        artifactVersion = apiVersion;
        this.setDateLibrary("java8");

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.ARTIFACT_VERSION, this.getArtifactVersion());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        updateOption(DATE_LIBRARY, this.getDateLibrary());

        // Override type mapping
        typeMapping.put("file", "FileUpload");
        typeMapping.put("UUID", "String");
        typeMapping.put("date", "String");
        typeMapping.put("DateTime", "String");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-vertx-web";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Vert.x-Web Server (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiTestTemplateFiles.clear();

        importMapping.remove("JsonCreator");
        importMapping.remove("com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonInclude", "com.fasterxml.jackson.annotation.JsonInclude");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("FileUpload", "io.vertx.ext.web.FileUpload");
        importMapping.put("JsonObject", "io.vertx.core.json.JsonObject");

        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();

        String sourcePackageFolder = sourceFolder + File.separator + invokerPackage.replace(".", File.separator);
        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("supportFiles/openapi.mustache", resourceFolder, "openapi.yaml"));
        supportingFiles.add(new SupportingFile("supportFiles/HttpServerVerticle.mustache", sourcePackageFolder, "HttpServerVerticle.java"));
        supportingFiles.add(new SupportingFile("supportFiles/ApiResponse.mustache", sourcePackageFolder, "ApiResponse.java"));
        supportingFiles.add(new SupportingFile("supportFiles/pom.mustache", "", "pom.xml"));

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = unaliasSchema(p);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        if (ModelUtils.isArraySchema(target)) {
            Schema<?> items = getSchemaItems(schema);
            String typeDeclaration = super.getTypeDeclarationForArray(items);
            return getSchemaType(target) + "<" + typeDeclaration + ">";
        } else if (ModelUtils.isMapSchema(target)) {
            // Note: ModelUtils.isMapSchema(p) returns true when p is a composed schema that also defines
            // additionalproperties: true
            Schema<?> inner = ModelUtils.getAdditionalProperties(target);
            if (inner == null) {
                LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
                p.setAdditionalProperties(inner);
            }
            return getSchemaType(target) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(target);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!model.isEnum) {
            model.imports.add("JsonInclude");
            model.imports.add("JsonProperty");
            if (model.hasEnums) {
                model.imports.add("JsonValue");
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap newObjs = super.postProcessOperationsWithModels(objs, allModels);
        OperationMap operations = newObjs.getOperations();
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                operation.httpMethod = operation.httpMethod.toLowerCase(Locale.ROOT);

                if (operation.returnType == null) {
                    operation.returnType = "Void";
                }
                if (operation.allParams.stream().anyMatch(p -> p.isFormParam && p.isFile)) {
                    // If there is a file upload, exclude other form params since it's not clear how the user should access to these
                    operation.allParams = operation
                        .allParams
                        .stream()
                        .filter(p -> !p.isFormParam || p.isFile)
                        .collect(Collectors.toList());
                } else if (operation.allParams.stream().anyMatch(p -> p.isFormParam)) {
                    // In Vert.x 4 Web OpenAPI the forms are handled as single json object
                    // We create a dummy param here and remove the other ones
                    CodegenParameter dummyParam = new CodegenParameter();
                    dummyParam.isFormParam = true;
                    dummyParam.isFile = false;
                    dummyParam.dataType = "JsonObject";
                    dummyParam.paramName = "formBody";
                    operation.allParams = Stream.concat(
                        operation.allParams.stream().filter(p -> !p.isFormParam),
                        Stream.of(dummyParam)
                    ).collect(Collectors.toList());
                }
            }
        }
        return newObjs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        codegenModel.imports.remove("ApiModel");
        codegenModel.imports.remove("ApiModelProperty");
        return codegenModel;
    }
}
