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

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JavaVertXWebServerCodegen extends AbstractJavaCodegen {

    protected String resourceFolder = "src/main/resources";
    protected String apiVersion = "1.0.0-SNAPSHOT";

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

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-vertx-web";
    }

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

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> newObjs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) newObjs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
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
