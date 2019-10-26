/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class JavaVertXWebServerCodegen extends AbstractJavaCodegen {

    protected String resourceFolder = "src/main/resources";
    protected String rootPackage = "org.openapitools.server";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    public static final String ROOT_PACKAGE = "rootPackage";

    public JavaVertXWebServerCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code" + File.separator + "javaVertXServer";

        modelTemplateFiles.clear();
        modelTemplateFiles.put("model.mustache", ".java");

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
        apiTemplateFiles.put("apiHandler.mustache", "Handler.java");

        embeddedTemplateDir = templateDir = "JavaVertXWebServer";

        apiPackage = rootPackage + ".api";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-java-vertx-web-server";
        artifactVersion = apiVersion;
        this.setDateLibrary("java8");

        // clioOptions default redifinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.ARTIFACT_VERSION, this.getArtifactVersion());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(this.DATE_LIBRARY, this.getDateLibrary());

        additionalProperties.put(ROOT_PACKAGE, rootPackage);
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-vertx-web";
    }

    public String getHelp() {
        return "Generates a Java Vert.x-Web Server.";
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

        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();

        String sourcePackageFolder = sourceFolder + File.separator + rootPackage.replace(".", File.separator);
        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("supportFiles/openapi.mustache", resourceFolder, "openapi.yaml"));
        supportingFiles.add(new SupportingFile("supportFiles/HttpServerVerticle.mustache", sourcePackageFolder, "HttpServerVerticle.java"));
        supportingFiles.add(new SupportingFile("supportFiles/MainVerticle.mustache", sourcePackageFolder, "MainVerticle.java"));
        supportingFiles.add(new SupportingFile("supportFiles/ApiResponse.mustache", sourcePackageFolder, "ApiResponse.java"));
        supportingFiles.add(new SupportingFile("supportFiles/ApiException.mustache", sourcePackageFolder, "ApiException.java"));
        supportingFiles.add(new SupportingFile("supportFiles/ParameterCast.mustache", sourcePackageFolder, "ParameterCast.java"));
        supportingFiles.add(new SupportingFile("supportFiles/pom.mustache", "", "pom.xml"));

        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
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
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation codegenOperation =
                super.fromOperation(path, httpMethod, operation, servers);

        for (CodegenParameter codegenParameter : codegenOperation.allParams) {
            if (codegenParameter.dataType.equals("File")) {
                codegenParameter.dataType = "FileUpload";
                codegenOperation.imports.remove("File");
                codegenOperation.imports.add("FileUpload");
            }
        }
        return codegenOperation;
    }
}
