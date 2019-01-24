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
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaJAXRSSpecServerCodegen extends AbstractJavaJAXRSServerCodegen {

    public static final String INTERFACE_ONLY = "interfaceOnly";
    public static final String RETURN_RESPONSE = "returnResponse";
    public static final String GENERATE_POM = "generatePom";
    public static final String USE_SWAGGER_ANNOTATIONS = "useSwaggerAnnotations";
    public static final String JACKSON = "jackson";

    private boolean interfaceOnly = false;
    private boolean returnResponse = false;
    private boolean generatePom = true;
    private boolean useSwaggerAnnotations = true;
    private boolean useJackson = false;

    private String primaryResourceName;

    public JavaJAXRSSpecServerCodegen() {
        super();
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-server";
        outputFolder = "generated-code/JavaJaxRS-Spec";

        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        apiTestTemplateFiles.clear(); // TODO: add api test template
        modelTestTemplateFiles.clear(); // TODO: add model test template

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        additionalProperties.put("title", title);

        typeMapping.put("date", "LocalDate");

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        super.embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "spec";

        for (int i = 0; i < cliOptions.size(); i++) {
            if (CodegenConstants.LIBRARY.equals(cliOptions.get(i).getOpt())) {
                cliOptions.remove(i);
                break;
            }
        }

        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);

        Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();

        supportedLibraries.put(DEFAULT_LIBRARY, "JAXRS");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(CliOption.newBoolean(GENERATE_POM, "Whether to generate pom.xml if the file does not already exist.").defaultValue(String.valueOf(generatePom)));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY, "Whether to generate only API interface stubs without the server files.").defaultValue(String.valueOf(interfaceOnly)));
        cliOptions.add(CliOption.newBoolean(RETURN_RESPONSE, "Whether generate API interface should return javax.ws.rs.core.Response instead of a deserialized entity. Only useful if interfaceOnly is true.").defaultValue(String.valueOf(returnResponse)));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_ANNOTATIONS, "Whether to generate Swagger annotations.", useSwaggerAnnotations));
    }

    @Override
    public void processOpts() {
        if (additionalProperties.containsKey(GENERATE_POM)) {
            generatePom = Boolean.valueOf(additionalProperties.get(GENERATE_POM).toString());
        }
        if (additionalProperties.containsKey(INTERFACE_ONLY)) {
            interfaceOnly = Boolean.valueOf(additionalProperties.get(INTERFACE_ONLY).toString());
            if (!interfaceOnly) {
                additionalProperties.remove(INTERFACE_ONLY);
            }
        }
        if (additionalProperties.containsKey(RETURN_RESPONSE)) {
            returnResponse = Boolean.valueOf(additionalProperties.get(RETURN_RESPONSE).toString());
            if (!returnResponse) {
                additionalProperties.remove(RETURN_RESPONSE);
            }
        }
        if (additionalProperties.containsKey(USE_SWAGGER_ANNOTATIONS)) {
            useSwaggerAnnotations = Boolean.valueOf(additionalProperties.get(USE_SWAGGER_ANNOTATIONS).toString());
        }
        writePropertyBack(USE_SWAGGER_ANNOTATIONS, useSwaggerAnnotations);

        useJackson = convertPropertyToBoolean(JACKSON);

        if (interfaceOnly) {
            // Change default artifactId if genereating interfaces only, before command line options are applied in base class.
            artifactId = "openapi-jaxrs-client";
        }

        super.processOpts();

        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
        if (generatePom) {
            writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        }
        if (!interfaceOnly) {
            writeOptional(outputFolder, new SupportingFile("RestApplication.mustache",
                    (sourceFolder + '/' + invokerPackage).replace(".", "/"), "RestApplication.java"));
        }

        supportingFiles.add(new SupportingFile("openapi.mustache",
                "src/main/openapi",
                "openapi.yaml")
        );
    }


    @Override
    public String getName() {
        return "jaxrs-spec";
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        String basePath = resourcePath;
        if (basePath.startsWith("/")) {
            basePath = basePath.substring(1);
        }
        int pos = basePath.indexOf("/");
        if (pos > 0) {
            basePath = basePath.substring(0, pos);
        }

        String operationKey = basePath;
        if (StringUtils.isEmpty(basePath)) {
            basePath = tag;
            operationKey = "";
            primaryResourceName = tag;
        } else if (basePath.matches("\\{.*\\}")) {
            basePath = tag;
            operationKey = "";
            co.subresourceOperation = true;
        } else {
            if (co.path.startsWith("/" + basePath)) {
                co.path = co.path.substring(("/" + basePath).length());
            }
            co.subresourceOperation = !co.path.isEmpty();
        }
        List<CodegenOperation> opList = operations.get(operationKey);
        if (opList == null || opList.isEmpty()) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(operationKey, opList);
        }
        opList.add(co);
        co.baseName = basePath;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (!useSwaggerAnnotations) {
            codegenModel.imports.remove("ApiModelProperty");
            codegenModel.imports.remove("ApiModel");
        }
        if (!useJackson) {
            codegenModel.imports.remove("JsonSerialize");
            codegenModel.imports.remove("ToStringSerializer");
            codegenModel.imports.remove("JsonValue");
            codegenModel.imports.remove("JsonProperty");
        }
        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS Server according to JAXRS 2.0 specification.";
    }

    @Override
    public String toApiName(final String name) {
        String computed = name;
        if (computed.length() == 0) {
            return primaryResourceName + "Api";
        }
        computed = sanitizeName(computed);
        return camelize(computed) + "Api";
    }
}
