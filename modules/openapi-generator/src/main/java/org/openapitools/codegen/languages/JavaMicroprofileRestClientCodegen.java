/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipTestFeatures;
import org.openapitools.codegen.languages.features.LoggingTestFeatures;
import org.openapitools.codegen.languages.features.UseGenericResponseFeatures;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;
import java.util.Map;

public class JavaMicroprofileRestClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, UseGenericResponseFeatures, GzipTestFeatures, LoggingTestFeatures {

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaMicroprofileRestClientCodegen.class);

    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaMicroprofile";
    
    protected static final String DISABLE_MULTIPART = "disableMultipart";

    protected boolean useBeanValidation = false;

    protected boolean useGenericResponse = false;

    protected boolean useGzipFeatureForTests = false;

    protected boolean useLoggingFeatureForTests = false;

    public JavaMicroprofileRestClientCodegen() {
        super();

        supportsInheritance = true;

        sourceFolder = "src/gen/java";
        invokerPackage = "org.openapitools.api";
        artifactId = "gen-microprofile-rest-client";
        dateLibrary = "legacy";

        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        outputFolder = "generated-code/JavaJaxRS-CXF";

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME;

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));

        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE_FOR_TESTS, "Use Gzip Feature for tests"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE_FOR_TESTS, "Use Logging Feature for tests"));

        cliOptions.add(CliOption.newBoolean(USE_GENERIC_RESPONSE, "Use generic response"));
        cliOptions.add(CliOption.newBoolean(DISABLE_MULTIPART, "Disable multipart import"));
    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            boolean useBeanValidationProp = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION);
            this.setUseBeanValidation(useBeanValidationProp);
        }

        if (additionalProperties.containsKey(USE_GENERIC_RESPONSE)) {
            this.setUseGenericResponse(convertPropertyToBoolean(USE_GENERIC_RESPONSE));
        }

        if (useGenericResponse) {
            writePropertyBack(USE_GENERIC_RESPONSE, useGenericResponse);
        }

        this.setUseGzipFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE_FOR_TESTS));
        this.setUseLoggingFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE_FOR_TESTS));


        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        String apiFolder = (sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
        supportingFiles.add(new SupportingFile("api_exception.mustache", apiFolder, "ApiException.java"));
        supportingFiles.add(new SupportingFile("api_exception_mapper.mustache", apiFolder, "ApiExceptionMapper.java"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));

    }

    @Override
    public String getName() {
        return "microprofile-rest-client";
    }


    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        co.subresourceOperation = !co.path.isEmpty();
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        model.imports.remove("ApiModelProperty");
        model.imports.remove("ApiModel");
        model.imports.remove("JsonSerialize");
        model.imports.remove("ToStringSerializer");
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (codegenModel.imports.contains("ApiModel")) {
            // Remove io.swagger.annotations.ApiModel import
            codegenModel.imports.remove("ApiModel");
        }

        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        return AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
    }

    @Override
    public String getHelp() {
        return "Generates a MicroProfile Rest Client";
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public void setUseGzipFeatureForTests(boolean useGzipFeatureForTests) {
        this.useGzipFeatureForTests = useGzipFeatureForTests;
    }

    public void setUseLoggingFeatureForTests(boolean useLoggingFeatureForTests) {
        this.useLoggingFeatureForTests = useLoggingFeatureForTests;
    }

    public void setUseGenericResponse(boolean useGenericResponse) {
        this.useGenericResponse = useGenericResponse;
    }

}
