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

import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipTestFeatures;
import org.openapitools.codegen.languages.features.LoggingTestFeatures;
import org.openapitools.codegen.languages.features.UseGenericResponseFeatures;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;
import java.util.Map;

public class JavaCXFClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, UseGenericResponseFeatures, GzipTestFeatures, LoggingTestFeatures {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaCXFClientCodegen.class);

    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";

    protected boolean useBeanValidation = false;

    protected boolean useGenericResponse = false;

    protected boolean useGzipFeatureForTests = false;

    protected boolean useLoggingFeatureForTests = false;

    private boolean useJackson = false;

    public JavaCXFClientCodegen() {
        super();

        supportsInheritance = true;

        sourceFolder = "src"+ File.separator +"gen"+ File.separator +"java";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-client";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        outputFolder = "generated-code/JavaJaxRS-CXF";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.SOURCE_FOLDER, this.getSourceFolder());
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(DATE_LIBRARY, this.getDateLibrary());

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");
        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE_FOR_TESTS, "Use Gzip Feature for tests"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE_FOR_TESTS, "Use Logging Feature for tests"));
        cliOptions.add(CliOption.newBoolean(USE_GENERIC_RESPONSE, "Use generic response"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(USE_GENERIC_RESPONSE)) {
            this.setUseGenericResponse(convertPropertyToBooleanAndWriteBack(USE_GENERIC_RESPONSE));
        }

        if (additionalProperties.containsKey(USE_GZIP_FEATURE_FOR_TESTS)) {
            this.setUseGzipFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE_FOR_TESTS));
        }

        if (additionalProperties.containsKey(USE_LOGGING_FEATURE_FOR_TESTS)) {
            this.setUseLoggingFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE_FOR_TESTS));
        }

        if (additionalProperties.containsKey(JACKSON)) {
            useJackson = convertPropertyToBooleanAndWriteBack(JACKSON);
        }

        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
            .doNotOverwrite());
    }

    @Override
    public String getName() {
        return "jaxrs-cxf-client";
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


        if (useJackson) {
            //Add jackson imports when model has inner enum
            if (Boolean.FALSE.equals(model.isEnum) && Boolean.TRUE.equals(model.hasEnums)) {
                model.imports.add("JsonCreator");
                model.imports.add("JsonValue");
            }

            //Add JsonNullable import and mark property nullable for templating if necessary
            if (openApiNullable) {
                if (Boolean.FALSE.equals(property.required) && Boolean.TRUE.equals(property.isNullable)) {
                    property.getVendorExtensions().put("x-is-jackson-optional-nullable", true);
                    model.imports.add("JsonNullable");
                    model.imports.add("JsonIgnore");
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        return AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS Client based on Apache CXF framework.";
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public boolean isUseBeanValidation() {
        return useBeanValidation;
    }

    @Override
    public void setUseGzipFeatureForTests(boolean useGzipFeatureForTests) {
        this.useGzipFeatureForTests = useGzipFeatureForTests;
    }

    public boolean isUseGzipFeatureForTests() {
        return useGzipFeatureForTests;
    }

    @Override
    public void setUseLoggingFeatureForTests(boolean useLoggingFeatureForTests) {
        this.useLoggingFeatureForTests = useLoggingFeatureForTests;
    }

    public boolean isUseLoggingFeatureForTests() {
        return useLoggingFeatureForTests;
    }

    @Override
    public void setUseGenericResponse(boolean useGenericResponse) {
        this.useGenericResponse = useGenericResponse;
    }

    public boolean isUseGenericResponse() {
        return useGenericResponse;
    }

    public boolean isUseJackson() {
        return useJackson;
    }
}
