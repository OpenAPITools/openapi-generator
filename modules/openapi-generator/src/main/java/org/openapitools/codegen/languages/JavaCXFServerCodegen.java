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
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.GzipTestFeatures;
import org.openapitools.codegen.languages.features.LoggingTestFeatures;
import org.openapitools.codegen.languages.features.UseGenericResponseFeatures;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;
import java.util.Map;

public class JavaCXFServerCodegen extends AbstractJavaJAXRSServerCodegen
        implements CXFServerFeatures, GzipTestFeatures, LoggingTestFeatures, UseGenericResponseFeatures {
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaCXFServerCodegen.class);

    protected boolean addConsumesProducesJson = true;

    protected boolean generateSpringApplication = false;

    protected boolean useSpringAnnotationConfig = false;

    protected boolean useSwaggerFeature = false;

    protected boolean useSwaggerUI = false;

    protected boolean useWadlFeature = false;

    protected boolean useMultipartFeature = false;

    protected boolean useBeanValidationFeature = false;

    protected boolean generateSpringBootApplication = false;

    protected boolean generateJbossDeploymentDescriptor = false;

    protected boolean useGzipFeature = false;

    protected boolean useGzipFeatureForTests = false;

    protected boolean useLoggingFeature = false;

    protected boolean useLoggingFeatureForTests = false;

    protected boolean useAnnotatedBasePath = false;

    protected boolean generateNonSpringApplication = false;

    protected boolean useGenericResponse = false;

    public JavaCXFServerCodegen() {
        super();

        supportsInheritance = true;

        artifactId = "openapi-cxf-server";

        outputFolder = "generated-code/JavaJaxRS-CXF";

        // clioOptions default redifinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());

        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

        cliOptions.add(CliOption.newBoolean(GENERATE_SPRING_APPLICATION, "Generate Spring application"));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_ANNOTATION_CONFIG, "Use Spring Annotation Config"));

        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_FEATURE, "Use Swagger Feature"));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_UI, "Use Swagger UI"));

        cliOptions.add(CliOption.newBoolean(USE_WADL_FEATURE, "Use WADL Feature"));
        cliOptions.add(CliOption.newBoolean(USE_MULTIPART_FEATURE, "Use Multipart Feature"));

        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Use Gzip Feature"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE_FOR_TESTS, "Use Gzip Feature for tests"));

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION_FEATURE, "Use BeanValidation Feature"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE, "Use Logging Feature"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE_FOR_TESTS, "Use Logging Feature for tests"));

        cliOptions.add(CliOption.newBoolean(GENERATE_SPRING_BOOT_APPLICATION, "Generate Spring Boot application"));
        cliOptions.add(
                CliOption.newBoolean(GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR, "Generate Jboss Deployment Descriptor"));

        cliOptions
                .add(CliOption.newBoolean(ADD_CONSUMES_PRODUCES_JSON, "Add @Consumes/@Produces Json to API interface"));

        cliOptions.add(CliOption.newBoolean(USE_ANNOTATED_BASE_PATH, "Use @Path annotations for basePath"));

        cliOptions.add(CliOption.newBoolean(GENERATE_NON_SPRING_APPLICATION, "Generate non-Spring application"));
        cliOptions.add(CliOption.newBoolean(USE_GENERIC_RESPONSE, "Use generic response"));

    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(ADD_CONSUMES_PRODUCES_JSON)) {
            this.setAddConsumesProducesJson(convertPropertyToBooleanAndWriteBack(ADD_CONSUMES_PRODUCES_JSON));
        }

        if (additionalProperties.containsKey(USE_GENERIC_RESPONSE)) {
            this.setUseGenericResponse(convertPropertyToBoolean(USE_GENERIC_RESPONSE));
        }

        if (useGenericResponse) {
            writePropertyBack(USE_GENERIC_RESPONSE, useGenericResponse);
        }

        if (additionalProperties.containsKey(GENERATE_SPRING_APPLICATION)) {
            this.setGenerateSpringApplication(convertPropertyToBooleanAndWriteBack(GENERATE_SPRING_APPLICATION));

            this.setUseSwaggerFeature(convertPropertyToBooleanAndWriteBack(USE_SWAGGER_FEATURE));
            this.setUseSwaggerUI(convertPropertyToBooleanAndWriteBack(USE_SWAGGER_UI));

            this.setUseWadlFeature(convertPropertyToBooleanAndWriteBack(USE_WADL_FEATURE));
            this.setUseMultipartFeature(convertPropertyToBooleanAndWriteBack(USE_MULTIPART_FEATURE));
            this.setUseGzipFeature(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE));
            this.setUseGzipFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE_FOR_TESTS));
            this.setUseLoggingFeature(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE));
            this.setUseLoggingFeatureForTests(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE_FOR_TESTS));
            this.setUseSpringAnnotationConfig(convertPropertyToBooleanAndWriteBack(USE_SPRING_ANNOTATION_CONFIG));

            boolean useBeanValidationFeature = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION_FEATURE);
            this.setUseBeanValidationFeature(useBeanValidationFeature);
            if (useBeanValidationFeature) {
                LOGGER.info("make sure your target server supports Bean Validation 1.1");
            }

            this.setGenerateSpringBootApplication(convertPropertyToBooleanAndWriteBack(GENERATE_SPRING_BOOT_APPLICATION));
        }

        if (additionalProperties.containsKey(GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR)) {
            boolean generateJbossDeploymentDescriptorProp = convertPropertyToBooleanAndWriteBack(
                    GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR);
            this.setGenerateJbossDeploymentDescriptor(generateJbossDeploymentDescriptorProp);
        }

        if (additionalProperties.containsKey(USE_ANNOTATED_BASE_PATH)) {
            boolean useAnnotatedBasePathProp = convertPropertyToBooleanAndWriteBack(USE_ANNOTATED_BASE_PATH);
            this.setUseAnnotatedBasePath(useAnnotatedBasePathProp);
        }

        if (additionalProperties.containsKey(GENERATE_NON_SPRING_APPLICATION)) {
            boolean generateNonSpringApplication = convertPropertyToBooleanAndWriteBack(GENERATE_NON_SPRING_APPLICATION);
            this.setGenerateNonSpringApplication(generateNonSpringApplication);
        }

        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen

        writeOptional(outputFolder, new SupportingFile("server/pom.mustache", "", "pom.xml"));

        writeOptional(outputFolder,
                new SupportingFile("server/openapi-generator-ignore.mustache", "", ".openapi-generator-ignore"));

        if (this.generateSpringApplication) {
            writeOptional(outputFolder, new SupportingFile("server/readme.md", "", "readme.md"));

            writeOptional(outputFolder, new SupportingFile("server/ApplicationContext.xml.mustache",
                    ("src/main/resources"), "ApplicationContext.xml"));
            writeOptional(outputFolder, new SupportingFile("server/web.mustache",
                    ("src/main/webapp/WEB-INF"), "web.xml"));
            writeOptional(outputFolder, new SupportingFile("server/context.xml.mustache",
                    ("src/main/webapp/WEB-INF"), "context.xml"));

            // Jboss
            if (generateJbossDeploymentDescriptor) {
                writeOptional(outputFolder, new SupportingFile("server/jboss-web.xml.mustache",
                        ("src/main/webapp/WEB-INF"), "jboss-web.xml"));

            }

            // Spring Boot
            if (this.generateSpringBootApplication) {
                writeOptional(outputFolder, new SupportingFile("server/SpringBootApplication.mustache",
                        (testFolder + '/' + apiPackage).replace(".", "/"), "SpringBootApplication.java"));
                writeOptional(outputFolder, new SupportingFile("server/application.properties.mustache",
                        (testResourcesFolder + '/'), "application.properties"));

            }
        }

        if (this.generateNonSpringApplication) {
            writeOptional(outputFolder, new SupportingFile("server/nonspring-web.mustache",
                    ("src/main/webapp/WEB-INF"), "web.xml"));
        }
    }

    @Override
    public String getName() {
        return "jaxrs-cxf";
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
    public String getHelp() {
        return "Generates a Java JAXRS Server application based on Apache CXF framework.";
    }

    public void setGenerateSpringApplication(boolean generateSpringApplication) {
        this.generateSpringApplication = generateSpringApplication;
    }

    public void setUseSpringAnnotationConfig(boolean useSpringAnnotationConfig) {
        this.useSpringAnnotationConfig = useSpringAnnotationConfig;
    }

    public void setUseSwaggerFeature(boolean useSwaggerFeature) {
        this.useSwaggerFeature = useSwaggerFeature;
    }


    public void setUseWadlFeature(boolean useWadlFeature) {
        this.useWadlFeature = useWadlFeature;
    }


    public void setUseMultipartFeature(boolean useMultipartFeature) {
        this.useMultipartFeature = useMultipartFeature;
    }


    public void setUseGzipFeature(boolean useGzipFeature) {
        this.useGzipFeature = useGzipFeature;
    }


    public void setUseLoggingFeature(boolean useLoggingFeature) {
        this.useLoggingFeature = useLoggingFeature;
    }


    public void setUseBeanValidationFeature(boolean useBeanValidationFeature) {
        this.useBeanValidationFeature = useBeanValidationFeature;
    }

    public void setGenerateSpringBootApplication(boolean generateSpringBootApplication) {
        this.generateSpringBootApplication = generateSpringBootApplication;
    }

    public void setGenerateJbossDeploymentDescriptor(boolean generateJbossDeploymentDescriptor) {
        this.generateJbossDeploymentDescriptor = generateJbossDeploymentDescriptor;
    }

    public void setUseGzipFeatureForTests(boolean useGzipFeatureForTests) {
        this.useGzipFeatureForTests = useGzipFeatureForTests;
    }

    public void setUseLoggingFeatureForTests(boolean useLoggingFeatureForTests) {
        this.useLoggingFeatureForTests = useLoggingFeatureForTests;
    }

    public void setUseSwaggerUI(boolean useSwaggerUI) {
        this.useSwaggerUI = useSwaggerUI;
    }

    public void setAddConsumesProducesJson(boolean addConsumesProducesJson) {
        this.addConsumesProducesJson = addConsumesProducesJson;
    }

    public void setUseAnnotatedBasePath(boolean useAnnotatedBasePath) {
        this.useAnnotatedBasePath = useAnnotatedBasePath;
    }

    public void setGenerateNonSpringApplication(boolean generateNonSpringApplication) {
        this.generateNonSpringApplication = generateNonSpringApplication;
    }

    public void setUseGenericResponse(boolean useGenericResponse) {
        this.useGenericResponse = useGenericResponse;
    }

}
