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
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.OptionalFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JavaCamelServerCodegen extends SpringCodegen implements BeanValidationFeatures, PerformBeanValidationFeatures, OptionalFeatures {
    private static final String APPLICATION_JSON = "application/json";
    private static final String APPLICATION_XML = "application/xml";

    public static final String PROJECT_NAME = "projectName";
    public static final String CAMEL_REST_COMPONENT = "camelRestComponent";
    public static final String CAMEL_REST_BINDING_MODE = "camelRestBindingMode";
    public static final String CAMEL_REST_CLIENT_REQUEST_VALIDATION = "camelRestClientRequestValidation";
    public static final String CAMEL_USE_DEFAULT_VALIDATION_ERROR_PROCESSOR = "camelUseDefaultValidationtErrorProcessor";
    public static final String CAMEL_VALIDATION_ERROR_PROCESSOR = "camelValidationErrorProcessor";
    public static final String CAMEL_SECURITY_DEFINITIONS = "camelSecurityDefinitions";
    public static final String CAMEL_DATAFORMAT_PROPERTIES = "camelDataformatProperties";

    private String camelRestComponent = "servlet";
    private String camelRestBindingMode = "auto";
    private boolean camelRestClientRequestValidation = false;
    private boolean camelUseDefaultValidationtErrorProcessor = true;
    private String camelValidationErrorProcessor = "validationErrorProcessor";
    private boolean camelSecurityDefinitions = true;
    private String camelDataformatProperties = "";

    private final Logger LOGGER = LoggerFactory.getLogger(JavaCamelServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-camel";
    }

    public String getHelp() {
        return "Generates a Java Camel server (beta).";
    }

    public JavaCamelServerCodegen() {
        super();
        templateDir = "java-camel-server";
        addCliOptions();
        artifactId = "openapi-camel";
        super.library = "";
    }

    @Override
    public void processOpts() {
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        if (!additionalProperties.containsKey(DATE_LIBRARY)) {
            additionalProperties.put(DATE_LIBRARY, "legacy");
        }
        super.processOpts();
        super.apiTemplateFiles.remove("apiController.mustache");
        LOGGER.info("***** Java Apache Camel Server Generator *****");
        supportingFiles.clear();
        manageAdditionalProperties();

        Map<String, String> dataFormatProperties = new HashMap<>();
        if (!"off".equals(camelRestBindingMode)) {
            Arrays.stream(camelDataformatProperties.split(",")).forEach(property -> {
                String[] dataFormatProperty = property.split("=");
                if (dataFormatProperty.length == 2) {
                    dataFormatProperties.put(dataFormatProperty[0].trim(), dataFormatProperty[1].trim());
                }
            });
        }
        additionalProperties.put(CAMEL_DATAFORMAT_PROPERTIES, dataFormatProperties.entrySet());

        supportingFiles.add(new SupportingFile("restConfiguration.mustache",
                (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                "RestConfiguration.java"));
        if (performBeanValidation) {
            apiTemplateFiles.put("validation.mustache", "Validator.java");
            if (camelUseDefaultValidationtErrorProcessor) {
                supportingFiles.add(new SupportingFile("errorProcessor.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "ValidationErrorProcessor.java"));
            }
        }
        if (SPRING_BOOT.equals(library)) {
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
            supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                    (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                    "OpenApiGeneratorApplication.java"));

            if (!interfaceOnly) {
                apiTemplateFiles.put("routesImpl.mustache", "RoutesImpl.java");
            }

            supportingFiles.add(new SupportingFile("application.mustache",
                    ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
            supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                    (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                    "RFC3339DateFormat.java"));
            apiTestTemplateFiles.put("test.mustache", ".java");
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        boolean bindingModeOff = false;
        if (co.hasProduces) {
            for (Map<String, String> produces : co.produces) {
                String mediaType = produces.get("mediaType");
                if (!APPLICATION_JSON.equals(mediaType) && !APPLICATION_XML.equals(mediaType)) {
                    bindingModeOff = true;
                }
                if (APPLICATION_JSON.equals(mediaType)) {
                    produces.put("isJson", "true");
                }
                if (APPLICATION_XML.equals(mediaType)) {
                    produces.put("isXml", "true");
                }
            }
        }
        if (co.hasConsumes) {
            for (Map<String, String> consumes : co.consumes) {
                String mediaType = consumes.get("mediaType");
                if (!APPLICATION_JSON.equals(mediaType) && !APPLICATION_XML.equals(mediaType)) {
                    bindingModeOff = true;
                }
                if (APPLICATION_JSON.equals(mediaType)) {
                    consumes.put("isJson", "true");
                }
                if (APPLICATION_XML.equals(mediaType)) {
                    consumes.put("isXml", "true");
                }
            }
        }
        co.vendorExtensions.put(CAMEL_REST_BINDING_MODE, bindingModeOff);
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
    }

    private void addCliOptions() {
        cliOptions.add(new CliOption(CAMEL_REST_COMPONENT, "name of the Camel component to use as the REST consumer").defaultValue(camelRestComponent));
        cliOptions.add(new CliOption(CAMEL_REST_BINDING_MODE, "binding mode to be used by the REST consumer").defaultValue(camelRestBindingMode));
        cliOptions.add(CliOption.newBoolean(CAMEL_REST_CLIENT_REQUEST_VALIDATION, "enable validation of the client request to check whether the Content-Type and Accept headers from the client is supported by the Rest-DSL configuration", camelRestClientRequestValidation));
        cliOptions.add(CliOption.newBoolean(CAMEL_USE_DEFAULT_VALIDATION_ERROR_PROCESSOR, "generate default validation error processor", camelUseDefaultValidationtErrorProcessor));
        cliOptions.add(new CliOption(CAMEL_VALIDATION_ERROR_PROCESSOR, "validation error processor bean name").defaultValue(camelValidationErrorProcessor));
        cliOptions.add(CliOption.newBoolean(CAMEL_SECURITY_DEFINITIONS, "generate camel security definitions", camelSecurityDefinitions));
        cliOptions.add(new CliOption(CAMEL_DATAFORMAT_PROPERTIES, "list of dataformat properties separated by comma (propertyName1=propertyValue2,...").defaultValue(camelDataformatProperties));
    }

    private void manageAdditionalProperties() {
        camelRestComponent = manageAdditionalProperty(CAMEL_REST_COMPONENT, camelRestComponent);
        camelRestBindingMode = manageAdditionalProperty(CAMEL_REST_BINDING_MODE, camelRestBindingMode);
        camelRestClientRequestValidation = manageAdditionalProperty(CAMEL_REST_CLIENT_REQUEST_VALIDATION, camelRestClientRequestValidation);
        camelUseDefaultValidationtErrorProcessor = manageAdditionalProperty(CAMEL_USE_DEFAULT_VALIDATION_ERROR_PROCESSOR, camelUseDefaultValidationtErrorProcessor);
        camelValidationErrorProcessor = manageAdditionalProperty(CAMEL_VALIDATION_ERROR_PROCESSOR, camelValidationErrorProcessor);
        camelSecurityDefinitions = manageAdditionalProperty(CAMEL_SECURITY_DEFINITIONS, camelSecurityDefinitions);
        camelDataformatProperties = manageAdditionalProperty(CAMEL_DATAFORMAT_PROPERTIES, camelDataformatProperties);
    }

    private <T> T manageAdditionalProperty(String propertyName, T defaultValue) {
        if (additionalProperties.containsKey(propertyName)) {
            Object propertyValue = additionalProperties.get(propertyName);
            if (defaultValue instanceof Boolean && !(propertyValue instanceof Boolean)) {
                return (T) manageBooleanAdditionalProperty((String) propertyValue);
            }
            return (T) additionalProperties.get(propertyName);
        }
        additionalProperties.put(propertyName, defaultValue);
        return defaultValue;
    }

    private Boolean manageBooleanAdditionalProperty(String propertyValue) {
        return Boolean.parseBoolean(propertyValue);
    }
}
