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

import org.apache.commons.lang3.BooleanUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.JbossFeature;
import org.openapitools.codegen.languages.features.SwaggerFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JavaResteasyEapServerCodegen extends AbstractJavaJAXRSServerCodegen
        implements JbossFeature, BeanValidationFeatures, SwaggerFeatures {

    protected boolean generateJbossDeploymentDescriptor = true;
    protected boolean useSwaggerFeature = false;

    public JavaResteasyEapServerCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        artifactId = "openapi-jaxrs-resteasy-eap-server";
        useBeanValidation = true;
        outputFolder = "generated-code/JavaJaxRS-Resteasy-eap";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());

        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        apiTestTemplateFiles.clear(); // TODO: add test template

        // clear model and api doc template as AbstractJavaJAXRSServerCodegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        embeddedTemplateDir = templateDir = "JavaJaxRS" + File.separator + "resteasy" + File.separator + "eap";

        cliOptions.add(CliOption.newBoolean(GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR, "Generate Jboss Deployment Descriptor",generateJbossDeploymentDescriptor));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_FEATURE, "Use dynamic Swagger generator",useSwaggerFeature));

    }

    @Override
    public String getName() {
        return "jaxrs-resteasy-eap";
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS-Resteasy Server application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR)) {
            boolean generateJbossDeploymentDescriptorProp = convertPropertyToBooleanAndWriteBack(GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR);
            this.setGenerateJbossDeploymentDescriptor(generateJbossDeploymentDescriptorProp);
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(USE_SWAGGER_FEATURE)) {
            this.setUseSwaggerFeature(convertPropertyToBoolean(USE_SWAGGER_FEATURE));
        }

        writePropertyBack(USE_SWAGGER_FEATURE, useSwaggerFeature);

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("gradle.mustache", "", "build.gradle")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("settingsGradle.mustache", "", "settings.gradle")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("web.mustache", ("src/main/webapp/WEB-INF"), "web.xml")
                .doNotOverwrite());

        supportingFiles.add(new SupportingFile("JacksonConfig.mustache", (projectFolder + File.separator + "java" + '/' + invokerPackage).replace(".", "/"), "JacksonConfig.java"));

        if (generateJbossDeploymentDescriptor) {
            supportingFiles.add(new SupportingFile("jboss-web.mustache", ("src/main/webapp/WEB-INF"), "jboss-web.xml")
                .doNotOverwrite());
        }

        supportingFiles.add(new SupportingFile("RestApplication.mustache", (projectFolder + File.separator + "java" + '/' + invokerPackage).replace(".", "/"), "RestApplication.java")
            .doNotOverwrite());

    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        // Add imports for Jackson
        if (!BooleanUtils.toBoolean(model.isEnum)) {
            model.imports.add("JsonProperty");

            if (BooleanUtils.toBoolean(model.hasEnums)) {
                model.imports.add("JsonValue");
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        // Add imports for Jackson
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            // for enum model
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                cm.imports.add(importMapping.get("JsonValue"));
                Map<String, String> item = new HashMap<String, String>();
                item.put("import", importMapping.get("JsonValue"));
                imports.add(item);
            }
        }

        return objs;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public void setGenerateJbossDeploymentDescriptor(boolean generateJbossDeploymentDescriptor) {
        this.generateJbossDeploymentDescriptor = generateJbossDeploymentDescriptor;
    }

    public void setUseSwaggerFeature(boolean useSwaggerFeature) {
        this.useSwaggerFeature = useSwaggerFeature;
    }
}
