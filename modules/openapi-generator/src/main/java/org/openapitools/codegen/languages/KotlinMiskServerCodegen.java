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

import lombok.Setter;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class KotlinMiskServerCodegen extends AbstractKotlinCodegen implements BeanValidationFeatures {

    public static final String MODULE_CLASS_NAME = "moduleClassName";

    private final Logger LOGGER = LoggerFactory.getLogger(KotlinMiskServerCodegen.class);
    private static final String ROOT_PACKAGE = "rootPackage";

    private boolean useBeanValidation = true;

    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    @Setter protected String moduleClassName = "OpenApiModule";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-misk";
    }

    @Override
    public String getHelp() {
        return "Generates a kotlin-misk server.";
    }

    public KotlinMiskServerCodegen() {
        super();

        addSwitch(USE_BEANVALIDATION, "Use BeanValidation API annotations to validate data types", useBeanValidation);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.PROTOBUF))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        embeddedTemplateDir = templateDir = "kotlin-misk";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + "kotlin-misk";

        addOption(MODULE_CLASS_NAME, "Name of the generated module class", moduleClassName);

        apiTestTemplateFiles.clear();
        apiTestTemplateFiles.put("api_test.mustache", ".kt");

        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        modelDocTemplateFiles.clear();
        modelDocTemplateFiles.put("model_doc.mustache", ".md");

        supportingFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("apiAction.mustache", "Action.kt");
        apiTemplateFiles.put("apiImpl.mustache", "Impl.kt");
        apiTemplateFiles.put("apiInterface.mustache", ".kt");
        modelTemplateFiles.put("model.mustache", ".kt");

        apiPackage = rootPackage + ".api";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-kotlin-misk-server";
        artifactVersion = apiVersion;

        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(ROOT_PACKAGE, rootPackage);

        // Add supporting files
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.kts.mustache", "", "build.gradle.kts"));
        supportingFiles.add(new SupportingFile("settings.gradle.kts.mustache", "", "settings.gradle.kts"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(MODULE_CLASS_NAME)) {
            setModuleClassName((String) additionalProperties.get(MODULE_CLASS_NAME));
        }
        additionalProperties.put(MODULE_CLASS_NAME, moduleClassName);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        applyJakartaPackage();

        String apiModuleFolder = (sourceFolder + File.separator + apiPackage).replace(".", File.separator);
        String moduleFileName = moduleClassName + ".kt";
        supportingFiles.add(new SupportingFile("miskModule.mustache", apiModuleFolder, moduleFileName));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();

        for (CodegenOperation operation : operations) {

            if (operation.hasConsumes) {
                for (Map<String, String> consumes : operation.consumes) {
                    consumes.computeIfPresent("mediaType", (__, mediaType) -> mapMediaType(mediaType));
                }
            }

            if (operation.hasProduces) {
                for (Map<String, String> produces : operation.produces) {
                    produces.computeIfPresent("mediaType", (__, mediaType) -> mapMediaType(mediaType));
                }
            }

            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }

        return objs;
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public boolean getUseBeanValidation() {
        return this.useBeanValidation;
    }

    private String mapMediaType(String mediaType) {
        return MEDIA_MAPPING.getOrDefault(mediaType, "MediaTypes.APPLICATION_OCTETSTREAM /* @todo(unknown) -> " + mediaType + " */ ");
    }

    private final static Map<String, String> MEDIA_MAPPING = getMappings();

    private static Map<String, String> getMappings() {
        // add new values in order
        Map<String, String> result = new HashMap<>();
        result.put("*/*", "MediaTypes.ALL");

        result.put("application/grpc", "MediaTypes.APPLICATION_GRPC");
        result.put("application/javascript", "MediaTypes.APPLICATION_JAVASCRIPT");
        result.put("application/json", "MediaTypes.APPLICATION_JSON");
        result.put("application/octetstream", "MediaTypes.APPLICATION_OCTETSTREAM");
        result.put("application/pdf", "MediaTypes.APPLICATION_OCTETSTREAM");
        result.put("application/x-protobuf", "MediaTypes.APPLICATION_PROTOBUF");
        result.put("application/x-www-form-urlencoded", "MediaTypes.APPLICATION_FORM_URLENCODED");
        result.put("application/xml", "MediaTypes.APPLICATION_XML");
        result.put("application/zip", "MediaTypes.APPLICATION_ZIP");

        result.put("image/gif", "MediaTypes.IMAGE_GIF");
        result.put("image/jpeg", "MediaTypes.IMAGE_JPEG");
        result.put("image/png", "MediaTypes.IMAGE_PNG");
        result.put("image/svg+xml", "MediaTypes.IMAGE_SVG");
        result.put("image/x-icon", "MediaTypes.IMAGE_ICO");

        result.put("multipart/form-data", "MediaTypes.FORM_DATA");

        result.put("text/css", "MediaTypes.TEXT_CSS");
        result.put("text/html", "MediaTypes.TEXT_HTML");
        result.put("text/plain", "MediaTypes.TEXT_PLAIN_UTF8");

        return result;
    }
}
