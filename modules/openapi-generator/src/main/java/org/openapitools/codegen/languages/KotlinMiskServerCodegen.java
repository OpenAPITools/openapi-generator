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
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class KotlinMiskServerCodegen extends AbstractKotlinCodegen implements BeanValidationFeatures {

    private final Logger LOGGER = LoggerFactory.getLogger(KotlinMiskServerCodegen.class);

    public static final String MODULE_CLASS_NAME = "moduleClassName";
    public static final String ACTION_PATH_PREFIX = "actionPathPrefix";

    private static final String ROOT_PACKAGE = "rootPackage";
    public static final String GENERATE_STUB_IMPL_CLASSES = "generateStubImplClasses";
    public static final String ADD_MODEL_MOSHI_JSON_ANNOTATION = "addModelMoshiJsonAnnotation";
    public static final String ACTION_ANNOTATIONS = "actionAnnotations";
    public static final String ACTION_IMPORTS = "actionImports";
    public static final String ACTION_PARENT_CLASS = "actionParentClass";
    public static final String ACTION_REQUEST_CONTENT_TYPE = "actionRequestContentType";
    public static final String ACTION_REQUEST_CONTENT_TYPE_PREFIX = "actionRequestContentTypePrefix";
    public static final String TESTING_MODULE = "testingModule";
    private static final String TESTING_MODULE_NAME = "testingModuleName";

    private boolean useBeanValidation = true;

    @Setter
    private boolean generateStubImplClasses = false;

    @Setter
    private boolean addModelMoshiJsonAnnotation = true;

    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    @Setter protected String moduleClassName = "OpenApiModule";
    @Setter protected String actionPathPrefix = "";
    @Setter protected List<String> actionAnnotations =
            List.of("@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)");
    @Setter protected List<String> actionImports =
            List.of("misk.web.actions.WebAction","misk.web.interceptors.LogRequestResponse");
    @Setter protected String actionParentClass = "WebAction";
    @Setter protected String actionRequestContentType = "@RequestContentType";
    @Setter protected String actionRequestContentTypePrefix = "MediaTypes";
    @Setter protected String testingModule = "misk.testing.MiskTestModule";
    @Setter protected String testingModuleName;

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
        addSwitch(GENERATE_STUB_IMPL_CLASSES, "Generate Stub Impl Classes", generateStubImplClasses);
        addSwitch(ADD_MODEL_MOSHI_JSON_ANNOTATION, "Add a Moshi JSON adapter annotation to all model classes", addModelMoshiJsonAnnotation);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.PROTOBUF))
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
        addOption(ACTION_PATH_PREFIX, "Prefix for action path", actionPathPrefix);
        addOption(ACTION_IMPORTS, "String Imports for Actions separated by a semicolon(;)", String.join(";", actionImports));
        addOption(ACTION_ANNOTATIONS, "String Annotations for Actions separated by a semicolon(;)", String.join(";", actionAnnotations));
        addOption(ACTION_PARENT_CLASS, "Parent Class for Action", actionParentClass);
        addOption(ACTION_REQUEST_CONTENT_TYPE, "Request ContentType for Action", actionRequestContentType);
        addOption(ACTION_REQUEST_CONTENT_TYPE_PREFIX, "Request ContentType Prefix for Action", actionRequestContentTypePrefix);
        addOption(TESTING_MODULE, "Testing module class", testingModule);

        apiTestTemplateFiles.clear();
        apiTestTemplateFiles.put("api_test.mustache", ".kt");

        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        modelDocTemplateFiles.clear();
        modelDocTemplateFiles.put("model_doc.mustache", ".md");

        supportingFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("apiAction.mustache", "Action.kt");

        if (generateStubImplClasses) {
            apiTemplateFiles.put("apiImpl.mustache", "Impl.kt");
            apiTemplateFiles.put("apiInterface.mustache", ".kt");
        }

        modelTemplateFiles.put("model.mustache", ".kt");

        apiPackage = rootPackage + ".api";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-kotlin-misk-server";
        artifactVersion = apiVersion;

        typeMapping.put("array", "kotlin.collections.List");

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

        if (additionalProperties.containsKey(ACTION_ANNOTATIONS)) {
            convertPropertyToTypeAndWriteBack(ACTION_ANNOTATIONS,
                    it -> Arrays.asList(it.split(";")), this::setActionAnnotations);
        }
        writePropertyBack(ACTION_ANNOTATIONS, actionAnnotations);

        if (additionalProperties.containsKey(ACTION_IMPORTS)) {
            convertPropertyToTypeAndWriteBack(ACTION_IMPORTS,
                    it -> Arrays.asList(it.split(";")), this::setActionImports);
        }
        writePropertyBack(ACTION_IMPORTS, actionImports);

        if (additionalProperties.containsKey(ACTION_PARENT_CLASS)) {
            setActionParentClass((String) additionalProperties.get(ACTION_PARENT_CLASS));
        }
        writePropertyBack(ACTION_PARENT_CLASS, actionParentClass);

        if (additionalProperties.containsKey(MODULE_CLASS_NAME)) {
            setModuleClassName((String) additionalProperties.get(MODULE_CLASS_NAME));
        }
        writePropertyBack(MODULE_CLASS_NAME, moduleClassName);

        if (additionalProperties.containsKey(ACTION_PATH_PREFIX)) {
            setActionPathPrefix((String) additionalProperties.get(ACTION_PATH_PREFIX));
        }
        writePropertyBack(ACTION_PATH_PREFIX, actionPathPrefix);

        if (additionalProperties.containsKey(ACTION_REQUEST_CONTENT_TYPE)) {
            setActionRequestContentType((String) additionalProperties.get(ACTION_REQUEST_CONTENT_TYPE));
        }
        writePropertyBack(ACTION_REQUEST_CONTENT_TYPE, actionRequestContentType);

        if (additionalProperties.containsKey(ACTION_REQUEST_CONTENT_TYPE_PREFIX)) {
            setActionRequestContentTypePrefix((String) additionalProperties.get(ACTION_REQUEST_CONTENT_TYPE_PREFIX));
        }
        writePropertyBack(ACTION_REQUEST_CONTENT_TYPE_PREFIX, actionRequestContentTypePrefix);

        if (additionalProperties.containsKey(TESTING_MODULE)) {
            setTestingModule((String) additionalProperties.get(TESTING_MODULE));
        }
        writePropertyBack(TESTING_MODULE, testingModule);

        writePropertyBack(TESTING_MODULE_NAME, getTestingModuleName());

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(GENERATE_STUB_IMPL_CLASSES)) {
            setGenerateStubImplClasses(convertPropertyToBoolean(GENERATE_STUB_IMPL_CLASSES));
        }
        writePropertyBack(GENERATE_STUB_IMPL_CLASSES, generateStubImplClasses);

        if (additionalProperties.containsKey(ADD_MODEL_MOSHI_JSON_ANNOTATION)) {
            setAddModelMoshiJsonAnnotation(convertPropertyToBoolean(ADD_MODEL_MOSHI_JSON_ANNOTATION));
        }
        writePropertyBack(ADD_MODEL_MOSHI_JSON_ANNOTATION, addModelMoshiJsonAnnotation);

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
        return MEDIA_MAPPING.getOrDefault(mediaType, "APPLICATION_OCTETSTREAM /* @todo(unknown) -> " + mediaType + " */ ");
    }

    public String getTestingModuleName() {
        return testingModule.substring(testingModule.lastIndexOf(".")+1);
    }

    private final static Map<String, String> MEDIA_MAPPING = getMappings();

    private static Map<String, String> getMappings() {
        // add new values in order
        Map<String, String> result = new HashMap<>();
        result.put("*/*", "ALL");

        result.put("application/grpc", "APPLICATION_GRPC");
        result.put("application/javascript", "APPLICATION_JAVASCRIPT");
        result.put("application/json", "APPLICATION_JSON");
        result.put("application/jwt", "APPLICATION_JWT");
        result.put("application/octetstream", "APPLICATION_OCTETSTREAM");
        result.put("application/pdf", "APPLICATION_OCTETSTREAM");
        result.put("application/x-protobuf", "APPLICATION_PROTOBUF");
        result.put("application/x-www-form-urlencoded", "APPLICATION_FORM_URLENCODED");
        result.put("application/xml", "APPLICATION_XML");
        result.put("application/zip", "APPLICATION_ZIP");

        result.put("image/gif", "IMAGE_GIF");
        result.put("image/x-icon", "IMAGE_ICO");
        result.put("image/jpeg", "IMAGE_JPEG");
        result.put("image/png", "IMAGE_PNG");
        result.put("image/svg+xml", "IMAGE_SVG");
        result.put("image/tiff", "IMAGE_TIFF");

        result.put("multipart/form-data", "FORM_DATA");

        result.put("text/css", "TEXT_CSS");
        result.put("text/html", "TEXT_HTML");
        result.put("text/plain", "TEXT_PLAIN_UTF8");

        return result;
    }
}