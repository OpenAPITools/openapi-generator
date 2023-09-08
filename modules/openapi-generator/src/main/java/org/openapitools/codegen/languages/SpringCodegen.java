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

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenSecurity;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.VendorExtension;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.openapitools.codegen.languages.features.OptionalFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.languages.features.SwaggerUIFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;

public class SpringCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, OptionalFeatures, SwaggerUIFeatures {
    private final Logger LOGGER = LoggerFactory.getLogger(SpringCodegen.class);


    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String INTERFACE_ONLY = "interfaceOnly";
    public static final String USE_FEIGN_CLIENT_URL = "useFeignClientUrl";
    public static final String USE_FEIGN_CLIENT = "useFeignClient";
    public static final String DELEGATE_PATTERN = "delegatePattern";
    public static final String SINGLE_CONTENT_TYPES = "singleContentTypes";
    public static final String VIRTUAL_SERVICE = "virtualService";
    public static final String SKIP_DEFAULT_INTERFACE = "skipDefaultInterface";
    public static final String GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS = "generatedConstructorWithRequiredArgs";

    public static final String RESOURCE_FOLDER = "resourceFolder";
    public static final String RESOURCE_FOLDER_DESC = "resource folder for generated resources";

    public static final String ASYNC = "async";
    public static final String REACTIVE = "reactive";
    public static final String RESPONSE_WRAPPER = "responseWrapper";
    public static final String USE_TAGS = "useTags";
    public static final String SPRING_BOOT = "spring-boot";
    public static final String SPRING_CLOUD_LIBRARY = "spring-cloud";
    public static final String SPRING_HTTP_INTERFACE = "spring-http-interface";
    public static final String API_FIRST = "apiFirst";
    public static final String SPRING_CONTROLLER = "useSpringController";
    public static final String HATEOAS = "hateoas";
    public static final String RETURN_SUCCESS_CODE = "returnSuccessCode";
    public static final String UNHANDLED_EXCEPTION_HANDLING = "unhandledException";
    public static final String USE_RESPONSE_ENTITY = "useResponseEntity";
    public static final String USE_ENUM_CASE_INSENSITIVE = "useEnumCaseInsensitive";
    public static final String USE_SPRING_BOOT3 = "useSpringBoot3";
    public static final String REQUEST_MAPPING_OPTION = "requestMappingMode";
    public static final String USE_REQUEST_MAPPING_ON_CONTROLLER = "useRequestMappingOnController";
    public static final String USE_REQUEST_MAPPING_ON_INTERFACE = "useRequestMappingOnInterface";

    public enum RequestMappingMode {
        api_interface("Generate the @RequestMapping annotation on the generated Api Interface."),
        controller("Generate the @RequestMapping annotation on the generated Api Controller Implementation."),
        none("Do not add a class level @RequestMapping annotation.");

        public String getDescription() {
            return description;
        }

        private String description;

        RequestMappingMode(String description) {
            this.description = description;
        }
    }

    public static final String OPEN_BRACE = "{";
    public static final String CLOSE_BRACE = "}";

    protected String title = "OpenAPI Spring";
    protected String configPackage = "org.openapitools.configuration";
    protected String basePackage = "org.openapitools";
    protected String resourceFolder = projectFolder + "/resources";

    protected boolean interfaceOnly = false;
    protected boolean useFeignClientUrl = true;
    protected boolean delegatePattern = false;
    protected boolean delegateMethod = false;
    protected boolean singleContentTypes = false;
    protected boolean async = false;
    protected boolean reactive = false;
    protected String responseWrapper = "";
    protected boolean skipDefaultInterface = false;
    protected boolean useTags = false;
    protected boolean useBeanValidation = true;
    protected boolean performBeanValidation = false;
    protected boolean apiFirst = false;
    protected boolean useOptional = false;
    protected boolean virtualService = false;
    protected boolean hateoas = false;
    protected boolean returnSuccessCode = false;
    protected boolean unhandledException = false;
    protected boolean useSpringController = false;
    protected boolean useSwaggerUI = true;
    protected boolean useResponseEntity = true;
    protected boolean useEnumCaseInsensitive = false;
    protected boolean useSpringBoot3 = false;
    protected boolean generatedConstructorWithRequiredArgs = true;
    protected RequestMappingMode requestMappingMode = RequestMappingMode.controller;

    public SpringCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(SecurityFeature.OAuth2_Implicit, SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials, SecurityFeature.OAuth2_Password,
                        SecurityFeature.ApiKey, SecurityFeature.BasicAuth))
                .excludeGlobalFeatures(GlobalFeature.Callbacks, GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling)
                .includeGlobalFeatures(GlobalFeature.XMLStructureDefinitions)
                .includeSchemaSupportFeatures(SchemaSupportFeature.Polymorphism)
                .excludeParameterFeatures(ParameterFeature.Cookie));

        outputFolder = "generated-code/javaSpring";
        embeddedTemplateDir = templateDir = "JavaSpring";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-spring";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        apiTestTemplateFiles.clear(); // TODO: add test template

        // spring uses the jackson lib
        additionalProperties.put(JACKSON, "true");
        additionalProperties.put("openbrace", OPEN_BRACE);
        additionalProperties.put("closebrace", CLOSE_BRACE);

        cliOptions.add(new CliOption(TITLE, "server title name or client service name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code")
                .defaultValue(this.getConfigPackage()));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package (invokerPackage) for generated code")
                .defaultValue(this.getBasePackage()));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY,
                "Whether to generate only API interface stubs without the server files.", interfaceOnly));
        cliOptions.add(CliOption.newBoolean(USE_FEIGN_CLIENT_URL,
                "Whether to generate Feign client with url parameter.", useFeignClientUrl));
        cliOptions.add(CliOption.newBoolean(DELEGATE_PATTERN,
                "Whether to generate the server files using the delegate pattern", delegatePattern));
        cliOptions.add(CliOption.newBoolean(SINGLE_CONTENT_TYPES,
                "Whether to select only one produces/consumes content-type by operation.", singleContentTypes));
        cliOptions.add(CliOption.newBoolean(SKIP_DEFAULT_INTERFACE,
                "Whether to skip generation of default implementations for java8 interfaces", skipDefaultInterface));
        cliOptions.add(CliOption.newBoolean(ASYNC, "use async Callable controllers", async));
        cliOptions.add(CliOption.newBoolean(REACTIVE, "wrap responses in Mono/Flux Reactor types (spring-boot only)",
                reactive));
        cliOptions.add(new CliOption(RESPONSE_WRAPPER,
                "wrap the responses in given type (Future, Callable, CompletableFuture,ListenableFuture, DeferredResult, RxObservable, RxSingle or fully qualified type)"));
        cliOptions.add(CliOption.newBoolean(VIRTUAL_SERVICE,
                "Generates the virtual service. For more details refer - https://github.com/virtualansoftware/virtualan/wiki"));
        cliOptions.add(
                CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames", useTags));
        cliOptions
                .add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION,
                "Use Bean Validation Impl. to perform BeanValidation", performBeanValidation));
        cliOptions.add(CliOption.newBoolean(API_FIRST,
                "Generate the API from the OAI spec at server compile time (API first approach)", apiFirst));
        cliOptions
                .add(CliOption.newBoolean(USE_OPTIONAL, "Use Optional container for optional parameters", useOptional));
        cliOptions.add(
                CliOption.newBoolean(HATEOAS, "Use Spring HATEOAS library to allow adding HATEOAS links", hateoas));
        cliOptions
                .add(CliOption.newBoolean(RETURN_SUCCESS_CODE, "Generated server returns 2xx code", returnSuccessCode));
        cliOptions.add(CliOption.newBoolean(SPRING_CONTROLLER, "Annotate the generated API as a Spring Controller", useSpringController));

        CliOption requestMappingOpt = new CliOption(REQUEST_MAPPING_OPTION,
            "Where to generate the class level @RequestMapping annotation.")
            .defaultValue(requestMappingMode.name());
        for (RequestMappingMode mode: RequestMappingMode.values()) {
            requestMappingOpt.addEnum(mode.name(), mode.getDescription());
        }
        cliOptions.add(requestMappingOpt);

        cliOptions.add(CliOption.newBoolean(UNHANDLED_EXCEPTION_HANDLING,
                "Declare operation methods to throw a generic exception and allow unhandled exceptions (useful for Spring `@ControllerAdvice` directives).",
                unhandledException));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_UI,
            "Open the OpenApi specification in swagger-ui. Will also import and configure needed dependencies",
            useSwaggerUI));
        cliOptions.add(CliOption.newBoolean(USE_RESPONSE_ENTITY,
                "Use the `ResponseEntity` type to wrap return values of generated API methods. "
                    + "If disabled, method are annotated using a `@ResponseStatus` annotation, which has the status of the first response declared in the Api definition",
                useResponseEntity));
        cliOptions.add(CliOption.newBoolean(USE_ENUM_CASE_INSENSITIVE,
                "Use `equalsIgnoreCase` when String for enum comparison",
                useEnumCaseInsensitive));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_BOOT3,
            "Generate code and provide dependencies for use with Spring Boot 3.x. (Use jakarta instead of javax in imports). Enabling this option will also enable `useJakartaEe`.",
            useSpringBoot3));
        cliOptions.add(CliOption.newBoolean(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS,
            "Whether to generate constructors with required args for models",
            generatedConstructorWithRequiredArgs));
        cliOptions.add(new CliOption(RESOURCE_FOLDER, RESOURCE_FOLDER_DESC).defaultValue(this.getResourceFolder()));

        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application.");
        supportedLibraries.put(SPRING_CLOUD_LIBRARY,
            "Spring-Cloud-Feign client with Spring-Boot auto-configured settings.");
        supportedLibraries.put(SPRING_HTTP_INTERFACE, "Spring 6 HTTP interfaces (testing)");
        setLibrary(SPRING_BOOT);
        final CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC)
                .defaultValue(SPRING_BOOT);
        library.setEnum(supportedLibraries);
        cliOptions.add(library);

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "spring";
    }

    @Override
    public String getHelp() {
        return "Generates a Java SpringBoot Server application using the SpringDoc integration.";
    }

    @Override
    public DocumentationProvider defaultDocumentationProvider() {
        return isLibrary(SPRING_HTTP_INTERFACE) ? null : DocumentationProvider.SPRINGDOC;
    }

    public List<DocumentationProvider> supportedDocumentationProvider() {
        List<DocumentationProvider> supportedProviders = new ArrayList<>();
        supportedProviders.add(DocumentationProvider.NONE);
        supportedProviders.add(DocumentationProvider.SOURCE);
        supportedProviders.add(DocumentationProvider.SPRINGFOX);
        supportedProviders.add(DocumentationProvider.SPRINGDOC);
        return supportedProviders;
    }

    @Override
    public List<AnnotationLibrary> supportedAnnotationLibraries() {
        List<AnnotationLibrary> supportedLibraries = new ArrayList<>();
        supportedLibraries.add(AnnotationLibrary.NONE);
        supportedLibraries.add(AnnotationLibrary.SWAGGER1);
        supportedLibraries.add(AnnotationLibrary.SWAGGER2);
        return supportedLibraries;
    }

    /**
     * Whether the selected {@link DocumentationProviderFeatures.DocumentationProvider} requires us to bootstrap and
     * configure swagger-ui by ourselves. Springdoc, for example ships its own swagger-ui integration.
     *
     * @return true if the selected DocumentationProvider requires us to bootstrap swagger-ui.
     */
    private boolean selectedDocumentationProviderRequiresSwaggerUiBootstrap() {
        return getDocumentationProvider().equals(DocumentationProvider.SPRINGFOX) || getDocumentationProvider().equals(DocumentationProvider.SOURCE);
    }

    @Override
    public void processOpts() {
        final List<Pair<String, String>> configOptions = additionalProperties.entrySet().stream()
                .filter(e -> !Arrays.asList(API_FIRST, "hideGenerationTimestamp").contains(e.getKey()))
                .filter(e -> cliOptions.stream().map(CliOption::getOpt).anyMatch(opt -> opt.equals(e.getKey())))
                .map(e -> Pair.of(e.getKey(), e.getValue().toString())).collect(Collectors.toList());
        additionalProperties.put("configOptions", configOptions);

        // TODO remove "file" from reserved word list as feign client doesn't support using `baseName`
        // as the parameter name yet
        reservedWords.remove("file");

        // Process java8 option before common java ones to change the default
        // dateLibrary to java8.
        LOGGER.info("----------------------------------");
        if (!additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary("java8");
        }

        if (!additionalProperties.containsKey(BASE_PACKAGE)
                && additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            // set invokerPackage as basePackage:
            this.setBasePackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            additionalProperties.put(BASE_PACKAGE, basePackage);
            LOGGER.info("Set base package to invoker package ({})", basePackage);
        }

        if (additionalProperties.containsKey(REQUEST_MAPPING_OPTION)) {
            RequestMappingMode optValue = RequestMappingMode.valueOf(
                String.valueOf(additionalProperties.get(REQUEST_MAPPING_OPTION)));
            setRequestMappingMode(optValue);
            additionalProperties.remove(REQUEST_MAPPING_OPTION);
        }

        useOneOfInterfaces = true;
        legacyDiscriminatorBehavior = false;

        // Please refrain from updating values of Config Options after super.ProcessOpts() is called
        super.processOpts();

        if (SPRING_HTTP_INTERFACE.equals(library)) {
            documentationProvider = DocumentationProvider.NONE;
            annotationLibrary = AnnotationLibrary.NONE;
            useJakartaEe=true;
            useBeanValidation = false;
            performBeanValidation = false;

            additionalProperties.put(USE_JAKARTA_EE, useJakartaEe);
            additionalProperties.put(USE_BEANVALIDATION, useBeanValidation);
            additionalProperties.put(PERFORM_BEANVALIDATION, performBeanValidation);
            additionalProperties.put(DOCUMENTATION_PROVIDER, documentationProvider.toCliOptValue());
            additionalProperties.put(documentationProvider.getPropertyName(), true);
            additionalProperties.put(ANNOTATION_LIBRARY, annotationLibrary.toCliOptValue());
            additionalProperties.put(annotationLibrary.getPropertyName(), true);

            applyJakartaPackage();

            LOGGER.warn("For Spring HTTP Interface following options are disabled: documentProvider, annotationLibrary, useBeanValidation, performBeanValidation. "
                + "useJakartaEe defaulted to 'true'");
        }

        if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
            LOGGER.warn("The springfox documentation provider is deprecated for removal. Use the springdoc provider instead.");
        }

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        // TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        if (additionalProperties.containsKey(TITLE)) {
            this.setTitle((String) additionalProperties.get(TITLE));
        }

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        } else {
            additionalProperties.put(CONFIG_PACKAGE, configPackage);
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        } else {
            additionalProperties.put(BASE_PACKAGE, basePackage);
        }

        if (additionalProperties.containsKey(VIRTUAL_SERVICE)) {
            this.setVirtualService(Boolean.parseBoolean(additionalProperties.get(VIRTUAL_SERVICE).toString()));
        }

        if (additionalProperties.containsKey(INTERFACE_ONLY)) {
            this.setInterfaceOnly(Boolean.parseBoolean(additionalProperties.get(INTERFACE_ONLY).toString()));
        }

        if (additionalProperties.containsKey(USE_FEIGN_CLIENT_URL)) {
            this.setUseFeignClientUrl(Boolean.parseBoolean(additionalProperties.get(USE_FEIGN_CLIENT_URL).toString()));
        }
        writePropertyBack(USE_FEIGN_CLIENT_URL, useFeignClientUrl);

        if (additionalProperties.containsKey(DELEGATE_PATTERN)) {
            this.setDelegatePattern(Boolean.parseBoolean(additionalProperties.get(DELEGATE_PATTERN).toString()));
        }

        if (additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
            this.setSingleContentTypes(Boolean.parseBoolean(additionalProperties.get(SINGLE_CONTENT_TYPES).toString()));
        }

        if (additionalProperties.containsKey(SKIP_DEFAULT_INTERFACE)) {
            this.setSkipDefaultInterface(
                    Boolean.parseBoolean(additionalProperties.get(SKIP_DEFAULT_INTERFACE).toString()));
        }

        if (additionalProperties.containsKey(ASYNC)) {
            this.setAsync(Boolean.parseBoolean(additionalProperties.get(ASYNC).toString()));
            // fix for issue/1164
            convertPropertyToBooleanAndWriteBack(ASYNC);
        }

        if (additionalProperties.containsKey(REACTIVE)) {
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                throw new IllegalArgumentException("Currently, reactive option doesn't supported by Spring Cloud");
            }
            this.setReactive(Boolean.parseBoolean(additionalProperties.get(REACTIVE).toString()));
        }

        if (additionalProperties.containsKey(RESPONSE_WRAPPER)) {
            this.setResponseWrapper((String) additionalProperties.get(RESPONSE_WRAPPER));
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            this.setUseTags(Boolean.parseBoolean(additionalProperties.get(USE_TAGS).toString()));
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(PERFORM_BEANVALIDATION)) {
            this.setPerformBeanValidation(convertPropertyToBoolean(PERFORM_BEANVALIDATION));
        }
        writePropertyBack(PERFORM_BEANVALIDATION, performBeanValidation);

        if (additionalProperties.containsKey(USE_OPTIONAL)) {
            this.setUseOptional(convertPropertyToBoolean(USE_OPTIONAL));
        }

        if (additionalProperties.containsKey(API_FIRST)) {
            this.setApiFirst(Boolean.parseBoolean(additionalProperties.get(API_FIRST).toString()));
        }

        if (additionalProperties.containsKey(HATEOAS)) {
            this.setHateoas(Boolean.parseBoolean(additionalProperties.get(HATEOAS).toString()));
        }

        if (additionalProperties.containsKey(SPRING_CONTROLLER)) {
            this.setUseSpringController(convertPropertyToBoolean(SPRING_CONTROLLER));
        }
        writePropertyBack(SPRING_CONTROLLER, useSpringController);

        if (additionalProperties.containsKey(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS)) {
            this.generatedConstructorWithRequiredArgs = convertPropertyToBoolean(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS);
        }
        writePropertyBack(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS, generatedConstructorWithRequiredArgs);

        if (additionalProperties.containsKey(RETURN_SUCCESS_CODE)) {
            this.setReturnSuccessCode(Boolean.parseBoolean(additionalProperties.get(RETURN_SUCCESS_CODE).toString()));
        }

        if (additionalProperties.containsKey(USE_SWAGGER_UI)) {
            this.setUseSwaggerUI(convertPropertyToBoolean(USE_SWAGGER_UI));
        }

        if (getDocumentationProvider().equals(DocumentationProvider.NONE)) {
            this.setUseSwaggerUI(false);
        }

        writePropertyBack(USE_SWAGGER_UI, useSwaggerUI);

        if (additionalProperties.containsKey(UNHANDLED_EXCEPTION_HANDLING)) {
            this.setUnhandledException(
                    Boolean.parseBoolean(additionalProperties.get(UNHANDLED_EXCEPTION_HANDLING).toString()));
        }
        additionalProperties.put(UNHANDLED_EXCEPTION_HANDLING, this.isUnhandledException());

        if (additionalProperties.containsKey(USE_RESPONSE_ENTITY)) {
            this.setUseResponseEntity(
                    Boolean.parseBoolean(additionalProperties.get(USE_RESPONSE_ENTITY).toString()));
        }
        writePropertyBack(USE_RESPONSE_ENTITY, useResponseEntity);
        additionalProperties.put("springHttpStatus", new SpringHttpStatusLambda());

        if (additionalProperties.containsKey(USE_ENUM_CASE_INSENSITIVE)) {
            this.setUseEnumCaseInsensitive(Boolean.parseBoolean(additionalProperties.get(USE_ENUM_CASE_INSENSITIVE).toString()));
        }
        writePropertyBack(USE_ENUM_CASE_INSENSITIVE, useEnumCaseInsensitive);

        if (additionalProperties.containsKey(USE_SPRING_BOOT3)) {
            this.setUseSpringBoot3(convertPropertyToBoolean(USE_SPRING_BOOT3));
        }
        if (isUseSpringBoot3()) {
            if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                throw new IllegalArgumentException(DocumentationProvider.SPRINGFOX.getPropertyName() + " is not supported with Spring Boot > 3.x");
            }
            if (AnnotationLibrary.SWAGGER1.equals(getAnnotationLibrary())) {
                throw new IllegalArgumentException(AnnotationLibrary.SWAGGER1.getPropertyName() + " is not supported with Spring Boot > 3.x");
            }
            useJakartaEe=true;
            additionalProperties.put(USE_JAKARTA_EE, useJakartaEe);
            applyJakartaPackage();
        }
        writePropertyBack(USE_SPRING_BOOT3, isUseSpringBoot3());

        if (additionalProperties.containsKey(RESOURCE_FOLDER)) {
            this.setResourceFolder((String) additionalProperties.get(RESOURCE_FOLDER));
        }
        additionalProperties.put(RESOURCE_FOLDER, resourceFolder);


        typeMapping.put("file", "org.springframework.core.io.Resource");
        importMapping.put("org.springframework.core.io.Resource", "org.springframework.core.io.Resource");
        importMapping.put("DateTimeFormat", "org.springframework.format.annotation.DateTimeFormat");
        importMapping.put("ApiIgnore", "springfox.documentation.annotations.ApiIgnore");
        importMapping.put("ParameterObject", "org.springdoc.api.annotations.ParameterObject");
        if (isUseSpringBoot3()) {
            importMapping.put("ParameterObject", "org.springdoc.core.annotations.ParameterObject");
        }

        if (useOptional) {
            writePropertyBack(USE_OPTIONAL, useOptional);
        }

        if (interfaceOnly && delegatePattern) {
            delegateMethod = true;
            additionalProperties.put("delegate-method", true);
        }

        if (isUseSpringBoot3()) {
            supportingFiles.add(new SupportingFile("pom-sb3.mustache", "", "pom.xml"));
        } else {
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (!interfaceOnly) {
            if (SPRING_BOOT.equals(library)) {
                if (useSwaggerUI && selectedDocumentationProviderRequiresSwaggerUiBootstrap()) {
                    supportingFiles.add(new SupportingFile("swagger-ui.mustache", "src/main/resources/static", "swagger-ui.html"));
                }
                // rename template to SpringBootApplication.mustache
                supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "OpenApiGeneratorApplication.java"));
                supportingFiles.add(new SupportingFile("SpringBootTest.mustache",
                    (testFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                    "OpenApiGeneratorApplicationTests.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "RFC3339DateFormat.java"));
            }
            if (SPRING_CLOUD_LIBRARY.equals(library)) {

                supportingFiles.add(new SupportingFile("apiKeyRequestInterceptor.mustache",
                      (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                      "ApiKeyRequestInterceptor.java"));

                supportingFiles.add(new SupportingFile("oauth2ClientProperties.mustache",
                      resourceFolder, "oauth2-client.properties"));

                supportingFiles.add(new SupportingFile("clientPropertiesConfiguration.mustache",
                      (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                      "ClientPropertiesConfiguration.java"));

                supportingFiles.add(new SupportingFile("clientConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "ClientConfiguration.java"));
                apiTemplateFiles.put("apiClient.mustache", "Client.java");
                if (!additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
                    additionalProperties.put(SINGLE_CONTENT_TYPES, "true");
                    this.setSingleContentTypes(true);
                }
                // @RequestMapping not supported with spring cloud openfeign.
                setRequestMappingMode(RequestMappingMode.none);
                additionalProperties.put(USE_FEIGN_CLIENT, "true");
            } else if (SPRING_BOOT.equals(library)) {
                apiTemplateFiles.put("apiController.mustache", "Controller.java");
                if (containsEnums()) {
                    supportingFiles.add(new SupportingFile("converter.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "EnumConverterConfiguration.java"));
                }
                supportingFiles.add(new SupportingFile("application.mustache",
                        ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));
                supportingFiles.add(new SupportingFile("homeController.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "HomeController.java"));
                supportingFiles.add(new SupportingFile("openapi.mustache",
                        ("src/main/resources").replace("/", java.io.File.separator), "openapi.yaml"));
                if (!reactive && !apiFirst){
                    if (DocumentationProvider.SPRINGDOC.equals(getDocumentationProvider())){
                        supportingFiles.add(new SupportingFile("springdocDocumentationConfig.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                            "SpringDocConfiguration.java"));
                    } else if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                        supportingFiles.add(new SupportingFile("openapiDocumentationConfig.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                            "SpringFoxConfiguration.java"));
                    }
                }
            } else if (SPRING_HTTP_INTERFACE.equals(library)) {
                supportingFiles.add(new SupportingFile("httpInterfacesConfiguration.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "HttpInterfacesAbstractConfigurator.java"));
                writePropertyBack(USE_BEANVALIDATION, false);
            }
        }

        if (SPRING_BOOT.equals(library)) {
            supportingFiles.add(new SupportingFile("apiUtil.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtil.java"));
        }

        if (!delegatePattern || delegateMethod) {
            additionalProperties.put("jdk8-no-delegate", true);
        }

        if (delegatePattern && !delegateMethod) {
            additionalProperties.put("isDelegate", "true");
            apiTemplateFiles.put("apiDelegate.mustache", "Delegate.java");
        }

        additionalProperties.put("javaVersion", "1.8");
        if (SPRING_CLOUD_LIBRARY.equals(library)) {
            additionalProperties.put("jdk8-default-interface", false);
        } else {
            additionalProperties.put("jdk8-default-interface", !skipDefaultInterface);
        }

        if (async) {
            additionalProperties.put(RESPONSE_WRAPPER, "CompletableFuture");
        }
        if (reactive) {
            additionalProperties.put(RESPONSE_WRAPPER, "Mono");
        }

        // Some well-known Spring or Spring-Cloud response wrappers
        if (isNotEmpty(responseWrapper)) {
            additionalProperties.put("jdk8-default-interface", false);
            switch (responseWrapper) {
            case "Future":
            case "Callable":
            case "CompletableFuture":
                additionalProperties.put(RESPONSE_WRAPPER, "java.util.concurrent." + responseWrapper);
                break;
            case "ListenableFuture":
                additionalProperties.put(RESPONSE_WRAPPER, "org.springframework.util.concurrent.ListenableFuture");
                break;
            case "DeferredResult":
                additionalProperties.put(RESPONSE_WRAPPER,
                        "org.springframework.web.context.request.async.DeferredResult");
                break;
            case "RxObservable":
                additionalProperties.put(RESPONSE_WRAPPER, "rx.Observable");
                break;
            case "RxSingle":
                additionalProperties.put(RESPONSE_WRAPPER, "rx.Single");
                break;
            default:
                break;
            }
        }

        switch (getRequestMappingMode()) {
            case api_interface:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_INTERFACE, true);
                break;
            case controller:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_CONTROLLER, true);
                break;
            case none:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_INTERFACE, false);
                additionalProperties.put(USE_REQUEST_MAPPING_ON_CONTROLLER, false);
                break;
        }

        // add lambda for mustache templates
        additionalProperties.put("lambdaRemoveDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement(""))));
        additionalProperties.put("lambdaEscapeDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("lambdaSplitString", new SplitStringLambda());

        // apiController: hide implementation behind undocumented flag to temporarily preserve code
        additionalProperties.put("_api_controller_impl_", false);
        // HEADS-UP: Do not add more template file after this block
        if (apiFirst) {
            apiTemplateFiles.clear();
            modelTemplateFiles.clear();
        }
        supportsAdditionalPropertiesWithComposedSchema = true;
    }

    private boolean containsEnums() {
        if (openAPI == null) {
            return false;
        }

        Components components = this.openAPI.getComponents();
        if (components == null || components.getSchemas() == null) {
            return  false;
        }

        return components.getSchemas().values().stream()
                .anyMatch(it -> it.getEnum() != null && !it.getEnum().isEmpty());
    }

    private boolean supportLibraryUseTags(){
        return SPRING_BOOT.equals(library) || SPRING_CLOUD_LIBRARY.equals(library);
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if (supportLibraryUseTags() && !useTags) {
            String basePath = resourcePath;
            if (basePath.startsWith("/")) {
                basePath = basePath.substring(1);
            }
            final int pos = basePath.indexOf("/");
            if (pos > 0) {
                basePath = basePath.substring(0, pos);
            }

            if (basePath.isEmpty()) {
                basePath = "default";
            } else {
                co.subresourceOperation = !co.path.isEmpty();
            }
            final List<CodegenOperation> opList = operations.computeIfAbsent(basePath, k -> new ArrayList<>());
            opList.add(co);
            co.baseName = basePath;
            return;
        }
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);

    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /*
         * TODO the following logic should not need anymore in OAS 3.0 if
         * ("/".equals(swagger.getBasePath())) { swagger.setBasePath(""); }
         */

        if (!additionalProperties.containsKey(TITLE)) {
            // From the title, compute a reasonable name for the package and the API
            String title = openAPI.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase(Locale.ROOT).endsWith("API")) {
                    title = title.substring(0, title.length() - 3);
                }

                this.title = camelize(sanitizeName(title), LOWERCASE_FIRST_LETTER);
            }
            additionalProperties.put(TITLE, this.title);
        }

        if (!additionalProperties.containsKey(SERVER_PORT)) {
            final URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        if (openAPI.getPaths() != null) {
            for (final Map.Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
                final String pathname = openAPIGetPathsEntry.getKey();
                final PathItem path = openAPIGetPathsEntry.getValue();
                if (path.readOperations() != null) {
                    for (final Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            final List<Map<String, String>> tags = new ArrayList<>();
                            for (final String tag : operation.getTags()) {
                                final Map<String, String> value = new HashMap<>();
                                value.put("tag", tag);
                                tags.add(value);
                            }
                            if (operation.getTags().size() > 0) {
                                final String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.addExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        final OperationMap operations = objs.getOperations();
        if (operations != null) {
            final List<CodegenOperation> ops = operations.getOperation();
            for (final CodegenOperation operation : ops) {
                final List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (final CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                        doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {
                            @Override
                            public void setReturnType(final String returnType) {
                                resp.dataType = returnType;
                            }

                            @Override
                            public void setReturnContainer(final String returnContainer) {
                                resp.containerType = returnContainer;
                            }

                            @Override
                            public void setIsVoid(boolean isVoid) {
                                resp.isVoid = isVoid;
                            }
                        });
                    }
                }

                doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {

                    @Override
                    public void setReturnType(final String returnType) {
                        operation.returnType = returnType;
                    }

                    @Override
                    public void setReturnContainer(final String returnContainer) {
                        operation.returnContainer = returnContainer;
                    }

                    @Override
                    public void setIsVoid(boolean isVoid) {
                        operation.isVoid = isVoid;
                    }
                });

                prepareVersioningParameters(ops);
                handleImplicitHeaders(operation);
            }
            // The tag for the controller is the first tag of the first operation
            final CodegenOperation firstOperation = ops.get(0);
            final Tag firstTag = firstOperation.tags.get(0);
            final String firstTagName = firstTag.getName();
            // But use a sensible tag name if there is none
            objs.put("tagName", "default".equals(firstTagName) ? firstOperation.baseName : firstTagName);
            objs.put("tagDescription", escapeText(firstTag.getDescription()));
        }

        removeImport(objs, "java.util.List");

        return objs;
    }

    private interface DataTypeAssigner {
        void setReturnType(String returnType);

        void setReturnContainer(String returnContainer);

        void setIsVoid(boolean isVoid);
    }

    /**
     * @param returnType       The return type that needs to be converted
     * @param dataTypeAssigner An object that will assign the data to the respective
     *                         fields in the model.
     */
    private void doDataTypeAssignment(String returnType, DataTypeAssigner dataTypeAssigner) {
        final String rt = returnType;
        if (rt == null) {
            dataTypeAssigner.setReturnType("Void");
            dataTypeAssigner.setIsVoid(true);
        } else if (rt.startsWith("List") || rt.startsWith("java.util.List")) {
            final int start = rt.indexOf("<");
            final int end = rt.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(rt.substring(start + 1, end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (rt.startsWith("Map") || rt.startsWith("java.util.Map")) {
            final int start = rt.indexOf("<");
            final int end = rt.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(rt.substring(start + 1, end).split(",", 2)[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        } else if (rt.startsWith("Set") || rt.startsWith("java.util.Set")) {
            final int start = rt.indexOf("<");
            final int end = rt.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(rt.substring(start + 1, end).trim());
                dataTypeAssigner.setReturnContainer("Set");
            }
        }
    }

    private void prepareVersioningParameters(List<CodegenOperation> operations) {
        for (CodegenOperation operation : operations) {
            if (operation.getHasHeaderParams()) {
                List<CodegenParameter> versionParams = operation.headerParams.stream()
                    .filter(param -> {
                        String xVersionParam = Objects.toString(param.vendorExtensions.get(VendorExtension.X_VERSION_PARAM.getName()), "false");
                        return Boolean.parseBoolean(xVersionParam);
                    })
                    .collect(Collectors.toList());
                operation.hasVersionHeaders = !versionParams.isEmpty();
                operation.vendorExtensions.put("versionHeaderParamsList", versionParams);
            }

            if (operation.getHasQueryParams()) {
                List<CodegenParameter> versionParams = operation.queryParams.stream()
                    .filter(param -> {
                        String xVersionParam = Objects.toString(param.vendorExtensions.get(VendorExtension.X_VERSION_PARAM.getName()), "false");
                        return Boolean.parseBoolean(xVersionParam);
                    })
                    .collect(Collectors.toList());
                operation.hasVersionQueryParams = !versionParams.isEmpty();
                operation.vendorExtensions.put("versionQueryParamsList", versionParams);
            }
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        if (SPRING_CLOUD_LIBRARY.equals(library)) {
            final List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
            if (authMethods != null) {
                for (final CodegenSecurity authMethod : authMethods) {
                    authMethod.name = camelize(sanitizeName(authMethod.name), LOWERCASE_FIRST_LETTER);
                }
            }
        }
        return objs;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name) + apiNameSuffix;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("File".equals(type)) {
            String example;

            if (p.defaultValue == null) {
                example = p.example;
            } else {
                example = p.defaultValue;
            }

            if (example == null) {
                example = "/path/to/file";
            }
            example = "new org.springframework.core.io.FileSystemResource(new java.io.File(\"" + escapeText(example)
                    + "\"))";
            p.example = example;
        } else {
            super.setParameterExampleValue(p);
        }
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public String getConfigPackage() {
        return configPackage;
    }

    public boolean isUnhandledException() {
        return unhandledException;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getBasePackage() {
        return basePackage;
    }

    public void setInterfaceOnly(boolean interfaceOnly) {
        this.interfaceOnly = interfaceOnly;
    }

    public void setUseFeignClientUrl(boolean useFeignClientUrl) {
        this.useFeignClientUrl = useFeignClientUrl;
    }

    public void setDelegatePattern(boolean delegatePattern) {
        this.delegatePattern = delegatePattern;
    }

    public void setSingleContentTypes(boolean singleContentTypes) {
        this.singleContentTypes = singleContentTypes;
    }

    public void setSkipDefaultInterface(boolean skipDefaultInterface) {
        this.skipDefaultInterface = skipDefaultInterface;
    }

    public void setVirtualService(boolean virtualService) {
        this.virtualService = virtualService;
    }

    public void setAsync(boolean async) {
        this.async = async;
    }

    public void setReactive(boolean reactive) {
        this.reactive = reactive;
    }

    public void setResponseWrapper(String responseWrapper) {
        this.responseWrapper = responseWrapper;
    }

    public void setUseTags(boolean useTags) {
        this.useTags = useTags;
    }

    public void setApiFirst(boolean apiFirst) {
        this.apiFirst = apiFirst;
    }

    public void setHateoas(boolean hateoas) {
        this.hateoas = hateoas;
    }

    public void setUseSpringController(boolean useSpringController) {
        this.useSpringController = useSpringController;
    }

    public void setReturnSuccessCode(boolean returnSuccessCode) {
        this.returnSuccessCode = returnSuccessCode;
    }

    public void setUnhandledException(boolean unhandledException) {
        this.unhandledException = unhandledException;
    }

    public void setUseResponseEntity(boolean useResponseEntity) {
        this.useResponseEntity = useResponseEntity;
    }

    public void setUseEnumCaseInsensitive(boolean useEnumCaseInsensitive) {
        this.useEnumCaseInsensitive = useEnumCaseInsensitive;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // add org.springframework.format.annotation.DateTimeFormat when needed
        if (property.isDate || property.isDateTime) {
            model.imports.add("DateTimeFormat");
        }

        if ("null".equals(property.example)) {
            property.example = null;
        }

        // Add imports for Jackson
        if (!Boolean.TRUE.equals(model.isEnum)) {
            model.imports.add("JsonProperty");

            if (Boolean.TRUE.equals(model.hasEnums)) {
                model.imports.add("JsonValue");
            }
        } else { // enum class
            // Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey(JACKSON)) {
                model.imports.add("JsonCreator");
            }
        }

        // Add imports for java.util.Arrays
        if (property.isByteArray) {
            model.imports.add("Arrays");
        }

        if (model.getVendorExtensions().containsKey("x-jackson-optional-nullable-helpers")) {
            model.imports.add("Arrays");
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (getAnnotationLibrary() != AnnotationLibrary.SWAGGER1) {
            // remove swagger imports
            codegenModel.imports.remove("ApiModelProperty");
            codegenModel.imports.remove("ApiModel");
        }

        return codegenModel;
    }

    /**
     * Analyse and post process all Models.
     *  Add parentVars to every Model which has a parent. This allows to generate
     *  fluent setter methods for inherited properties.
     * @param objs the models map.
     * @return the processed models map.
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);
        objs = super.updateAllModels(objs);

        for (ModelsMap modelsAttrs : objs.values()) {
            for (ModelMap mo : modelsAttrs.getModels()) {
                CodegenModel codegenModel = mo.getModel();
                Set<String> inheritedImports = new HashSet<>();
                Map<String, CodegenProperty> propertyHash = new HashMap<>(codegenModel.vars.size());
                for (final CodegenProperty property : codegenModel.vars) {
                    propertyHash.put(property.name, property);
                }
                CodegenModel parentCodegenModel = codegenModel.parentModel;
                while (parentCodegenModel != null) {
                    for (final CodegenProperty property : parentCodegenModel.vars) {
                        // helper list of parentVars simplifies templating
                        if (!propertyHash.containsKey(property.name)) {
                            propertyHash.put(property.name, property);
                            final CodegenProperty parentVar = property.clone();
                            parentVar.isInherited = true;
                            LOGGER.info("adding parent variable {}", property.name);
                            codegenModel.parentVars.add(parentVar);
                            Set<String> imports = parentVar.getImports(true, this.importBaseType, generatorMetadata.getFeatureSet()).stream().filter(Objects::nonNull).collect(Collectors.toSet());
                            for (String imp: imports) {
                                // Avoid dupes
                                if (!codegenModel.getImports().contains(imp)) {
                                    inheritedImports.add(imp);
                                    codegenModel.getImports().add(imp);
                                }
                            }
                        }
                    }
                    parentCodegenModel = parentCodegenModel.getParentModel();
                }
                if (codegenModel.getParentModel() != null) {
                    codegenModel.parentRequiredVars = new ArrayList<>(codegenModel.getParentModel().requiredVars);
                }
                // There must be a better way ...
                for (String imp: inheritedImports) {
                    String qimp = importMapping().get(imp);
                    if (qimp != null) {
                        Map<String,String> toAdd = new HashMap<>();
                        toAdd.put("import", qimp);
                        modelsAttrs.getImports().add(toAdd);
                    }
                }
            }
        }
        return objs;
    }


    /*
     * Add dynamic imports based on the parameters and vendor extensions of an operation.
     * The imports are expanded by the mustache {{import}} tag available to model and api
     * templates.
     */
    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {

        // add Pageable import only if x-spring-paginated explicitly used
        // this allows to use a custom Pageable schema without importing Spring Pageable.
        if (Boolean.TRUE.equals(operation.getExtensions().get("x-spring-paginated"))) {
            importMapping.put("Pageable", "org.springframework.data.domain.Pageable");
        }

        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, servers);

        // add org.springframework.format.annotation.DateTimeFormat when needed
        codegenOperation.allParams.stream().filter(p -> p.isDate || p.isDateTime).findFirst()
            .ifPresent(p -> codegenOperation.imports.add("DateTimeFormat"));

        // add org.springframework.data.domain.Pageable import when needed
        if (codegenOperation.vendorExtensions.containsKey("x-spring-paginated")) {
            codegenOperation.imports.add("Pageable");
            if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                codegenOperation.imports.add("ApiIgnore");
            }
            if (DocumentationProvider.SPRINGDOC.equals(getDocumentationProvider())) {
                codegenOperation.imports.add("ParameterObject");
            }
        }

        if (reactive) {
            if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                codegenOperation.imports.add("ApiIgnore");
            }
        }
        return codegenOperation;
    }

    @Override
    public ModelsMap postProcessModelsEnum(ModelsMap objs) {
        objs = super.postProcessModelsEnum(objs);

        // Add imports for Jackson
        final List<Map<String, String>> imports = objs.getImports();
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            boolean addNullableImports = false;
            for (CodegenProperty var : cm.vars) {
                addNullableImports = isAddNullableImports(cm, addNullableImports, var);
            }
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                cm.imports.add(importMapping.get("JsonValue"));
                final Map<String, String> item = new HashMap<>();
                item.put("import", importMapping.get("JsonValue"));
                imports.add(item);
            }
            if (addNullableImports) {
                Map<String, String> imports2Classnames = new HashMap<>();
                imports2Classnames.put("NoSuchElementException", "java.util.NoSuchElementException");
                addImports(imports, cm, imports2Classnames);
            }
        }

        return objs;
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @Override
    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }

    @Override
    public void setUseOptional(boolean useOptional) {
        this.useOptional = useOptional;
    }

    @Override
    public void setUseSwaggerUI(boolean useSwaggerUI) {
        this.useSwaggerUI = useSwaggerUI;
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.add(VendorExtension.X_OPERATION_EXTRA_ANNOTATION);
        extensions.add(VendorExtension.X_SPRING_PAGINATED);
        extensions.add(VendorExtension.X_VERSION_PARAM);
        extensions.add(VendorExtension.X_PATTERN_MESSAGE);
        return extensions;
    }

    public boolean isUseSpringBoot3() {
        return useSpringBoot3;
    }

    public void setUseSpringBoot3(boolean useSpringBoot3) {
        this.useSpringBoot3 = useSpringBoot3;
    }

    public RequestMappingMode getRequestMappingMode() {
        return requestMappingMode;
    }

    public void setRequestMappingMode(RequestMappingMode requestMappingMode) {
        this.requestMappingMode = requestMappingMode;
    }

    @Override
    public CodegenParameter fromParameter( final Parameter parameter, final Set<String> imports ) {
        CodegenParameter codegenParameter = super.fromParameter( parameter, imports );
        if(!isListOrSet(codegenParameter)){
            return codegenParameter;
        }
        codegenParameter.datatypeWithEnum = replaceBeanValidationCollectionType(codegenParameter.items, codegenParameter.datatypeWithEnum  );
        codegenParameter.dataType = replaceBeanValidationCollectionType(codegenParameter.items, codegenParameter.dataType  );
        return codegenParameter;
    }
    @Override
    public CodegenProperty fromProperty( String name, Schema p, boolean required, boolean schemaIsFromAdditionalProperties ) {
        CodegenProperty codegenProperty = super.fromProperty( name, p, required, schemaIsFromAdditionalProperties );
        if(!isListOrSet(codegenProperty)){
            return codegenProperty;
        }
        codegenProperty.datatypeWithEnum = replaceBeanValidationCollectionType(codegenProperty.items, codegenProperty.datatypeWithEnum );
        codegenProperty.dataType = replaceBeanValidationCollectionType(codegenProperty.items, codegenProperty.dataType  );
        return codegenProperty;
    }

    // The default validation applied for non-container and non-map types is sufficient for the SpringCodegen.
    // Maps are very complex for bean validation, so it's currently not supported.
    private static boolean isListOrSet(CodegenProperty codegenProperty) {
        return codegenProperty.isContainer && !codegenProperty.isMap;
    }

    // The default validation applied for non-container and non-map types is sufficient for the SpringCodegen.
    // Maps are very complex for bean validation, so it's currently not supported.
    private static boolean isListOrSet(CodegenParameter codegenParameter) {
        return codegenParameter.isContainer && !codegenParameter.isMap;
    }

    private String replaceBeanValidationCollectionType(CodegenProperty codegenProperty, String dataType) {
        if (!useBeanValidation() || !codegenProperty.isModel || isResponseType(codegenProperty)) {
            return dataType;
        }

        if (StringUtils.isEmpty( dataType ) || dataType.contains( "@Valid" )) {
            return dataType;
        }
        return dataType.replace( "<", "<@Valid " );
    }

    public void setResourceFolder( String resourceFolder ) {
        this.resourceFolder = resourceFolder;
    }

    public String getResourceFolder() {
        return resourceFolder;
    }


    // This should prevent, that the response data types not contains a @Valid annotation.
    // However, the side effect is that attributes with response as name are also affected.
    private static boolean isResponseType(CodegenProperty codegenProperty) {
        return codegenProperty.baseName.toLowerCase(Locale.ROOT).contains("response");
    }

    // SPRING_HTTP_INTERFACE does not support bean validation.
    public boolean useBeanValidation() {
        return useBeanValidation && !SPRING_HTTP_INTERFACE.equals(library);
    }
}
