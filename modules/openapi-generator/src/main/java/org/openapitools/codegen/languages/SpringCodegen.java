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

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

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
    public static final String USE_FEIGN_CLIENT_CONTEXT_ID = "useFeignClientContextId";
    public static final String DELEGATE_PATTERN = "delegatePattern";
    public static final String SINGLE_CONTENT_TYPES = "singleContentTypes";
    public static final String VIRTUAL_SERVICE = "virtualService";
    public static final String SKIP_DEFAULT_INTERFACE = "skipDefaultInterface";
    public static final String GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS = "generatedConstructorWithRequiredArgs";

    public static final String RESOURCE_FOLDER = "resourceFolder";
    public static final String RESOURCE_FOLDER_DESC = "resource folder for generated resources";

    public static final String ASYNC = "async";
    public static final String REACTIVE = "reactive";
    public static final String SSE = "serverSentEvents";
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
    public static final String GENERATE_GENERIC_RESPONSE_ENTITY = "generateGenericResponseEntity";
    public static final String USE_ENUM_CASE_INSENSITIVE = "useEnumCaseInsensitive";
    public static final String USE_SPRING_BOOT3 = "useSpringBoot3";
    public static final String REQUEST_MAPPING_OPTION = "requestMappingMode";
    public static final String USE_REQUEST_MAPPING_ON_CONTROLLER = "useRequestMappingOnController";
    public static final String USE_REQUEST_MAPPING_ON_INTERFACE = "useRequestMappingOnInterface";
    public static final String USE_SEALED = "useSealed";
    public static final String OPTIONAL_ACCEPT_NULLABLE = "optionalAcceptNullable";
    public static final String USE_SPRING_BUILT_IN_VALIDATION = "useSpringBuiltInValidation";
    public static final String USE_DEDUCTION_FOR_ONE_OF_INTERFACES = "useDeductionForOneOfInterfaces";
    public static final String SPRING_API_VERSION = "springApiVersion";

    @Getter
    public enum RequestMappingMode {
        api_interface("Generate the @RequestMapping annotation on the generated Api Interface."),
        controller("Generate the @RequestMapping annotation on the generated Api Controller Implementation."),
        none("Do not add a class level @RequestMapping annotation.");

        private String description;

        RequestMappingMode(String description) {
            this.description = description;
        }
    }

    public static final String OPEN_BRACE = "{";
    public static final String CLOSE_BRACE = "}";

    @Setter protected String title = "OpenAPI Spring";
    @Getter @Setter
    protected String configPackage = "org.openapitools.configuration";
    @Getter @Setter
    protected String basePackage = "org.openapitools";
    @Getter @Setter
    protected String resourceFolder = projectFolder + "/resources";

    @Setter protected boolean interfaceOnly = false;
    @Setter protected boolean useFeignClientUrl = true;
    @Setter protected boolean useFeignClientContextId = true;
    @Setter protected boolean delegatePattern = false;
    protected boolean delegateMethod = false;
    @Setter protected boolean singleContentTypes = false;
    @Setter protected boolean async = false;
    @Setter protected boolean reactive = false;
    @Setter protected boolean sse = false;
    @Setter protected String responseWrapper = null;
    @Setter protected boolean skipDefaultInterface = false;
    @Setter protected boolean useTags = false;
    protected boolean performBeanValidation = false;
    @Setter protected boolean apiFirst = false;
    protected boolean useOptional = false;
    @Setter protected boolean useSealed = false;
    @Setter protected boolean virtualService = false;
    @Setter protected boolean hateoas = false;
    @Setter protected boolean returnSuccessCode = false;
    @Getter @Setter
    protected boolean unhandledException = false;
    @Setter protected boolean useSpringController = false;
    protected boolean useSwaggerUI = true;
    @Setter protected boolean useResponseEntity = true;
    @Setter protected boolean generateGenericResponseEntity = false;
    @Setter protected boolean useEnumCaseInsensitive = false;
    @Getter @Setter
    protected boolean useSpringBoot3 = false;
    protected boolean generatedConstructorWithRequiredArgs = true;
    @Getter @Setter
    protected RequestMappingMode requestMappingMode = RequestMappingMode.controller;
    @Getter @Setter
    protected boolean optionalAcceptNullable = true;
    @Getter @Setter
    protected boolean useSpringBuiltInValidation = false;
    @Getter @Setter
    protected boolean useDeductionForOneOfInterfaces = false;

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

        useBeanValidation = true;
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

        // Enable discriminator-based oneOf interface generation by default
        useOneOfInterfaces = true;
        legacyDiscriminatorBehavior = false;
        updateOption(USE_ONE_OF_INTERFACES, String.valueOf(useOneOfInterfaces));
        updateOption(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, String.valueOf(legacyDiscriminatorBehavior));

        apiTestTemplateFiles.clear(); // TODO: add test template

        // spring uses the jackson lib
        jackson = true;
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
        cliOptions.add(CliOption.newBoolean(USE_FEIGN_CLIENT_CONTEXT_ID,
                "Whether to generate Feign client with contextId parameter.", useFeignClientContextId));
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
        cliOptions.add(CliOption.newBoolean(USE_SPRING_BUILT_IN_VALIDATION,
                "Disable `@Validated` at the class level when using built-in validation.",
                useSpringBuiltInValidation));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION,
                "Use Bean Validation Impl. to perform BeanValidation", performBeanValidation));
        cliOptions.add(CliOption.newBoolean(USE_SEALED,
                "Whether to generate sealed model interfaces and classes"));
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
        for (RequestMappingMode mode : RequestMappingMode.values()) {
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
        cliOptions.add(CliOption.newBoolean(GENERATE_GENERIC_RESPONSE_ENTITY,
                "Use a generic type for the `ResponseEntity` wrapping return values of generated API methods. "
                        + "If enabled, method are generated with return type ResponseEntity<?>",
                generateGenericResponseEntity));
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
        cliOptions.add(CliOption.newBoolean(OPTIONAL_ACCEPT_NULLABLE,
                "Use `ofNullable` instead of just `of` to accept null values when using Optional.",
                optionalAcceptNullable));

        cliOptions.add(CliOption.newBoolean(USE_DEDUCTION_FOR_ONE_OF_INTERFACES, "whether to use deduction for generated oneOf interfaces", useDeductionForOneOfInterfaces));
        cliOptions.add(CliOption.newString(SPRING_API_VERSION, "Value for 'version' attribute in @RequestMapping (for Spring 7 and above)."));
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

    @Override
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
            LOGGER.info("Set base package to invoker package ({})", basePackage);
        }

        convertPropertyToTypeAndWriteBack(REQUEST_MAPPING_OPTION, RequestMappingMode::valueOf, this::setRequestMappingMode);

        // Please refrain from updating values of Config Options after super.ProcessOpts() is called
        super.processOpts();

        if (SPRING_HTTP_INTERFACE.equals(library)) {
            documentationProvider = DocumentationProvider.NONE;
            annotationLibrary = AnnotationLibrary.NONE;
            useJakartaEe = true;
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

        convertPropertyToStringAndWriteBack(TITLE, this::setTitle);
        convertPropertyToStringAndWriteBack(CONFIG_PACKAGE, this::setConfigPackage);
        convertPropertyToStringAndWriteBack(BASE_PACKAGE, this::setBasePackage);
        convertPropertyToBooleanAndWriteBack(VIRTUAL_SERVICE, this::setVirtualService);
        convertPropertyToBooleanAndWriteBack(INTERFACE_ONLY, this::setInterfaceOnly);
        convertPropertyToBooleanAndWriteBack(USE_FEIGN_CLIENT_URL, this::setUseFeignClientUrl);
        convertPropertyToBooleanAndWriteBack(USE_FEIGN_CLIENT_CONTEXT_ID, this::setUseFeignClientContextId);
        convertPropertyToBooleanAndWriteBack(DELEGATE_PATTERN, this::setDelegatePattern);
        convertPropertyToBooleanAndWriteBack(SINGLE_CONTENT_TYPES, this::setSingleContentTypes);
        convertPropertyToBooleanAndWriteBack(SKIP_DEFAULT_INTERFACE, this::setSkipDefaultInterface);
        convertPropertyToBooleanAndWriteBack(ASYNC, this::setAsync);
        if (additionalProperties.containsKey(REACTIVE)) {
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                throw new IllegalArgumentException("Currently, reactive option doesn't supported by Spring Cloud");
            }
            convertPropertyToBooleanAndWriteBack(REACTIVE, this::setReactive);
            convertPropertyToBooleanAndWriteBack(SSE, this::setSse);
        }

        convertPropertyToStringAndWriteBack(RESPONSE_WRAPPER, this::setResponseWrapper);
        convertPropertyToBooleanAndWriteBack(USE_TAGS, this::setUseTags);
        convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION, this::setUseBeanValidation);
        convertPropertyToBooleanAndWriteBack(PERFORM_BEANVALIDATION, this::setPerformBeanValidation);
        convertPropertyToBooleanAndWriteBack(USE_OPTIONAL, this::setUseOptional);
        convertPropertyToBooleanAndWriteBack(API_FIRST, this::setApiFirst);
        convertPropertyToBooleanAndWriteBack(HATEOAS, this::setHateoas);
        convertPropertyToBooleanAndWriteBack(SPRING_CONTROLLER, this::setUseSpringController);
        convertPropertyToBooleanAndWriteBack(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS, value -> this.generatedConstructorWithRequiredArgs = value);
        convertPropertyToBooleanAndWriteBack(RETURN_SUCCESS_CODE, this::setReturnSuccessCode);
        convertPropertyToBooleanAndWriteBack(USE_SWAGGER_UI, this::setUseSwaggerUI);
        convertPropertyToBooleanAndWriteBack(USE_SEALED, this::setUseSealed);
        if (getDocumentationProvider().equals(DocumentationProvider.NONE)) {
            this.setUseSwaggerUI(false);
        }

        convertPropertyToBooleanAndWriteBack(UNHANDLED_EXCEPTION_HANDLING, this::setUnhandledException);
        convertPropertyToBooleanAndWriteBack(USE_RESPONSE_ENTITY, this::setUseResponseEntity);
        convertPropertyToBooleanAndWriteBack(GENERATE_GENERIC_RESPONSE_ENTITY, this::setGenerateGenericResponseEntity);
        if (!useResponseEntity) {
            this.setGenerateGenericResponseEntity(false);
            this.additionalProperties.put(GENERATE_GENERIC_RESPONSE_ENTITY, false);
        }
        convertPropertyToBooleanAndWriteBack(OPTIONAL_ACCEPT_NULLABLE, this::setOptionalAcceptNullable);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_BUILT_IN_VALIDATION, this::setUseSpringBuiltInValidation);
        convertPropertyToBooleanAndWriteBack(USE_DEDUCTION_FOR_ONE_OF_INTERFACES, this::setUseDeductionForOneOfInterfaces);

        additionalProperties.put("springHttpStatus", new SpringHttpStatusLambda());

        convertPropertyToBooleanAndWriteBack(USE_ENUM_CASE_INSENSITIVE, this::setUseEnumCaseInsensitive);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_BOOT3, this::setUseSpringBoot3);
        if (isUseSpringBoot3()) {
            if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                throw new IllegalArgumentException(DocumentationProvider.SPRINGFOX.getPropertyName() + " is not supported with Spring Boot > 3.x");
            }
            if (AnnotationLibrary.SWAGGER1.equals(getAnnotationLibrary())) {
                throw new IllegalArgumentException(AnnotationLibrary.SWAGGER1.getPropertyName() + " is not supported with Spring Boot > 3.x");
            }
            useJakartaEe = true;
            applyJakartaPackage();
        }
        convertPropertyToStringAndWriteBack(RESOURCE_FOLDER, this::setResourceFolder);

        typeMapping.put("file", "org.springframework.core.io.Resource");
        importMapping.put("Nullable", "org.springframework.lang.Nullable");
        importMapping.put("org.springframework.core.io.Resource", "org.springframework.core.io.Resource");
        importMapping.put("DateTimeFormat", "org.springframework.format.annotation.DateTimeFormat");
        importMapping.put("ApiIgnore", "springfox.documentation.annotations.ApiIgnore");
        importMapping.put("ParameterObject", "org.springdoc.api.annotations.ParameterObject");
        if (isUseSpringBoot3()) {
            importMapping.put("ParameterObject", "org.springdoc.core.annotations.ParameterObject");
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

                if (ProcessUtils.hasOAuthMethods(openAPI)) {
                    supportingFiles.add(new SupportingFile("clientPropertiesConfiguration.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                            "ClientPropertiesConfiguration.java"));
                }

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
                supportingFiles.add(new SupportingFile("application.mustache",
                        ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));
                supportingFiles.add(new SupportingFile("homeController.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "HomeController.java"));
                supportingFiles.add(new SupportingFile("openapi.mustache",
                        ("src/main/resources").replace("/", java.io.File.separator), "openapi.yaml"));
                if (!reactive && !apiFirst) {
                    if (DocumentationProvider.SPRINGDOC.equals(getDocumentationProvider())) {
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
            // The response wrapper when Reactive is enabled must depend on the return type:
            // Flux<X> when X is an array
            // Mono<X> otherwise
            // But there are corner cases when also using response entity.
            // When reactive is enabled, all this is managed in the mustache templates.
            additionalProperties.put(RESPONSE_WRAPPER, "");
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

    private boolean supportLibraryUseTags() {
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

        if (SPRING_BOOT.equals(library) && ModelUtils.containsEnums(this.openAPI)) {
            supportingFiles.add(new SupportingFile("converter.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "EnumConverterConfiguration.java"));
        }

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
        Object apiVersion = additionalProperties.get(SPRING_API_VERSION);
        boolean hasApiVersion = apiVersion != null;
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
            if (hasApiVersion) {
                operation.vendorExtensions.putIfAbsent(VendorExtension.X_SPRING_API_VERSION.getName(), apiVersion);
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

        // to prevent inheritors (JavaCamelServerCodegen etc.) mistakenly use it
        if (getName().contains("spring")) {
            model.imports.add("Nullable");
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

        if (getAnnotationLibrary() != AnnotationLibrary.SWAGGER2) {
            // remove swagger imports
            codegenModel.imports.remove("Schema");
        }

        return codegenModel;
    }

    @Override
    protected boolean isConstructorWithAllArgsAllowed(CodegenModel codegenModel) {
        if (lombokAnnotations != null && lombokAnnotations.containsKey("AllArgsConstructor")) {
            // constructor generated by lombok
            return false;
        }
        if ((!generatedConstructorWithRequiredArgs && !codegenModel.vars.isEmpty())
                || !codegenModel.optionalVars.isEmpty()) {
            return super.isConstructorWithAllArgsAllowed(codegenModel);
        }
        return false;
    }

    /*
     * Add dynamic imports based on the parameters and vendor extensions of an operation.
     * The imports are expanded by the mustache {{import}} tag available to model and api
     * templates.
     *
     * #8315 Also handles removing 'size', 'page' and 'sort' query parameters if using 'x-spring-paginated'.
     */
    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {

        // add Pageable import only if x-spring-paginated explicitly used
        // this allows to use a custom Pageable schema without importing Spring Pageable.
        if (Boolean.TRUE.equals(operation.getExtensions().get("x-spring-paginated"))) {
            importMapping.put("Pageable", "org.springframework.data.domain.Pageable");
        }

        Set<String> provideArgsClassSet = reformatProvideArgsParams(operation);

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

            // #8315 Spring Data Web default query params recognized by Pageable
            List<String> defaultPageableQueryParams = new ArrayList<>(
                Arrays.asList("page", "size", "sort")
            );

            // #8315 Remove matching Spring Data Web default query params if 'x-spring-paginated' with Pageable is used
            codegenOperation.queryParams.removeIf(param -> defaultPageableQueryParams.contains(param.baseName));
            codegenOperation.allParams.removeIf(param -> param.isQueryParam && defaultPageableQueryParams.contains(param.baseName));
        }
        if (codegenOperation.vendorExtensions.containsKey("x-spring-provide-args") && !provideArgsClassSet.isEmpty()) {
            codegenOperation.imports.addAll(provideArgsClassSet);
        }

        // to prevent inheritors (JavaCamelServerCodegen etc.) mistakenly use it
        if (getName().contains("spring")) {
          codegenOperation.allParams.stream().filter(CodegenParameter::notRequiredOrIsNullable).findAny()
              .ifPresent(p -> codegenOperation.imports.add("Nullable"));
        }

        if (reactive) {
            if (DocumentationProvider.SPRINGFOX.equals(getDocumentationProvider())) {
                codegenOperation.imports.add("ApiIgnore");
            }
            if (sse) {
                var MEDIA_EVENT_STREAM = "text/event-stream";
                // inspecting used streaming media types
                /*
                 expected definition:
                 content:
                    text/event-stream:
                        schema:
                        type: array
                        format: event-stream
                        items:
                            type: <type> or
                            $ref: <typeRef>
                 */
                Map<String, List<Schema>> schemaTypes = operation.getResponses().entrySet().stream()
                        .map(e -> Pair.of(e.getValue(), fromResponse(e.getKey(), e.getValue())))
                        .filter(p -> p.getRight().is2xx) // consider only success
                        .map(p -> p.getLeft().getContent().get(MEDIA_EVENT_STREAM))
                        .map(MediaType::getSchema)
                        .collect(Collectors.toList()).stream()
                        .collect(Collectors.groupingBy(Schema::getType));
                if (schemaTypes.containsKey("array")) {
                    // we have a match with SSE pattern
                    // double check potential conflicting, multiple specs
                    if (schemaTypes.keySet().size() > 1) {
                        throw new RuntimeException("only 1 response media type supported, when SSE is detected");
                    }
                    // double check schema format
                    List<Schema> eventTypes = schemaTypes.get("array");
                    if (eventTypes.stream().anyMatch(schema -> !"event-stream".equalsIgnoreCase(schema.getFormat()))) {
                        throw new RuntimeException("schema format 'event-stream' is required, when SSE is detected");
                    }
                    // double check item types
                    Set<String> itemTypes = eventTypes.stream()
                            .map(schema -> schema.getItems().getType() != null
                                    ? schema.getItems().getType()
                                    : schema.getItems().get$ref())
                            .collect(Collectors.toSet());
                    if (itemTypes.size() > 1) {
                        throw new RuntimeException("only single item type is supported, when SSE is detected");
                    }
                    codegenOperation.vendorExtensions.put("x-sse", true);
                } // Not an SSE compliant definition
            }
        }
        return codegenOperation;
    }

    private Set<String> reformatProvideArgsParams(Operation operation) {
        Set<String> provideArgsClassSet = new HashSet<>();
        Object argObj = operation.getExtensions().get("x-spring-provide-args");
        if (argObj instanceof List) {
            List<String> provideArgs = (List<String>) argObj;
            if (!provideArgs.isEmpty()) {
                List<String> formattedArgs = new ArrayList<>();
                for (String oneArg : provideArgs) {
                    if (StringUtils.isNotEmpty(oneArg)) {
                        String regexp = "(?<AnnotationTag>@)?(?<ClassPath>(?<PackageName>(\\w+\\.)*)(?<ClassName>\\w+))(?<Params>\\(.*?\\))?\\s?";
                        Matcher matcher = Pattern.compile(regexp).matcher(oneArg);
                        List<String> newArgs = new ArrayList<>();
                        while (matcher.find()) {
                            String className = matcher.group("ClassName");
                            String classPath = matcher.group("ClassPath");
                            String packageName = matcher.group("PackageName");
                            String params = matcher.group("Params");
                            String annoTag = matcher.group("AnnotationTag");
                            String shortPhrase = StringUtils.join(annoTag, className, params);
                            newArgs.add(shortPhrase);
                            if (StringUtils.isNotEmpty(packageName)) {
                                importMapping.put(className, classPath);
                                provideArgsClassSet.add(className);
                                LOGGER.trace("put import mapping {} {}", className, classPath);
                            }
                        }
                        String newArg = String.join(" ", newArgs);
                        LOGGER.trace("new arg {} {}", newArg);
                        formattedArgs.add(newArg);
                    }
                }
                operation.getExtensions().put("x-spring-provide-args", formattedArgs);
            }
        }
        return provideArgsClassSet;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);

        Map<String, CodegenModel> allModels = getAllModels(objs);
        // conditionally force the generation of no args constructor
        for (CodegenModel cm : allModels.values()) {
            boolean hasLombokNoArgsConstructor = lombokAnnotations != null && lombokAnnotations.containsKey("NoArgsConstructor");
            if (!hasLombokNoArgsConstructor
                    && (cm.hasRequired || cm.vendorExtensions.containsKey("x-java-all-args-constructor"))) {
                cm.vendorExtensions.put("x-java-no-args-constructor", true);
            }
        }
        return objs;
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
        extensions.add(VendorExtension.X_SPRING_API_VERSION);
        return extensions;
    }
}
