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
import static org.openapitools.codegen.utils.StringUtils.camelize;

import io.swagger.v3.oas.models.media.Schema;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

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
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.OptionalFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;

public class SpringCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, OptionalFeatures {
    private final Logger LOGGER = LoggerFactory.getLogger(SpringCodegen.class);

    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String INTERFACE_ONLY = "interfaceOnly";
    public static final String DELEGATE_PATTERN = "delegatePattern";
    public static final String SINGLE_CONTENT_TYPES = "singleContentTypes";
    public static final String VIRTUAL_SERVICE = "virtualService";
    public static final String SKIP_DEFAULT_INTERFACE = "skipDefaultInterface";

    public static final String JAVA_8 = "java8";
    public static final String ASYNC = "async";
    public static final String REACTIVE = "reactive";
    public static final String RESPONSE_WRAPPER = "responseWrapper";
    public static final String USE_TAGS = "useTags";
    public static final String SPRING_MVC_LIBRARY = "spring-mvc";
    public static final String SPRING_BOOT = "spring-boot";
    public static final String SPRING_CLOUD_LIBRARY = "spring-cloud";
    public static final String IMPLICIT_HEADERS = "implicitHeaders";
    public static final String OPENAPI_DOCKET_CONFIG = "swaggerDocketConfig";
    public static final String API_FIRST = "apiFirst";
    public static final String OAS3 = "oas3";
    public static final String SPRING_CONTROLLER = "useSpringController";
    public static final String HATEOAS = "hateoas";
    public static final String RETURN_SUCCESS_CODE = "returnSuccessCode";
    public static final String UNHANDLED_EXCEPTION_HANDLING = "unhandledException";

    public static final String OPEN_BRACE = "{";
    public static final String CLOSE_BRACE = "}";

    protected String title = "OpenAPI Spring";
    protected String configPackage = "org.openapitools.configuration";
    protected String basePackage = "org.openapitools";
    protected boolean interfaceOnly = false;
    protected boolean delegatePattern = false;
    protected boolean delegateMethod = false;
    protected boolean singleContentTypes = false;
    protected boolean java8 = true;
    protected boolean async = false;
    protected boolean reactive = false;
    protected String responseWrapper = "";
    protected boolean skipDefaultInterface = false;
    protected boolean useTags = false;
    protected boolean useBeanValidation = true;
    protected boolean performBeanValidation = false;
    protected boolean implicitHeaders = false;
    protected boolean openapiDocketConfig = false;
    protected boolean apiFirst = false;
    protected boolean useOptional = false;
    protected boolean virtualService = false;
    protected boolean hateoas = false;
    protected boolean returnSuccessCode = false;
    protected boolean unhandledException = false;
    protected boolean useSpringController = false;
    protected boolean oas3 = false;

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
        cliOptions.add(CliOption.newBoolean(DELEGATE_PATTERN,
                "Whether to generate the server files using the delegate pattern", delegatePattern));
        cliOptions.add(CliOption.newBoolean(SINGLE_CONTENT_TYPES,
                "Whether to select only one produces/consumes content-type by operation.", singleContentTypes));
        updateJava8CliOptions();
        cliOptions.add(CliOption.newBoolean(SKIP_DEFAULT_INTERFACE,
                "Whether to generate default implementations for java8 interfaces", skipDefaultInterface));
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
        cliOptions.add(CliOption.newBoolean(IMPLICIT_HEADERS,
                "Skip header parameters in the generated API methods using @ApiImplicitParams annotation.",
                implicitHeaders));
        cliOptions.add(CliOption.newBoolean(OPENAPI_DOCKET_CONFIG,
                "Generate Spring OpenAPI Docket configuration class.", openapiDocketConfig));
        cliOptions.add(CliOption.newBoolean(API_FIRST,
                "Generate the API from the OAI spec at server compile time (API first approach)", apiFirst));
        cliOptions
                .add(CliOption.newBoolean(USE_OPTIONAL, "Use Optional container for optional parameters", useOptional));
        cliOptions.add(
                CliOption.newBoolean(HATEOAS, "Use Spring HATEOAS library to allow adding HATEOAS links", hateoas));
        cliOptions
                .add(CliOption.newBoolean(RETURN_SUCCESS_CODE, "Generated server returns 2xx code", returnSuccessCode));
        cliOptions.add(CliOption.newBoolean(OAS3, "Use OAS 3 Swagger annotations instead of OAS 2 annotations", oas3));
        cliOptions.add(CliOption.newBoolean(SPRING_CONTROLLER, "Annotate the generated API as a Spring Controller", useSpringController));
        cliOptions.add(CliOption.newBoolean(UNHANDLED_EXCEPTION_HANDLING,
                "Declare operation methods to throw a generic exception and allow unhandled exceptions (useful for Spring `@ControllerAdvice` directives).",
                unhandledException));

        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application using the SpringFox integration.");
        supportedLibraries.put(SPRING_MVC_LIBRARY, "Spring-MVC Server application using the SpringFox integration.");
        supportedLibraries.put(SPRING_CLOUD_LIBRARY,
                "Spring-Cloud-Feign client with Spring-Boot auto-configured settings.");
        setLibrary(SPRING_BOOT);
        final CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC)
                .defaultValue(SPRING_BOOT);
        library.setEnum(supportedLibraries);
        cliOptions.add(library);

    }

    private void updateJava8CliOptions() {
        final CliOption option = cliOptions.stream().filter(o -> JAVA_8.equals(o.getOpt())).findFirst()
                .orElseThrow(() -> new RuntimeException("Missing java8 option"));
        final Map<String, String> java8ModeOptions = option.getEnum();
        java8ModeOptions.put("true",
                "Use Java 8 classes such as Base64. Use java8 default interface when a responseWrapper is used. IMPORTANT: This option has been deprecated as Java 8 is the default.");
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
        return "Generates a Java SpringBoot Server application using the SpringFox integration.";
    }

    @Override
    public void processOpts() {

        final List<Pair<String, String>> configOptions = additionalProperties.entrySet().stream()
                .filter(e -> !Arrays.asList(API_FIRST, "hideGenerationTimestamp").contains(e.getKey()))
                .filter(e -> cliOptions.stream().map(CliOption::getOpt).anyMatch(opt -> opt.equals(e.getKey())))
                .map(e -> Pair.of(e.getKey(), e.getValue().toString())).collect(Collectors.toList());
        additionalProperties.put("configOptions", configOptions);

        // Process java8 option before common java ones to change the default
        // dateLibrary to java8.
        LOGGER.info("----------------------------------");
        if (additionalProperties.containsKey(JAVA_8)) {
            this.setJava8(Boolean.parseBoolean(additionalProperties.get(JAVA_8).toString()));
            additionalProperties.put(JAVA_8, java8);
            LOGGER.warn(
                    "java8 option has been deprecated as it's set to true by default (JDK7 support has been deprecated)");
        }
        if (java8 && !additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary("java8");
        }

        if (!additionalProperties.containsKey(BASE_PACKAGE)
                && additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            // set invokerPackage as basePackage:
            this.setBasePackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            additionalProperties.put(BASE_PACKAGE, basePackage);
            LOGGER.info("Set base package to invoker package ({})", basePackage);
        }

        super.processOpts();

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
            if (!SPRING_BOOT.equals(library)) {
                throw new IllegalArgumentException("Currently, reactive option is only supported with Spring-boot");
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

        if (additionalProperties.containsKey(IMPLICIT_HEADERS)) {
            this.setImplicitHeaders(Boolean.parseBoolean(additionalProperties.get(IMPLICIT_HEADERS).toString()));
        }

        if (additionalProperties.containsKey(OPENAPI_DOCKET_CONFIG)) {
            this.setOpenapiDocketConfig(
                    Boolean.parseBoolean(additionalProperties.get(OPENAPI_DOCKET_CONFIG).toString()));
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

        if (additionalProperties.containsKey(OAS3)) {
            this.setOas3(convertPropertyToBoolean(OAS3));
        }
        writePropertyBack(OAS3, oas3);

        if (additionalProperties.containsKey(RETURN_SUCCESS_CODE)) {
            this.setReturnSuccessCode(Boolean.parseBoolean(additionalProperties.get(RETURN_SUCCESS_CODE).toString()));
        }

        if (additionalProperties.containsKey(UNHANDLED_EXCEPTION_HANDLING)) {
            this.setUnhandledException(
                    Boolean.parseBoolean(additionalProperties.get(UNHANDLED_EXCEPTION_HANDLING).toString()));
        }
        additionalProperties.put(UNHANDLED_EXCEPTION_HANDLING, this.isUnhandledException());

        typeMapping.put("file", "org.springframework.core.io.Resource");
        importMapping.put("org.springframework.core.io.Resource", "org.springframework.core.io.Resource");

        if (useOptional) {
            writePropertyBack(USE_OPTIONAL, useOptional);
        }

        if (interfaceOnly && delegatePattern) {
            if (java8) {
                delegateMethod = true;
                additionalProperties.put("delegate-method", true);
            } else {
                throw new IllegalArgumentException(
                        String.format(Locale.ROOT, "Can not generate code with `%s` and `%s` true while `%s` is false.",
                                DELEGATE_PATTERN, INTERFACE_ONLY, JAVA_8));
            }
        }

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (!interfaceOnly) {
            if (SPRING_BOOT.equals(library)) {
                supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "OpenAPI2SpringBoot.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "RFC3339DateFormat.java"));
            }
            if (SPRING_MVC_LIBRARY.equals(library)) {
                supportingFiles.add(new SupportingFile("webApplication.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "WebApplication.java"));
                supportingFiles.add(new SupportingFile("webMvcConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "WebMvcConfiguration.java"));
                supportingFiles.add(new SupportingFile("openapiUiConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "OpenAPIUiConfiguration.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "RFC3339DateFormat.java"));
            }
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                supportingFiles.add(new SupportingFile("apiKeyRequestInterceptor.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "ApiKeyRequestInterceptor.java"));
                supportingFiles.add(new SupportingFile("clientConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "ClientConfiguration.java"));
                apiTemplateFiles.put("apiClient.mustache", "Client.java");
                if (!additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
                    additionalProperties.put(SINGLE_CONTENT_TYPES, "true");
                    this.setSingleContentTypes(true);
                }
            } else {
                apiTemplateFiles.put("apiController.mustache", "Controller.java");
                supportingFiles.add(new SupportingFile("application.mustache",
                        ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));
                supportingFiles.add(new SupportingFile("homeController.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "HomeController.java"));
                if (!reactive && !apiFirst && this.openapiDocketConfig) {
                    supportingFiles.add(new SupportingFile("openapiDocumentationConfig.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                            "OpenAPIDocumentationConfig.java"));
                } else {
                    supportingFiles.add(new SupportingFile("openapi.mustache",
                            ("src/main/resources").replace("/", java.io.File.separator), "openapi.yaml"));
                }
            }
        } else if (openapiDocketConfig && !SPRING_CLOUD_LIBRARY.equals(library) && !reactive && !apiFirst) {
            supportingFiles.add(new SupportingFile("openapiDocumentationConfig.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                    "OpenAPIDocumentationConfig.java"));
        }

        if (!SPRING_CLOUD_LIBRARY.equals(library)) {
            supportingFiles.add(new SupportingFile("apiUtil.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtil.java"));
        }

        if (apiFirst) {
            apiTemplateFiles.clear();
            modelTemplateFiles.clear();
        }

        if ("threetenbp".equals(dateLibrary)) {
            supportingFiles.add(new SupportingFile("customInstantDeserializer.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                    "CustomInstantDeserializer.java"));
            if (SPRING_BOOT.equals(library) || SPRING_CLOUD_LIBRARY.equals(library)) {
                supportingFiles.add(new SupportingFile("jacksonConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "JacksonConfiguration.java"));
            }
        }

        if ((!delegatePattern && java8) || delegateMethod) {
            additionalProperties.put("jdk8-no-delegate", true);
        }

        if (delegatePattern && !delegateMethod) {
            additionalProperties.put("isDelegate", "true");
            apiTemplateFiles.put("apiDelegate.mustache", "Delegate.java");
        }

        if (java8) {
            additionalProperties.put("javaVersion", "1.8");
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                additionalProperties.put("jdk8-default-interface", false);
            } else {
                additionalProperties.put("jdk8-default-interface", !skipDefaultInterface);
            }
            additionalProperties.put("jdk8", true);
            if (async) {
                additionalProperties.put(RESPONSE_WRAPPER, "CompletableFuture");
            }
            if (reactive) {
                additionalProperties.put(RESPONSE_WRAPPER, "Mono");
            }
        } else if (async) {
            additionalProperties.put(RESPONSE_WRAPPER, "Callable");
        }

        // Springfox cannot be used with oas3 or apiFirst or reactive. So, write the property back after determining
        // whether it should be enabled or not.
        boolean useSpringFox = false;
        if (!apiFirst && !reactive && !oas3) {
            useSpringFox = true;
            additionalProperties.put("useSpringfox", true);
        }
        writePropertyBack("useSpringfox", useSpringFox);

        // Some well-known Spring or Spring-Cloud response wrappers
        if (isNotEmpty(responseWrapper)) {
            additionalProperties.put("jdk8", false);
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

        // add lambda for mustache templates
        additionalProperties.put("lambdaRemoveDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement(""))));
        additionalProperties.put("lambdaEscapeDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("lambdaSplitString", new SplitStringLambda());
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co,
            Map<String, List<CodegenOperation>> operations) {
        if ((SPRING_BOOT.equals(library) || SPRING_MVC_LIBRARY.equals(library)) && !useTags) {
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
        } else {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        }
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

                this.title = camelize(sanitizeName(title), true);
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        final Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            final List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
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
                });

                if (implicitHeaders) {
                    removeHeadersFromAllParams(operation.allParams);
                }
            }
        }

        return objs;
    }

    private interface DataTypeAssigner {
        void setReturnType(String returnType);

        void setReturnContainer(String returnContainer);
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
        } else if (rt.startsWith("List")) {
            final int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("List<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (rt.startsWith("Map")) {
            final int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Map<".length(), end).split(",", 2)[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        } else if (rt.startsWith("Set")) {
            final int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Set<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("Set");
            }
        }
    }

    /**
     * This method removes header parameters from the list of parameters
     *
     * @param allParams list of all parameters
     */
    private void removeHeadersFromAllParams(List<CodegenParameter> allParams) {
        if (allParams.isEmpty()) {
            return;
        }
        final ArrayList<CodegenParameter> copy = new ArrayList<>(allParams);
        allParams.clear();

        for (final CodegenParameter p : copy) {
            if (!p.isHeaderParam) {
                allParams.add(p);
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
                    authMethod.name = camelize(sanitizeName(authMethod.name), true);
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
        return camelize(name) + "Api";
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

    public void setDelegatePattern(boolean delegatePattern) {
        this.delegatePattern = delegatePattern;
    }

    public void setSingleContentTypes(boolean singleContentTypes) {
        this.singleContentTypes = singleContentTypes;
    }

    public void setSkipDefaultInterface(boolean skipDefaultInterface) {
        this.skipDefaultInterface = skipDefaultInterface;
    }

    public void setJava8(boolean java8) {
        this.java8 = java8;
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

    public void setImplicitHeaders(boolean implicitHeaders) {
        this.implicitHeaders = implicitHeaders;
    }

    public void setOpenapiDocketConfig(boolean openapiDocketConfig) {
        this.openapiDocketConfig = openapiDocketConfig;
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

    public void setOas3(boolean oas3) {
        this.oas3 = oas3;
    }

    public void setReturnSuccessCode(boolean returnSuccessCode) {
        this.returnSuccessCode = returnSuccessCode;
    }

    public void setUnhandledException(boolean unhandledException) {
        this.unhandledException = unhandledException;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

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
        if (oas3) {
            // remove swagger2 imports
            codegenModel.imports.remove("ApiModelProperty");
            codegenModel.imports.remove("ApiModel");
        }
        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        // Add imports for Jackson
        final List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final List<Object> models = (List<Object>) objs.get("models");
        for (final Object _mo : models) {
            final Map<String, Object> mo = (Map<String, Object>) _mo;
            final CodegenModel cm = (CodegenModel) mo.get("model");
            // for enum model
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                cm.imports.add(importMapping.get("JsonValue"));
                final Map<String, String> item = new HashMap<>();
                item.put("import", importMapping.get("JsonValue"));
                imports.add(item);
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
}
