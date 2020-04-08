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
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.OptionalFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class SpringCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures,
        OptionalFeatures {
    private static final Logger LOGGER = LoggerFactory.getLogger(SpringCodegen.class);

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

    public SpringCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

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
        additionalProperties.put("jackson", "true");
        additionalProperties.put("openbrace", OPEN_BRACE);
        additionalProperties.put("closebrace", CLOSE_BRACE);

        cliOptions.add(new CliOption(TITLE, "server title name or client service name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code").defaultValue(this.getConfigPackage()));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package (invokerPackage) for generated code").defaultValue(this.getBasePackage()));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY, "Whether to generate only API interface stubs without the server files.", interfaceOnly));
        cliOptions.add(CliOption.newBoolean(DELEGATE_PATTERN, "Whether to generate the server files using the delegate pattern", delegatePattern));
        cliOptions.add(CliOption.newBoolean(SINGLE_CONTENT_TYPES, "Whether to select only one produces/consumes content-type by operation.", singleContentTypes));
        updateJava8CliOptions();
        cliOptions.add(CliOption.newBoolean(SKIP_DEFAULT_INTERFACE, "Whether to generate default implementations for java8 interfaces", skipDefaultInterface));
        cliOptions.add(CliOption.newBoolean(ASYNC, "use async Callable controllers", async));
        cliOptions.add(CliOption.newBoolean(REACTIVE, "wrap responses in Mono/Flux Reactor types (spring-boot only)", reactive));
        cliOptions.add(new CliOption(RESPONSE_WRAPPER, "wrap the responses in given type (Future, Callable, CompletableFuture,ListenableFuture, DeferredResult, HystrixCommand, RxObservable, RxSingle or fully qualified type)"));
        cliOptions.add(CliOption.newBoolean(VIRTUAL_SERVICE, "Generates the virtual service. For more details refer - https://github.com/elan-venture/virtualan/wiki"));
        cliOptions.add(CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames", useTags));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Use Bean Validation Impl. to perform BeanValidation", performBeanValidation));
        cliOptions.add(CliOption.newBoolean(IMPLICIT_HEADERS, "Skip header parameters in the generated API methods using @ApiImplicitParams annotation.", implicitHeaders));
        cliOptions.add(CliOption.newBoolean(OPENAPI_DOCKET_CONFIG, "Generate Spring OpenAPI Docket configuration class.", openapiDocketConfig));
        cliOptions.add(CliOption.newBoolean(API_FIRST, "Generate the API from the OAI spec at server compile time (API first approach)", apiFirst));
        cliOptions.add(CliOption.newBoolean(USE_OPTIONAL, "Use Optional container for optional parameters", useOptional));
        cliOptions.add(CliOption.newBoolean(HATEOAS, "Use Spring HATEOAS library to allow adding HATEOAS links", hateoas));
        cliOptions.add(CliOption.newBoolean(RETURN_SUCCESS_CODE, "Generated server returns 2xx code", returnSuccessCode));
        cliOptions.add(CliOption.newBoolean(UNHANDLED_EXCEPTION_HANDLING, "Declare operation methods to throw a generic exception and allow unhandled exceptions (useful for Spring `@ControllerAdvice` directives).", unhandledException));

        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application using the SpringFox integration.");
        supportedLibraries.put(SPRING_MVC_LIBRARY, "Spring-MVC Server application using the SpringFox integration.");
        supportedLibraries.put(SPRING_CLOUD_LIBRARY, "Spring-Cloud-Feign client with Spring-Boot auto-configured settings.");
        setLibrary(SPRING_BOOT);
        CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC).defaultValue(SPRING_BOOT);
        library.setEnum(supportedLibraries);
        cliOptions.add(library);

    }

    private void updateJava8CliOptions() {
        CliOption option = cliOptions.stream().filter(o -> JAVA_8.equals(o.getOpt())).findFirst()
                .orElseThrow(() -> new RuntimeException("Missing java8 option"));
        Map<String, String> java8ModeOptions = option.getEnum();
        java8ModeOptions.put("true", "Use Java 8 classes such as Base64. Use java8 default interface when a responseWrapper is used");
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

        List<Pair<String, String>> configOptions = additionalProperties.entrySet().stream()
                .filter(e -> !Arrays.asList(API_FIRST, "hideGenerationTimestamp").contains(e.getKey()))
                .filter(e -> cliOptions.stream().map(CliOption::getOpt).anyMatch(opt -> opt.equals(e.getKey())))
                .map(e -> Pair.of(e.getKey(), e.getValue().toString()))
                .collect(Collectors.toList());
        additionalProperties.put("configOptions", configOptions);

        // Process java8 option before common java ones to change the default dateLibrary to java8.
        LOGGER.info("----------------------------------");
        if (additionalProperties.containsKey(JAVA_8)) {
            LOGGER.info("has JAVA8");
            this.setJava8(Boolean.valueOf(additionalProperties.get(JAVA_8).toString()));
            additionalProperties.put(JAVA_8, java8);
        }
        if (this.java8 && !additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary("java8");
        }

        if (!additionalProperties.containsKey(BASE_PACKAGE) && additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            // set invokerPackage as basePackage:
            this.setBasePackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            additionalProperties.put(BASE_PACKAGE, basePackage);
            LOGGER.info("Set base package to invoker package (" + basePackage + ")");
        }

        super.processOpts();

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
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
            this.setVirtualService(Boolean.valueOf(additionalProperties.get(VIRTUAL_SERVICE).toString()));
        }

        if (additionalProperties.containsKey(INTERFACE_ONLY)) {
            this.setInterfaceOnly(Boolean.valueOf(additionalProperties.get(INTERFACE_ONLY).toString()));
        }

        if (additionalProperties.containsKey(DELEGATE_PATTERN)) {
            this.setDelegatePattern(Boolean.valueOf(additionalProperties.get(DELEGATE_PATTERN).toString()));
        }

        if (additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
            this.setSingleContentTypes(Boolean.valueOf(additionalProperties.get(SINGLE_CONTENT_TYPES).toString()));
        }

        if (additionalProperties.containsKey(SKIP_DEFAULT_INTERFACE)) {
            this.setSkipDefaultInterface(Boolean.valueOf(additionalProperties.get(SKIP_DEFAULT_INTERFACE).toString()));
        }

        if (additionalProperties.containsKey(ASYNC)) {
            this.setAsync(Boolean.valueOf(additionalProperties.get(ASYNC).toString()));
            //fix for issue/1164
            convertPropertyToBooleanAndWriteBack(ASYNC);
        }

        if (additionalProperties.containsKey(REACTIVE)) {
            if (!SPRING_BOOT.equals(library)) {
                throw new IllegalArgumentException("Currently, reactive option is only supported with Spring-boot");
            }
            this.setReactive(Boolean.valueOf(additionalProperties.get(REACTIVE).toString()));
        }

        if (additionalProperties.containsKey(RESPONSE_WRAPPER)) {
            this.setResponseWrapper((String) additionalProperties.get(RESPONSE_WRAPPER));
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            this.setUseTags(Boolean.valueOf(additionalProperties.get(USE_TAGS).toString()));
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
            this.setImplicitHeaders(Boolean.valueOf(additionalProperties.get(IMPLICIT_HEADERS).toString()));
        }

        if (additionalProperties.containsKey(OPENAPI_DOCKET_CONFIG)) {
            this.setOpenapiDocketConfig(Boolean.valueOf(additionalProperties.get(OPENAPI_DOCKET_CONFIG).toString()));
        }

        if (additionalProperties.containsKey(API_FIRST)) {
            this.setApiFirst(Boolean.valueOf(additionalProperties.get(API_FIRST).toString()));
        }

        if (additionalProperties.containsKey(HATEOAS)) {
            this.setHateoas(Boolean.valueOf(additionalProperties.get(HATEOAS).toString()));
        }

        if (additionalProperties.containsKey(RETURN_SUCCESS_CODE)) {
            this.setReturnSuccessCode(Boolean.valueOf(additionalProperties.get(RETURN_SUCCESS_CODE).toString()));
        }

        if (additionalProperties.containsKey(UNHANDLED_EXCEPTION_HANDLING)) {
            this.setUnhandledException(Boolean.valueOf(additionalProperties.get(UNHANDLED_EXCEPTION_HANDLING).toString()));
        }
        additionalProperties.put(UNHANDLED_EXCEPTION_HANDLING, this.isUnhandledException());

        typeMapping.put("file", "Resource");
        importMapping.put("Resource", "org.springframework.core.io.Resource");

        if (useOptional) {
            writePropertyBack(USE_OPTIONAL, useOptional);
        }

        if (this.interfaceOnly && this.delegatePattern) {
            if (this.java8) {
                this.delegateMethod = true;
                additionalProperties.put("delegate-method", true);
            } else {
                throw new IllegalArgumentException(
                        String.format(Locale.ROOT, "Can not generate code with `%s` and `%s` true while `%s` is false.",
                                DELEGATE_PATTERN, INTERFACE_ONLY, JAVA_8));
            }
        }

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (!this.interfaceOnly) {
            if (library.equals(SPRING_BOOT)) {
                supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator), "OpenAPI2SpringBoot.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator), "RFC3339DateFormat.java"));
            }
            if (library.equals(SPRING_MVC_LIBRARY)) {
                supportingFiles.add(new SupportingFile("webApplication.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "WebApplication.java"));
                supportingFiles.add(new SupportingFile("webMvcConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "WebMvcConfiguration.java"));
                supportingFiles.add(new SupportingFile("openapiUiConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "OpenAPIUiConfiguration.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "RFC3339DateFormat.java"));
            }
            if (library.equals(SPRING_CLOUD_LIBRARY)) {
                supportingFiles.add(new SupportingFile("apiKeyRequestInterceptor.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "ApiKeyRequestInterceptor.java"));
                supportingFiles.add(new SupportingFile("clientConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "ClientConfiguration.java"));
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
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "HomeController.java"));
                if (!this.reactive && !this.apiFirst) {
                    supportingFiles.add(new SupportingFile("openapiDocumentationConfig.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "OpenAPIDocumentationConfig.java"));
                } else {
                    supportingFiles.add(new SupportingFile("openapi.mustache",
                            ("src/main/resources").replace("/", java.io.File.separator), "openapi.yaml"));
                }
            }
        } else if (this.openapiDocketConfig && !library.equals(SPRING_CLOUD_LIBRARY) && !this.reactive && !this.apiFirst) {
            supportingFiles.add(new SupportingFile("openapiDocumentationConfig.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "OpenAPIDocumentationConfig.java"));
        }

        if (!SPRING_CLOUD_LIBRARY.equals(library)) {
            supportingFiles.add(new SupportingFile("apiUtil.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtil.java"));
        }

        if (this.apiFirst) {
            apiTemplateFiles.clear();
            modelTemplateFiles.clear();
        }

        if ("threetenbp".equals(dateLibrary)) {
            supportingFiles.add(new SupportingFile("customInstantDeserializer.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "CustomInstantDeserializer.java"));
            if (library.equals(SPRING_BOOT) || library.equals(SPRING_CLOUD_LIBRARY)) {
                supportingFiles.add(new SupportingFile("jacksonConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "JacksonConfiguration.java"));
            }
        }

        if ((!this.delegatePattern && this.java8) || this.delegateMethod) {
            additionalProperties.put("jdk8-no-delegate", true);
        }


        if (this.delegatePattern && !this.delegateMethod) {
            additionalProperties.put("isDelegate", "true");
            apiTemplateFiles.put("apiDelegate.mustache", "Delegate.java");
        }


        if (this.java8) {
            additionalProperties.put("javaVersion", "1.8");
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                additionalProperties.put("jdk8-default-interface", false);
            } else {
                additionalProperties.put("jdk8-default-interface", !this.skipDefaultInterface);
            }
            additionalProperties.put("jdk8", true);
            if (this.async) {
                additionalProperties.put(RESPONSE_WRAPPER, "CompletableFuture");
            }
            if (this.reactive) {
                additionalProperties.put(RESPONSE_WRAPPER, "Mono");
            }
        } else if (this.async) {
            additionalProperties.put(RESPONSE_WRAPPER, "Callable");
        }


        if (!this.apiFirst && !this.reactive) {
            additionalProperties.put("useSpringfox", true);
        }


        // Some well-known Spring or Spring-Cloud response wrappers
        if (isNotEmpty(this.responseWrapper)) {
            additionalProperties.put("jdk8", false);
            additionalProperties.put("jdk8-default-interface", false);
            switch (this.responseWrapper) {
                case "Future":
                case "Callable":
                case "CompletableFuture":
                    additionalProperties.put(RESPONSE_WRAPPER, "java.util.concurrent." + this.responseWrapper);
                    break;
                case "ListenableFuture":
                    additionalProperties.put(RESPONSE_WRAPPER, "org.springframework.util.concurrent.ListenableFuture");
                    break;
                case "DeferredResult":
                    additionalProperties.put(RESPONSE_WRAPPER, "org.springframework.web.context.request.async.DeferredResult");
                    break;
                case "HystrixCommand":
                    additionalProperties.put(RESPONSE_WRAPPER, "com.netflix.hystrix.HystrixCommand");
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
        additionalProperties.put("lambdaEscapeDoubleQuote",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("lambdaSplitString", new SplitStringLambda());
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if ((library.equals(SPRING_BOOT) || library.equals(SPRING_MVC_LIBRARY)) && !useTags) {
            String basePath = resourcePath;
            if (basePath.startsWith("/")) {
                basePath = basePath.substring(1);
            }
            int pos = basePath.indexOf("/");
            if (pos > 0) {
                basePath = basePath.substring(0, pos);
            }

            if (basePath.equals("")) {
                basePath = "default";
            } else {
                co.subresourceOperation = !co.path.isEmpty();
            }
            List<CodegenOperation> opList = operations.computeIfAbsent(basePath, k -> new ArrayList<>());
            opList.add(co);
            co.baseName = basePath;
        } else {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /* TODO the following logic should not need anymore in OAS 3.0
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }
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

        if(!additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        if (openAPI.getPaths() != null) {
            for (String pathname : openAPI.getPaths().keySet()) {
                PathItem path = openAPI.getPaths().get(pathname);
                if (path.readOperations() != null) {
                    for (Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if (tags.size() > 0) {
                                tags.get(tags.size() - 1).remove("hasMore");
                            }
                            if (operation.getTags().size() > 0) {
                                String tag = operation.getTags().get(0);
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
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (final CodegenOperation operation : ops) {
                List<CodegenResponse> responses = operation.responses;
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
     * @param dataTypeAssigner An object that will assign the data to the respective fields in the model.
     */
    private void doDataTypeAssignment(String returnType, DataTypeAssigner dataTypeAssigner) {
        final String rt = returnType;
        if (rt == null) {
            dataTypeAssigner.setReturnType("Void");
        } else if (rt.startsWith("List")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("List<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (rt.startsWith("Map")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Map<".length(), end).split(",", 2)[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        } else if (rt.startsWith("Set")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Set<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("Set");
            }
        }
    }

    /**
     * This method removes header parameters from the list of parameters and also
     * corrects last allParams hasMore state.
     *
     * @param allParams list of all parameters
     */
    private void removeHeadersFromAllParams(List<CodegenParameter> allParams) {
        if (allParams.isEmpty()) {
            return;
        }
        final ArrayList<CodegenParameter> copy = new ArrayList<>(allParams);
        allParams.clear();

        for (CodegenParameter p : copy) {
            if (!p.isHeaderParam) {
                allParams.add(p);
            }
        }
        if (!allParams.isEmpty()) {
            allParams.get(allParams.size() - 1).hasMore = false;
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        if (library.equals(SPRING_CLOUD_LIBRARY)) {
            List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
            if (authMethods != null) {
                for (CodegenSecurity authMethod : authMethods) {
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
            example = "new org.springframework.core.io.FileSystemResource(new java.io.File(\"" + escapeText(example) + "\"))";
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
        return this.configPackage;
    }

    public boolean isUnhandledException() {
        return unhandledException;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getBasePackage() {
        return this.basePackage;
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

    public void setSkipDefaultInterface(boolean skipDefaultInterface) { this.skipDefaultInterface = skipDefaultInterface; }

    public void setJava8(boolean java8) { this.java8 = java8; }

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

        //Add imports for Jackson
        if (!Boolean.TRUE.equals(model.isEnum)) {
            model.imports.add("JsonProperty");

            if (Boolean.TRUE.equals(model.hasEnums)) {
                model.imports.add("JsonValue");
            }
        } else { // enum class
            //Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey("jackson")) {
                model.imports.add("JsonCreator");
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        //Add imports for Jackson
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

    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }

    @Override
    public void setUseOptional(boolean useOptional) {
        this.useOptional = useOptional;
    }

    @Override
    public void postProcessParameter(CodegenParameter p) {
        // we use a custom version of this function to remove the l, d, and f suffixes from Long/Double/Float
        // defaultValues
        // remove the l because our users will use Long.parseLong(String defaultValue)
        // remove the d because our users will use Double.parseDouble(String defaultValue)
        // remove the f because our users will use Float.parseFloat(String defaultValue)
        // NOTE: for CodegenParameters we DO need these suffixes because those defaultValues are used as java value
        // literals assigned to Long/Double/Float
        if (p.defaultValue == null) {
            return;
        }
        Boolean fixLong = (p.isLong && "l".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
        Boolean fixDouble = (p.isDouble && "d".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
        Boolean fixFloat = (p.isFloat && "f".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
        if (fixLong || fixDouble || fixFloat) {
            p.defaultValue = p.defaultValue.substring(0, p.defaultValue.length()-1);
        }
    }

}
