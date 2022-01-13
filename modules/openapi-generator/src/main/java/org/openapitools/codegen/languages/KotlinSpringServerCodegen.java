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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class KotlinSpringServerCodegen extends AbstractKotlinCodegen
        implements BeanValidationFeatures {

    private final Logger LOGGER =
            LoggerFactory.getLogger(KotlinSpringServerCodegen.class);

    private static final HashSet<String> VARIABLE_RESERVED_WORDS =
            new HashSet<>(Arrays.asList(
                    "ApiClient",
                    "ApiException",
                    "ApiResponse"
            ));

    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String SPRING_BOOT = "spring-boot";
    public static final String EXCEPTION_HANDLER = "exceptionHandler";
    public static final String GRADLE_BUILD_FILE = "gradleBuildFile";
    public static final String SWAGGER_ANNOTATIONS = "swaggerAnnotations";
    public static final String SERVICE_INTERFACE = "serviceInterface";
    public static final String SERVICE_IMPLEMENTATION = "serviceImplementation";
    public static final String REACTIVE = "reactive";
    public static final String INTERFACE_ONLY = "interfaceOnly";
    public static final String DELEGATE_PATTERN = "delegatePattern";
    public static final String USE_TAGS = "useTags";

    private String basePackage;
    private String invokerPackage;
    private String serverPort = "8080";
    private String title = "OpenAPI Kotlin Spring";
    private String resourceFolder = "src/main/resources";
    private boolean useBeanValidation = true;
    private boolean exceptionHandler = true;
    private boolean gradleBuildFile = true;
    private boolean swaggerAnnotations = false;
    private boolean serviceInterface = false;
    private boolean serviceImplementation = false;
    private boolean reactive = false;
    private boolean interfaceOnly = false;
    private boolean delegatePattern = false;
    protected boolean useTags = false;

    public KotlinSpringServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        reservedWords.addAll(VARIABLE_RESERVED_WORDS);

        outputFolder = "generated-code/kotlin-spring";
        embeddedTemplateDir = templateDir = "kotlin-spring";

        artifactId = "openapi-spring";
        basePackage = invokerPackage = "org.openapitools";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);

        // Use lists instead of arrays
        typeMapping.put("array", "kotlin.collections.List");
        typeMapping.put("list", "kotlin.collections.List");

        // use resource for file handling
        typeMapping.put("file", "org.springframework.core.io.Resource");

        addOption(TITLE, "server title name or client service name", title);
        addOption(BASE_PACKAGE, "base package (invokerPackage) for generated code", basePackage);
        addOption(SERVER_PORT, "configuration the port in which the sever is to run on", serverPort);
        addOption(CodegenConstants.MODEL_PACKAGE, "model package for generated code", modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "api package for generated code", apiPackage);
        addSwitch(EXCEPTION_HANDLER, "generate default global exception handlers (not compatible with reactive. enabling reactive will disable exceptionHandler )", exceptionHandler);
        addSwitch(GRADLE_BUILD_FILE, "generate a gradle build file using the Kotlin DSL", gradleBuildFile);
        addSwitch(SWAGGER_ANNOTATIONS, "generate swagger annotations to go alongside controllers and models", swaggerAnnotations);
        addSwitch(SERVICE_INTERFACE, "generate service interfaces to go alongside controllers. In most " +
                "cases this option would be used to update an existing project, so not to override implementations. " +
                "Useful to help facilitate the generation gap pattern", serviceInterface);
        addSwitch(SERVICE_IMPLEMENTATION, "generate stub service implementations that extends service " +
                "interfaces. If this is set to true service interfaces will also be generated", serviceImplementation);
        addSwitch(USE_BEANVALIDATION, "Use BeanValidation API annotations to validate data types", useBeanValidation);
        addSwitch(REACTIVE, "use coroutines for reactive behavior", reactive);
        addSwitch(INTERFACE_ONLY, "Whether to generate only API interface stubs without the server files.", interfaceOnly);
        addSwitch(DELEGATE_PATTERN, "Whether to generate the server files using the delegate pattern", delegatePattern);
        addSwitch(USE_TAGS, "Whether to use tags for creating interface and controller class names", useTags);
        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application.");
        setLibrary(SPRING_BOOT);

        CliOption cliOpt = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC);
        cliOpt.setDefault(SPRING_BOOT);
        cliOpt.setEnum(supportedLibraries);
        cliOptions.add(cliOpt);
    }

    public String getResourceFolder() {
        return this.resourceFolder;
    }

    public void setResourceFolder(String resourceFolder) {
        this.resourceFolder = resourceFolder;
    }

    public String getBasePackage() {
        return this.basePackage;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getInvokerPackage() {
        return this.invokerPackage;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public String getServerPort() {
        return this.serverPort;
    }

    public void setServerPort(String serverPort) {
        this.serverPort = serverPort;
    }

    public boolean getExceptionHandler() {
        return this.exceptionHandler;
    }

    public void setExceptionHandler(boolean exceptionHandler) {
        this.exceptionHandler = exceptionHandler;
    }

    public boolean getGradleBuildFile() {
        return this.gradleBuildFile;
    }

    public void setGradleBuildFile(boolean gradleBuildFile) {
        this.gradleBuildFile = gradleBuildFile;
    }

    public boolean getSwaggerAnnotations() {
        return this.swaggerAnnotations;
    }

    public void setSwaggerAnnotations(boolean swaggerAnnotations) {
        this.swaggerAnnotations = swaggerAnnotations;
    }

    public boolean getServiceInterface() {
        return this.serviceInterface;
    }

    public void setServiceInterface(boolean serviceInterface) {
        this.serviceInterface = serviceInterface;
    }

    public boolean getServiceImplementation() {
        return this.serviceImplementation;
    }

    public void setServiceImplementation(boolean serviceImplementation) {
        this.serviceImplementation = serviceImplementation;
    }

    public boolean getUseBeanValidation() {
        return this.useBeanValidation;
    }

    public void setInterfaceOnly(boolean interfaceOnly) {
        this.interfaceOnly = interfaceOnly;
    }

    public void setDelegatePattern(boolean delegatePattern) {
        this.delegatePattern = delegatePattern;
    }

    public void setUseTags(boolean useTags) {
        this.useTags = useTags;
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public boolean isReactive() {
        return reactive;
    }

    public void setReactive(boolean reactive) {
        this.reactive = reactive;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-spring";
    }

    @Override
    public String getHelp() {
        return "Generates a Kotlin Spring application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // optional jackson mappings for BigDecimal support
        importMapping.put("ToStringSerializer", "com.fasterxml.jackson.databind.ser.std.ToStringSerializer");
        importMapping.put("JsonSerialize", "com.fasterxml.jackson.databind.annotation.JsonSerialize");

        // Swagger import mappings
        importMapping.put("ApiModel", "io.swagger.annotations.ApiModel");
        importMapping.put("ApiModelProperty", "io.swagger.annotations.ApiModelProperty");

        // Jackson import mappings
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("JsonCreator", "com.fasterxml.jackson.annotation.JsonCreator");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonSubTypes", "com.fasterxml.jackson.annotation.JsonSubTypes");
        importMapping.put("JsonTypeInfo", "com.fasterxml.jackson.annotation.JsonTypeInfo");
        // import JsonCreator if JsonProperty is imported
        // used later in recursive import in postProcessingModels
        importMapping.put("com.fasterxml.jackson.annotation.JsonProperty", "com.fasterxml.jackson.annotation.JsonCreator");

        if (!additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            additionalProperties.put(CodegenConstants.LIBRARY, library);
        }

        // Set basePackage from invokerPackage
        if (!additionalProperties.containsKey(BASE_PACKAGE)
                && additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            additionalProperties.put(BASE_PACKAGE, basePackage);
            LOGGER.info("Set base package to invoker package ({})", basePackage);
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        } else {
            additionalProperties.put(BASE_PACKAGE, basePackage);
        }

        if (additionalProperties.containsKey(SERVER_PORT)) {
            this.setServerPort((String) additionalProperties.get(SERVER_PORT));
        } else {
            additionalProperties.put(SERVER_PORT, serverPort);
        }

        if (additionalProperties.containsKey(EXCEPTION_HANDLER)) {
            this.setExceptionHandler(Boolean.parseBoolean(additionalProperties.get(EXCEPTION_HANDLER).toString()));
        }
        writePropertyBack(EXCEPTION_HANDLER, exceptionHandler);

        if (additionalProperties.containsKey(GRADLE_BUILD_FILE)) {
            this.setGradleBuildFile(Boolean.parseBoolean(additionalProperties.get(GRADLE_BUILD_FILE).toString()));
        }
        writePropertyBack(GRADLE_BUILD_FILE, gradleBuildFile);

        if (additionalProperties.containsKey(SWAGGER_ANNOTATIONS)) {
            this.setSwaggerAnnotations(Boolean.parseBoolean(additionalProperties.get(SWAGGER_ANNOTATIONS).toString()));
        }
        writePropertyBack(SWAGGER_ANNOTATIONS, swaggerAnnotations);

        if (additionalProperties.containsKey(SERVICE_INTERFACE)) {
            this.setServiceInterface(Boolean.parseBoolean(additionalProperties.get(SERVICE_INTERFACE).toString()));
        }
        writePropertyBack(SERVICE_INTERFACE, serviceInterface);

        if (additionalProperties.containsKey(SERVICE_IMPLEMENTATION)) {
            this.setServiceImplementation(Boolean.parseBoolean(additionalProperties.get(SERVICE_IMPLEMENTATION).toString()));
        }
        writePropertyBack(SERVICE_IMPLEMENTATION, serviceImplementation);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(REACTIVE) && library.equals(SPRING_BOOT)) {
            this.setReactive(convertPropertyToBoolean(REACTIVE));
            // spring webflux doesn't support @ControllerAdvice
            this.setExceptionHandler(false);
        }
        writePropertyBack(REACTIVE, reactive);
        writePropertyBack(EXCEPTION_HANDLER, exceptionHandler);

        if (additionalProperties.containsKey(INTERFACE_ONLY)) {
            this.setInterfaceOnly(Boolean.parseBoolean(additionalProperties.get(INTERFACE_ONLY).toString()));
        }

        if (additionalProperties.containsKey(DELEGATE_PATTERN)) {
            this.setDelegatePattern(Boolean.parseBoolean(additionalProperties.get(DELEGATE_PATTERN).toString()));
            if (!this.interfaceOnly) {
                this.setSwaggerAnnotations(true);
            }
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            this.setUseTags(Boolean.parseBoolean(additionalProperties.get(USE_TAGS).toString()));
        }

        modelTemplateFiles.put("model.mustache", ".kt");

        if (!this.interfaceOnly && this.delegatePattern) {
            apiTemplateFiles.put("apiInterface.mustache", ".kt");
            apiTemplateFiles.put("apiController.mustache", "Controller.kt");
        } else if (interfaceOnly) {
            apiTemplateFiles.put("apiInterface.mustache", ".kt");
        } else {
            apiTemplateFiles.put("api.mustache", "Controller.kt");
            apiTestTemplateFiles.put("api_test.mustache", ".kt");
        }

        if (SPRING_BOOT.equals(library)) {
            supportingFiles.add(new SupportingFile("apiUtil.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtil.kt"));
        }

        if (this.serviceInterface) {
            apiTemplateFiles.put("service.mustache", "Service.kt");
        } else if (this.serviceImplementation) {
            LOGGER.warn("If you set `serviceImplementation` to true, `serviceInterface` will also be set to true");
            additionalProperties.put(SERVICE_INTERFACE, true);
            apiTemplateFiles.put("service.mustache", "Service.kt");
            apiTemplateFiles.put("serviceImpl.mustache", "ServiceImpl.kt");
        }

        if (this.delegatePattern) {
            additionalProperties.put("isDelegate", "true");
            apiTemplateFiles.put("apiDelegate.mustache", "Delegate.kt");
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));


        if (this.exceptionHandler) {
            supportingFiles.add(new SupportingFile("exceptions.mustache",
                    sanitizeDirectory(sourceFolder + File.separator + apiPackage), "Exceptions.kt"));
        }

        if (library.equals(SPRING_BOOT)) {
            LOGGER.info("Setup code generator for Kotlin Spring Boot");
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));

            if (this.gradleBuildFile) {
                supportingFiles.add(new SupportingFile("buildGradleKts.mustache", "", "build.gradle.kts"));
                supportingFiles.add(new SupportingFile("settingsGradle.mustache", "", "settings.gradle"));
            }

            if (!this.interfaceOnly) {
                supportingFiles.add(new SupportingFile("application.mustache", resourceFolder, "application.yaml"));
                supportingFiles.add(new SupportingFile("springBootApplication.mustache",
                    sanitizeDirectory(sourceFolder + File.separator + basePackage), "Application.kt"));
            }
        }

        // spring uses the jackson lib, and we disallow configuration.
        additionalProperties.put("jackson", "true");

        // add lambda for mustache templates
        additionalProperties.put("lambdaEscapeDoubleQuote",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("escapeDoubleQuote", new EscapeLambda("\"", "\\\""));
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if (library.equals(SPRING_BOOT) && !useTags) {
            String basePath = resourcePath;
            if (basePath.startsWith("/")) {
                basePath = basePath.substring(1);
            }
            int pos = basePath.indexOf("/");
            if (pos > 0) {
                basePath = basePath.substring(0, pos);
            }

            if (basePath.isEmpty()) {
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

        if (!additionalProperties.containsKey(TITLE)) {
            // The purpose of the title is for:
            // - README documentation
            // - The spring.application.name
            // - And linking the @RequestMapping
            // This is an additional step we add when pre-processing the API spec, if
            // there is no user configuration set for the `title` of the project,
            // we try build and normalise a title from the API spec itself.
            String title = openAPI.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase(Locale.ROOT).endsWith("API"))
                    title = title.substring(0, title.length() - 3);

                this.title = camelize(sanitizeName(title), true);
            }
            additionalProperties.put(TITLE, this.title);
        }

        if (!additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        // TODO: Handle tags
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
        } else {
            //Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey("jackson")) {
                model.imports.add("JsonCreator");
            }
        }

        if (model.discriminator != null && additionalProperties.containsKey("jackson")) {
            model.imports.addAll(Arrays.asList("JsonSubTypes", "JsonTypeInfo"));
        }
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        //Add imports for Jackson
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        List<Object> models = (List<Object>) objs.get("models");

        models.stream()
                .map(mo -> (Map<String, Object>) mo)
                .map(mo -> (CodegenModel) mo.get("model"))
                .filter(cm -> Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null)
                .forEach(cm -> {
                    cm.imports.add(importMapping.get("JsonValue"));
                    cm.imports.add(importMapping.get("JsonProperty"));
                    Map<String, String> itemJsonValue = new HashMap<>();
                    itemJsonValue.put("import", importMapping.get("JsonValue"));
                    imports.add(itemJsonValue);
                    Map<String, String> itemJsonProperty = new HashMap<>();
                    itemJsonProperty.put("import", importMapping.get("JsonProperty"));
                    imports.add(itemJsonProperty);
                });

        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            ops.forEach(operation -> {
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    responses.forEach(resp -> {

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
                    });
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
//                if(implicitHeaders){
//                    removeHeadersFromAllParams(operation.allParams);
//                }
            });
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
    private void doDataTypeAssignment(final String returnType, DataTypeAssigner dataTypeAssigner) {
        if (returnType == null) {
            dataTypeAssigner.setReturnType("Unit");
        } else if (returnType.startsWith("kotlin.collections.List")) {
            int end = returnType.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring("kotlin.collections.List<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (returnType.startsWith("kotlin.collections.Map")) {
            int end = returnType.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring("kotlin.collections.Map<".length(), end).split(",")[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        }
    }

    private static String sanitizeDirectory(String in) {
        return in.replace(".", File.separator);
    }

    // TODO could probably be made more generic, and moved to the `mustache` package if required by other components.
    private static class EscapeLambda implements Mustache.Lambda {
        private String from;
        private String to;

        EscapeLambda(final String from, final String to) {
            this.from = from;
            this.to = Matcher.quoteReplacement(to);
        }

        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            writer.write(fragment.execute().replaceAll(from, to));
        }
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(final String name) {
        // Allow for explicitly configured spring.*
        if (name.startsWith("org.springframework.")) {
            return name;
        }
        return super.toModelName(name);
    }

    /**
     * Check the type to see if it needs import the library/module/package
     *
     * @param type name of the type
     * @return true if the library/module/package of the corresponding type needs to be imported
     */
    @Override
    protected boolean needToImport(String type) {
        // provides extra protection against improperly trying to import language primitives and java types
        boolean imports = !type.startsWith("org.springframework.") && super.needToImport(type);
        return imports;
    }
}
