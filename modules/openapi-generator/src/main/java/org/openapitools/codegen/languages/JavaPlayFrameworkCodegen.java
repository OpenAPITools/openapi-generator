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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaPlayFrameworkCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    private final Logger LOGGER = LoggerFactory.getLogger(JavaPlayFrameworkCodegen.class);
    public static final String TITLE = "title";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String CONTROLLER_ONLY = "controllerOnly";
    public static final String USE_INTERFACES = "useInterfaces";
    public static final String HANDLE_EXCEPTIONS = "handleExceptions";
    public static final String WRAP_CALLS = "wrapCalls";
    public static final String USE_SWAGGER_UI = "useSwaggerUI";
    public static final String SUPPORT_ASYNC = "supportAsync";

    private static final String X_JWKS_URL = "x-jwksUrl";
    private static final String X_TOKEN_INTROSPECT_URL = "x-tokenIntrospectUrl";


    protected String title = "openapi-java-playframework";
    protected String configPackage = "org.openapitools.configuration";
    protected String basePackage = "org.openapitools";
    protected boolean controllerOnly = false;
    protected boolean useInterfaces = true;
    protected boolean useBeanValidation = true;
    protected boolean handleExceptions = true;
    protected boolean wrapCalls = true;
    protected boolean useSwaggerUI = true;
    protected boolean supportAsync = false;

    public JavaPlayFrameworkCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        outputFolder = "generated-code/javaPlayFramework";
        apiTestTemplateFiles.clear();
        embeddedTemplateDir = templateDir = "JavaPlayFramework";
        apiPackage = "controllers";
        modelPackage = "apimodels";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-java-playframework";

        projectFolder = "";
        sourceFolder = projectFolder + "/app";
        projectTestFolder = projectFolder + "/test";
        testFolder = projectTestFolder;

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.SOURCE_FOLDER, this.getSourceFolder());
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        additionalProperties.put("java8", true);
        additionalProperties.put(JACKSON, "true");

        cliOptions.add(new CliOption(TITLE, "server title name or client service name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code").defaultValue(getConfigPackage()));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package for generated code").defaultValue(getBasePackage()));

        //Custom options for this generator
        cliOptions.add(createBooleanCliWithDefault(CONTROLLER_ONLY, "Whether to generate only API interface stubs without the server files.", controllerOnly));
        cliOptions.add(createBooleanCliWithDefault(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(createBooleanCliWithDefault(USE_INTERFACES, "Makes the controllerImp implements an interface to facilitate automatic completion when updating from version x to y of your spec", useInterfaces));
        cliOptions.add(createBooleanCliWithDefault(HANDLE_EXCEPTIONS, "Add a 'throw exception' to each controller function. Add also a custom error handler where you can put your custom logic", handleExceptions));
        cliOptions.add(createBooleanCliWithDefault(WRAP_CALLS, "Add a wrapper to each controller function to handle things like metrics, response modification, etc..", wrapCalls));
        cliOptions.add(createBooleanCliWithDefault(USE_SWAGGER_UI, "Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies", useSwaggerUI));
        cliOptions.add(createBooleanCliWithDefault(SUPPORT_ASYNC, "Support Async operations", supportAsync));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-play-framework";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Play Framework Server application.";
    }

    @Override
    public void processOpts() {
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

        if (additionalProperties.containsKey(CONTROLLER_ONLY)) {
            this.setControllerOnly(convertPropertyToBoolean(CONTROLLER_ONLY));
        }
        writePropertyBack(CONTROLLER_ONLY, controllerOnly);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(USE_INTERFACES)) {
            this.setUseInterfaces(convertPropertyToBoolean(USE_INTERFACES));
        }
        writePropertyBack(USE_INTERFACES, useInterfaces);

        if (additionalProperties.containsKey(HANDLE_EXCEPTIONS)) {
            this.setHandleExceptions(convertPropertyToBoolean(HANDLE_EXCEPTIONS));
        }
        writePropertyBack(HANDLE_EXCEPTIONS, handleExceptions);

        if (additionalProperties.containsKey(WRAP_CALLS)) {
            this.setWrapCalls(convertPropertyToBoolean(WRAP_CALLS));
        }
        writePropertyBack(WRAP_CALLS, wrapCalls);

        if (additionalProperties.containsKey(USE_SWAGGER_UI)) {
            this.setUseSwaggerUI(convertPropertyToBoolean(USE_SWAGGER_UI));
        }
        writePropertyBack(USE_SWAGGER_UI, useSwaggerUI);

        if (additionalProperties.containsKey(SUPPORT_ASYNC)) {
            this.setSupportAsync(convertPropertyToBoolean(SUPPORT_ASYNC));
        }
        writePropertyBack(SUPPORT_ASYNC, supportAsync);

        //We don't use annotation anymore
        importMapping.remove("ApiModelProperty");
        importMapping.remove("ApiModel");

        //Root folder
        supportingFiles.add(new SupportingFile("README.mustache", "", "README"));
        supportingFiles.add(new SupportingFile("LICENSE.mustache", "", "LICENSE"));
        supportingFiles.add(new SupportingFile("build.mustache", "", "build.sbt"));

        //Project folder
        supportingFiles.add(new SupportingFile("buildproperties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("plugins.mustache", "project", "plugins.sbt"));

        //Conf folder
        supportingFiles.add(new SupportingFile("logback.mustache", "conf", "logback.xml"));
        supportingFiles.add(new SupportingFile("application.mustache", "conf", "application.conf"));
        supportingFiles.add(new SupportingFile("routes.mustache", "conf", "routes"));

        //App/Utils folder
        if (!this.controllerOnly && this.useInterfaces) {
            supportingFiles.add(new SupportingFile("module.mustache", "app", "Module.java"));
        }
        supportingFiles.add(new SupportingFile("openapiUtils.mustache", "app/openapitools", "OpenAPIUtils.java"));
        supportingFiles.add(new SupportingFile("securityApiUtils.mustache", "app/openapitools", "SecurityAPIUtils.java"));
        if (this.handleExceptions) {
            supportingFiles.add(new SupportingFile("errorHandler.mustache", "app/openapitools", "ErrorHandler.java"));
        }

        if (this.wrapCalls) {
            supportingFiles.add(new SupportingFile("apiCall.mustache", "app/openapitools", "ApiCall.java"));
        }

        if (this.useSwaggerUI) {
            //App/Controllers
            supportingFiles.add(new SupportingFile("openapi.mustache", "public", "openapi.json"));
            supportingFiles.add(new SupportingFile("apiDocController.mustache", String.format(Locale.ROOT, "app/%s", apiPackage.replace(".", File.separator)), "ApiDocController.java"));
        }

        //We remove the default api.mustache that is used
        apiTemplateFiles.remove("api.mustache");
        apiTemplateFiles.put("newApiController.mustache", "Controller.java");
        if (!this.controllerOnly) {
            apiTemplateFiles.put("newApi.mustache", "ControllerImp.java");
            if (this.useInterfaces) {
                apiTemplateFiles.put("newApiInterface.mustache", "ControllerImpInterface.java");
            }
        }

        additionalProperties.put("javaVersion", "1.8");
        additionalProperties.put("jdk8", "true");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");

        importMapping.put("InputStream", "java.io.InputStream");
        typeMapping.put("file", "InputStream");
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        //We don't use annotation anymore
        model.imports.remove("ApiModelProperty");
        model.imports.remove("ApiModel");
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (codegenModel.description != null) {
            codegenModel.imports.remove("ApiModel");
        }
        return codegenModel;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getConfigPackage() {
        return configPackage;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public String getBasePackage() {
        return basePackage;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public void setControllerOnly(boolean controllerOnly) {
        this.controllerOnly = controllerOnly;
    }

    public void setUseInterfaces(boolean useInterfaces) {
        this.useInterfaces = useInterfaces;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public void setHandleExceptions(boolean handleExceptions) {
        this.handleExceptions = handleExceptions;
    }

    public void setWrapCalls(boolean wrapCalls) {
        this.wrapCalls = wrapCalls;
    }

    public void setUseSwaggerUI(boolean useSwaggerUI) {
        this.useSwaggerUI = useSwaggerUI;
    }

    public void setSupportAsync(boolean supportAsync) {
        this.supportAsync = supportAsync;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");

        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {

                for (CodegenParameter param : operation.allParams) {
                    if (param.isFormParam && param.isFile) {
                        param.dataType = "Http.MultipartFormData.FilePart<TemporaryFile>";
                    }
                }

                for (CodegenParameter param : operation.formParams) {
                    if (param.isFile) {
                        param.dataType = "Http.MultipartFormData.FilePart<TemporaryFile>";
                    }
                }

                Pattern pathVariableMatcher = Pattern.compile("\\{([^}]+)}");
                Matcher match = pathVariableMatcher.matcher(operation.path);
                while (match.find()) {
                    String completeMatch = match.group();
                    String replacement = ":" + camelize(match.group(1), true);
                    operation.path = operation.path.replace(completeMatch, replacement);
                }

                if (operation.returnType != null) {
                    if (operation.returnType.equals("Boolean")) {
                        operation.vendorExtensions.put("x-missing-return-info-if-needed", "true");
                    }
                    if (operation.returnType.equals("BigDecimal")) {
                        operation.vendorExtensions.put("x-missing-return-info-if-needed", "1.0");
                    }
                    if (operation.returnType.startsWith("List")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("List<".length(), end).trim();
                            operation.returnTypeIsPrimitive = languageSpecificPrimitives().contains(operation.returnType) || operation.returnType == null;
                            operation.returnContainer = "List";
                        }
                    } else if (operation.returnType.startsWith("Map")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("Map<".length(), end).split(",")[1].trim();
                            operation.returnTypeIsPrimitive = languageSpecificPrimitives().contains(operation.returnType) || operation.returnType == null;
                            operation.returnContainer = "Map";
                        }
                    } else if (operation.returnType.startsWith("Set")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("Set<".length(), end).trim();
                            operation.returnTypeIsPrimitive = languageSpecificPrimitives().contains(operation.returnType) || operation.returnType == null;
                            operation.returnContainer = "Set";
                        }
                    }
                }
            }
        }

        return objs;
    }

    private CliOption createBooleanCliWithDefault(String optionName, String description, boolean defaultValue) {
        CliOption defaultOption = CliOption.newBoolean(optionName, description);
        defaultOption.setDefault(Boolean.toString(defaultValue));
        return defaultOption;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        List<? extends CodegenSecurity> securities = super.fromSecurity(securitySchemeMap);
        List<CodegenSecurity> extendedSecurities = new ArrayList<>();

        for (CodegenSecurity codegenSecurity : securities) {
            ExtendedCodegenSecurity extendedCodegenSecurity = new ExtendedCodegenSecurity(codegenSecurity);
            Object jwksUrl = extendedCodegenSecurity.vendorExtensions.get(X_JWKS_URL);

            if (jwksUrl instanceof String) {
                extendedCodegenSecurity.jwksUrl = (String) jwksUrl;
            }

            Object tokenIntrospectUrl = extendedCodegenSecurity.vendorExtensions.get(X_TOKEN_INTROSPECT_URL);

            if (tokenIntrospectUrl instanceof String) {
                extendedCodegenSecurity.tokenIntrospectUrl = (String) tokenIntrospectUrl;
            }
            extendedSecurities.add(extendedCodegenSecurity);
        }

        return extendedSecurities;
    }


    class ExtendedCodegenSecurity extends CodegenSecurity {
        public String jwksUrl;
        public String tokenIntrospectUrl;

        public ExtendedCodegenSecurity(CodegenSecurity cm) {
            super();

            this.name = cm.name;
            this.type = cm.type;
            this.scheme = cm.scheme;
            this.isBasic = cm.isBasic;
            this.isOAuth = cm.isOAuth;
            this.isApiKey = cm.isApiKey;
            this.isBasicBasic = cm.isBasicBasic;
            this.isBasicBearer = cm.isBasicBearer;
            this.isHttpSignature = cm.isHttpSignature;
            this.bearerFormat = cm.bearerFormat;
            this.vendorExtensions = new HashMap<String, Object>(cm.vendorExtensions);
            this.keyParamName = cm.keyParamName;
            this.isKeyInQuery = cm.isKeyInQuery;
            this.isKeyInHeader = cm.isKeyInHeader;
            this.isKeyInCookie = cm.isKeyInCookie;
            this.flow = cm.flow;
            this.authorizationUrl = cm.authorizationUrl;
            this.tokenUrl = cm.tokenUrl;
            this.refreshUrl = cm.refreshUrl;
            this.scopes = cm.scopes;
            this.isCode = cm.isCode;
            this.isPassword = cm.isPassword;
            this.isApplication = cm.isApplication;
            this.isImplicit = cm.isImplicit;
        }

        @Override
        public CodegenSecurity filterByScopeNames(List<String> filterScopes) {
            CodegenSecurity codegenSecurity = super.filterByScopeNames(filterScopes);
            ExtendedCodegenSecurity extendedCodegenSecurity =  new ExtendedCodegenSecurity(codegenSecurity);
            extendedCodegenSecurity.jwksUrl = this.jwksUrl;
            extendedCodegenSecurity.tokenIntrospectUrl = this.tokenIntrospectUrl;
            return extendedCodegenSecurity;
        }

        @Override
        public boolean equals(Object o) {
            boolean result = super.equals(o);
            JavaPlayFrameworkCodegen.ExtendedCodegenSecurity that = (JavaPlayFrameworkCodegen.ExtendedCodegenSecurity) o;
            return result &&
                    Objects.equals(jwksUrl, that.jwksUrl) &&
                    Objects.equals(tokenIntrospectUrl, that.tokenIntrospectUrl);

        }

        @Override
        public int hashCode() {
            int superHash = super.hashCode();
            return Objects.hash(superHash, tokenIntrospectUrl, jwksUrl);
        }

        @Override
        public String toString() {
            String superString = super.toString();
            final StringBuilder sb = new StringBuilder(superString);
            sb.append(", jwksUrl='").append(jwksUrl).append('\'');
            sb.append(", tokenIntrospectUrl='").append(tokenIntrospectUrl).append('\'');
            return sb.toString();
        }

    }
}
