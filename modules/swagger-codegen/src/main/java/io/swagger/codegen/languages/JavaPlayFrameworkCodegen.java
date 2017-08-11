package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.codegen.*;
import io.swagger.codegen.languages.features.BeanValidationFeatures;
import io.swagger.models.Model;
import io.swagger.models.Swagger;
import io.swagger.util.Json;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JavaPlayFrameworkCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {

    public static final String TITLE = "title";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String CONTROLLER_ONLY = "controllerOnly";
    public static final String USE_INTERFACES = "useInterfaces";
    public static final String HANDLE_EXCEPTIONS = "handleExceptions";
    public static final String WRAP_CALLS = "wrapCalls";
    public static final String USE_SWAGGER_UI = "useSwaggerUI";

    protected String title = "swagger-petstore";
    protected String configPackage = "io.swagger.configuration";
    protected String basePackage = "io.swagger";
    protected boolean controllerOnly = false;
    protected boolean useInterfaces = true;
    protected boolean useBeanValidation = true;
    protected boolean handleExceptions = true;
    protected boolean wrapCalls = true;
    protected boolean useSwaggerUI = true;

    public JavaPlayFrameworkCodegen() {
        super();
        outputFolder = "generated-code/javaPlayFramework";
        apiTestTemplateFiles.clear();
        embeddedTemplateDir = templateDir = "JavaPlayFramework";
        apiPackage = "controllers";
        modelPackage = "apimodels";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-java-playframework";

        projectFolder = "";
        sourceFolder = projectFolder + File.separator + "app";
        projectTestFolder = projectFolder + File.separator + "test";
        testFolder = projectTestFolder;

        additionalProperties.put(CONFIG_PACKAGE, configPackage);
        additionalProperties.put(BASE_PACKAGE, basePackage);
        additionalProperties.put("java8", true);
        additionalProperties.put("jackson", "true");

        cliOptions.add(new CliOption(TITLE, "server title name or client service name"));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code"));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package for generated code"));

        //Custom options for this generator
        cliOptions.add(createBooleanCliWithDefault(CONTROLLER_ONLY, "Whether to generate only API interface stubs without the server files.", controllerOnly));
        cliOptions.add(createBooleanCliWithDefault(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(createBooleanCliWithDefault(USE_INTERFACES, "Makes the controllerImp implements an interface to facilitate automatic completion when updating from version x to y of your spec", useInterfaces));
        cliOptions.add(createBooleanCliWithDefault(HANDLE_EXCEPTIONS, "Add a 'throw exception' to each controller function. Add also a custom error handler where you can put your custom logic", handleExceptions));
        cliOptions.add(createBooleanCliWithDefault(WRAP_CALLS, "Add a wrapper to each controller function to handle things like metrics, response modification, etc..", wrapCalls));
        cliOptions.add(createBooleanCliWithDefault(USE_SWAGGER_UI, "Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies", useSwaggerUI));
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
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
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
        supportingFiles.add(new SupportingFile("swaggerUtils.mustache", "app/swagger", "SwaggerUtils.java"));
        if (this.handleExceptions) {
            supportingFiles.add(new SupportingFile("errorHandler.mustache", "app/swagger", "ErrorHandler.java"));
        }

        if(this.wrapCalls) {
            supportingFiles.add(new SupportingFile("apiCall.mustache", "app/swagger", "ApiCall.java"));
        }

        if(this.useSwaggerUI) {
            //App/Controllers
            supportingFiles.add(new SupportingFile("swagger.mustache", "public", "swagger.json"));
            supportingFiles.add(new SupportingFile("apiDocController.mustache", "app/controllers", "ApiDocController.java"));
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
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);
        if(codegenModel.description != null) {
            codegenModel.imports.remove("ApiModel");
        }
        return codegenModel;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public void setBasePackage(String configPackage) {
        this.basePackage = configPackage;
    }

    public void setControllerOnly(boolean controllerOnly) { this.controllerOnly = controllerOnly; }

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

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {

                for (CodegenParameter param : operation.allParams) {
                    if (param.isFormParam && param.isFile) {
                        param.dataType = "Http.MultipartFormData.FilePart";
                    }
                }

                for (CodegenParameter param : operation.formParams) {
                    if (param.isFile) {
                        param.dataType = "Http.MultipartFormData.FilePart";
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
        Swagger swagger = (Swagger)objs.get("swagger");
        System.out.println("swagger" + swagger.toString());
        if(swagger != null) {
            try {
                objs.put("swagger-json", Json.pretty().writeValueAsString(swagger));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
        return super.postProcessSupportingFileData(objs);
    }
}
