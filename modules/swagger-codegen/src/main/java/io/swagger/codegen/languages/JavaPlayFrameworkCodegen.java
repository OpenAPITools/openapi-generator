package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.codegen.languages.features.BeanValidationFeatures;

import java.io.File;
import java.util.List;
import java.util.Map;

public class JavaPlayFrameworkCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {

    public static final String TITLE = "title";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String CONTROLLER_ONLY = "controllerOnly";
    public static final String SINGLE_CONTENT_TYPES = "singleContentTypes";
    public static final String RESPONSE_WRAPPER = "responseWrapper";
    public static final String USE_TAGS = "useTags";

    protected String title = "swagger-petstore";
    protected String configPackage = "io.swagger.configuration";
    protected String basePackage = "io.swagger";
    protected boolean controllerOnly = false;
    protected boolean singleContentTypes = false;
    protected String responseWrapper = "";
    protected boolean useTags = false;
    protected boolean useBeanValidation = true;

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

        additionalProperties.put("jackson", "true");

        cliOptions.add(new CliOption(TITLE, "server title name or client service name"));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code"));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package for generated code"));
        cliOptions.add(CliOption.newBoolean(CONTROLLER_ONLY, "Whether to generate only API interface stubs without the server files."));
        cliOptions.add(CliOption.newBoolean(SINGLE_CONTENT_TYPES, "Whether to select only one produces/consumes content-type by operation."));
        cliOptions.add(new CliOption(RESPONSE_WRAPPER, "wrap the responses in given type (Future,Callable,CompletableFuture,ListenableFuture,DeferredResult,HystrixCommand,RxObservable,RxSingle or fully qualified type)"));
        cliOptions.add(CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames"));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
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
            this.setControllerOnly(Boolean.valueOf(additionalProperties.get(CONTROLLER_ONLY).toString()));
        }

        if (additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
            this.setSingleContentTypes(Boolean.valueOf(additionalProperties.get(SINGLE_CONTENT_TYPES).toString()));
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

        if (useBeanValidation) {
            writePropertyBack(USE_BEANVALIDATION, useBeanValidation);
        }

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
        supportingFiles.add(new SupportingFile("swaggerUtils.mustache", "app/swagger", "SwaggerUtils.java"));

        //App/Controllers
        supportingFiles.add(new SupportingFile("apiDocController.mustache", "app/controllers", "ApiDocController.java"));

        //We remove the default api.mustache that is used
        apiTemplateFiles.remove("api.mustache");
        apiTemplateFiles.put("newApiController.mustache", "Controller.java");
        if (!this.controllerOnly) {
            apiTemplateFiles.put("newApi.mustache", "ControllerImp.java");
        }

        additionalProperties.put("javaVersion", "1.8");
        additionalProperties.put("jdk8", "true");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");

        // Some well-known Spring or Spring-Cloud response wrappers
        switch (this.responseWrapper) {
            case "Future":
            case "Callable":
            case "CompletableFuture":
                additionalProperties.put(RESPONSE_WRAPPER, "java.util.concurrent" + this.responseWrapper);
                break;
            default:
                break;
        }
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

    public void setSingleContentTypes(boolean singleContentTypes) {
        this.singleContentTypes = singleContentTypes;
    }

    public void setResponseWrapper(String responseWrapper) { this.responseWrapper = responseWrapper; }

    public void setUseTags(boolean useTags) {
        this.useTags = useTags;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {

                //This is to fix this bug in the swagger-play project: https://github.com/swagger-api/swagger-play/issues/131
                //We need to explicitly add the model package name in front of the dataType because if we don't, the
                //implicitParam is not valid and show error when loading the documentation
                //This can be removed safely after the bug has been fixed
                for (CodegenParameter param : operation.allParams) {
                    if (!param.isPathParam ) {
                        if (!param.isPrimitiveType && !param.isListContainer && !param.isMapContainer) {
                            param.dataTypeForImplicitParam = String.format("%s.%s", modelPackage, param.dataType);
                        } else {
                            param.dataTypeForImplicitParam = param.dataType;
                        }
                    }
                }

                if (operation.path.contains("{")) {
                    operation.path = operation.path.replace("{", ":").replace("}", "");
                }

                if (operation.returnType != null) {
                    if (operation.returnType.startsWith("List")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("List<".length(), end).trim();
                            operation.returnContainer = "List";
                        }
                    } else if (operation.returnType.startsWith("Map")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("Map<".length(), end).split(",")[1].trim();
                            operation.returnContainer = "Map";
                        }
                    } else if (operation.returnType.startsWith("Set")) {
                        String rt = operation.returnType;
                        int end = rt.lastIndexOf(">");
                        if (end > 0) {
                            operation.returnType = rt.substring("Set<".length(), end).trim();
                            operation.returnContainer = "Set";
                        }
                    }
                }
            }
        }

        return objs;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }
}
