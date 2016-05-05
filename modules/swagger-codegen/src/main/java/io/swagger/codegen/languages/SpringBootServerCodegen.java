package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import java.io.File;
import java.util.*;

public class SpringBootServerCodegen extends JavaClientCodegen implements CodegenConfig{
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    protected String title = "Petstore Server";
    protected String configPackage = "";
    protected String basePackage = "";
    protected String templateFileName = "api.mustache";

    public SpringBootServerCodegen() {
        super();
        outputFolder = "generated-code/javaSpringBoot";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put(templateFileName, ".java");
        embeddedTemplateDir = templateDir = "JavaSpringBoot";
        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";
        configPackage = "io.swagger.configuration";
        invokerPackage = "io.swagger.api";
        basePackage = "io.swagger";
        artifactId = "swagger-springboot-server";

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("title", title);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CONFIG_PACKAGE, configPackage);
        additionalProperties.put(BASE_PACKAGE, basePackage);

        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code"));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package for generated code"));
        
        supportedLibraries.clear();
        supportedLibraries.put(DEFAULT_LIBRARY, "Default Spring Boot server stub.");
        supportedLibraries.put("j8-async", "Use async servlet feature and Java 8's default interface. Generating interface with service " +
                "declaration is useful when using Maven plugin. Just provide a implementation with @Controller to instantiate service.");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "springboot";
    }

    @Override
    public String getHelp() {
        return "Generates a Java SpringBoot Server application using the SpringFox integration.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        }

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("apiException.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiException.java"));
        supportingFiles.add(new SupportingFile("apiOriginFilter.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiOriginFilter.java"));
        supportingFiles.add(new SupportingFile("apiResponseMessage.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiResponseMessage.java"));
        supportingFiles.add(new SupportingFile("notFoundException.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "NotFoundException.java"));

        supportingFiles.add(new SupportingFile("swaggerDocumentationConfig.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "SwaggerDocumentationConfig.java"));
        supportingFiles.add(new SupportingFile("homeController.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "HomeController.java"));

        supportingFiles.add(new SupportingFile("swagger2SpringBoot.mustache",
                (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator), "Swagger2SpringBoot.java"));
        
        
        supportingFiles.add(new SupportingFile("application.properties",
                ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));

    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        String basePath = resourcePath;
        if (basePath.startsWith("/")) {
            basePath = basePath.substring(1);
        }
        int pos = basePath.indexOf("/");
        if (pos > 0) {
            basePath = basePath.substring(0, pos);
        }

        if (basePath == "") {
            basePath = "default";
        } else {
            if (co.path.startsWith("/" + basePath)) {
                co.path = co.path.substring(("/" + basePath).length());
            }
            co.subresourceOperation = !co.path.isEmpty();
        }
        List<CodegenOperation> opList = operations.get(basePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(basePath, opList);
        }
        opList.add(co);
        co.baseName = basePath;
    }
    
    @Override
    public void preprocessSwagger(Swagger swagger) {
    	System.out.println("preprocessSwagger");
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }

        String host = swagger.getHost();
        String port = "8080";
        if (host != null) {
            String[] parts = host.split(":");
            if (parts.length > 1) {
                port = parts[1];
            }
        }
        
        this.additionalProperties.put("serverPort", port);
        if (swagger != null && swagger.getPaths() != null) {
            for (String pathname : swagger.getPaths().keySet()) {
                Path path = swagger.getPath(pathname);
                if (path.getOperations() != null) {
                    for (Operation operation : path.getOperations()) {
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
                            operation.setVendorExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                    }
                }

                if (operation.returnType == null) {
                    operation.returnType = "Void";
                } else if (operation.returnType.startsWith("List")) {
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
        if("j8-async".equals(getLibrary())) {
            apiTemplateFiles.remove(this.templateFileName);
            this.templateFileName = "api-j8-async.mustache";
            apiTemplateFiles.put(this.templateFileName, ".java");

            int originalPomFileIdx = -1;
            for (int i = 0; i < supportingFiles.size(); i++) {
                if ("pom.xml".equals(supportingFiles.get(i).destinationFilename)) {
                    originalPomFileIdx = i;
                    break;
                }
            }
            if (originalPomFileIdx > -1) {
                supportingFiles.remove(originalPomFileIdx);
            }
            supportingFiles.add(new SupportingFile("pom-j8-async.mustache", "", "pom.xml"));
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

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public void setBasePackage(String configPackage) {
        this.basePackage = configPackage;
    }
    
    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove the import of "Object" to avoid compilation error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.endsWith(".Object")) iterator.remove();
        }
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (CodegenProperty var : cm.vars) {
                // handle default value for enum, e.g. available => StatusEnum.available
                if (var.isEnum && var.defaultValue != null && !"null".equals(var.defaultValue)) {
                    var.defaultValue = var.datatypeWithEnum + "." + var.defaultValue;
                }
            }
        }
        return objs;
    }
}
