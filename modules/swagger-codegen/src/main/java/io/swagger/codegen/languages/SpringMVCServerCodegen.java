package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Iterator;

public class SpringMVCServerCodegen extends JavaClientCodegen implements CodegenConfig {
    protected String title = "Petstore Server";
    protected String configPackage = "";

    public SpringMVCServerCodegen() {
        super();
        outputFolder = "generated-code/javaSpringMVC";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        templateDir = "JavaSpringMVC";
        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";
        configPackage = "io.swagger.configuration";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-spring-mvc-server";

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("title", title);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put("configPackage", configPackage);

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float")
        );

        cliOptions.add(new CliOption("configPackage", "configuration package for generated code"));

    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "spring-mvc";
    }

    public String getHelp() {
        return "Generates a Java Spring-MVC Server application using the SpringFox integration.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("configPackage")) {
            this.setConfigPackage((String) additionalProperties.get("configPackage"));
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

        supportingFiles.add(new SupportingFile("swaggerConfig.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "SwaggerConfig.java"));
        supportingFiles.add(new SupportingFile("webApplication.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "WebApplication.java"));
        supportingFiles.add(new SupportingFile("webMvcConfiguration.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "WebMvcConfiguration.java"));
        supportingFiles.add(new SupportingFile("swaggerUiConfiguration.mustache",
                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "SwaggerUiConfiguration.java"));
        supportingFiles.add(new SupportingFile("swagger.properties",
                ("src.main.resources").replace(".", java.io.File.separator), "swagger.properties"));

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
                System.out.println(operation.operationId);
                io.swagger.util.Json.prettyPrint(operation);

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

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove the import of "Object" to avoid compilation error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.endsWith(".Object")) iterator.remove();
        }
        return objs;
    }
}
