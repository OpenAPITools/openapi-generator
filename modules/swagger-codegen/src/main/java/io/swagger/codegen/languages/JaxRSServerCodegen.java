package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.util.Json;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class JaxRSServerCodegen extends JavaClientCodegen implements CodegenConfig {
    public static final String DEFAULT_IMPL_SOURCE_FOLDER = "src/main/java";
    protected String invokerPackage = "io.swagger.api";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-jaxrs-server";
    protected String artifactVersion = "1.0.0";
    protected String title = "Swagger Server";

    public JaxRSServerCodegen() {
        super.processOpts();

        sourceFolder = "target/generated-sources";

        outputFolder = System.getProperty("swagger.codegen.jaxrs.genfolder", "target/generated-sources");
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiService.mustache", ".java");
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        apiTemplateFiles.put("apiServiceFactory.mustache", ".java");
        templateDir = "JavaJaxRS";
        apiPackage = System.getProperty("swagger.codegen.jaxrs.apipackage", "io.swagger.api");
        modelPackage = System.getProperty("swagger.codegen.jaxrs.modelpackage", "io.swagger.model");

        additionalProperties.put("invokerPackage", invokerPackage);
        additionalProperties.put("groupId", groupId);
        additionalProperties.put("artifactId", artifactId);
        additionalProperties.put("artifactVersion", artifactVersion);
        additionalProperties.put("title", title);


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
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "jaxrs";
    }

    public String getHelp() {
        return "Generates a Java JAXRS Server application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiException.java"));
        supportingFiles.add(new SupportingFile("ApiOriginFilter.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiOriginFilter.java"));
        supportingFiles.add(new SupportingFile("ApiResponseMessage.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiResponseMessage.java"));
        supportingFiles.add(new SupportingFile("NotFoundException.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "NotFoundException.java"));
        supportingFiles.add(new SupportingFile("web.mustache",
                ("src/main/webapp/WEB-INF"), "web.xml"));

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
                if (operation.returnType == null) {
                    operation.returnType = "Void";
                } else if (operation.returnType.startsWith("List")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("List<".length(), end);
                        operation.returnContainer = "List";
                    }
                } else if (operation.returnType.startsWith("Map")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("Map<".length(), end);
                        operation.returnContainer = "Map";
                    }
                } else if (operation.returnType.startsWith("Set")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("Set<".length(), end);
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
        name = name.replaceAll("[^a-zA-Z0-9]+", "_");
        return camelize(name) + "Api";
    }

    @Override
    public String apiFilename(String templateName, String tag) {

        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/impl" + result.substring(ix, result.length() - 5) + "ServiceImpl.java";

            String output = System.getProperty("swagger.codegen.jaxrs.impl.source", DEFAULT_IMPL_SOURCE_FOLDER);
            if (output != null) {
                result = result.replace(apiFileFolder(), implFileFolder(output));
            }
        } else if (templateName.endsWith("Factory.mustache")) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/factories" + result.substring(ix, result.length() - 5) + "ServiceFactory.java";

            String output = System.getProperty("swagger.codegen.jaxrs.impl.source", DEFAULT_IMPL_SOURCE_FOLDER);
            if (output != null) {
                result = result.replace(apiFileFolder(), implFileFolder(output));
            }
        } else if (templateName.endsWith("Service.mustache")) {
            int ix = result.lastIndexOf('.');
            result = result.substring(0, ix) + "Service.java";
        }

        return result;
    }

    @Override
    public void processSwagger(Swagger swagger) {
        super.processSwagger(swagger);

        try {
            File file = new File( outputFolder + "/src/main/resources/swagger.json" );
            file.getParentFile().mkdirs();

            FileWriter swaggerFile = new FileWriter(file);
            swaggerFile.write( Json.pretty(swagger));
            swaggerFile.flush();
            swaggerFile.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', File.separatorChar);
    }

    public boolean shouldOverwrite(String filename) {
        return filename.startsWith( outputFolder + File.separatorChar + sourceFolder);

//        return super.shouldOverwrite(filename) && !filename.endsWith("ServiceImpl.java")
//                && !filename.endsWith("ServiceFactory.java");
    }
}
