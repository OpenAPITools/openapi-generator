package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.codegen.*;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.util.Yaml;

import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JavaInflectorServerCodegen extends JavaClientCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaInflectorServerCodegen.class);

    protected String title = "Swagger Inflector";

    public JavaInflectorServerCodegen() {
        super();

        sourceFolder = "src/main/java";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        embeddedTemplateDir = templateDir = "JavaInflector";
        invokerPackage = "io.swagger.handler";
        artifactId = "swagger-inflector-server";

        apiPackage = System.getProperty("swagger.codegen.inflector.apipackage", "io.swagger.handler");
        modelPackage = System.getProperty("swagger.codegen.inflector.modelpackage", "io.swagger.model");

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("title", title);

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "byte[]",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float")
        );
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "inflector";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Inflector Server application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("web.mustache", "src/main/webapp/WEB-INF", "web.xml"));
        supportingFiles.add(new SupportingFile("inflector.mustache", "", "inflector.yaml"));
        supportingFiles.add(new SupportingFile("swagger.mustache",
                        "src/main/swagger",
                        "swagger.yaml")
        );
        supportingFiles.add(new SupportingFile("StringUtil.mustache",
                (sourceFolder + '/' + invokerPackage).replace(".", "/"), "StringUtil.java"));
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(p);
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
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Swagger swagger = (Swagger)objs.get("swagger");
        if(swagger != null) {
            try {
                objs.put("swagger-yaml", Yaml.mapper().writeValueAsString(swagger));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultController";
        }
        name = name.replaceAll("[^a-zA-Z0-9]+", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        return camelize(name)+ "Controller";
    }

    @Override
    public boolean shouldOverwrite(String filename) {
        return super.shouldOverwrite(filename)  &&
        !filename.endsWith("pom.xml") &&
        !filename.endsWith("README.md") &&
        !filename.endsWith("inflector.yaml");
    }
}
