package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class JavaUndertowServerCodegen extends AbstractJavaCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaUndertowServerCodegen.class);

    protected String title = "OpenAPI Undertow Server";
    protected String implFolder = "src/main/java";

    public JavaUndertowServerCodegen() {
        super();

        sourceFolder = "src/main/java";
        apiTestTemplateFiles.clear(); // TODO: add test template
        embeddedTemplateDir = templateDir = "undertow";
        invokerPackage = "io.swagger.handler";
        artifactId = "openapi-undertow-server";
        dateLibrary = "legacy"; //TODO: add joda support

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        apiPackage = System.getProperty("swagger.codegen.undertow.apipackage", "io.swagger.handler");
        modelPackage = System.getProperty("swagger.codegen.undertow.modelpackage", "io.swagger.model");

        additionalProperties.put("title", title);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-undertow-server";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Undertow Server application (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiTemplateFiles.remove("api.mustache");

        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));

        // keep the yaml in config folder for framework validation.
        supportingFiles.add(new SupportingFile("openapi.mustache", ("src.main.resources.config").replace(".", java.io.File.separator), "openapi.json"));
        supportingFiles.add(new SupportingFile("handler.mustache", ("src.main.java.io.swagger.handler").replace(".", java.io.File.separator), "PathHandlerProvider.java"));
        supportingFiles.add(new SupportingFile("service.mustache", ("src.main.resources.META-INF.services").replace(".", java.io.File.separator), "com.networknt.server.HandlerProvider"));

        // configuration files
        supportingFiles.add(new SupportingFile("server.json", ("src.main.resources.config").replace(".", java.io.File.separator), "server.json"));
        supportingFiles.add(new SupportingFile("security.json", ("src.main.resources.config").replace(".", java.io.File.separator), "security.json"));
        supportingFiles.add(new SupportingFile("primary.crt", ("src.main.resources.config.oauth").replace(".", java.io.File.separator), "primary.crt"));

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
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        //Add imports for Jackson
        if (!BooleanUtils.toBoolean(model.isEnum)) {
            model.imports.add("JsonProperty");

            if (BooleanUtils.toBoolean(model.hasEnums)) {
                model.imports.add("JsonValue");
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

    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("api.mustache")) {
            int ix = result.indexOf(sourceFolder);
            String beg = result.substring(0, ix);
            String end = result.substring(ix + sourceFolder.length());
            new java.io.File(beg + implFolder).mkdirs();
            result = beg + implFolder + end;
        }
        return result;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultHandler";
        }
        name = name.replaceAll("[^a-zA-Z0-9]+", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        return camelize(name) + "Handler";
    }
}
