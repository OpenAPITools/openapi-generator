package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;

import java.io.File;

public class StaticDocCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-client";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "docs";

    public StaticDocCodegen() {
        super();
        outputFolder = "docs";
        modelTemplateFiles.put("model.mustache", ".html");
        apiTemplateFiles.put("operation.mustache", ".html");
        embeddedTemplateDir = templateDir = "swagger-static";

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.js"));
        supportingFiles.add(new SupportingFile("assets/css/bootstrap-responsive.css",
                outputFolder + "/assets/css", "bootstrap-responsive.css"));
        supportingFiles.add(new SupportingFile("assets/css/bootstrap.css",
                outputFolder + "/assets/css", "bootstrap.css"));
        supportingFiles.add(new SupportingFile("assets/css/style.css",
                outputFolder + "/assets/css", "style.css"));
        supportingFiles.add(new SupportingFile("assets/images/logo.png",
                outputFolder + "/assets/images", "logo.png"));
        supportingFiles.add(new SupportingFile("assets/js/bootstrap.js",
                outputFolder + "/assets/js", "bootstrap.js"));
        supportingFiles.add(new SupportingFile("assets/js/jquery-1.8.3.min.js",
                outputFolder + "/assets/js", "jquery-1.8.3.min.js"));
        supportingFiles.add(new SupportingFile("assets/js/main.js",
                outputFolder + "/assets/js", "main.js"));
        supportingFiles.add(new SupportingFile("index.mustache",
                outputFolder, "index.html"));

        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "dynamic-html";
    }

    @Override
    public String getHelp() {
        return "Generates a dynamic HTML site.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "operations";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "models";
    }
}