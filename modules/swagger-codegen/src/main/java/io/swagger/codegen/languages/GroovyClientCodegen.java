package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;

import java.io.File;
import java.util.*;

public class GroovyClientCodegen extends JavaClientCodegen {
    public static final String CONFIG_PACKAGE = "configPackage";
    protected String title = "Petstore Server";
    protected String configPackage = "";
    protected String templateFileName = "api.mustache";

    public GroovyClientCodegen() {
        super();

        sourceFolder = projectFolder + File.separator + "groovy";
        outputFolder = "generated-code/groovy";
        modelTemplateFiles.put("model.mustache", ".groovy");
        apiTemplateFiles.put(templateFileName, ".groovy");
        embeddedTemplateDir = templateDir = "Groovy";
        
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
        additionalProperties.put(CONFIG_PACKAGE, configPackage);

        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code"));

        supportedLibraries.clear();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "groovy";
    }

    @Override
    public String getHelp() {
        return "Generates a Groovy API client (beta).";
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

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        // TODO readme to be added later
        //supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiUtils.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtils.groovy"));

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

}
