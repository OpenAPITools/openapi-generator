package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CsharpNetcoreFunctionsServerCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static final Logger LOGGER = LoggerFactory.getLogger(CsharpNetcoreFunctionsServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "csharp-netcore-functions";
    }

    public String getHelp() {
        return "Generates a csharp server.";
    }

    public CsharpNetcoreFunctionsServerCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "csharp";
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("functions.mustache", ".cs");
        embeddedTemplateDir = templateDir = "csharp";
        apiPackage = "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "generatedSrc", "README.md"));
        supportingFiles.add(new SupportingFile("project.mustache", "generatedSrc", "project.json"));
    }
	
	@Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "generatedSrc" + File.separator + "Functions";
    }
	
	@Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "generatedSrc" + File.separator +  "Models";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/Docs").replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + "Tests" + File.separator + "Tests" + File.separator + apiPackage();
    }

}
