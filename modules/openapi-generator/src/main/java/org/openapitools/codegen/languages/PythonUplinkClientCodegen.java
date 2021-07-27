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

public class PythonUplinkClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static final Logger LOGGER = LoggerFactory.getLogger(PythonUplinkClientCodegen.class);

    private static final String SRC_DIR = "src";
    private static final String DEFAULT_PACKAGE_NAME = "openapi_server";
    
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "python-uplink";
    }

    public String getHelp() {
        return "Generates a python-uplink client.";
    }

    public PythonUplinkClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "python-uplink";
//        modelTemplateFiles.put("model.mustache", ".py");
//        apiTemplateFiles.put("apis.mustache", ".py");
        
        supportingFiles.add(new SupportingFile("apis.mustache", "src", "api.py"));
        
        embeddedTemplateDir = templateDir = "python-uplink";
        apiPackage = "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue(DEFAULT_PACKAGE_NAME));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue("1.0.0"));

    }
    
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + SRC_DIR + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + SRC_DIR + File.separator + modelPackage().replace('.', File.separatorChar);
    }

}
