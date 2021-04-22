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

public class TinyCppClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "SHOLOSOSOSO";

    static final Logger LOGGER = LoggerFactory.getLogger(TinyCppClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "tiny-cpp";
    }

    public String getHelp() {
        return "Generates a tiny-cpp client.";
    }

    public TinyCppClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "tiny-cpp";
        
        modelTemplateFiles.put("model.mustache", ".md");
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");
        apiTemplateFiles.put("api.mustache", ".md");
        embeddedTemplateDir = templateDir = "tiny-cpp-client";
        apiPackage = "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("helpers-header.mustache", "", "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", "", "Helpers.cpp"));
        // TODO: Fill this out.
    }
}
