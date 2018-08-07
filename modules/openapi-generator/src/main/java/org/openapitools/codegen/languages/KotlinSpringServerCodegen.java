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

public class KotlinSpringServerCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(KotlinSpringServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "kotlin-spring";
    }

    public String getHelp() {
        return "Generates a kotlin-spring server.";
    }

    public KotlinSpringServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "kotlin-spring";
        modelTemplateFiles.put("model.mustache", ".zz");
        apiTemplateFiles.put("api.mustache", ".zz");
        embeddedTemplateDir = templateDir = "kotlin-spring";
        apiPackage = File.separator + "Apis";
        modelPackage = File.separator + "Models";
        // TODO: Fill this out.
    }
}
