package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
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

public class CurlClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "cURL requests generator";

    private final Logger LOGGER = LoggerFactory.getLogger(CurlClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "curl";
    }

    public String getHelp() {
        return "Generates a set of cURL requests for each available example request in the specification file";
    }

    public CurlClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "curl";
        apiTemplateFiles.put("api.mustache", ".sh");
        embeddedTemplateDir = templateDir = "curl";
        apiPackage = "Apis";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        // TODO: Fill this out.
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        if (openAPI != null) {
            openAPI.getPaths().forEach((path, config) -> {
                config.readOperations().forEach(operation -> {
                    operation.setTags(Collections.singletonList(operation.getOperationId()));
                });
            });
        }
        super.preprocessOpenAPI(openAPI);
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator was written by Julien Lengrand-Lambert (https://github.com/jlengrand)    #");
        System.out.println("################################################################################");
    }
}
