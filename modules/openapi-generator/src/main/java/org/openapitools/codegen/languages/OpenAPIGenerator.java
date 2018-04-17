package org.openapitools.codegen.languages;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openapitools.codegen.*;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.core.util.Json;

public class OpenAPIGenerator extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(OpenAPIGenerator.class);

    public OpenAPIGenerator() {
        super();
        embeddedTemplateDir = templateDir = "openapi";
        outputFolder = "generated-code/openapi";

        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "openapi";
    }

    @Override
    public String getHelp() {
        return "Creates a static openapi.json file (OpenAPI spec v3.0).";
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        String swaggerString = Json.pretty(openAPI);

        try {
            String outputFile = outputFolder + File.separator + "openapi.json";
            FileUtils.writeStringToFile(new File(outputFile), swaggerString);
            LOGGER.info("wrote file to " + outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }
}
