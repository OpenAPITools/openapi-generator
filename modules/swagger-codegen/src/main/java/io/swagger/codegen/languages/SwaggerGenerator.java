package io.swagger.codegen.languages;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Swagger;
import io.swagger.util.Json;

public class SwaggerGenerator extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(SwaggerGenerator.class);

    public SwaggerGenerator() {
        super();
        embeddedTemplateDir = templateDir = "swagger";
        outputFolder = "generated-code/swagger";

        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "swagger";
    }

    @Override
    public String getHelp() {
        return "Creates a static swagger.json file.";
    }

    @Override
    public void processSwagger(Swagger swagger) {
        String swaggerString = Json.pretty(swagger);

        try {
            String outputFile = outputFolder + File.separator + "swagger.json";
            FileUtils.writeStringToFile(new File(outputFile), swaggerString);
            LOGGER.debug("wrote file to " + outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
    }
}