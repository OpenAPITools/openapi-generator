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
import io.swagger.util.Yaml;

public class SwaggerYamlGenerator extends DefaultCodegen implements CodegenConfig {
	
    private static final Logger LOGGER = LoggerFactory.getLogger(SwaggerYamlGenerator.class);

    public SwaggerYamlGenerator() {
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
        return "swagger-yaml";
    }

    @Override
    public String getHelp() {
        return "Creates a static swagger.yaml file.";
    }

    @Override
    public void processSwagger(Swagger swagger) {
        try {
            String swaggerString = Yaml.mapper().writeValueAsString(swagger);
            String outputFile = outputFolder + File.separator + "swagger.yaml";
            FileUtils.writeStringToFile(new File(outputFile), swaggerString);
            LOGGER.debug("wrote file to " + outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
    }
}