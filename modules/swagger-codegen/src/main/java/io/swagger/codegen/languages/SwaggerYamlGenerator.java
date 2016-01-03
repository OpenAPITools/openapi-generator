package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Swagger;
import io.swagger.util.Yaml;
import org.apache.commons.io.FileUtils;

import java.io.File;

public class SwaggerYamlGenerator extends DefaultCodegen implements CodegenConfig {
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
            System.out.println("wrote file to " + outputFile);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}