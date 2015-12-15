package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Swagger;
import io.swagger.util.Json;
import org.apache.commons.io.FileUtils;

import java.io.File;

public class SwaggerGenerator extends DefaultCodegen implements CodegenConfig {
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
            System.out.println("wrote file to " + outputFile);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}