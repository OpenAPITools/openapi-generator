package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.io.File;

public class OpenAPIYamlGenerator extends DefaultCodegen implements CodegenConfig {
    public static final String OUTPUT_NAME = "outputFile";

    private static final Logger LOGGER = LoggerFactory.getLogger(OpenAPIYamlGenerator.class);

    protected String outputFile = "openapi.yaml";

    public OpenAPIYamlGenerator() {
        super();
        embeddedTemplateDir = templateDir = "openapi-yaml";
        outputFolder = "generated-code/openapi-yaml";
        cliOptions.add(new CliOption(OUTPUT_NAME, "output filename"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("openapi.mustache",
                "openapi",
                "openapi.yaml"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "openapi-yaml";
    }

    @Override
    public String getHelp() {
        return "Creates a static openapi.yaml file (OpenAPI spec v3).";
    }


    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(OUTPUT_NAME)) {
            this.outputFile = additionalProperties.get(OUTPUT_NAME).toString();
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
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
