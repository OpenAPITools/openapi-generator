package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;


public class JavaMicronautServerCodegen extends JavaMicronautAbstractCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String NAME = "java-micronaut-server";

    public JavaMicronautServerCodegen() {
        super();

        title = "OpenAPI Micronaut Server";
        apiPackage = "org.openapitools.controller";
        apiDocPath = "docs/controllers";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        additionalProperties.put("server", "true");

        // Api file
        // The api name should end with "Controller"
        setApiNamePrefix("");
        setApiNameSuffix("Controller");
        apiTemplateFiles.clear();
        apiTemplateFiles.put("server/controller.mustache", ".java");

        // Set the type mappings
        // It could be also StreamingFileUpload
        typeMapping.put("file", "CompletedFileUpload");
        importMapping.put("CompletedFileUpload", "io.micronaut.http.multipart.CompletedFileUpload");
        // TODO handle multipart with multiple files and same parameter name
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public String getHelp() {
        return "Generates a Java Micronaut Server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Add test files
        apiTestTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        if (testTool.equals(OPT_TEST_JUNIT)) {
            apiTestTemplateFiles.put("server/test/controller_test.mustache", ".java");
            modelTestTemplateFiles.put("server/test/model_test.mustache", ".java");
        } else if (testTool.equals(OPT_TEST_SPOCK)) {
            apiTestTemplateFiles.put("server/test/controller_test.groovy.mustache", ".groovy");
            modelTestTemplateFiles.put("server/test/model_test.groovy.mustache", ".groovy");
        }

        // Add documentation files
        supportingFiles.add(new SupportingFile("server/doc/README.mustache", "", "README.md").doNotOverwrite());
        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("server/doc/controller_doc.mustache", ".md");

        // Add Application.java file
        String invokerFolder = (sourceFolder + '/' + invokerPackage).replace('.', '/');
        supportingFiles.add(new SupportingFile("common/configuration/Application.mustache", invokerFolder, "Application.java"));
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        super.setParameterExampleValue(p);

        if (p.isFile) {
            // The CompletedFileUpload cannot be initialized
            p.example = "null";
        }
    }
}
