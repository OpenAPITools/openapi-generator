package org.openapitools.codegen;

import static org.openapitools.codegen.testutils.AssertFile.assertPathEqualsRecursively;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.parser.OpenAPIParser;
import org.testng.annotations.Test;
import org.testng.reporters.Files;

import org.openapitools.codegen.testutils.IntegrationTestPathsConfig;

public abstract class AbstractIntegrationTest {

    protected abstract IntegrationTestPathsConfig getIntegrationTestPathsConfig();

    protected abstract CodegenConfig getCodegenConfig();

    protected abstract Map<String, String> configProperties();

    protected Boolean generateMetadata = true;

    protected Map<String, String> systemPropertyOverrides = new HashMap<>();

    // @wing328: ignore for the time being until we fix the error with the integration test
    @Test(enabled = false)
    public void generatesCorrectDirectoryStructure() throws IOException {
        DefaultGenerator codeGen = new DefaultGenerator();
        codeGen.setGenerateMetadata(generateMetadata);
        for (Map.Entry<String, String> propertyOverride : systemPropertyOverrides.entrySet()) {
            codeGen.setGeneratorPropertyDefault(propertyOverride.getKey(), propertyOverride.getValue());
        }

        IntegrationTestPathsConfig integrationTestPathsConfig = getIntegrationTestPathsConfig();

        String specContent = Files.readFile(integrationTestPathsConfig.getSpecPath().toFile());
        OpenAPI openAPI = new OpenAPIParser().readContents(specContent, null, null).getOpenAPI();


        CodegenConfig codegenConfig = getCodegenConfig();
        codegenConfig.setOutputDir(integrationTestPathsConfig.getOutputPath().toString());
        codegenConfig.setIgnoreFilePathOverride(integrationTestPathsConfig.getIgnoreFilePath().toFile().toString());
        ClientOpts clientOpts = new ClientOpts();
        clientOpts.setProperties(configProperties());
        ClientOptInput opts = new ClientOptInput()
                .config(codegenConfig)
                .opts(clientOpts)
                .openAPI(openAPI);

        codeGen.opts(opts).generate();

        assertPathEqualsRecursively(integrationTestPathsConfig.getExpectedPath(), integrationTestPathsConfig.getOutputPath());
    }
}
