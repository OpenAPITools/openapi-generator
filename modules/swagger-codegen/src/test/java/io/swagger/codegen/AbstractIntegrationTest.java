package io.swagger.codegen;

import org.testng.annotations.Test;
import org.testng.reporters.Files;

import java.io.IOException;
import java.util.Map;

import io.swagger.codegen.utils.IntegrationTestPathsConfig;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import static io.swagger.codegen.utils.AssertFile.assertPathEqualsRecursively;

public abstract class AbstractIntegrationTest {

    protected abstract IntegrationTestPathsConfig getIntegrationTestPathsConfig();

    protected abstract CodegenConfig getCodegenConfig();

    protected abstract Map<String, String> configProperties();

    @Test
    public void generatesCorrectDirectoryStructure() throws IOException {
        DefaultGenerator codeGen = new DefaultGenerator();
        IntegrationTestPathsConfig integrationTestPathsConfig = getIntegrationTestPathsConfig();

        String specContent = Files.readFile(integrationTestPathsConfig.getSpecPath().toFile());
        Swagger swagger = new SwaggerParser().parse(specContent);

        CodegenConfig codegenConfig = getCodegenConfig();
        codegenConfig.setOutputDir(integrationTestPathsConfig.getOutputPath().toString());

        ClientOpts clientOpts = new ClientOpts();
        clientOpts.setProperties(configProperties());
        ClientOptInput opts = new ClientOptInput()
                .config(codegenConfig)
                .opts(clientOpts)
                .swagger(swagger);

        codeGen.opts(opts).generate();

        assertPathEqualsRecursively(integrationTestPathsConfig.getExpectedPath(), integrationTestPathsConfig.getOutputPath());
    }
}
