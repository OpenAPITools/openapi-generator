package io.swagger.codegen.typescript.integrationtest;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.reporters.Files;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.TypeScriptAngular2ClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import static io.swagger.codegen.typescript.integrationtest.AssertFile.assertPathEqualsRecursively;

public class Angular2GenerationWithAditionPropertiesTest {

    private DefaultGenerator codeGen;
    private Path integrationTestPath;
    private Path outputPath;
    private Path specPath;
    private Path expectedPath;

    @BeforeMethod
    public void setUp() {
        codeGen = new DefaultGenerator();
        integrationTestPath = Paths.get("target/test-classes/integrationtest").toAbsolutePath();
        outputPath = integrationTestPath.resolve("typescript/additional-properties-result");
        expectedPath = integrationTestPath.resolve("typescript/additional-properties-expected");
        specPath = integrationTestPath.resolve("typescript/additional-properties-spec.json");

    }

    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptAngular2ClientCodegen();
    }

    protected Map<String, String> configProperties() {
        Map<String, String> propeties = new HashMap<>();
        propeties.put("npmName", "additionalPropertiesTest");
        propeties.put("npmVersion", "1.0.2");
        propeties.put("snapshot", "false");

        return propeties;
    }

    @Test(description = "The correct output is generated for a spec with additional-properties")
    public void shouldGenerateCorrectTypescriptModels() throws IOException {
        String specContent = Files.readFile(specPath.toFile());
        Swagger swagger = new SwaggerParser().parse(specContent);

        CodegenConfig codegenConfig = getCodegenConfig();
        codegenConfig.setOutputDir(outputPath.toString());

        ClientOpts clientOpts = new ClientOpts();
        clientOpts.setProperties(configProperties());
        ClientOptInput opts = new ClientOptInput()
                .config(codegenConfig)
                .opts(clientOpts)
                .swagger(swagger);

        codeGen.opts(opts).generate();

        assertPathEqualsRecursively(expectedPath, outputPath);
    }

}
