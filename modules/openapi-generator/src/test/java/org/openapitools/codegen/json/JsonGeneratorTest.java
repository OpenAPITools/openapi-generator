package org.openapitools.codegen.json;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.OpenAPIGenerator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class JsonGeneratorTest {

    @Test
    public void testGeneratePing() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();
        assertFileContains(Paths.get(outputPath + "/openapi.json"));
        assertFileContains(Paths.get(outputPath + "/README.md"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator-ignore"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator/FILES"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator/VERSION"));

        output.deleteOnExit();
    }


    @Test
    public void testGeneratePingOtherOutputFile() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(OpenAPIGenerator.OUTPUT_NAME, "ping.json");

        File output = Files.createTempDirectory("test").toFile();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();
        assertFileContains(Paths.get(outputPath + "/ping.json"));
        assertFileContains(Paths.get(outputPath + "/README.md"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator-ignore"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator/FILES"));
        assertFileContains(Paths.get(outputPath + "/.openapi-generator/VERSION"));

        output.deleteOnExit();
    }
}
