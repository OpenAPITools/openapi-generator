package org.openapitools.codegen.json;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.OpenAPIGenerator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
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


    @Test
    public void testSortOutput() throws Exception {
        File output = Files.createTempDirectory("test-sort-json").toFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put(OpenAPIGenerator.SORT_OUTPUT, "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/sort-output-test.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String json = Files.readString(Paths.get(output.getAbsolutePath(), "openapi.json"));

        // Paths alphabetical: /animals, /mammals, /zebra
        int idxAnimals = json.indexOf("\"/animals\"");
        int idxMammals = json.indexOf("\"/mammals\"");
        int idxZebra = json.indexOf("\"/zebra\"");
        Assert.assertTrue(idxAnimals != -1 && idxMammals != -1 && idxZebra != -1, "Expected paths must be present in output");
        Assert.assertTrue(idxAnimals < idxMammals, "/animals must come before /mammals");
        Assert.assertTrue(idxMammals < idxZebra, "/mammals must come before /zebra");

        // Schemas alphabetical: AnimalModel, MammalModel, ZebraModel
        int idxAnimal = json.indexOf("\"AnimalModel\"");
        int idxMammal = json.indexOf("\"MammalModel\"");
        int idxZebraM = json.indexOf("\"ZebraModel\"");
        Assert.assertTrue(idxAnimal != -1 && idxMammal != -1 && idxZebraM != -1, "Expected schemas must be present in output");
        Assert.assertTrue(idxAnimal < idxMammal, "AnimalModel must come before MammalModel");
        Assert.assertTrue(idxMammal < idxZebraM, "MammalModel must come before ZebraModel");

        // Parameters alphabetical: aFilter, mPage, zLimit
        int idxAFilter = json.indexOf("\"aFilter\"");
        int idxMPage = json.indexOf("\"mPage\"");
        int idxZLimit = json.indexOf("\"zLimit\"");
        Assert.assertTrue(idxAFilter != -1 && idxMPage != -1 && idxZLimit != -1, "Expected parameters must be present in output");
        Assert.assertTrue(idxAFilter < idxMPage, "aFilter must come before mPage");
        Assert.assertTrue(idxMPage < idxZLimit, "mPage must come before zLimit");

        // HTTP method order — GET before POST in /zebra (spec has POST first)
        int zebraBlock = json.indexOf("\"/zebra\"");
        int zebraGet = json.indexOf("\"get\"", zebraBlock);
        int zebraPost = json.indexOf("\"post\"", zebraBlock);
        Assert.assertTrue(zebraGet != -1 && zebraPost != -1, "Expected HTTP methods must be present in output");
        Assert.assertTrue(zebraGet < zebraPost, "GET must appear before POST within /zebra");

        // HTTP method order — GET before DELETE in /mammals (spec has DELETE first)
        int mammalsBlock = json.indexOf("\"/mammals\"");
        int mammalsGet = json.indexOf("\"get\"", mammalsBlock);
        int mammalsDelete = json.indexOf("\"delete\"", mammalsBlock);
        Assert.assertTrue(mammalsGet != -1 && mammalsDelete != -1, "Expected HTTP methods must be present in output");
        Assert.assertTrue(mammalsGet < mammalsDelete, "GET must appear before DELETE within /mammals");
    }

    @Test
    public void testSortOutputDisabledPreservesOriginalOrder() throws Exception {
        File output = Files.createTempDirectory("test-nosort-json").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi")
                .setAdditionalProperties(new HashMap<>())
                .setInputSpec("src/test/resources/3_0/sort-output-test.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String json = new String(Files.readAllBytes(Paths.get(output.getAbsolutePath(), "openapi.json")), StandardCharsets.UTF_8);

        // Without sortOutput, paths are in spec order: /zebra first, then /mammals, then /animals
        int idxZebra = json.indexOf("\"/zebra\"");
        int idxMammals = json.indexOf("\"/mammals\"");
        int idxAnimals = json.indexOf("\"/animals\"");
        Assert.assertTrue(idxZebra < idxMammals, "Without sortOutput /zebra must come before /mammals (spec order)");
        Assert.assertTrue(idxMammals < idxAnimals, "Without sortOutput /mammals must come before /animals (spec order)");
    }
}
