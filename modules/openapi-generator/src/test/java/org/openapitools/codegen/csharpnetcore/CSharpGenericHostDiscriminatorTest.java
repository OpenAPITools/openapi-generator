package org.openapitools.codegen.csharpnetcore;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.openapitools.codegen.DefaultGeneratorTestUtil.generateModels;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertEqualsNoOrder;

public class CSharpGenericHostDiscriminatorTest {

    public Path outputDirectory;

    @BeforeTest
    public void SetUp() throws Exception
    {
        outputDirectory = Files.createTempDirectory(CSharpGenericHostDiscriminatorTest.class.getName());
    }

    @AfterTest
    public void TearDown() throws Exception
    {
        deleteFile(outputDirectory.toFile());
    }

    @Test(description = "Composed model does not have properties itself")
    public void oneOfWithReferencesAndDiscriminatorTest() {
        OpenAPI openApiSpec = TestUtils.parseSpec("src/test/resources/3_0/oneOfDiscriminator.yaml");

        DefaultGenerator generator = getCSharpGeneratorFor(openApiSpec);
        Map<String, CodegenModel> generatedModels = generateModels(generator);

        CodegenModel fruit = generatedModels.get("FruitReqDisc");
        assertEquals(fruit.allVars.size(), 0);

        CodegenModel apple = generatedModels.get("AppleReqDisc");
        List<String> actualVariables = apple.allVars.stream().map(var -> var.baseName).collect(toList());
        assertEqualsNoOrder(actualVariables, asList("seeds", "fruitType"));

        CodegenModel banana = generatedModels.get("BananaReqDisc");
        actualVariables = banana.allVars.stream().map(var -> var.baseName).collect(toList());
        assertEqualsNoOrder(actualVariables, asList("length", "fruitType"));
    }

    private DefaultGenerator getCSharpGeneratorFor(OpenAPI openApiSpec) {
        CodegenConfig csharpConfig = new CSharpClientCodegen();
        csharpConfig.setOutputDir(outputDirectory.toString());
        csharpConfig.setLibrary("generichost");

        ClientOptInput generatorInputOptions = new ClientOptInput();
        generatorInputOptions.openAPI(openApiSpec);
        generatorInputOptions.config(csharpConfig);

        DefaultGenerator generator = new DefaultGenerator(true); // enable dryrun for faster performance
        generator.opts(generatorInputOptions);
        return generator;
    }

    private static void deleteFile(File file)
    {
        File[] childFiles = file.listFiles();
        if (childFiles != null) {
            for (File childFile : childFiles) {
                deleteFile(childFile);
            }
        }
        file.delete();
    }
}
