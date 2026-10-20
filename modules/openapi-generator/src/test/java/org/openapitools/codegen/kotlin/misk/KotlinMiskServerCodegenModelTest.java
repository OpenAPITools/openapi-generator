package org.openapitools.codegen.kotlin.misk;

import io.swagger.v3.oas.models.OpenAPI;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.KotlinMiskServerCodegen;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class KotlinMiskServerCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        KotlinMiskServerCodegen codegen = new KotlinMiskServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.ENABLE_POST_PROCESS_FILE, "true");

        List<File> files = generator.opts(input).generate();

        Assert.assertTrue(files.stream()
            .filter(file -> file.getPath().contains("model"))
            .anyMatch(file -> file.getName().endsWith(".kt")));
    }
}

