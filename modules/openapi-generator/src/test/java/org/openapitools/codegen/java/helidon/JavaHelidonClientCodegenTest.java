package org.openapitools.codegen.java.helidon;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.JavaHelidonClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class JavaHelidonClientCodegenTest {

    @DataProvider(name = "helidonLibraries")
    public Object[][] helidonLibraries() {
        return new Object[][] {
                { "se" },
                { "mp" }
        };
    }

    @Test(dataProvider = "helidonLibraries")
    public void testUseOneOfInterfaceOptionGeneratesInterface(String lib) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_5381.yaml", null, new ParseOptions()).getOpenAPI();

        JavaHelidonClientCodegen codegen = new JavaHelidonClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);
        codegen.setLibrary(lib);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/client/model/Foo.java"), "public class Foo implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/client/model/FooRef.java"), "public class FooRef implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/client/model/FooRefOrValue.java"), "public interface FooRefOrValue");
    }
}
