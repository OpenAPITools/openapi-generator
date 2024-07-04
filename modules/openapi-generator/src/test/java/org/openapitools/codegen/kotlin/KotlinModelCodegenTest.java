package org.openapitools.codegen.kotlin;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.openapitools.codegen.languages.KotlinServerCodegen;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.languages.KotlinVertxServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class KotlinModelCodegenTest {

    @DataProvider(name = "generators")
    public Object[][] client() {
        return new Object[][]{
                {new KotlinClientCodegen()},
                {new KotlinServerCodegen()},
                {new KotlinSpringServerCodegen()},
                {new KotlinVertxServerCodegen()},
        };
    }

    @Test(dataProvider = "generators")
    public void modelAsValues(AbstractKotlinCodegen codegen) throws IOException {
        String classNameProp = "val className";
        String colorProp = "val color";

        checkModel(codegen, false, classNameProp, colorProp);
    }

    @Test(dataProvider = "generators")
    private void modelMutable(AbstractKotlinCodegen codegen) throws IOException {
        String classNameProp = "var className";
        String colorProp = "var color";

        checkModel(codegen, true, classNameProp, colorProp);
    }

    private void checkModel(AbstractKotlinCodegen codegen, boolean mutable, String... props) throws IOException {
        String outputPath = generateModels(codegen, "src/test/resources/3_0/generic.yaml", mutable);
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/models/Animal.kt"), props);
    }

    private String generateModels(AbstractKotlinCodegen codegen, String fileName, boolean mutable) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(fileName, null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "models");
        codegen.additionalProperties().put(AbstractKotlinCodegen.MODEL_MUTABLE, mutable);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        generator.opts(input).generate();

        return outputPath;
    }

    @Test(dataProvider = "generators")
    public void valuesArrayWithUniqueItems(AbstractKotlinCodegen codegen) throws IOException {
        String outputPath = generateModels(codegen, "src/test/resources/3_0/issue_9848.yaml", false);

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/models/NonUniqueArray.kt"),
                codegen instanceof KotlinVertxServerCodegen
                        ? "val array: kotlin.Array<kotlin.String>"
                        : "val array: kotlin.collections.List<kotlin.String>"
        );

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/models/UniqueArray.kt"),
                "val array: kotlin.collections.Set<kotlin.String>");
    }

    @Test(dataProvider = "generators")
    public void mutableArrayWithUniqueItems(AbstractKotlinCodegen codegen) throws IOException {
        String outputPath = generateModels(codegen, "src/test/resources/3_0/issue_9848.yaml", true);

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/models/NonUniqueArray.kt"),
                codegen instanceof KotlinVertxServerCodegen
                        ? "var array: kotlin.Array<kotlin.String>"
                        : "var array: kotlin.collections.MutableList<kotlin.String>"
        );

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/models/UniqueArray.kt"),
                "var array: kotlin.collections.MutableSet<kotlin.String>");
    }
}
