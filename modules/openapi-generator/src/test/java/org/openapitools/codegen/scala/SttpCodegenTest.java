package org.openapitools.codegen.scala;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;

public class SttpCodegenTest {

    private final ScalaSttpClientCodegen codegen = new ScalaSttpClientCodegen();

    @Test
    public void encodePath() {
        Assert.assertEquals(codegen.encodePath("{user_name}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{userName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{UserName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("user_name"), "user_name");
        Assert.assertEquals(codegen.encodePath("before/{UserName}/after"), "before/${userName}/after");
    }

    @Test
    public void typeByteArray() {
      final Schema<?> schema = new Schema<Object>()
          .description("Schema with byte string");
      schema.setType("string");
      schema.setFormat("byte");
      String type = codegen.getTypeDeclaration(schema);
      Assert.assertEquals(type, "Array[Byte]");
    }

    @Test
    public void verifyOperatorName() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/issue_10187_operatorName.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttpClientCodegen codegen = new ScalaSttpClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Condition.scala");
        assertFileContains(path, "object ConditionEnums");
        assertFileContains(path, "val Equal = Value(\"&#x3D;\")");
        assertFileContains(path, "val NotEqual = Value(\"!&#x3D;\")");
        assertFileNotContains(path, "val X3D = Value(\"&#x3D;\")");
        assertFileNotContains(path, "val X3D = Value(\"!&#x3D;\")");
    }

}
