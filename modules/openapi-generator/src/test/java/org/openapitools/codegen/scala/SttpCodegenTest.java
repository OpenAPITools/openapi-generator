package org.openapitools.codegen.scala;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;
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

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

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

    @Test
    public void verifyApiKeyLocations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13474.json", null, new ParseOptions()).getOpenAPI();

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

        Path path = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/api/DefaultApi.scala");
        assertFileContains(path, ".method(Method.GET, uri\"$baseUrl/entities/?api_key=${apiKeyQuery}\")\n");
        assertFileContains(path, ".header(\"X-Api-Key\", apiKeyHeader)");
        assertFileContains(path, ".cookie(\"apikey\", apiKeyCookie)");
    }

    @Test
    public void circeSerdeWithMixedCaseFields() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/mixed-case-fields.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttpClientCodegen codegen = new ScalaSttpClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("jsonLibrary", "circe");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
        generator.opts(input).generate();

        Path modelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/MixedCaseModel.scala");
        Path jsonSupportPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/core/JsonSupport.scala");

        // Model should have camelCase Scala field names in the case class
        assertFileContains(modelPath, "firstName");
        assertFileContains(modelPath, "phoneNumber");
        assertFileContains(modelPath, "lastName");
        assertFileContains(modelPath, "zipCode");
        assertFileContains(modelPath, "address");

        // Encoder should use original baseName for JSON keys
        assertFileContains(modelPath, "\"first-name\"");    // kebab-case preserved
        assertFileContains(modelPath, "\"phone_number\"");   // snake_case preserved
        assertFileContains(modelPath, "\"lastName\"");       // camelCase preserved
        assertFileContains(modelPath, "\"ZipCode\"");        // PascalCase preserved
        assertFileContains(modelPath, "\"address\"");        // lowercase preserved

        // Decoder should use original baseName in downField
        assertFileContains(modelPath, "c.downField(\"first-name\")");
        assertFileContains(modelPath, "c.downField(\"phone_number\")");
        assertFileContains(modelPath, "c.downField(\"ZipCode\")");

        // Model should have explicit encoder/decoder companion object
        assertFileContains(modelPath, "object MixedCaseModel");
        assertFileContains(modelPath, "implicit val encoderMixedCaseModel");
        assertFileContains(modelPath, "implicit val decoderMixedCaseModel");

        // Model should import JsonSupport for enum implicits
        assertFileContains(modelPath, "import org.openapitools.client.core.JsonSupport._");

        // JsonSupport should NOT use AutoDerivation (we have explicit instances now)
        assertFileNotContains(jsonSupportPath, "AutoDerivation");

        // AdditionalTypeSerializers should have File and Any codecs for circe
        Path additionalSerializersPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/core/AdditionalTypeSerializers.scala");
        assertFileContains(additionalSerializersPath, "FileDecoder");
        assertFileContains(additionalSerializersPath, "FileEncoder");
        assertFileContains(additionalSerializersPath, "AnyDecoder");
        assertFileContains(additionalSerializersPath, "AnyEncoder");
    }

    @Test
    public void headerSerialization() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_21602.yaml", null, new ParseOptions()).getOpenAPI();

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

        Path path = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/api/DefaultApi.scala");
        assertFileContains(path, ".method(Method.GET, uri\"$baseUrl/ping\")\n");
        assertFileContains(path, "xOptionalHeader: Option[String] = None");
        assertFileContains(path, ".header(\"X-Optional-Header\", xOptionalHeader.map(_.toString()))");
        assertFileContains(path, "xRequiredHeader: String");
        assertFileContains(path, ".header(\"X-Required-Header\", xRequiredHeader.toString)");
        assertFileContains(path, "xOptionalSchemaHeader: Option[UUID] = None");
        assertFileContains(path, ".header(\"X-Optional-Schema-Header\", xOptionalSchemaHeader.map(_.toString()))");
        assertFileContains(path, "xRequiredSchemaHeader: UUID");
        assertFileContains(path, ".header(\"X-Required-Schema-Header\", xRequiredSchemaHeader.toString)");
    }

}
