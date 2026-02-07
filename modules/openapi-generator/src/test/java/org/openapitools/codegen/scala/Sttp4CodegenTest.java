package org.openapitools.codegen.scala;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.ScalaSttp4ClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class Sttp4CodegenTest {

    @Test
    public void verifyApiKeyLocations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13474.json", null, new ParseOptions()).getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
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
    public void verifyOneOfSupportWithCirce() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-oneOf.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
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
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        // Test oneOf without discriminator generates sealed trait with semiauto
        Path petPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileContains(petPath, "sealed trait Pet");
        assertFileContains(petPath, "object Pet {");
        assertFileContains(petPath, "import io.circe.generic.semiauto._");
        assertFileContains(petPath, "// oneOf without discriminator - using semiauto derivation");
        assertFileContains(petPath, "implicit val encoder: Encoder[Pet] = deriveEncoder");
        assertFileContains(petPath, "implicit val decoder: Decoder[Pet] = deriveDecoder");

        // Test oneOf with discriminator uses semiauto with Configuration
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "sealed trait Animal");
        assertFileContains(animalPath, "object Animal {");
        assertFileContains(animalPath, "import io.circe.generic.extras.semiauto._");
        assertFileContains(animalPath, "// oneOf with discriminator - using semiauto derivation with Configuration");
        assertFileContains(animalPath, "private implicit val config: Configuration = Configuration.default.withDiscriminator(\"petType\")");
        assertFileContains(animalPath, "implicit val encoder: Encoder[Animal] = deriveConfiguredEncoder");
        assertFileContains(animalPath, "implicit val decoder: Decoder[Animal] = deriveConfiguredDecoder");

        // Test oneOf with discriminator mapping
        Path vehiclePath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Vehicle.scala");
        assertFileContains(vehiclePath, "sealed trait Vehicle");
        assertFileContains(vehiclePath, "object Vehicle {");
        assertFileContains(vehiclePath, "// oneOf with discriminator - using semiauto derivation with Configuration");
        assertFileContains(vehiclePath, "Configuration.default.withDiscriminator(\"vehicleType\")");

        // Verify regular models are still case classes
        Path dogPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Dog.scala");
        assertFileContains(dogPath, "case class Dog(");
        assertFileContains(dogPath, "name: String");
        assertFileContains(dogPath, "breed: String");
    }

    @Test
    public void verifyOneOfSupportWithJson4s() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-oneOf.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("jsonLibrary", "json4s");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        // Test oneOf without discriminator generates sealed trait with json4s
        Path petPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileContains(petPath, "sealed trait Pet");
        assertFileContains(petPath, "object Pet {");
        assertFileContains(petPath, "import org.json4s._");
        assertFileContains(petPath, "// oneOf without discriminator - json4s custom serializer");
        assertFileContains(petPath, "implicit object PetSerializer extends Serializer[Pet]");
        assertFileContains(petPath, "Extraction.extract[Dog](json)");
        assertFileContains(petPath, "Extraction.extract[Cat](json)");

        // Test oneOf with discriminator
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "sealed trait Animal");
        assertFileContains(animalPath, "// oneOf with discriminator");
        assertFileContains(animalPath, "petType");
    }

    @Test
    public void verifyOneOfWithEmptyMembers() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-oneOf-empty-members.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
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
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        // Test sealed trait is generated correctly
        Path eventPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Event.scala");
        assertFileContains(eventPath, "sealed trait Event");
        
        // Test empty case classes (no properties except discriminator which was removed)
        assertFileContains(eventPath, "case class ClickEvent(\n) extends Event");
        assertFileContains(eventPath, "case class ViewEvent(\n) extends Event");
        
        // Test case class with properties (PurchaseEvent has amount)
        assertFileContains(eventPath, "case class PurchaseEvent(");
        assertFileContains(eventPath, "amount: Double");
        
        // Verify discriminator is configured
        assertFileContains(eventPath, "Configuration.default.withDiscriminator(\"eventType\")");
        
        // Verify the discriminator property was removed from inline members
        // ClickEvent and ViewEvent should have NO properties at all
        assertFileContains(eventPath, "case class ClickEvent(\n) extends Event");
        assertFileContains(eventPath, "case class ViewEvent(\n) extends Event");
    }
}
