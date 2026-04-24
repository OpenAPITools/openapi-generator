package org.openapitools.codegen.scala;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

/**
 * Tests for scala-sttp generator with circe JSON library.
 * Covers baseName field mapping, discriminator/polymorphism, and special type handling.
 */
public class ScalaSttpCirceCodegenTest {

    private DefaultGenerator generateFromSpec(String specPath, File output) {
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(specPath, null, new ParseOptions()).getOpenAPI();

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
        return generator;
    }

    @Test(description = "circe encoder/decoder uses baseName for JSON field names")
    public void verifyBaseNameFieldMapping() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        generateFromSpec("src/test/resources/3_0/scala/mixed-case-fields.yaml", output);

        // MixedCaseModel: verify baseName is used in encoder/decoder
        Path modelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/MixedCaseModel.scala");
        assertFileContains(modelPath, "\"first-name\"");
        assertFileContains(modelPath, "\"phone_number\"");
        assertFileContains(modelPath, "\"lastName\"");
        assertFileContains(modelPath, "\"ZipCode\"");
        assertFileContains(modelPath, "c.downField(\"first-name\")");
        assertFileContains(modelPath, "c.downField(\"phone_number\")");
        assertFileContains(modelPath, "c.downField(\"ZipCode\")");
        assertFileContains(modelPath, "implicit val encoderMixedCaseModel");
        assertFileContains(modelPath, "implicit val decoderMixedCaseModel");

        // BinaryPayload: File and untyped object fields
        Path binaryPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/BinaryPayload.scala");
        assertFileContains(binaryPath, "data: Option[File]");
        assertFileContains(binaryPath, "implicit val encoderBinaryPayload");
        assertFileContains(binaryPath, "implicit val decoderBinaryPayload");

        // AdditionalTypeSerializers: File, Any, and NaN-tolerant Double codecs
        Path serializersPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/core/AdditionalTypeSerializers.scala");
        assertFileContains(serializersPath, "FileDecoder");
        assertFileContains(serializersPath, "FileEncoder");
        assertFileContains(serializersPath, "AnyDecoder");
        assertFileContains(serializersPath, "AnyEncoder");
        assertFileContains(serializersPath, "NanTolerantDoubleDecoder");
        assertFileContains(serializersPath, "Double.NaN");
        assertFileContains(serializersPath, "Double.PositiveInfinity");
        assertFileContains(serializersPath, "Double.NegativeInfinity");

        // JsonSupport should NOT use AutoDerivation
        Path jsonSupportPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/core/JsonSupport.scala");
        assertFileNotContains(jsonSupportPath, "AutoDerivation");
    }

    @Test(description = "allOf + discriminator generates sealed trait with discriminator-based circe codecs")
    public void verifyAllOfDiscriminator() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        generateFromSpec("src/test/resources/3_0/scala/mixed-case-fields.yaml", output);

        // Animal should be a sealed trait with base fields as abstract defs
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "sealed trait Animal");
        assertFileContains(animalPath, "def className: String");
        assertFileContains(animalPath, "def color: Option[String]");

        // Discriminator-based encoder/decoder
        assertFileContains(animalPath, "implicit val encoderAnimal");
        assertFileContains(animalPath, "implicit val decoderAnimal");
        assertFileContains(animalPath, "\"DOG\"");
        assertFileContains(animalPath, "\"CAT\"");
        assertFileContains(animalPath, "c.downField(\"className\")");
        assertFileContains(animalPath, "DecodingFailure");

        // Cat and Dog inlined in Animal.scala, extending Animal
        assertFileContains(animalPath, "case class Cat");
        assertFileContains(animalPath, "case class Dog");
        assertFileContains(animalPath, "extends Animal");
        assertFileContains(animalPath, "declawed");
        assertFileContains(animalPath, "breed");

        // Cat/Dog should have their own encoder/decoder (for the discriminator to delegate to)
        assertFileContains(animalPath, "implicit val encoderCat");
        assertFileContains(animalPath, "implicit val decoderCat");
        assertFileContains(animalPath, "implicit val encoderDog");
        assertFileContains(animalPath, "implicit val decoderDog");

        // Cat and Dog should NOT have separate files
        Assert.assertFalse(
                Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Cat.scala").toFile().exists(),
                "Cat.scala should not exist (inlined in Animal.scala)");
        Assert.assertFalse(
                Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Dog.scala").toFile().exists(),
                "Dog.scala should not exist (inlined in Animal.scala)");
    }

    @Test(description = "container-wrapped model ref (Seq[Dog]) prevents inlining of oneOf child")
    public void verifyContainerWrappedRefPreventsInlining() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        generateFromSpec("src/test/resources/3_0/scala-sttp-circe/petstore.yaml", output);

        // Dog is referenced both as a oneOf child of Animal AND as Seq[Dog] in Kennel.dogs.
        // Since not all children can be inlined, Animal becomes a regular trait (not sealed).
        // Both Dog and Cat get their own files.
        Path dogPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Dog.scala");
        Assert.assertTrue(dogPath.toFile().exists(),
                "Dog.scala must exist as a separate file (used as array element in Kennel)");
        assertFileContains(dogPath, "case class Dog");
        assertFileContains(dogPath, "extends Animal");

        // Cat also gets its own file (regular trait = no inlining)
        Path catPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Cat.scala");
        Assert.assertTrue(catPath.toFile().exists(),
                "Cat.scala must exist (Animal is a regular trait, no children inlined)");
        assertFileContains(catPath, "case class Cat");
        assertFileContains(catPath, "extends Animal");

        // Animal is a regular trait (not sealed) because not all children can be inlined
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "trait Animal");
        assertFileNotContains(animalPath, "sealed trait Animal");
        assertFileNotContains(animalPath, "case class Cat");
        assertFileNotContains(animalPath, "case class Dog");

        // Kennel should reference Dog via Seq
        Path kennelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Kennel.scala");
        assertFileContains(kennelPath, "dogs: Option[Seq[Dog]]");
    }

    @Test(description = "oneOf + discriminator generates sealed trait (standard pattern)")
    public void verifyOneOfDiscriminator() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        generateFromSpec("src/test/resources/3_0/oneOfDiscriminator.yaml", output);

        // FruitReqDisc: sealed trait with inlined members (oneOf + discriminator, no mapping)
        Path fruitPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/FruitReqDisc.scala");
        assertFileContains(fruitPath, "sealed trait FruitReqDisc");
        assertFileContains(fruitPath, "case class AppleReqDisc");
        assertFileContains(fruitPath, "case class BananaReqDisc");
        assertFileContains(fruitPath, "extends FruitReqDisc");
        assertFileContains(fruitPath, "\"fruitType\"");

        // FruitOneOfEnumMappingDisc: sealed trait with explicit discriminator mapping
        Path fruitMappingPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/FruitOneOfEnumMappingDisc.scala");
        assertFileContains(fruitMappingPath, "sealed trait FruitOneOfEnumMappingDisc");
        assertFileContains(fruitMappingPath, "\"APPLE\"");
        assertFileContains(fruitMappingPath, "\"BANANA\"");

        // Inlined members should not have separate files
        Assert.assertFalse(
                Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/AppleReqDisc.scala").toFile().exists(),
                "AppleReqDisc.scala should not exist (inlined)");
    }
}
