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
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

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
    public void verifyEnumParameterImportAndWireValues() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-enum-param.yaml", null, new ParseOptions()).getOpenAPI();

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
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        // Enums are sealed traits + case objects: the API must import the type itself,
        // not Enumeration-style wildcard members (which would not bring the type into scope).
        Path apiPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/api/DefaultApi.scala");
        assertFileContains(apiPath, "import org.openapitools.client.model.PetStatus\n");
        assertFileNotContains(apiPath, "import org.openapitools.client.model.PetStatus._");

        // Models live in the same package as the enum: no import is needed at all
        // (sealed trait, not an Enumeration with a type alias inside the companion).
        Path modelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileNotContains(modelPath, "import org.openapitools.client.model.PetStatus");

        // Case objects carry their wire value as toString so path/query interpolation
        // (uri"...${param}...") serializes the wire value, not the Scala identifier.
        Path enumPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/PetStatus.scala");
        assertFileContains(enumPath, "case object Available extends PetStatus { override def toString: String = \"available\" }");
        assertFileContains(enumPath, "case object SoldOut extends PetStatus { override def toString: String = \"sold-out\" }");
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

        // Test oneOf without discriminator generates sealed trait with wrapper composition
        // (Dog and Cat are shared across Pet and Animal)
        Path petPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileContains(petPath, "sealed trait Pet");
        assertFileContains(petPath, "object Pet {");
        assertFileContains(petPath, "case class DogPet(value: Dog) extends Pet");
        assertFileContains(petPath, "case class CatPet(value: Cat) extends Pet");
        assertFileContains(petPath, "// oneOf without discriminator (with wrapper composition)");
        assertFileContains(petPath, "implicit val encoder: Encoder[Pet] = Encoder.instance");
        assertFileContains(petPath, "Decoder[Dog].map(DogPet.apply)");
        assertFileContains(petPath, "Decoder[Cat].map(CatPet.apply)");

        // Test oneOf with discriminator uses wrapper composition (shared members)
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "sealed trait Animal");
        assertFileContains(animalPath, "object Animal {");
        assertFileContains(animalPath, "case class DogAnimal(value: Dog) extends Animal");
        assertFileContains(animalPath, "case class CatAnimal(value: Cat) extends Animal");
        assertFileContains(animalPath, "// oneOf with discriminator (with wrapper composition)");
        assertFileContains(animalPath, "implicit val encoder: Encoder[Animal] = Encoder.instance");
        assertFileContains(animalPath, "c.get[String](\"petType\")");

        // Test oneOf with discriminator mapping
        Path vehiclePath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Vehicle.scala");
        assertFileContains(vehiclePath, "sealed trait Vehicle");
        assertFileContains(vehiclePath, "object Vehicle {");
        assertFileContains(vehiclePath, "// oneOf with discriminator");
        assertFileContains(vehiclePath,
                "case obj: Car => obj.asJson.mapObject((\"vehicleType\" -> \"car\".asJson) +: _)");
        assertFileContains(vehiclePath,
                "case obj: Truck => obj.asJson.mapObject((\"vehicleType\" -> \"truck\".asJson) +: _)");
        assertFileContains(vehiclePath, "c.downField(\"vehicleType\").as[String].flatMap {");
        assertFileContains(vehiclePath, "case \"car\" => c.as[Car].map(x => x: Vehicle)");
        assertFileContains(vehiclePath, "case \"truck\" => c.as[Truck].map(x => x: Vehicle)");

        // Test oneOf with discriminator that is a Scala keyword ("type")
        // The discriminator should use the original wire name, not the backtick-escaped Scala name
        Path shapePath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Shape.scala");
        assertFileContains(shapePath, "sealed trait Shape");
        assertFileContains(shapePath, "c.downField(\"type\").as[String].flatMap {");
        // Discriminator in serialization must not be backtick-escaped
        assertFileNotContains(shapePath, "`type`");

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

        // Test oneOf without discriminator generates sealed trait with json4s wrapper composition
        // (Dog and Cat are shared across Pet and Animal)
        Path petPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileContains(petPath, "sealed trait Pet");
        assertFileContains(petPath, "object Pet {");
        assertFileContains(petPath, "case class DogPet(value: Dog) extends Pet");
        assertFileContains(petPath, "case class CatPet(value: Cat) extends Pet");
        assertFileContains(petPath, "import org.json4s._");
        assertFileContains(petPath, "// oneOf without discriminator - json4s custom serializer (with wrapper composition)");
        assertFileContains(petPath, "implicit object PetSerializer extends Serializer[Pet]");
        assertFileContains(petPath, "Some(DogPet(x))");
        assertFileContains(petPath, "Some(CatPet(x))");
        assertFileContains(petPath, "Extraction.extract[Dog](json)");
        assertFileContains(petPath, "Extraction.extract[Cat](json)");

        // Test oneOf with discriminator uses wrapper composition (shared members)
        Path animalPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Animal.scala");
        assertFileContains(animalPath, "sealed trait Animal");
        assertFileContains(animalPath, "case class DogAnimal(value: Dog) extends Animal");
        assertFileContains(animalPath, "case class CatAnimal(value: Cat) extends Animal");
        assertFileContains(animalPath, "// oneOf with discriminator (with wrapper composition)");
        assertFileContains(animalPath, "petType");
    }

    @Test
    public void verifyOneOfWithEmptyMembers() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-oneOf-empty-members.yaml", null, new ParseOptions())
                .getOpenAPI();

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

        // Test empty case classes (no properties except discriminator which was
        // removed)
        assertFileContains(eventPath, "case class ClickEvent(\n) extends Event");
        assertFileContains(eventPath, "case class ViewEvent(\n) extends Event");

        // Test case class with properties (PurchaseEvent has amount)
        assertFileContains(eventPath, "case class PurchaseEvent(");
        assertFileContains(eventPath, "amount: Double");

        // Verify the discriminator is written by the sealed trait's encoder
        assertFileContains(eventPath, "c.downField(\"eventType\").as[String].flatMap {");

        // Verify the discriminator property was removed from inline members
        // ClickEvent and ViewEvent should have NO properties at all
        assertFileContains(eventPath, "case class ClickEvent(\n) extends Event");
        assertFileContains(eventPath, "case class ViewEvent(\n) extends Event");
    }

    @Test
    public void verifyOneOfWithParentProperties() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-oneOf-with-parent-props.yaml", null,
                        new ParseOptions())
                .getOpenAPI();

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

        Path parentPath = Paths.get(
                outputPath + "/src/main/scala/org/openapitools/client/model/Parent.scala");
        String content = Files.readString(parentPath);

        // Sealed trait exists with abstract member for parent property
        assertFileContains(parentPath, "sealed trait Parent {");
        assertFileContains(parentPath, "def createdAt: OffsetDateTime");

        // ChildOne exists (empty child is NOT dropped) and extends Parent
        assertFileContains(parentPath, "case class ChildOne(");
        assertFileContains(parentPath, ") extends Parent");

        // ChildOne has parent property propagated
        int childOneStart = content.indexOf("case class ChildOne(");
        int childOneEnd = content.indexOf(") extends Parent", childOneStart);
        Assert.assertTrue(childOneStart >= 0 && childOneEnd >= 0,
                "ChildOne case class should exist");
        String childOneBlock = content.substring(childOneStart, childOneEnd);
        Assert.assertTrue(childOneBlock.contains("createdAt: OffsetDateTime"),
                "ChildOne should have parent property 'createdAt'");

        // ChildOne does NOT have sibling properties
        Assert.assertFalse(childOneBlock.contains("status"),
                "ChildOne should NOT contain sibling property 'status'");
        Assert.assertFalse(childOneBlock.contains("detail"),
                "ChildOne should NOT contain sibling property 'detail'");

        // ChildTwo has parent property + its own properties
        assertFileContains(parentPath, "case class ChildTwo(");
        int childTwoStart = content.indexOf("case class ChildTwo(");
        int childTwoEnd = content.indexOf(") extends Parent", childTwoStart);
        Assert.assertTrue(childTwoStart >= 0 && childTwoEnd >= 0,
                "ChildTwo case class should exist");
        String childTwoBlock = content.substring(childTwoStart, childTwoEnd);
        Assert.assertTrue(childTwoBlock.contains("createdAt: OffsetDateTime"),
                "ChildTwo should have parent property 'createdAt'");
        Assert.assertTrue(childTwoBlock.contains("status: String"),
                "ChildTwo should have its own property 'status'");
        Assert.assertTrue(childTwoBlock.contains("detail: Option[Nested]"),
                "ChildTwo should have its own property 'detail'");
    }

    @Test
    public void verifyDateTimeLocalGeneratesLocalDateTime() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/date-time-local.yaml", null, new ParseOptions()).getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
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
        generator.opts(input).generate();

        Path modelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Event.scala");
        // date-time-local maps to LocalDateTime
        assertFileContains(modelPath, "import java.time.LocalDateTime");
        assertFileContains(modelPath, "startTime: LocalDateTime");
        assertFileContains(modelPath, "endTime: Option[LocalDateTime]");
        // date-time maps to OffsetDateTime
        assertFileContains(modelPath, "import java.time.OffsetDateTime");
        assertFileContains(modelPath, "createdAt: OffsetDateTime");
        assertFileContains(modelPath, "updatedAt: Option[OffsetDateTime]");
    }

    @Test
    public void verifyOptionalFieldsOmittedWhenNone() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-optional-fields.yaml", null, new ParseOptions()).getOpenAPI();

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

        // Optional fields set to None must be omitted from the JSON (sttp3 parity),
        // not serialized as null: strict servers reject explicit null for
        // non-nullable optional properties.
        Path petPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Pet.scala");
        assertFileContains(petPath, "t.tag.map(v => \"tag\" -> v.asJson)");
        assertFileNotContains(petPath, "deriveEncoder");
    }

    @Test
    public void verifyCirceCodecsUseOriginalJsonPropertyNames() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-mixed-case-fields.yaml", null, new ParseOptions())
                .getOpenAPI();

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

        // Scala identifiers stay camelCase; JSON keys are the original spec property names
        Path modelPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/MixedCaseModel.scala");
        assertFileContains(modelPath, "assignmentKey: String");
        assertFileContains(modelPath, "addressLine2: Option[String] = None");
        assertFileContains(modelPath, "Some(\"assignment_key\" -> t.assignmentKey.asJson)");
        assertFileContains(modelPath, "t.firstName.map(v => \"first-name\" -> v.asJson)");
        assertFileContains(modelPath, "t.zipCode.map(v => \"ZipCode\" -> v.asJson)");
        // an already-camelCase property must keep its name, so this stays byte-identical
        assertFileContains(modelPath, "t.lastName.map(v => \"lastName\" -> v.asJson)");
        assertFileContains(modelPath, "lastName <- c.downField(\"lastName\").as[Option[String]]");
        assertFileContains(modelPath, "t.addressLine2.map(v => \"address_line_2\" -> v.asJson)");
        assertFileContains(modelPath, "assignmentKey <- c.downField(\"assignment_key\").as[String]");
        assertFileContains(modelPath, "addressLine2 <- c.downField(\"address_line_2\").as[Option[String]]");
        assertFileContains(modelPath,
                "bookingStatus <- c.downField(\"booking_status\").as[Option[MixedCaseModelEnums.BookingStatus]]");
        assertFileNotContains(modelPath, "deriveEncoder");
        assertFileNotContains(modelPath, "deriveDecoder");

        // Inline oneOf members: original names too, and the discriminator property is
        // written by the sealed trait rather than carried as a member field
        Path paymentPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/PaymentMethod.scala");
        assertFileContains(paymentPath, "Some(\"card_holder_name\" -> t.cardHolderName.asJson)");
        assertFileContains(paymentPath, "t.lastFourDigits.map(v => \"last-four-digits\" -> v.asJson)");
        assertFileContains(paymentPath,
                "case obj: CreditCardPayment => obj.asJson.mapObject((\"payment_type\" -> \"credit_card\".asJson) +: _)");
        assertFileContains(paymentPath, "c.downField(\"payment_type\").as[String].flatMap {");
        assertFileNotContains(paymentPath, "paymentType");

        // oneOf without a discriminator encodes the bare member. Deriving it produced a
        // constructor-name wrapper ({"Circle":{..}}), which is not what oneOf means.
        Path shapePath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/model/Shape.scala");
        assertFileContains(shapePath, "case obj: Circle => obj.asJson");
        assertFileContains(shapePath, "Decoder[Circle].map(x => x: Shape)");
        assertFileNotContains(shapePath, "deriveEncoder");
    }

    @Test
    public void verifyModelNamedRequestDoesNotShadowSttpRequest() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/scala/sttp4-request-model-name.yaml", null, new ParseOptions())
                .getOpenAPI();

        ScalaSttp4ClientCodegen codegen = new ScalaSttp4ClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

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

        // A model named Request is imported explicitly, which outranks the
        // `import sttp.client4._` wildcard, so the return type must be qualified.
        Path apiPath = Paths.get(outputPath + "/src/main/scala/org/openapitools/client/api/DefaultApi.scala");
        assertFileContains(apiPath, "import org.openapitools.client.model.Request");
        assertFileContains(apiPath, "): sttp.client4.Request[Either[ResponseException[String], Response]] =");
    }
}
