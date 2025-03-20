package org.openapitools.codegen.scala;

import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.jetbrains.annotations.NotNull;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ScalaHttp4sClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

import static org.testng.Assert.*;

public class ScalaHttp4sClientCodegenTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("name", new StringSchema())
                .addProperty("created_at", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 3);
        assertEquals(cm.vars.stream().filter(CodegenProperty::getRequired).count(), 2);
    }

    @Test(description = "happy path test")
    public void happyPathTest() throws IOException {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        assertEquals(codegen.getName(), "scala-http4s");
        assertEquals(codegen.getTag(), CodegenType.CLIENT);

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setInputSpec("src/test/resources/3_0/scala-http4s/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = getDefaultGenerator();

        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "build.sbt");
        TestUtils.ensureContainsFile(files, output, "project/build.properties");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/package.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/_Authorization.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/_FailedRequest.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/ApiResponse.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/Category.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/FindPetsByStatusStatusParameterInner.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/Order.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/OrderStatus.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/Pet.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/PetStatus.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/Tag.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/models/User.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/apis/BaseClient.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/apis/JsonSupports.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/apis/PetApi.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/apis/StoreApi.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/org/openapitools/client/apis/UserApi.scala");

    }

    @NotNull
    private static DefaultGenerator getDefaultGenerator() {
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGenerateMetadata(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
        return generator;
    }

    @Test(description = "use Instant for date-time")
    public void dateTimeToInstant() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();

        final Schema<?> schema = new Schema<Object>()
                .description("Schema with date-time");
        schema.setType("string");
        schema.setFormat("date-time");
        String type = codegen.getTypeDeclaration(schema);
        assertEquals(type, "Instant");
    }

    @Test
    public void allowExcludeSbtFiles() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        assertFalse(codegen.isExcludeSbt());
        codegen.additionalProperties().put("excludeSbt", "true");
        assertEquals(codegen.additionalProperties().get("excludeSbt"), "true");
        codegen.processOpts();
        assertTrue(codegen.isExcludeSbt());
    }

    @Test
    public void allowExcludeApiFiles() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        assertFalse(codegen.isExcludeApi());
        codegen.additionalProperties().put("excludeApi", "true");
        assertEquals(codegen.additionalProperties().get("excludeApi"), "true");
        codegen.processOpts();
        assertTrue(codegen.isExcludeApi());
    }

    @Test
    public void convertVarNameCamelCase() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        assertEquals(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name(), codegen.getModelPropertyNaming());
        assertEquals(codegen.toVarName("name"), "name");
        assertEquals(codegen.toVarName("user-name"), "userName");
        assertEquals(codegen.toVarName("user_name"), "userName");
        assertEquals(codegen.toVarName("user|name"), "userName");
        assertEquals(codegen.toVarName("uSername"), "uSername");
        assertEquals(codegen.toVarName("USERNAME"), "USERNAME");
        assertEquals(codegen.toVarName("USER123NAME"), "USER123NAME");
        assertEquals(codegen.toVarName("1"), "`1`");
        assertEquals(codegen.toVarName("1a"), "`1a`");
        assertEquals(codegen.toVarName("1A"), "`1A`");
        assertEquals(codegen.toVarName("1AAAA"), "`1AAAA`");
        assertEquals(codegen.toVarName("1AAaa"), "`1aAaa`");
    }

    @Test
    public void encodePath() {
        ScalaHttp4sClientCodegen codegen = new ScalaHttp4sClientCodegen();
        assertEquals(codegen.encodePath("{user_name}"), "${userName}");
        assertEquals(codegen.encodePath("{userName}"), "${userName}");
        assertEquals(codegen.encodePath("{UserName}"), "${userName}");
        assertEquals(codegen.encodePath("user_name"), "user_name");
        assertEquals(codegen.encodePath("before/{UserName}/after"), "before/${userName}/after");
    }

}
