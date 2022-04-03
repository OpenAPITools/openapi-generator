package org.openapitools.codegen.kotlin.jvm_volley;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.*;

public class KotlinJvmVolleyModelCodegenTest {

    @Test
    public void modelsWithRoomModels() throws IOException {
        KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.additionalProperties().put(KotlinClientCodegen.GENERATE_ROOM_MODELS, true);
        codegen.additionalProperties().put(KotlinClientCodegen.ROOM_MODEL_PACKAGE, "models.room");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE.gson.name());

        String outputPath = checkModel(codegen, false);

        assertFileContains(Paths.get(outputPath + "/src/main/java/models/room/BigDogRoomModel.kt"), "toApiModel()");
        assertFileContains(Paths.get(outputPath + "/src/main/java/models/BigDog.kt"), "toRoomModel()");
    }

    @Test
    public void modelsWithoutRoomModels() throws IOException {
        KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.additionalProperties().put(KotlinClientCodegen.GENERATE_ROOM_MODELS, false);
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE.gson.name());

        String outputPath = checkModel(codegen, false);

        assertFileNotExists(Paths.get(outputPath + "/src/main/java/models/room/BigDogRoomModel.kt"));
        assertFileContains(Paths.get(outputPath + "/src/main/java/models/BigDog.kt"));
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/models/BigDog.kt"), "toRoomModel()");
    }

    private String checkModel(AbstractKotlinCodegen codegen, boolean mutable, String... props) throws IOException {
        String outputPath = generateModels(codegen, "src/test/resources/3_0/generic.yaml", mutable);
        assertFileContains(Paths.get(outputPath + "/src/main/java/models/Animal.kt"), props);
        return outputPath;
    }

    private String generateModels(AbstractKotlinCodegen codegen, String fileName, boolean mutable) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(fileName, null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary("jvm-volley");

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
}
