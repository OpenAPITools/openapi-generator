package org.openapitools.codegen.scala;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.testng.Assert.assertEquals;

public class ScalaHttp4sServerCodegenTest {

    @Test(description = "discriminator codecs use the JSON property name and fail decoding on unknown values")
    public void discriminatorOnReservedWord() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("scala-http4s-server")
                .setInputSpec("src/test/resources/3_0/scala-http4s-server/discriminator-reserved-word.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        List<File> files = new DefaultGenerator().opts(clientOptInput).generate();

        Path types = files.stream()
                .filter(file -> file.getName().equals("types.scala"))
                .findFirst()
                .orElseThrow()
                .toPath();

        // `type` is a reserved word, so its Scala identifier is escaped; the JSON keys must not be.
        TestUtils.assertFileNotContains(types, "&#x60;", "\"`type`\"");

        // Pet: discriminator without mapping
        TestUtils.assertFileContains(types,
                "case cat: Cat => cat.asJson.mapObject((\"type\" -> \"Cat\".asJson) +: _)",
                "case dog: Dog => dog.asJson.mapObject((\"type\" -> \"Dog\".asJson) +: _)");

        // Vehicle: discriminator with mapping
        TestUtils.assertFileContains(types,
                "case car: Car => car.asJson.mapObject((\"type\" -> \"car\".asJson) +: _)",
                "case bike: Bike => bike.asJson.mapObject((\"type\" -> \"bike\".asJson) +: _)");

        String content = Files.readString(types);
        assertEquals(TestUtils.countOccurrences(content, "cursor\\.downField\\(\"type\"\\)\\.as\\[String\\]"), 2);
        assertEquals(TestUtils.countOccurrences(content,
                "case discriminatorValue =>\\s+Left\\(io\\.circe\\.DecodingFailure\\(s\"Unknown discriminator value: \\$discriminatorValue\", cursor\\.history\\)\\)"), 2);
    }
}
