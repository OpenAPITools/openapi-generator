package org.openapitools.codegen.scala;

import com.google.common.collect.ImmutableList;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ScalaGatlingCodegen;
import static org.testng.Assert.assertEquals;

import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.stream.Collectors;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.util.SchemaTypeUtil;

public class ScalaGatlingCodegenTest {

    private final ScalaGatlingCodegen codegen = new ScalaGatlingCodegen();

    @Test
    public void happyPath() throws IOException {
        assertEquals(codegen.getName(), "scala-gatling");
        assertEquals(codegen.getTag(), CodegenType.CLIENT);

        final List<String> filenames = codegen.supportingFiles().stream()
                .map(supportingFile -> supportingFile.getDestinationFilename())
                .collect(Collectors.toList());
        assertEquals(filenames, ImmutableList.of(
                "build.gradle",
                "logback.xml",
                "default.conf",
                "CI.conf",
                "CD.conf",
                "stress.conf",
                "baseline.conf",
                "longevity.conf"));

        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);

        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 3);

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setInputSpec("src/test/resources/3_0/scala_reserved_words.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGenerateMetadata(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        List<File> files = generator.opts(clientOptInput).generate();

        assertEquals(files.size(), 9);

        TestUtils.ensureContainsFile(files, output, "src/gatling/scala/org/openapitools/client/model/SomeObj.scala");
        TestUtils.ensureContainsFile(files, output, "build.gradle");
        TestUtils.ensureContainsFile(files, output, "src/gatling/resources/conf/logback.xml");
        TestUtils.ensureContainsFile(files, output, "src/gatling/resources/conf/baseline.conf");
        TestUtils.ensureContainsFile(files, output, "src/gatling/resources/conf/stress.conf");
        TestUtils.ensureContainsFile(files, output, "src/gatling/resources/conf/longevity.conf");
    }

}
