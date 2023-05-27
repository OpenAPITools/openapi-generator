package org.openapitools.codegen.config;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.SpringCodegen;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;

public class MergedSpecBuilderTest {

    @Test
    public void shouldMergeYamlSpecs() throws IOException {
        mergeSpecs("yaml");
    }

    @Test
    public void shouldMergeJsonSpecs() throws IOException {
        mergeSpecs("json");
    }

    private void mergeSpecs(String fileExt) throws IOException {
        File output = Files.createTempDirectory("spec-directory").toFile().getCanonicalFile();
        output.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), output.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec2." + fileExt), output.toPath().resolve("spec2." + fileExt));

        String outputPath = output.getAbsolutePath().replace('\\', '/');

        String mergedSpec = new MergedSpecBuilder(outputPath, "_merged_file")
            .buildMergedSpec();

        assertFilesFromMergedSpec(mergedSpec);
    }

    private void assertFilesFromMergedSpec(String mergedSpec) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser()
            .readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Spec1Api.java"))
            .assertMethod("spec1Operation").hasReturnType("ResponseEntity<Spec1Model>")

            .toFileAssert()

            .assertMethod("spec1OperationComplex")
                .hasReturnType("ResponseEntity<Spec1Model>")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("RequestMapping", ImmutableMap.of("value", "\"/spec1/complex/{param1}/path\""))
            .toMethod()
            .hasParameter("param1")
                .withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("PathVariable", ImmutableMap.of("value", "\"param1\""));

        JavaFileAssert.assertThat(files.get("Spec2Api.java"))
            .assertMethod("spec2Operation").hasReturnType("ResponseEntity<Spec2Model>");

        JavaFileAssert.assertThat(files.get("Spec1Model.java"))
            .assertMethod("getSpec1Field").hasReturnType("String");

        JavaFileAssert.assertThat(files.get("Spec2Model.java"))
            .assertMethod("getSpec2Field").hasReturnType("BigDecimal");
    }

}