package org.openapitools.codegen;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ContainerDefaultEvaluatorTest {

    @Test
    public void testNull() throws IOException {
        executeTest(
                null,
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testEmptyString() throws IOException {
        executeTest(
                "",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testFalse() throws IOException {
        executeTest(
                "false",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testTrue() throws IOException {
        executeTest(
                "true",
                "private List<String> r0n0;",
                "private List<String> r0n1;",
                "private List<String> r1n0;",
                "private List<String> r1n1;",
                "private List<String> r0;",
                "private List<String> r1;",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testNone() throws IOException {
        executeTest(
                "none",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1 = new ArrayList<>();",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1 = new ArrayList<>();",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet = new LinkedHashSet<>();",
                "private Map<String, Object> myMap = new HashMap<>();"
        );
    }

    @Test
    public void test_7_5_0() throws IOException {
        executeTest(
                "7.5.0",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void test_7_4_0() throws IOException {
        executeTest(
                "7.4.0",
                "private List<String> r0n0;",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0;",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testCustomExpression1() throws IOException {
        executeTest(
                "7.5.0 | !required & ?nullable",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0;",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testCustomExpression2() throws IOException {
        executeTest(
                " ! required&?nullable| 7.5.0",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0;",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testNullable() throws IOException {
        executeTest(
                "nullable",
                "private List<String> r0n0 = new ArrayList<>();",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1;",
                "private List<String> r0 = new ArrayList<>();",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    @Test
    public void testNotRequired() throws IOException {
        executeTest(
                "!required",
                "private List<String> r0n0;",
                "private List<String> r0n1;",
                "private List<String> r1n0 = new ArrayList<>();",
                "private List<String> r1n1 = new ArrayList<>();",
                "private List<String> r0;",
                "private List<String> r1 = new ArrayList<>();",
                "private Set<String> mySet;",
                "private Map<String, Object> myMap;"
        );
    }

    private void executeTest(String containerDefaultToNull, String... expectedLines) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/nullable_required_combinations.yaml", null, new ParseOptions()).getOpenAPI();
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setContainerDefaultToNull(containerDefaultToNull);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Get200Response.java")).fileContains(expectedLines);
    }

    @Test
    public void testInvalidInput() {
        Assertions.assertThatThrownBy(
                () -> new ContainerDefaultEvaluator("NULLABLE") // we expect lowercase input
        ).isExactlyInstanceOf(IllegalArgumentException.class);
    }

}
