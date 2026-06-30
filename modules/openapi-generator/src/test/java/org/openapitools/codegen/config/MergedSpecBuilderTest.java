package org.openapitools.codegen.config;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.SpringCodegen;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.languages.SpringCodegen.*;
import static org.testng.Assert.*;

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
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
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
                .containsWithNameAndAttributes("RequestMapping", ImmutableMap.of("value", "Spec1Api.PATH_SPEC1_OPERATION_COMPLEX"))
                .toMethod()
                .assertParameter("param1")
                .hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("PathVariable", ImmutableMap.of("value", "\"param1\""))
                .toParameter()
                .toMethod()
                .toFileAssert()
                .fileContains("@RequestMapping(\"${openapi.mergedSpec.base-path:/my-context-root/v1}\")")
        ;

        JavaFileAssert.assertThat(files.get("Spec2Api.java"))
                .assertMethod("spec2Operation").hasReturnType("ResponseEntity<Spec2Model>")
                .toFileAssert()
                .fileContains("@RequestMapping(\"${openapi.mergedSpec.base-path:/my-context-root/v1}\")")
        ;

        JavaFileAssert.assertThat(files.get("Spec1Model.java"))
                .assertMethod("getSpec1Field").hasReturnType("String");

        JavaFileAssert.assertThat(files.get("Spec2Model.java"))
                .assertMethod("getSpec2Field").hasReturnType("BigDecimal");
    }

    // ---- Path-collision tests ----

    @Test
    public void shouldMergeSpecsWithCollidingPaths_yaml() throws IOException {
        shouldMergeSpecsWithCollidingPaths("yaml");
    }

    @Test
    public void shouldMergeSpecsWithCollidingPaths_json() throws IOException {
        shouldMergeSpecsWithCollidingPaths("json");
    }

    private void shouldMergeSpecsWithCollidingPaths(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-collision").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-collision." + fileExt), dir.toPath().resolve("spec-collision." + fileExt));

        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .buildMergedSpec();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        assertNotNull(openAPI.getPaths(), "Merged spec must have paths");

        // REF mode: same path in multiple specs — last file (alphabetically) wins
        PathItem spec1Path = openAPI.getPaths().get("/spec1");
        assertNotNull(spec1Path, "/spec1 path must exist in merged spec");

        // /collision path from spec-collision must also be present
        assertNotNull(openAPI.getPaths().get("/collision"), "/collision path must exist in merged spec");
    }

    // ---- Vendor extensions tests ----

    @Test
    public void shouldPreserveVendorExtensions_yaml() throws IOException {
        shouldPreserveVendorExtensions("yaml");
    }

    @Test
    public void shouldPreserveVendorExtensions_json() throws IOException {
        shouldPreserveVendorExtensions("json");
    }

    private void shouldPreserveVendorExtensions(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-extensions").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-extensions." + fileExt), dir.toPath().resolve("spec-extensions." + fileExt));

        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .buildMergedSpec();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        assertNotNull(openAPI.getPaths(), "Merged spec must have paths");

        PathItem extPath = openAPI.getPaths().get("/ext-path");
        assertNotNull(extPath, "/ext-path must exist");
        assertNotNull(extPath.getExtensions(), "Path-level extensions must be preserved");
        assertEquals(extPath.getExtensions().get("x-custom-path-ext"), "path-level-value", "x-custom-path-ext must be preserved on path");

        assertNotNull(extPath.getGet(), "GET operation must exist on /ext-path");
        assertNotNull(extPath.getGet().getExtensions(), "Operation-level extensions must be preserved");
        assertEquals(extPath.getGet().getExtensions().get("x-custom-op-ext"), "operation-level-value", "x-custom-op-ext must be preserved on operation");

        assertNotNull(openAPI.getComponents().getSchemas().get("ExtModel"), "ExtModel must exist");
        assertNotNull(openAPI.getComponents().getSchemas().get("ExtModel").getExtensions(), "Schema-level extensions must be preserved");
        assertEquals(openAPI.getComponents().getSchemas().get("ExtModel").getExtensions().get("x-custom-schema-ext"), "schema-level-value", "x-custom-schema-ext must be preserved on schema");
    }

    // ---- Component merging tests ----

    @Test
    public void shouldMergeComponentsFromBothSpecs_yaml() throws IOException {
        shouldMergeComponentsFromBothSpecs("yaml");
    }

    @Test
    public void shouldMergeComponentsFromBothSpecs_json() throws IOException {
        shouldMergeComponentsFromBothSpecs("json");
    }

    private void shouldMergeComponentsFromBothSpecs(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-components").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec2." + fileExt), dir.toPath().resolve("spec2." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-collision." + fileExt), dir.toPath().resolve("spec-collision." + fileExt));

        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .buildMergedSpec();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        Map<String, ?> schemas = openAPI.getComponents().getSchemas();
        assertNotNull(schemas.get("Spec1Model"), "Spec1Model must be present");
        assertNotNull(schemas.get("Spec2Model"), "Spec2Model must be present");
        assertNotNull(schemas.get("CollisionModel"), "CollisionModel must be present");
    }

    // ---- Identical duplicate schema test ----

    @Test
    public void shouldHandleDuplicateIdenticalSchemas_yaml() throws IOException {
        shouldHandleDuplicateIdenticalSchemas("yaml");
    }

    @Test
    public void shouldHandleDuplicateIdenticalSchemas_json() throws IOException {
        shouldHandleDuplicateIdenticalSchemas("json");
    }

    /**
     * spec-collision defines Spec1Model identically to spec1 — same name, same structure.
     * The merged result must contain exactly one Spec1Model without errors.
     */
    private void shouldHandleDuplicateIdenticalSchemas(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-dup-schema").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-collision." + fileExt), dir.toPath().resolve("spec-collision." + fileExt));

        // Must not throw
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .buildMergedSpec();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        // Spec1Model is defined in both spec1 and spec-collision with identical structure
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be present exactly once");
    }

    // ---- Non-spec file filter test ----

    @Test
    public void shouldIgnoreNonSpecFiles_yaml() throws IOException {
        shouldIgnoreNonSpecFiles("yaml");
    }

    @Test
    public void shouldIgnoreNonSpecFiles_json() throws IOException {
        shouldIgnoreNonSpecFiles("json");
    }

    private void shouldIgnoreNonSpecFiles(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-noext").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        // Copy the non-spec .txt file into the same directory
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-noext.txt"), dir.toPath().resolve("spec-noext.txt"));

        // Must not throw despite the .txt file being present
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .buildMergedSpec();

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

        // Spec from spec1 must be present; no error from the .txt file
        assertNotNull(openAPI.getPaths().get("/spec1"), "/spec1 path must be present");
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be present");
    }

    // ---- Schema name conflict warning test ----

    @Test
    public void shouldWarnOnSchemaNameConflict_yaml() throws IOException {
        shouldWarnOnSchemaNameConflict("yaml");
    }

    @Test
    public void shouldWarnOnSchemaNameConflict_json() throws IOException {
        shouldWarnOnSchemaNameConflict("json");
    }

    /**
     * spec1 and spec-schema-conflict both define Spec1Model but with different properties.
     * The merge must keep the first definition and emit a WARN log.
     */
    private void shouldWarnOnSchemaNameConflict(String fileExt) throws IOException {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(MergedSpecBuilder.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);

        try {
            File dir = Files.createTempDirectory("spec-schema-conflict").toFile().getCanonicalFile();
            dir.deleteOnExit();

            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-schema-conflict." + fileExt), dir.toPath().resolve("spec-schema-conflict." + fileExt));

            String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .buildMergedSpec();

            ParseOptions parseOptions = new ParseOptions();
            parseOptions.setResolve(true);
            OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, parseOptions).getOpenAPI();

            // First alphabetical file (spec-schema-conflict: differentField) is kept
            assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be present");
            assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model").getProperties().get("differentField"),
                    "differentField (from first-alphabetical spec) must be kept");
            assertNull(openAPI.getComponents().getSchemas().get("Spec1Model").getProperties().get("spec1Field"),
                    "spec1Field (from second spec) must NOT be present");

            // A WARN about the conflict must have been logged
            List<ILoggingEvent> warnLogs = listAppender.list.stream()
                    .filter(e -> e.getLevel() == ch.qos.logback.classic.Level.WARN)
                    .filter(e -> e.getFormattedMessage().contains("Spec1Model"))
                    .collect(Collectors.toList());
            assertFalse(warnLogs.isEmpty(), "A WARN log about the Spec1Model name conflict must be emitted");
        } finally {
            logger.detachAppender(listAppender);
        }
    }

    // ---- FAIL conflict strategy tests ----

    @Test
    public void shouldFailOnSchemaNameConflictWithFailStrategy_yaml() throws IOException {
        shouldFailOnSchemaNameConflictWithFailStrategy("yaml");
    }

    @Test
    public void shouldFailOnSchemaNameConflictWithFailStrategy_json() throws IOException {
        shouldFailOnSchemaNameConflictWithFailStrategy("json");
    }

    private void shouldFailOnSchemaNameConflictWithFailStrategy(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-schema-conflict-fail").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-schema-conflict." + fileExt), dir.toPath().resolve("spec-schema-conflict." + fileExt));

        try {
            new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .withConflictStrategy(MergedSpecBuilder.MergeConflictStrategy.FAIL)
                    .buildMergedSpec();
            fail("Expected RuntimeException due to schema name conflict with FAIL strategy");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().contains("Spec1Model"), "Exception message must mention the conflicting schema name");
        }
    }

    @Test
    public void shouldFailOnPathMethodConflictWithFailStrategy_yaml() throws IOException {
        shouldFailOnPathMethodConflictWithFailStrategy("yaml");
    }

    @Test
    public void shouldFailOnPathMethodConflictWithFailStrategy_json() throws IOException {
        shouldFailOnPathMethodConflictWithFailStrategy("json");
    }

    /**
     * spec-path-method-conflict defines the same path+method (GET /spec1) as spec1.
     * With FAIL strategy this must throw.
     */
    private void shouldFailOnPathMethodConflictWithFailStrategy(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-path-conflict-fail").toFile().getCanonicalFile();
        dir.deleteOnExit();

        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        // spec-collision defines a POST on the same path — no method conflict, use spec-schema-conflict for method conflict
        // Instead re-use spec1 copied as a second file to force a path+method duplicate
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1-duplicate." + fileExt));

        try {
            new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .withConflictStrategy(MergedSpecBuilder.MergeConflictStrategy.FAIL)
                    .buildMergedSpec();
            fail("Expected RuntimeException due to path+method conflict with FAIL strategy");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().contains("/spec1"), "Exception message must mention the conflicting path");
        }
    }
}
