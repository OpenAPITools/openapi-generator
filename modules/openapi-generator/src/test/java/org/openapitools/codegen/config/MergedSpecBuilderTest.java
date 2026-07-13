package org.openapitools.codegen.config;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.info.Info;
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
import java.util.Arrays;
import java.util.Collections;
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
    public void shouldFailOnPathMethodOverlap_yaml() throws IOException {
        shouldFailOnPathMethodOverlap("yaml");
    }

    @Test
    public void shouldFailOnPathMethodOverlap_json() throws IOException {
        shouldFailOnPathMethodOverlap("json");
    }

    /**
     * When the same path+method appears in two specs, the merge must fail under the FAIL conflict
     * strategy — there is no valid use case for duplicate HTTP methods on the same path across
     * spec files.
     */
    private void shouldFailOnPathMethodOverlap(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("spec-path-overlap").toFile().getCanonicalFile();
        dir.deleteOnExit();

        // Copy spec1 twice — both define the same paths and methods
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1-duplicate." + fileExt));

        try {
            new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .withConflictStrategy(MergedSpecBuilder.MergeConflictStrategy.FAIL)
                    .buildMergedSpec();
            fail("Expected RuntimeException due to duplicate path+method across specs");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().contains("Path+method conflict"),
                    "Exception message must mention the path+method conflict");
        }
    }

    @Test
    public void shouldWarnAndKeepFirstOnPathMethodOverlap_yaml() throws IOException {
        shouldWarnAndKeepFirstOnPathMethodOverlap("yaml");
    }

    @Test
    public void shouldWarnAndKeepFirstOnPathMethodOverlap_json() throws IOException {
        shouldWarnAndKeepFirstOnPathMethodOverlap("json");
    }

    /**
     * Under the default WARN conflict strategy, a duplicate path+method must NOT abort the merge:
     * it logs a warning and keeps the first definition.
     */
    private void shouldWarnAndKeepFirstOnPathMethodOverlap(String fileExt) throws IOException {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(MergedSpecBuilder.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);
        try {
            File dir = Files.createTempDirectory("spec-path-overlap-warn").toFile().getCanonicalFile();
            dir.deleteOnExit();
            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1-duplicate." + fileExt));
            String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .buildMergedSpec(); // default WARN
            ParseOptions opts = new ParseOptions(); opts.setResolve(true);
            OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
            assertNotNull(openAPI.getPaths().get("/spec1").getGet(), "GET /spec1 must be kept from the first spec");
            long warnCount = listAppender.list.stream()
                    .filter(e -> e.getLevel() == ch.qos.logback.classic.Level.WARN)
                    .filter(e -> e.getFormattedMessage().contains("Path+method conflict"))
                    .count();
            assertTrue(warnCount > 0, "A path+method conflict WARN must be logged");
        } finally {
            logger.detachAppender(listAppender);
        }
    }
    // ========================================================================
    // DEEP mode — basic and structural tests
    // ========================================================================

    @Test
    public void shouldDeepMergeNonConflictingSpecs_yaml() throws IOException {
        shouldDeepMergeNonConflictingSpecs("yaml");
    }

    @Test
    public void shouldDeepMergeNonConflictingSpecs_json() throws IOException {
        shouldDeepMergeNonConflictingSpecs("json");
    }

    /** DEEP mode with spec1+spec2 (no conflicts): both paths and both schemas must be inlined. */
    private void shouldDeepMergeNonConflictingSpecs(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-basic").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec2." + fileExt), dir.toPath().resolve("spec2." + fileExt));
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
        assertNotNull(openAPI.getPaths().get("/spec1"), "/spec1 must be present");
        assertNotNull(openAPI.getPaths().get("/spec2"), "/spec2 must be present");
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be inlined");
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec2Model"), "Spec2Model must be inlined");
    }

    @Test
    public void shouldDeepMergeCollidingPathDifferentMethods_yaml() throws IOException {
        shouldDeepMergeCollidingPathDifferentMethods("yaml");
    }

    @Test
    public void shouldDeepMergeCollidingPathDifferentMethods_json() throws IOException {
        shouldDeepMergeCollidingPathDifferentMethods("json");
    }

    /**
     * spec1 defines GET /spec1; spec-collision defines POST /spec1 and GET /collision.
     * DEEP merge must combine both methods on /spec1 and include /collision.
     */
    private void shouldDeepMergeCollidingPathDifferentMethods(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-path-methods").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-collision." + fileExt), dir.toPath().resolve("spec-collision." + fileExt));
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
        PathItem spec1Path = openAPI.getPaths().get("/spec1");
        assertNotNull(spec1Path, "/spec1 must be present");
        assertNotNull(spec1Path.getGet(), "GET /spec1 from spec1 must be present");
        assertNotNull(spec1Path.getPost(), "POST /spec1 from spec-collision must be present");
        assertNotNull(openAPI.getPaths().get("/collision"), "/collision must be present");
        assertNotNull(openAPI.getComponents().getSchemas().get("CollisionModel"), "CollisionModel must be inlined");
    }

    @Test
    public void shouldDeepMergeDeduplicatesIdenticalSchemasSilently_yaml() throws IOException {
        shouldDeepMergeDeduplicatesIdenticalSchemasSilently("yaml");
    }

    @Test
    public void shouldDeepMergeDeduplicatesIdenticalSchemasSilently_json() throws IOException {
        shouldDeepMergeDeduplicatesIdenticalSchemasSilently("json");
    }

    /**
     * spec1 and spec-collision both define Spec1Model with identical structure.
     * DEEP merge must succeed with no exception and no WARN log — identical duplicates are
     * silently deduplicated, unlike conflicting definitions which produce a warning.
     */
    private void shouldDeepMergeDeduplicatesIdenticalSchemasSilently(String fileExt) throws IOException {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(MergedSpecBuilder.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);
        try {
            File dir = Files.createTempDirectory("deep-dedup").toFile().getCanonicalFile();
            dir.deleteOnExit();
            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
            Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-collision." + fileExt), dir.toPath().resolve("spec-collision." + fileExt));
            String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
            ParseOptions opts = new ParseOptions(); opts.setResolve(true);
            OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
            assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be present");
            long warnCount = listAppender.list.stream()
                    .filter(e -> e.getLevel() == ch.qos.logback.classic.Level.WARN)
                    .filter(e -> e.getFormattedMessage().contains("Spec1Model"))
                    .count();
            assertEquals(warnCount, 0L, "Identical duplicate schemas must NOT produce a WARN");
        } finally {
            logger.detachAppender(listAppender);
        }
    }

    @Test
    public void shouldDeepMergePreservesExtensions_yaml() throws IOException {
        shouldDeepMergePreservesExtensions("yaml");
    }

    @Test
    public void shouldDeepMergePreservesExtensions_json() throws IOException {
        shouldDeepMergePreservesExtensions("json");
    }

    /** DEEP mode must preserve path-level, operation-level, and schema-level x- extensions. */
    private void shouldDeepMergePreservesExtensions(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-ext").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-extensions." + fileExt), dir.toPath().resolve("spec-extensions." + fileExt));
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
        PathItem extPath = openAPI.getPaths().get("/ext-path");
        assertNotNull(extPath, "/ext-path must be present");
        assertEquals(extPath.getExtensions().get("x-custom-path-ext"), "path-level-value",
                "path-level extension must be preserved in DEEP mode");
        assertEquals(extPath.getGet().getExtensions().get("x-custom-op-ext"), "operation-level-value",
                "operation-level extension must be preserved in DEEP mode");
        assertEquals(openAPI.getComponents().getSchemas().get("ExtModel").getExtensions().get("x-custom-schema-ext"), "schema-level-value",
                "schema-level extension must be preserved in DEEP mode");
    }

    @Test
    public void shouldDeepMergeMergesTopLevelVendorExtensions() {
        OpenAPI spec1 = new OpenAPI().openapi("3.0.3").info(new Info().title("s1").version("1.0.0"));
        spec1.addExtension("x-team", "platform");
        spec1.addExtension("x-shared", "first-wins");
        spec1.setPaths(new io.swagger.v3.oas.models.Paths());
        spec1.setComponents(new Components());
        OpenAPI spec2 = new OpenAPI().openapi("3.0.3").info(new Info().title("s2").version("1.0.0"));
        spec2.addExtension("x-service", "users");
        spec2.addExtension("x-shared", "MUST_NOT_OVERWRITE");
        spec2.setPaths(new io.swagger.v3.oas.models.Paths());
        spec2.setComponents(new Components());
        MergedSpecBuilder builder = new MergedSpecBuilder(".", "_merged");
        OpenAPI merged = builder.mergeSpecs(Arrays.asList(spec1, spec2), Collections.emptyList());
        assertNotNull(merged.getExtensions(), "Merged spec must have top-level extensions");
        assertEquals(merged.getExtensions().get("x-team"), "platform", "x-team from spec1 must be present");
        assertEquals(merged.getExtensions().get("x-service"), "users", "x-service from spec2 must be present");
        assertEquals(merged.getExtensions().get("x-shared"), "first-wins", "first definition must win on key conflict");
    }

    @Test
    public void shouldDeepMergeMergesPathLevelMetadata_yaml() throws IOException {
        shouldDeepMergeMergesPathLevelMetadata("yaml");
    }

    @Test
    public void shouldDeepMergeMergesPathLevelMetadata_json() throws IOException {
        shouldDeepMergeMergesPathLevelMetadata("json");
    }

    /**
     * spec1 defines GET /spec1 with no path-level params.
     * spec-pathlevel defines PATCH /spec1 with a path-level X-Request-ID header param and x-path-ext extension.
     * Using explicit file list with spec1 first (so spec-pathlevel is the "incoming" PathItem),
     * DEEP merge must copy the path-level parameter and extension from the incoming PathItem.
     */
    private void shouldDeepMergeMergesPathLevelMetadata(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("deep-pathlevel-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir = Files.createTempDirectory("deep-pathlevel-out").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        java.nio.file.Path spec1Path = inputDir.toPath().resolve("spec1." + fileExt);
        java.nio.file.Path specPathLevel = inputDir.toPath().resolve("spec-pathlevel." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), spec1Path);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-pathlevel." + fileExt), specPathLevel);
        // spec1 first: creates /spec1 PathItem with GET; spec-pathlevel second: incoming PATCH + path-level metadata
        String mergedSpec = new MergedSpecBuilder(
                Arrays.asList(spec1Path.toAbsolutePath().toString(), specPathLevel.toAbsolutePath().toString()),
                outputDir.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        // Verify the merged file actually contains the parameter before re-parsing
        String mergedContent = new String(java.nio.file.Files.readAllBytes(Paths.get(mergedSpec)), java.nio.charset.StandardCharsets.UTF_8);
        assertTrue(mergedContent.contains("X-Request-ID"),
                "Merged file must contain X-Request-ID in serialized form");
        assertTrue(mergedContent.contains("x-path-ext"),
                "Merged file must contain x-path-ext extension in serialized form");

        // Parse without resolve so path-level parameters aren't inlined into operations
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, new ParseOptions()).getOpenAPI();
        PathItem spec1PathItem = openAPI.getPaths().get("/spec1");
        assertNotNull(spec1PathItem, "/spec1 must be present");
        assertNotNull(spec1PathItem.getGet(), "GET /spec1 from spec1 must be present");
        assertNotNull(spec1PathItem.getPatch(), "PATCH /spec1 from spec-pathlevel must be present");
        assertNotNull(spec1PathItem.getParameters(), "Path-level parameters must be present after merge");
        assertTrue(spec1PathItem.getParameters().stream()
                .anyMatch(p -> "X-Request-ID".equals(p.getName()) && "header".equals(p.getIn())),
                "X-Request-ID path-level parameter from incoming spec must be merged");
        assertNotNull(spec1PathItem.getExtensions(), "Path-level extensions must be present after merge");
        assertEquals(spec1PathItem.getExtensions().get("x-path-ext"), "path-level-extension-from-incoming",
                "x-path-ext path-level extension from incoming spec must be merged");
    }

    // ========================================================================
    // Explicit file list constructor tests
    // ========================================================================

    @Test
    public void shouldMergeExplicitFileListRefMode_yaml() throws IOException {
        shouldMergeExplicitFileListRefMode("yaml");
    }

    @Test
    public void shouldMergeExplicitFileListRefMode_json() throws IOException {
        shouldMergeExplicitFileListRefMode("json");
    }

    /**
     * REF mode with explicit file list: files live in a separate input dir, output goes to a
     * different dir. The merged spec must correctly reference all paths via $ref and resolve cleanly.
     */
    private void shouldMergeExplicitFileListRefMode(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("list-ref-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir = Files.createTempDirectory("list-ref-out").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        java.nio.file.Path spec1Path = inputDir.toPath().resolve("spec1." + fileExt);
        java.nio.file.Path spec2Path = inputDir.toPath().resolve("spec2." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), spec1Path);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec2." + fileExt), spec2Path);
        String mergedSpec = new MergedSpecBuilder(
                Arrays.asList(spec1Path.toAbsolutePath().toString(), spec2Path.toAbsolutePath().toString()),
                outputDir.getAbsolutePath(), "_merged"
        ).buildMergedSpec(); // REF mode is default
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
        assertNotNull(openAPI.getPaths().get("/spec1"), "/spec1 must resolve via $ref");
        assertNotNull(openAPI.getPaths().get("/spec2"), "/spec2 must resolve via $ref");
    }

    @Test
    public void shouldMergeExplicitFileListDeepMode_yaml() throws IOException {
        shouldMergeExplicitFileListDeepMode("yaml");
    }

    @Test
    public void shouldMergeExplicitFileListDeepMode_json() throws IOException {
        shouldMergeExplicitFileListDeepMode("json");
    }

    /** DEEP mode with explicit file list: output is a self-contained spec with all schemas inlined. */
    private void shouldMergeExplicitFileListDeepMode(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("list-deep-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir = Files.createTempDirectory("list-deep-out").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        java.nio.file.Path spec1Path = inputDir.toPath().resolve("spec1." + fileExt);
        java.nio.file.Path spec2Path = inputDir.toPath().resolve("spec2." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), spec1Path);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec2." + fileExt), spec2Path);
        String mergedSpec = new MergedSpecBuilder(
                Arrays.asList(spec1Path.toAbsolutePath().toString(), spec2Path.toAbsolutePath().toString()),
                outputDir.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, opts).getOpenAPI();
        assertNotNull(openAPI.getPaths().get("/spec1"), "/spec1 must be present");
        assertNotNull(openAPI.getPaths().get("/spec2"), "/spec2 must be present");
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec1Model"), "Spec1Model must be inlined");
        assertNotNull(openAPI.getComponents().getSchemas().get("Spec2Model"), "Spec2Model must be inlined");
    }

    @Test
    public void shouldExplicitFileListRespectsOrder_yaml() throws IOException {
        shouldExplicitFileListRespectsOrder("yaml");
    }

    @Test
    public void shouldExplicitFileListRespectsOrder_json() throws IOException {
        shouldExplicitFileListRespectsOrder("json");
    }

    /**
     * When a schema conflict exists, the first file in the explicit list wins.
     * Listing spec-schema-conflict first keeps "differentField"; listing spec1 first keeps "spec1Field".
     */
    private void shouldExplicitFileListRespectsOrder(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("list-order-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir1 = Files.createTempDirectory("list-order-out1").toFile().getCanonicalFile();
        outputDir1.deleteOnExit();
        File outputDir2 = Files.createTempDirectory("list-order-out2").toFile().getCanonicalFile();
        outputDir2.deleteOnExit();
        java.nio.file.Path spec1Path = inputDir.toPath().resolve("spec1." + fileExt);
        java.nio.file.Path conflictPath = inputDir.toPath().resolve("spec-schema-conflict." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), spec1Path);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-schema-conflict." + fileExt), conflictPath);
        ParseOptions opts = new ParseOptions(); opts.setResolve(true);
        // conflict-first: differentField wins
        String merged1 = new MergedSpecBuilder(
                Arrays.asList(conflictPath.toAbsolutePath().toString(), spec1Path.toAbsolutePath().toString()),
                outputDir1.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        OpenAPI api1 = new OpenAPIParser().readLocation(merged1, null, opts).getOpenAPI();
        assertNotNull(api1.getComponents().getSchemas().get("Spec1Model").getProperties().get("differentField"),
                "differentField must win when spec-schema-conflict is listed first");
        // spec1-first: spec1Field wins
        String merged2 = new MergedSpecBuilder(
                Arrays.asList(spec1Path.toAbsolutePath().toString(), conflictPath.toAbsolutePath().toString()),
                outputDir2.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        OpenAPI api2 = new OpenAPIParser().readLocation(merged2, null, opts).getOpenAPI();
        assertNotNull(api2.getComponents().getSchemas().get("Spec1Model").getProperties().get("spec1Field"),
                "spec1Field must win when spec1 is listed first");
    }

    @Test
    public void shouldFailOnEmptyExplicitFileList() {
        try {
            new MergedSpecBuilder(Collections.emptyList(), System.getProperty("java.io.tmpdir"), "_merged")
                    .buildMergedSpec();
            fail("Expected RuntimeException for empty file list");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().toLowerCase(java.util.Locale.ROOT).contains("empty") || e.getMessage().toLowerCase(java.util.Locale.ROOT).contains("nothing"),
                    "Exception must indicate the list is empty");
        }
    }

    // ========================================================================
    // DEEP mode — external $ref resolution, security, operationId, $ref params
    // ========================================================================

    @Test
    public void shouldDeepMergeResolvesExternalRefs_yaml() throws IOException {
        shouldDeepMergeResolvesExternalRefs("yaml");
    }

    @Test
    public void shouldDeepMergeResolvesExternalRefs_json() throws IOException {
        shouldDeepMergeResolvesExternalRefs("json");
    }

    /**
     * A spec referencing a schema in a separate file via an external $ref must produce a
     * self-contained DEEP-merged document: the external schema is inlined into components and no
     * dangling cross-file reference remains, even when the output dir differs from the source dir.
     */
    private void shouldDeepMergeResolvesExternalRefs(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("deep-extref-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir = Files.createTempDirectory("deep-extref-out").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        java.nio.file.Path specPath = inputDir.toPath().resolve("spec-extref." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-extref." + fileExt), specPath);
        // The external components file lives next to the spec but is NOT part of the merge list.
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/common-components." + fileExt),
                inputDir.toPath().resolve("common-components." + fileExt));

        String mergedSpec = new MergedSpecBuilder(
                Arrays.asList(specPath.toAbsolutePath().toString()),
                outputDir.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();

        // The serialized merged file must not contain any external file reference.
        String mergedContent = new String(Files.readAllBytes(Paths.get(mergedSpec)), java.nio.charset.StandardCharsets.UTF_8);
        assertFalse(mergedContent.contains("common-components"),
                "Merged output must not contain a dangling external file $ref");

        // Parse without resolve — the referenced schema must already be inlined into components.
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, new ParseOptions()).getOpenAPI();
        assertNotNull(openAPI.getComponents(), "Merged components must exist");
        assertNotNull(openAPI.getComponents().getSchemas().get("SharedModel"),
                "External SharedModel schema must be inlined into the merged components");
        assertNotNull(openAPI.getPaths().get("/extref"), "/extref must be present");
    }

    @Test
    public void shouldDeepMergePreservesRootSecurityOnOperations_yaml() throws IOException {
        shouldDeepMergePreservesRootSecurityOnOperations("yaml");
    }

    @Test
    public void shouldDeepMergePreservesRootSecurityOnOperations_json() throws IOException {
        shouldDeepMergePreservesRootSecurityOnOperations("json");
    }

    /**
     * Root-level security applies to every operation that does not override it. After DEEP merge
     * (which cannot keep a single root security across specs) each such operation must carry the
     * inherited security requirement, while an operation that opts out (empty security) stays open.
     */
    private void shouldDeepMergePreservesRootSecurityOnOperations(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-security").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-security." + fileExt), dir.toPath().resolve("spec-security." + fileExt));
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, new ParseOptions()).getOpenAPI();

        // The merged document must not rely on a root-level security block.
        assertTrue(openAPI.getSecurity() == null || openAPI.getSecurity().isEmpty(),
                "DEEP merge must not keep a single root-level security block");

        io.swagger.v3.oas.models.Operation secured = openAPI.getPaths().get("/secured").getGet();
        assertNotNull(secured.getSecurity(), "Inherited root security must be propagated to the operation");
        assertTrue(secured.getSecurity().stream().anyMatch(r -> r.containsKey("apiKeyAuth")),
                "The apiKeyAuth requirement must be present on the operation");

        io.swagger.v3.oas.models.Operation open = openAPI.getPaths().get("/open").getGet();
        assertNotNull(open.getSecurity(), "Explicit empty security must be preserved");
        assertTrue(open.getSecurity().isEmpty(), "Operation opting out of auth must keep empty security");
    }

    @Test
    public void shouldDeepMergeRenamesDuplicateOperationIdWithWarn_yaml() throws IOException {
        shouldDeepMergeRenamesDuplicateOperationIdWithWarn("yaml");
    }

    @Test
    public void shouldDeepMergeRenamesDuplicateOperationIdWithWarn_json() throws IOException {
        shouldDeepMergeRenamesDuplicateOperationIdWithWarn("json");
    }

    /**
     * Two operations on different paths sharing the same operationId would produce an invalid
     * document. Under the default WARN strategy the merge keeps the first and renames the later
     * one (e.g. spec1Operation_2) so all operationIds remain unique.
     */
    private void shouldDeepMergeRenamesDuplicateOperationIdWithWarn(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-dupopid").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-dupopid." + fileExt), dir.toPath().resolve("spec-dupopid." + fileExt));
        String mergedSpec = new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, new ParseOptions()).getOpenAPI();

        List<String> operationIds = openAPI.getPaths().values().stream()
                .flatMap(p -> p.readOperations().stream())
                .map(io.swagger.v3.oas.models.Operation::getOperationId)
                .collect(Collectors.toList());
        // spec1 (sorted first) keeps spec1Operation; the duplicate from spec-dupopid is renamed.
        assertTrue(operationIds.contains("spec1Operation"), "First operationId must be kept");
        assertTrue(operationIds.contains("spec1Operation_2"), "Duplicate operationId must be renamed to spec1Operation_2");
        assertEquals(operationIds.stream().distinct().count(), (long) operationIds.size(),
                "All operationIds must be unique after merge");
    }

    @Test
    public void shouldDeepMergeFailsOnDuplicateOperationIdWithFailStrategy_yaml() throws IOException {
        shouldDeepMergeFailsOnDuplicateOperationIdWithFailStrategy("yaml");
    }

    @Test
    public void shouldDeepMergeFailsOnDuplicateOperationIdWithFailStrategy_json() throws IOException {
        shouldDeepMergeFailsOnDuplicateOperationIdWithFailStrategy("json");
    }

    private void shouldDeepMergeFailsOnDuplicateOperationIdWithFailStrategy(String fileExt) throws IOException {
        File dir = Files.createTempDirectory("deep-dupopid-fail").toFile().getCanonicalFile();
        dir.deleteOnExit();
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec1." + fileExt), dir.toPath().resolve("spec1." + fileExt));
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-dupopid." + fileExt), dir.toPath().resolve("spec-dupopid." + fileExt));
        try {
            new MergedSpecBuilder(dir.getAbsolutePath().replace('\\', '/'), "_merged")
                    .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                    .withConflictStrategy(MergedSpecBuilder.MergeConflictStrategy.FAIL)
                    .buildMergedSpec();
            fail("Expected RuntimeException due to duplicate operationId with FAIL strategy");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().contains("operationId conflict"),
                    "Exception must mention the operationId conflict");
        }
    }

    @Test
    public void shouldDeepMergePreservesDistinctRefPathParameters_yaml() throws IOException {
        shouldDeepMergePreservesDistinctRefPathParameters("yaml");
    }

    @Test
    public void shouldDeepMergePreservesDistinctRefPathParameters_json() throws IOException {
        shouldDeepMergePreservesDistinctRefPathParameters("json");
    }

    /**
     * Two specs define the same path with different HTTP methods, each carrying a distinct
     * path-level parameter. Both must be retained on the merged path. Previously, parameters were
     * deduplicated using a name+in identity that collapsed every {@code $ref} parameter to the same
     * "null:null" key, dropping all but the first; the identity now falls back to the {@code $ref}
     * value so distinct references survive too.
     */
    private void shouldDeepMergePreservesDistinctRefPathParameters(String fileExt) throws IOException {
        File inputDir = Files.createTempDirectory("deep-refparams-in").toFile().getCanonicalFile();
        inputDir.deleteOnExit();
        File outputDir = Files.createTempDirectory("deep-refparams-out").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        java.nio.file.Path a = inputDir.toPath().resolve("spec-refparams-a." + fileExt);
        java.nio.file.Path b = inputDir.toPath().resolve("spec-refparams-b." + fileExt);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-refparams-a." + fileExt), a);
        Files.copy(Paths.get("src/test/resources/bugs/mergerTest/spec-refparams-b." + fileExt), b);
        String mergedSpec = new MergedSpecBuilder(
                Arrays.asList(a.toAbsolutePath().toString(), b.toAbsolutePath().toString()),
                outputDir.getAbsolutePath(), "_merged"
        ).withMergeMode(MergedSpecBuilder.MergeMode.DEEP).buildMergedSpec();

        // Parse without resolve so path-level parameters are not inlined into operations.
        OpenAPI openAPI = new OpenAPIParser().readLocation(mergedSpec, null, new ParseOptions()).getOpenAPI();
        PathItem refParams = openAPI.getPaths().get("/refparams");
        assertNotNull(refParams, "/refparams must be present");
        assertNotNull(refParams.getGet(), "GET /refparams from spec A must be present");
        assertNotNull(refParams.getPost(), "POST /refparams from spec B must be present");
        assertNotNull(refParams.getParameters(), "path-level parameters must be present");
        List<String> names = refParams.getParameters().stream()
                .map(p -> p.get$ref() != null ? p.get$ref() : p.getName())
                .collect(Collectors.toList());
        assertTrue(names.stream().anyMatch(n -> n.contains("ParamA") || "paramA".equals(n)),
                "ParamA must be retained on the merged path");
        assertTrue(names.stream().anyMatch(n -> n.contains("ParamB") || "paramB".equals(n)),
                "ParamB must be retained on the merged path");
        assertEquals(refParams.getParameters().size(), 2, "Both distinct path parameters must be kept");
    }

    @Test
    public void shouldMergeSpecsRetainsDistinctRefParametersOnCollidingPath() {
        // Build two specs in-memory that both define "/x" with a distinct $ref path-level parameter.
        // Calling mergeSpecs directly bypasses the parser's external-ref resolution, so the
        // parameters stay as $refs — exercising the $ref-based dedup identity. Without it, both
        // $ref parameters would share the "null:null" key and the second would be lost.
        OpenAPI a = new OpenAPI();
        a.setPaths(new io.swagger.v3.oas.models.Paths());
        PathItem pa = new PathItem();
        pa.setParameters(new java.util.ArrayList<>(Collections.singletonList(
                new io.swagger.v3.oas.models.parameters.Parameter().$ref("#/components/parameters/ParamA"))));
        pa.setGet(new io.swagger.v3.oas.models.Operation().operationId("getX"));
        a.getPaths().addPathItem("/x", pa);

        OpenAPI b = new OpenAPI();
        b.setPaths(new io.swagger.v3.oas.models.Paths());
        PathItem pb = new PathItem();
        pb.setParameters(new java.util.ArrayList<>(Collections.singletonList(
                new io.swagger.v3.oas.models.parameters.Parameter().$ref("#/components/parameters/ParamB"))));
        pb.setPost(new io.swagger.v3.oas.models.Operation().operationId("postX"));
        b.getPaths().addPathItem("/x", pb);

        OpenAPI merged = new MergedSpecBuilder("dummy", "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                .mergeSpecs(Arrays.asList(a, b), Collections.emptyList());

        PathItem mergedPath = merged.getPaths().get("/x");
        assertNotNull(mergedPath.getParameters(), "path-level parameters must be present");
        List<String> refs = mergedPath.getParameters().stream()
                .map(io.swagger.v3.oas.models.parameters.Parameter::get$ref)
                .collect(Collectors.toList());
        assertTrue(refs.contains("#/components/parameters/ParamA"), "ParamA $ref must be retained");
        assertTrue(refs.contains("#/components/parameters/ParamB"),
                "ParamB $ref must be retained (not collapsed to the same null:null key as ParamA)");
        assertEquals(mergedPath.getParameters().size(), 2, "Both distinct $ref parameters must be kept");
    }

    @Test
    public void shouldMergeSpecsCarriesRootTagsExternalDocsWebhooksAndPathItems() {
        // Spec A: tag "alpha", externalDocs to a.example, a webhook, and a reusable path item.
        OpenAPI a = new OpenAPI();
        a.setPaths(new io.swagger.v3.oas.models.Paths());
        a.addTagsItem(new io.swagger.v3.oas.models.tags.Tag().name("alpha").description("Alpha tag"));
        a.setExternalDocs(new io.swagger.v3.oas.models.ExternalDocumentation()
                .url("https://a.example.com").description("A docs"));
        PathItem hookA = new PathItem().post(new io.swagger.v3.oas.models.Operation().operationId("hookA"));
        a.addWebhooks("newThing", hookA);
        a.setComponents(new io.swagger.v3.oas.models.Components());
        a.getComponents().addPathItem("SharedItem",
                new PathItem().get(new io.swagger.v3.oas.models.Operation().operationId("sharedGet")));

        // Spec B: new tag "beta" plus a colliding "alpha" (must be ignored), a different
        // externalDocs (must be ignored — first wins), a distinct webhook and a distinct path item.
        OpenAPI b = new OpenAPI();
        b.setPaths(new io.swagger.v3.oas.models.Paths());
        b.addTagsItem(new io.swagger.v3.oas.models.tags.Tag().name("beta"));
        b.addTagsItem(new io.swagger.v3.oas.models.tags.Tag().name("alpha").description("SHOULD BE IGNORED"));
        b.setExternalDocs(new io.swagger.v3.oas.models.ExternalDocumentation().url("https://b.example.com"));
        PathItem hookB = new PathItem().post(new io.swagger.v3.oas.models.Operation().operationId("hookB"));
        b.addWebhooks("otherThing", hookB);
        b.setComponents(new io.swagger.v3.oas.models.Components());
        b.getComponents().addPathItem("OtherItem",
                new PathItem().get(new io.swagger.v3.oas.models.Operation().operationId("otherGet")));

        OpenAPI merged = new MergedSpecBuilder("dummy", "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                .mergeSpecs(Arrays.asList(a, b), Collections.emptyList());

        // Tags: alpha (first definition) + beta, deduplicated by name.
        assertNotNull(merged.getTags(), "Root tags must be carried over");
        List<String> tagNames = merged.getTags().stream()
                .map(io.swagger.v3.oas.models.tags.Tag::getName).collect(Collectors.toList());
        assertTrue(tagNames.contains("alpha") && tagNames.contains("beta"), "Both tags must be present");
        assertEquals(tagNames.size(), 2, "Duplicate tag name must not be added twice");
        String alphaDesc = merged.getTags().stream()
                .filter(t -> "alpha".equals(t.getName())).findFirst().get().getDescription();
        assertEquals(alphaDesc, "Alpha tag", "First tag definition must win on name collision");

        // External docs: first definition wins.
        assertNotNull(merged.getExternalDocs(), "Root externalDocs must be carried over");
        assertEquals(merged.getExternalDocs().getUrl(), "https://a.example.com",
                "First externalDocs must win");

        // Webhooks: both distinct webhooks retained.
        assertNotNull(merged.getWebhooks(), "Webhooks must be carried over");
        assertTrue(merged.getWebhooks().containsKey("newThing"), "Webhook from spec A must be present");
        assertTrue(merged.getWebhooks().containsKey("otherThing"), "Webhook from spec B must be present");

        // components.pathItems: both distinct reusable path items retained.
        assertNotNull(merged.getComponents().getPathItems(), "components.pathItems must be carried over");
        assertTrue(merged.getComponents().getPathItems().containsKey("SharedItem"),
                "pathItem from spec A must be present");
        assertTrue(merged.getComponents().getPathItems().containsKey("OtherItem"),
                "pathItem from spec B must be present");
    }

    @Test
    public void shouldRejectMixing30And31Specs() {
        // Spec A is 3.0.3 and spec B is 3.1.0. Mixing major.minor versions is not supported
        // because version-specific fields (webhooks, components.pathItems) and semantics are not
        // translated, so the merge must fail fast rather than emit an invalid/misleading document.
        OpenAPI a = new OpenAPI();
        a.setOpenapi("3.0.3");
        a.setPaths(new io.swagger.v3.oas.models.Paths());
        a.getPaths().addPathItem("/a",
                new PathItem().get(new io.swagger.v3.oas.models.Operation().operationId("getA")));

        OpenAPI b = new OpenAPI();
        b.setOpenapi("3.1.0");
        b.setPaths(new io.swagger.v3.oas.models.Paths());
        b.addWebhooks("newThing",
                new PathItem().post(new io.swagger.v3.oas.models.Operation().operationId("hookB")));

        MergedSpecBuilder builder = new MergedSpecBuilder("dummy", "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP);
        try {
            builder.mergeSpecs(Arrays.asList(a, b), Collections.emptyList());
            fail("Expected RuntimeException when merging incompatible OpenAPI versions");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage().contains("incompatible OpenAPI versions"),
                    "Exception must explain the version incompatibility");
        }
    }

    @Test
    public void shouldUseHighestPatchVersionForCompatibleSpecs() {
        // Same major.minor (3.0.x) with different patch levels is compatible; the merged document
        // should declare the highest patch version encountered.
        OpenAPI a = new OpenAPI();
        a.setOpenapi("3.0.1");
        a.setPaths(new io.swagger.v3.oas.models.Paths());
        a.getPaths().addPathItem("/a",
                new PathItem().get(new io.swagger.v3.oas.models.Operation().operationId("getA")));

        OpenAPI b = new OpenAPI();
        b.setOpenapi("3.0.3");
        b.setPaths(new io.swagger.v3.oas.models.Paths());
        b.getPaths().addPathItem("/b",
                new PathItem().get(new io.swagger.v3.oas.models.Operation().operationId("getB")));

        OpenAPI merged = new MergedSpecBuilder("dummy", "_merged")
                .withMergeMode(MergedSpecBuilder.MergeMode.DEEP)
                .mergeSpecs(Arrays.asList(a, b), Collections.emptyList());

        assertEquals(merged.getOpenapi(), "3.0.3",
                "Merged version must be the highest patch among compatible specs");
    }
}