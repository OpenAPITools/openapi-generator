package org.openapitools.codegen.kotlin;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * End-to-end coverage for {@code forcedGenerateSchemas} combined with <em>fully-qualified</em>
 * {@code schemaMappings} on the kotlin-spring generator.
 * <p>
 * These tests use a generic model graph where Widget/Group/Shape/Circle/Square are mapped to
 * hand-written classes but are also force-generated. The stock {@code ApiXxx} models must be
 * emitted "as if" the mappings did not apply:
 * <ul>
 *     <li>declarations use the prefixed stock name (never the dotted FQN, which is invalid Kotlin);</li>
 *     <li>references between forced schemas resolve to the stock names (so a forced ApiCircle
 *     implements the generated ApiShape rather than the hand-written sealed interface);</li>
 *     <li>references from non-forced models still honor the mapping (production stays unchanged);</li>
 *     <li>typeMapping / primitive resolution is preserved (Label: string -&gt; kotlin.String).</li>
 * </ul>
 * The pre-existing {@code DefaultGeneratorTest.forcedGenerateSchemaOverridesSchemaMappingSkip} did
 * not catch the underlying bug because it used the Java generator, a simple (non-FQN) mapped name,
 * a leaf schema, and asserted only file existence.
 */
public class ForcedGenerateSchemasKotlinTest {

    private static final String SPEC = "src/test/resources/3_0/kotlin/forced-generate-schema-mapping.yaml";
    private static final String MODEL_DIR = "/src/main/kotlin/org/openapitools/model/";

    private File generate(File output, String... forcedSchemas) {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setModelNamePrefix("Api");
        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        codegen.schemaMapping().put("Widget", "com.example.mapped.Widget");
        codegen.schemaMapping().put("Group", "com.example.mapped.Group");
        codegen.schemaMapping().put("Shape", "com.example.mapped.Shape");
        codegen.schemaMapping().put("Circle", "com.example.mapped.Circle");
        codegen.schemaMapping().put("Square", "com.example.mapped.Square");

        codegen.forcedGenerateSchemas().addAll(Arrays.asList(forcedSchemas));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        generator.opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec(SPEC))
                        .config(codegen))
                .generate();

        return new File(output, MODEL_DIR);
    }

    @Test
    public void forcedFqnMappedSchemasAreGeneratedWithValidStockNames() throws IOException {
        File output = Files.createTempDirectory("forced-gen").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // The stock, Api-prefixed models are emitted.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertTrue(new File(modelDir, name + ".kt").exists(), name + ".kt must be generated");
        }

        // No file was emitted under the dotted FQN name (which would have been invalid Kotlin).
        for (String name : Arrays.asList("Widget", "Group", "Shape", "Circle", "Square")) {
            assertFalse(new File(modelDir, "com.example.mapped." + name + ".kt").exists(),
                    "no file must be emitted with the dotted FQN name for " + name);
        }

        // No forced model declaration or reference leaks the mapped FQN.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertFileNotContains(Paths.get(modelDir + File.separator + name + ".kt"), "com.example.mapped.");
        }
    }

    @Test
    public void forcedSchemasReferenceEachOtherByStockName() throws IOException {
        File output = Files.createTempDirectory("forced-gen").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Intra-forced references resolve to the stock names.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiWidget.kt"), "ApiGroup");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiGroup.kt"), "ApiShape");
        // Forced Circle/Square implement the generated ApiShape, not the handwritten sealed one.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.kt"), "ApiCircle");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.kt"), "ApiSquare");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiCircle.kt"), "ApiShape");
        // The discriminator property must be marked as inherited (override) and default to the
        // mapping value, exactly as it would if no schemaMapping existed. Missing the `override`
        // would fail to compile against the `type` declared on the sealed ApiShape interface.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiCircle.kt"),
                "override val type: kotlin.String = \"circle\"");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiSquare.kt"),
                "override val type: kotlin.String = \"square\"");
    }

    @Test
    public void typeMappingIsPreservedForForcedSchemas() throws IOException {
        File output = Files.createTempDirectory("forced-gen").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Label (type: string) still resolves to a Kotlin String inside the forced ApiCircle.
        Path circle = Paths.get(modelDir + File.separator + "ApiCircle.kt");
        assertFileContains(circle, "label");
        assertFileContains(circle, "String");
        assertFileNotContains(circle, "Label");
    }

    @Test
    public void nonForcedModelStillHonorsTheMapping() throws IOException {
        File output = Files.createTempDirectory("forced-gen").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Container is neither mapped nor forced: its reference to Widget must resolve to the mapped
        // FQN, proving the forced context does not leak into non-forced models.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.kt"), "com.example.mapped.Widget");
    }

    @Test
    public void wildcardForcesAllMappedSchemas() throws IOException {
        File output = Files.createTempDirectory("forced-gen").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, CodegenConstants.FORCE_GENERATE_ALL_SCHEMAS);

        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertTrue(new File(modelDir, name + ".kt").exists(), name + ".kt must be generated with the wildcard");
        }
        // The wildcard selects only mapping-suppressed schemas. Container remains a normal model,
        // so its reference continues to use the mapped production class.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.kt"), "com.example.mapped.Widget");
        assertFileNotContains(Paths.get(modelDir + File.separator + "ApiContainer.kt"), "ApiWidget");
    }
}
