package org.openapitools.codegen.csharpnetcore;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.testng.Assert.assertTrue;

/**
 * End-to-end coverage for {@code forcedGenerateSchemas} combined with <em>fully-qualified</em>
 * {@code schemaMappings} on the C# generator.
 * <p>
 * This mirrors {@code ForcedGenerateSchemasKotlinTest} on a different generator family to
 * demonstrate that the two-phase forced-schema generation in {@link DefaultGenerator} is
 * supported across representative generator families. The same generic model graph is used
 * (Widget/Group/Shape/Circle/Square are mapped to hand-written classes but also force-generated).
 */
public class ForcedGenerateSchemasCSharpTest {

    private static final String SPEC = "src/test/resources/3_0/kotlin/forced-generate-schema-mapping.yaml";
    private static final String MODEL_DIR = "/src/Org.OpenAPITools/Model/";

    private File generate(File output, String... forcedSchemas) {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("restsharp");
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setModelNamePrefix("Api");

        codegen.schemaMapping().put("Widget", "Com.Example.Mapped.Widget");
        codegen.schemaMapping().put("Group", "Com.Example.Mapped.Group");
        codegen.schemaMapping().put("Shape", "Com.Example.Mapped.Shape");
        codegen.schemaMapping().put("Circle", "Com.Example.Mapped.Circle");
        codegen.schemaMapping().put("Square", "Com.Example.Mapped.Square");

        codegen.forcedGenerateSchemas().addAll(Arrays.asList(forcedSchemas));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec(SPEC))
                        .config(codegen))
                .generate();

        return new File(output, MODEL_DIR);
    }

    @Test
    public void forcedFqnMappedSchemasAreGeneratedWithStockNames() throws IOException {
        File output = Files.createTempDirectory("forced-gen-csharp").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // The forced+mapped schemas are emitted as stock ApiXxx classes despite the FQN mapping.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertTrue(new File(modelDir, name + ".cs").exists(), name + ".cs must be generated");
        }
        assertFileContains(Paths.get(modelDir + File.separator + "ApiWidget.cs"), "class ApiWidget");

        // No forced model declaration or reference leaks the mapped FQN.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertFileNotContains(Paths.get(modelDir + File.separator + name + ".cs"), "Com.Example.Mapped.");
        }

        // Container is neither mapped nor forced: its reference to Widget keeps the mapped class.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.cs"), "Com.Example.Mapped.Widget");
    }

    @Test
    public void wildcardForcesAllMappedSchemas() throws IOException {
        File output = Files.createTempDirectory("forced-gen-csharp").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, CodegenConstants.FORCE_GENERATE_ALL_SCHEMAS);

        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare", "ApiContainer")) {
            assertTrue(new File(modelDir, name + ".cs").exists(), name + ".cs must be generated with the wildcard");
        }
        // The forced (mapping-suppressed) schemas never leak the mapped FQN into their stock files.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertFileNotContains(Paths.get(modelDir + File.separator + name + ".cs"), "Com.Example.Mapped.");
        }
        // The wildcard selects only mapping-suppressed schemas. Container remains a normal model,
        // so its reference continues to use the mapped production class.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.cs"), "Com.Example.Mapped.Widget");
        assertFileNotContains(Paths.get(modelDir + File.separator + "ApiContainer.cs"), "ApiWidget");
    }
}
