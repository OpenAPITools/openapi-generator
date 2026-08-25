package org.openapitools.codegen.java.spring;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * End-to-end coverage for {@code forcedGenerateSchemas} combined with <em>fully-qualified</em>
 * {@code schemaMappings} on the java-spring generator. This is the Java counterpart of
 * {@link org.openapitools.codegen.kotlin.ForcedGenerateSchemasKotlinTest} and guards the same
 * behavior for the Java model-name resolution path ({@code AbstractJavaCodegen.toModelName}):
 * <ul>
 *     <li>forced declarations use the prefixed stock name (never the dotted FQN, invalid Java);</li>
 *     <li>references between forced schemas resolve to the stock names (a forced ApiCircle
 *     implements the generated ApiShape interface rather than the handwritten one);</li>
 *     <li>references from non-forced models still honor the mapping (production stays unchanged);</li>
 *     <li>type/primitive resolution is preserved (Label: string -&gt; String).</li>
 * </ul>
 */
public class ForcedGenerateSchemasSpringTest {

    private static final String SPEC = "src/test/resources/3_0/kotlin/forced-generate-schema-mapping.yaml";
    private static final String MODEL_DIR = "/src/main/java/org/openapitools/model/";

    private File generate(File output, String... forcedSchemas) {
        return generate(output, new SpringCodegen(), false, forcedSchemas);
    }

    private File generate(File output, SpringCodegen codegen, boolean generateApis, String... forcedSchemas) {
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
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, Boolean.toString(generateApis));
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

    private static class CapturingSpringCodegen extends SpringCodegen {
        private List<ModelMap> supportingModels;

        @Override
        public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
            supportingModels = ((List<?>) objs.get("models")).stream()
                    .map(ModelMap.class::cast)
                    .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
            return super.postProcessSupportingFileData(objs);
        }
    }

    private static class ShadowMutatingSpringCodegen extends SpringCodegen {
        private int modelPasses;

        @Override
        public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
            if (++modelPasses > 1) {
                additionalProperties.put("shadowPassLeak", true);
            }
            return super.postProcessAllModels(objs);
        }
    }

    @Test
    public void forcedFqnMappedSchemasAreGeneratedWithValidStockNames() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // The stock, Api-prefixed models are emitted.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertTrue(new File(modelDir, name + ".java").exists(), name + ".java must be generated");
        }

        // No file was emitted under the dotted FQN name (which would have been invalid Java).
        for (String name : Arrays.asList("Widget", "Group", "Shape", "Circle", "Square")) {
            assertFalse(new File(modelDir, "com.example.mapped." + name + ".java").exists(),
                    "no file must be emitted with the dotted FQN name for " + name);
        }

        // No forced model declaration or reference leaks the mapped FQN.
        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertFileNotContains(Paths.get(modelDir + File.separator + name + ".java"), "com.example.mapped.");
        }
    }

    @Test
    public void forcedSchemasReferenceEachOtherByStockName() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Intra-forced references resolve to the stock names.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiWidget.java"), "ApiGroup");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiGroup.java"), "ApiShape");
        // Forced Circle/Square implement the generated ApiShape interface, not the handwritten one.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.java"), "ApiCircle");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.java"), "ApiSquare");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiCircle.java"), "implements ApiShape");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiSquare.java"), "implements ApiShape");
        // The generated ApiShape interface carries the discriminator @JsonSubTypes with stock names.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.java"),
                "@JsonSubTypes.Type(value = ApiCircle.class, name = \"circle\")");
        assertFileContains(Paths.get(modelDir + File.separator + "ApiShape.java"),
                "@JsonSubTypes.Type(value = ApiSquare.class, name = \"square\")");
    }

    @Test
    public void typeMappingIsPreservedForForcedSchemas() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Label (type: string) still resolves to a Java String inside the forced ApiCircle — the
        // string alias must not surface as its own ApiLabel type.
        Path circle = Paths.get(modelDir + File.separator + "ApiCircle.java");
        assertFileContains(circle, "private String label;");
        assertFileNotContains(circle, "ApiLabel");
    }

    @Test
    public void nonForcedModelStillHonorsTheMapping() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, "Widget", "Group", "Shape", "Circle", "Square");

        // Container is neither mapped nor forced: its reference to Widget must resolve to the mapped
        // FQN, proving the forced context does not leak into non-forced models.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.java"), "com.example.mapped.Widget");
    }

    @Test
    public void forcedSchemasDoNotLeakIntoApisOrSupportingModelMetadata() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();
        CapturingSpringCodegen codegen = new CapturingSpringCodegen();

        generate(output, codegen, true, "Widget", "Group", "Shape", "Circle", "Square", "Container");

        Path api = Paths.get(output + "/src/main/java/org/openapitools/api/WidgetApi.java");
        assertFileContains(api, "com.example.mapped.Widget");
        assertFileNotContains(api, "ApiWidget");

        List<String> supportingModelNames = codegen.supportingModels.stream()
                .map(ModelMap::getModel)
                .map(model -> model.classname)
                .collect(Collectors.toList());
        assertTrue(supportingModelNames.contains("ApiContainer"));
        assertFalse(supportingModelNames.contains("ApiWidget"));
    }

    @Test
    public void wildcardForcesAllMappedSchemas() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();

        File modelDir = generate(output, CodegenConstants.FORCE_GENERATE_ALL_SCHEMAS);

        for (String name : Arrays.asList("ApiWidget", "ApiGroup", "ApiShape", "ApiCircle", "ApiSquare")) {
            assertTrue(new File(modelDir, name + ".java").exists(), name + ".java must be generated with the wildcard");
        }
        // The wildcard selects only mapping-suppressed schemas. Container remains a normal model,
        // so its reference continues to use the mapped production class.
        assertFileContains(Paths.get(modelDir + File.separator + "ApiContainer.java"), "com.example.mapped.Widget");
        assertFileNotContains(Paths.get(modelDir + File.separator + "ApiContainer.java"), "ApiWidget");
    }

    @Test
    public void shadowPassRestoresMutableGeneratorProperties() throws IOException {
        File output = Files.createTempDirectory("forced-gen-spring").toFile().getCanonicalFile();
        output.deleteOnExit();
        ShadowMutatingSpringCodegen codegen = new ShadowMutatingSpringCodegen();

        generate(output, codegen, false, "Widget");

        assertFalse(codegen.additionalProperties().containsKey("shadowPassLeak"));
    }
}
