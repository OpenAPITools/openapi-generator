package org.openapitools.codegen;

import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.openapitools.codegen.languages.GroovyClientCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.openapitools.codegen.languages.PerlClientCodegen;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.openapitools.codegen.languages.PowerShellClientCodegen;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.languages.PythonPydanticV1ClientCodegen;
import org.openapitools.codegen.languages.RClientCodegen;
import org.openapitools.codegen.languages.RubyClientCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class ForcedGenerateSchemasSupportedFamiliesTest {

    private static final String SPEC = "src/test/resources/3_0/kotlin/forced-generate-schema-mapping.yaml";

    @DataProvider(name = "supportedGenerators")
    public Object[][] supportedGenerators() {
        return new Object[][]{
                {new JavaClientCodegen()},
                {new GroovyClientCodegen()},
                {new KotlinClientCodegen()},
                {new CSharpClientCodegen()},
                {new PythonClientCodegen()},
                {new PythonPydanticV1ClientCodegen()},
                {new PhpClientCodegen()},
                {new GoClientCodegen()},
                {new PerlClientCodegen()},
                {new PowerShellClientCodegen()},
                {new RClientCodegen()},
                {new RubyClientCodegen()}
        };
    }

    @Test(dataProvider = "supportedGenerators")
    public void supportedGeneratorEmitsStockShadowModelAndRestoresMappings(CodegenConfig codegen) throws Exception {
        File output = Files.createTempDirectory("forced-gen-" + codegen.getName()).toFile().getCanonicalFile();
        output.deleteOnExit();

        String stockFilename = codegen.toModelFilename("Widget");
        String stockRelatedModelName = codegen.toModelName("Group");
        String mappedName = "com.example.mapped.Widget";
        String mappedRelatedName = "com.example.mapped.Group";
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.schemaMapping().put("Widget", mappedName);
        codegen.schemaMapping().put("Group", mappedRelatedName);
        codegen.forcedGenerateSchemas().addAll(List.of("Widget", "Group"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        List<File> generatedFiles = generator.opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec(SPEC))
                        .config(codegen))
                .generate();

        File widgetFile = generatedFiles.stream()
                .filter(file -> file.getName().contains(stockFilename))
                .findFirst()
                .orElseThrow(() -> new AssertionError(
                        codegen.getName() + " must emit the forced schema under its stock filename"));
        String widgetContents = Files.readString(Path.of(widgetFile.toURI()));
        assertTrue(widgetContents.contains(stockRelatedModelName),
                codegen.getName() + " must use the stock name for a reference between shadow models");
        assertFalse(widgetContents.contains(mappedRelatedName),
                codegen.getName() + " must not leak mapped names into shadow models");
        assertEquals(codegen.schemaMapping().get("Widget"), mappedName,
                codegen.getName() + " must restore schema mappings after the shadow pass");
        assertEquals(codegen.schemaMapping().get("Group"), mappedRelatedName,
                codegen.getName() + " must restore all schema mappings after the shadow pass");
    }
}
