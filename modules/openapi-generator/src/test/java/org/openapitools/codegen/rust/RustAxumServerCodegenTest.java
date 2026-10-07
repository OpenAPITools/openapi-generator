package org.openapitools.codegen.rust;

import io.swagger.v3.oas.models.media.IntegerSchema;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.RustAxumServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.openapitools.codegen.TestUtils.linearize;

public class RustAxumServerCodegenTest {
    @Test
    public void testObjectStructFieldIsPublic() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path typesPath = Path.of(target.toString(), "/src/types.rs");
        TestUtils.assertFileExists(typesPath);
        TestUtils.assertFileContains(typesPath, "pub struct Object(pub serde_json::Value);");
    }

    @Test
    public void testPreventDuplicateOperationDeclaration() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/issue_21144.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path outputPath = Path.of(target.toString(), "/src/server/mod.rs");
        String routerSpec = linearize("Router::new() " +
                ".route(\"/api/test\", " +
                "delete(test_delete::<I, A, E, C>).post(test_post::<I, A, E, C>) ) " +
                ".route(\"/api/test/{test_id}\", " +
                "get(test_get::<I, A, E, C>) ) " +
                ".with_state(api_impl)");
        TestUtils.assertFileExists(outputPath);
        TestUtils.assertFileContains(outputPath, routerSpec);
    }

    @Test
    public void testIntegerSchemaTypeMapping() {
        RustAxumServerCodegen codegen = new RustAxumServerCodegen();
        IntegerSchema schema = new IntegerSchema();

        schema.setFormat("uint32");
        Assert.assertEquals(codegen.getSchemaType(schema), "u32");

        schema = new IntegerSchema();
        schema.setFormat("uint64");
        Assert.assertEquals(codegen.getSchemaType(schema), "u64");

        schema = new IntegerSchema();
        schema.setFormat("int32");
        schema.setMinimum(BigDecimal.ZERO);
        Assert.assertEquals(codegen.getSchemaType(schema), "u32");

        schema = new IntegerSchema();
        schema.setFormat("int64");
        schema.setMinimum(BigDecimal.ZERO);
        Assert.assertEquals(codegen.getSchemaType(schema), "u64");

        schema = new IntegerSchema();
        schema.setFormat(null);
        schema.setMinimum(BigDecimal.ZERO);
        schema.setMaximum(BigDecimal.valueOf(255));
        Assert.assertEquals(codegen.getSchemaType(schema), "u8");
    }

    @Test
    public void testGeneratedIntegerTypes() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_0/rust-axum/integer-types.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelsPath = Path.of(target.toString(), "/src/models.rs");
        TestUtils.assertFileExists(modelsPath);
        TestUtils.assertFileContains(modelsPath, "pub legacy_uint32: u32");
        TestUtils.assertFileContains(modelsPath, "pub legacy_uint64: u64");
        TestUtils.assertFileContains(modelsPath, "pub positive_int32: u32");
        TestUtils.assertFileContains(modelsPath, "pub positive_int64: u64");
        TestUtils.assertFileContains(modelsPath, "pub small_positive: u8");
        TestUtils.assertFileContains(modelsPath, "pub struct GetIntegersQueryParams");
    }

    @Test
    public void testDiscriminatorReferencingEnum() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_0/rust-axum/rust-axum-discriminator-enum-ref.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelsPath = Path.of(target.toString(), "/src/models.rs");
        TestUtils.assertFileExists(modelsPath);
        // Every discriminator value is an enum value -> tagged
        TestUtils.assertFileContains(modelsPath, "#[serde(tag = \"kind\")]\n#[allow(non_camel_case_types, clippy::large_enum_variant)]\npub enum Pet {");
        TestUtils.assertFileContains(modelsPath, "#[serde(default = \"Dog::_name_for_kind\")]");
        TestUtils.assertFileContains(modelsPath, "#[serde(serialize_with = \"Dog::_serialize_kind\")]");
        TestUtils.assertFileContains(modelsPath, "fn _name_for_kind() -> models::PetKind {");
        TestUtils.assertFileContains(modelsPath, "models::PetKind::Dog");
        TestUtils.assertFileContains(modelsPath, "fn _serialize_kind<S>(_: &models::PetKind, s: S)");
        TestUtils.assertFileContains(modelsPath, "s.serialize_str(\"dog\")");
        TestUtils.assertFileContains(modelsPath, "models::PetKind::Cat");
        TestUtils.assertFileContains(modelsPath, "pub fn new() -> Dog {");
        TestUtils.assertFileContains(modelsPath, "kind: Self::_name_for_kind(),");
        // Discriminator value "Square" is not an enum value -> untagged
        TestUtils.assertFileContains(modelsPath, "#[serde(untagged)]\n#[allow(non_camel_case_types, clippy::large_enum_variant)]\npub enum Shape {");
        // Discriminator property of "Mouse" is optional -> untagged
        TestUtils.assertFileContains(modelsPath, "#[serde(untagged)]\n#[allow(non_camel_case_types, clippy::large_enum_variant)]\npub enum Rodent {");
        TestUtils.assertFileNotContains(modelsPath, "Mouse::_name_for_kind");
    }
}
