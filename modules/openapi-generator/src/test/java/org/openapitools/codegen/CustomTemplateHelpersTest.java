package org.openapitools.codegen;

import io.vavr.collection.Array;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Tests for the Customer Template Helpers feature
 */
public class CustomTemplateHelpersTest {

    @Test
    public void testMustacheSingleCustomHelper() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintSuccessHelper.java"))
        );

        final CodegenConfigurator configurator = getConfigurator(
                baseTestDir,
                jarPath,
                "mustache",
                "src/test/resources/templating/templates/defaultgenerator/customhelpers/mustache/singlehelper"
        );

        assertCompilePetApiAndTestContent(baseTestDir, configurator, "SUCCESS");
    }

    @Test
    public void testMustacheMultiCustomHelpers() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintSuccessHelper.java"),
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintOkHelper.java")
                )
        );

        final CodegenConfigurator configurator = getConfigurator(
                baseTestDir,
                jarPath,
                "mustache",
                "src/test/resources/templating/templates/defaultgenerator/customhelpers/mustache/multihelpers"
        );

        assertCompilePetApiAndTestContent(baseTestDir, configurator, "SUCCESS-OK");
    }

    @Test
    public void testShouldNotThrowOnMustacheContextReuse() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintSuccessHelper.java"),
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintOkHelper.java")
                )
        );

        final String outputDir = Path.of(baseTestDir.toAbsolutePath().toString(), "generated").toAbsolutePath().toString();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setTemplatingEngineName("mustache")
                .addAdditionalProperty(CodegenConstants.TEMPLATE_ENGINE_HELPERS, jarPath)
                .setTemplateDir("src/test/resources/templating/templates/defaultgenerator/customhelpers/mustache/multihelpers")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(outputDir);

        final DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/PetApi.java");
        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/StoreApi.java");
        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/UserApi.java");

        File petApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/PetApi.java");
        File storeApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/StoreApi.java");
        File userApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/UserApi.java");

        Assert.assertTrue(petApiFile.exists());
        Assert.assertTrue(storeApiFile.exists());
        Assert.assertTrue(userApiFile.exists());

        Assert.assertEquals(Files.readString(petApiFile.toPath()), "SUCCESS-OK");
        Assert.assertEquals(Files.readString(storeApiFile.toPath()), "SUCCESS-OK");
        Assert.assertEquals(Files.readString(userApiFile.toPath()), "SUCCESS-OK");
    }

    @Test
    public void testShouldThrowOnMustacheContextReuseWhenNotAHelper() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintSuccessHelper.java"),
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintOkHelper.java")
                )
        );

        final String outputDir = Path.of(baseTestDir.toAbsolutePath().toString(), "generated").toAbsolutePath().toString();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setTemplatingEngineName("mustache")
                .addAdditionalProperty(CodegenConstants.TEMPLATE_ENGINE_HELPERS, jarPath)
                .addAdditionalProperty("PrintOkHelper", "This should throw because it causes a conflict when adding helpers to the context")
                .setTemplateDir("src/test/resources/templating/templates/defaultgenerator/customhelpers/mustache/multihelpers")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(outputDir);

        final DefaultGenerator generator = new DefaultGenerator();
        Assert.assertThrows(() -> {
            generator.opts(configurator.toClientOptInput()).generate();
        });
    }

    @Test
    public void testHandlebarsSingleCustomHelper() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(new File("src/test/java/org/openapitools/codegen/customhelpers/handlebars/PrintSuccessHelper.java"))
        );

        final CodegenConfigurator configurator = getConfigurator(
                baseTestDir,
                jarPath,
                "handlebars",
                "src/test/resources/templating/templates/defaultgenerator/customhelpers/handlebars/singlehelper"
        );

        assertCompilePetApiAndTestContent(baseTestDir, configurator, "SUCCESS");
    }

    @Test
    public void testHandlebarsMultiCustomHelpers() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(
                        new File("src/test/java/org/openapitools/codegen/customhelpers/handlebars/PrintSuccessHelper.java"),
                        new File("src/test/java/org/openapitools/codegen/customhelpers/handlebars/PrintOkHelper.java")
                )
        );

        final CodegenConfigurator configurator = getConfigurator(
                baseTestDir,
                jarPath,
                "handlebars",
                "src/test/resources/templating/templates/defaultgenerator/customhelpers/handlebars/multihelpers"
        );

        assertCompilePetApiAndTestContent(baseTestDir, configurator, "SUCCESS-OK");
    }

    @Test
    public void testShouldNotThrowOnHandlebarsContextReuse() throws IOException {
        final Path baseTestDir = Files.createTempDirectory("test");

        String jarPath = assertCompileFileToJar(
                baseTestDir,
                Array.of(
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintSuccessHelper.java"),
                        new File("src/test/java/org/openapitools/codegen/customhelpers/mustache/PrintOkHelper.java")
                )
        );

        final String outputDir = Path.of(baseTestDir.toAbsolutePath().toString(), "generated").toAbsolutePath().toString();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setTemplatingEngineName("mustache")
                .addAdditionalProperty(CodegenConstants.TEMPLATE_ENGINE_HELPERS, jarPath)
                .setTemplateDir("src/test/resources/templating/templates/defaultgenerator/customhelpers/mustache/multihelpers")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(outputDir);

        final DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/PetApi.java");
        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/StoreApi.java");
        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/UserApi.java");

        File petApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/PetApi.java");
        File storeApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/StoreApi.java");
        File userApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/UserApi.java");

        Assert.assertTrue(petApiFile.exists());
        Assert.assertTrue(storeApiFile.exists());
        Assert.assertTrue(userApiFile.exists());

        Assert.assertEquals(Files.readString(petApiFile.toPath()), "SUCCESS-OK");
        Assert.assertEquals(Files.readString(storeApiFile.toPath()), "SUCCESS-OK");
        Assert.assertEquals(Files.readString(userApiFile.toPath()), "SUCCESS-OK");
    }

    private static String assertCompileFileToJar(Path baseTestDir, Iterable<File> inputJavaFiles) {
        String jarPath = Path.of(baseTestDir.toAbsolutePath().toString(), "TemplateHelpers.jar").toAbsolutePath().toString();

        try {
            Assert.assertTrue(TestUtils.compileFilesToJar(
                    inputJavaFiles,
                    baseTestDir.toFile(),
                    jarPath
            ));
        } catch (Exception e) {
            Assert.fail("Custom helper compilation failed, cannot test feature", e);
        }

        return jarPath;
    }

    private static void assertCompilePetApiAndTestContent(Path baseTestDir, CodegenConfigurator configurator, String expectedContent) throws IOException {
        final String outputDir = Path.of(baseTestDir.toAbsolutePath().toString(), "generated").toAbsolutePath().toString();

        final DefaultGenerator generator = getDefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.ensureContainsFile(files, new File(outputDir), "src/main/java/org/openapitools/client/api/PetApi.java");
        File actualApiFile = new File(outputDir, "src/main/java/org/openapitools/client/api/PetApi.java");
        Assert.assertTrue(actualApiFile.exists());

        Assert.assertEquals(Files.readString(actualApiFile.toPath()), expectedContent);
    }

    private static CodegenConfigurator getConfigurator(Path baseTestDir, String jarPath, String templateEngineName, String templateDir) {
        final String outputDir = Path.of(baseTestDir.toAbsolutePath().toString(), "generated").toAbsolutePath().toString();
        return new CodegenConfigurator()
                .setGeneratorName("java")
                .setTemplatingEngineName(templateEngineName)
                .addAdditionalProperty(CodegenConstants.TEMPLATE_ENGINE_HELPERS, jarPath)
                .addGlobalProperty(CodegenConstants.APIS, "Pet")
                .setTemplateDir(templateDir)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(outputDir);
    }

    private static DefaultGenerator getDefaultGenerator() {
        final DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

        return generator;
    }
}
