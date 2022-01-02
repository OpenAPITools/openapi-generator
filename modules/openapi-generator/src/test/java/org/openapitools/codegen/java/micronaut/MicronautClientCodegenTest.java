package org.openapitools.codegen.java.micronaut;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.JavaMicronautClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import static java.util.stream.Collectors.groupingBy;
import static org.testng.Assert.*;
import static org.testng.Assert.fail;

public class MicronautClientCodegenTest {
    private final String PETSTORE_PATH = "src/test/resources/petstore.json";

    @Test
    public void clientOptsUnicity() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.cliOptions()
                .stream()
                .collect(groupingBy(CliOption::getOpt))
                .forEach((k, v) -> assertEquals(v.size(), 1, k + " is described multiple times"));
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://one.com/v2"));
        openAPI.setInfo(new Info());
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools");
    }

    @Test
    public void doConfigureAuthParam() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.APIS);

        // Files generated
        assertFileExists(outputPath + "/src/main/java/org/openapitools/auth/Authorization.java");
        // Endpoints are annotated with @Authorization Bindable
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Authorization");
    }

    @Test
    public void doNotConfigureAuthParam() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileNotExists(outputPath + "/src/main/java/org/openapitools/auth/");
        assertFileNotContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Authorization");
    }

    @Test
    public void doUseValidationParam() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Valid");
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@NotNull");
    }

    @Test
    public void doGenerateForMaven() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_BUILD,
                JavaMicronautClientCodegen.OPT_BUILD_MAVEN);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES);

        // Files are not generated
        assertFileExists(outputPath + "/pom.xml");
        assertFileNotExists(outputPath + "/build.gradle");
    }

    @Test
    public void doGenerateForGradle() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_BUILD,
                JavaMicronautClientCodegen.OPT_BUILD_GRADLE);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES);

        // Files are not generated
        assertFileExists(outputPath + "/build.gradle");
        assertFileNotExists(outputPath + "/pom.xml");
    }

    @Test
    public void doGenerateForTestJUnit() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_BUILD,
                JavaMicronautClientCodegen.OPT_BUILD_ALL);
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_TEST,
                JavaMicronautClientCodegen.OPT_TEST_JUNIT);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.API_TESTS, CodegenConstants.APIS, CodegenConstants.MODELS);

        // Files are not generated
        assertFileContains(outputPath + "build.gradle", "testRuntime(\"junit");
        assertFileContains(outputPath + "pom.xml", "<artifactId>micronaut-test-junit");
        assertFileNotContains(outputPath + "build.gradle", "testRuntime(\"spock");
        assertFileNotContains(outputPath + "pom.xml", "<artifactId>micronaut-test-spock");
        assertFileExists(outputPath + "src/test/java/");
        assertFileExists(outputPath + "src/test/java/org/openapitools/api/PetApiTest.java");
        assertFileContains(outputPath + "src/test/java/org/openapitools/api/PetApiTest.java", "PetApiTest", "@MicronautTest");
    }

    @Test
    public void doGenerateForTestSpock() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_BUILD,
                JavaMicronautClientCodegen.OPT_BUILD_ALL);
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_TEST,
                JavaMicronautClientCodegen.OPT_TEST_SPOCK);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.API_TESTS, CodegenConstants.APIS, CodegenConstants.MODELS);

        // Files are not generated
        assertFileNotContains(outputPath + "build.gradle", "testRuntime(\"junit");
        assertFileNotContains(outputPath + "pom.xml", "<artifactId>micronaut-test-junit");
        assertFileContains(outputPath + "build.gradle", "testRuntime(\"spock");
        assertFileContains(outputPath + "pom.xml", "<artifactId>micronaut-test-spock");
        assertFileExists(outputPath + "src/test/groovy");
        assertFileExists(outputPath + "src/test/groovy/org/openapitools/api/PetApiSpec.groovy");
        assertFileContains(outputPath + "src/test/groovy/org/openapitools/api/PetApiSpec.groovy", "PetApiSpec", "@MicronautTest");
    }

    /**
     *
     * @param codegen - the code generator
     * @param configPath - the path to the config starting from src/test/resources
     * @param filesToGenerate - which files to generate - can be CodegenConstants.MODELS, APIS, SUPPORTING_FILES, ...
     * @return - the path to the generated folder
     */
    protected String generateFiles(JavaMicronautClientCodegen codegen, String configPath, String... filesToGenerate) {
        File output = null;
        try {
            output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        } catch (IOException e) {
            fail("Unable to create temporary directory for output");
        }
        output.deleteOnExit();

        // Create parser
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(configPath, null, new ParseOptions()).getOpenAPI();

        // Configure codegen
        codegen.setOutputDir(outputPath);

        // Create input
        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        // Generate
        DefaultGenerator generator = new DefaultGenerator();
        // by default nothing is generated
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        // set all the files user wants to generate
        for (String files: filesToGenerate) {
            generator.setGeneratorPropertyDefault(files, "true");
        }

        generator.opts(input).generate();

        return outputPath + "/";
    }

    public static void assertFileContains(String path, String... lines) {
        String file = readFile(path);
        for (String line : lines)
            assertTrue(file.contains(linearize(line)), "File does not contain line [" + line + "]");
    }

    public static void assertFileNotContains(String path, String... lines) {
        String file = readFile(path);
        for (String line : lines)
            assertFalse(file.contains(linearize(line)), "File contains line [" + line + "]");
    }

    public static void assertFileExists(String path) {
        assertTrue(Paths.get(path).toFile().exists(), "File \"" + path + "\" should exist");
    }

    public static void assertFileNotExists(String path) {
        assertFalse(Paths.get(path).toFile().exists(), "File \"" + path + "\" should not exist");
    }

    public static String readFile(String path) {
        String file = null;
        try {
            String generatedFile = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
            file = linearize(generatedFile);
            assertNotNull(file);
        } catch (IOException e) {
            fail("Unable to evaluate file " + path);
        }

        return file;
    }

    public static String linearize(String target) {
        return target.replaceAll("\r?\n", "").replaceAll("\\s+", "\\s");
    }
}
