package org.openapitools.codegen.java.micronaut;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.JavaMicronautClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;
import static java.util.stream.Collectors.groupingBy;
import static org.testng.Assert.*;


public class MicronautClientCodegenTest extends AbstractMicronautCodegenTest {
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
    public void testApiAndModelFilesPresent() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "org.test.test");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.test.test.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.test.test.api");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.APIS,
                CodegenConstants.MODELS);

        String apiFolder = outputPath + "src/main/java/org/test/test/api/";
        assertFileExists(apiFolder + "PetApi.java");
        assertFileExists(apiFolder + "StoreApi.java");
        assertFileExists(apiFolder + "UserApi.java");

        String modelFolder = outputPath + "src/main/java/org/test/test/model/";
        assertFileExists(modelFolder + "Pet.java");
        assertFileExists(modelFolder + "User.java");
        assertFileExists(modelFolder + "Order.java");

        String resources = outputPath + "src/main/resources/";
        assertFileExists(resources + "application.yml");
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
        codegen.additionalProperties().put(JavaMicronautClientCodegen.USE_BEANVALIDATION, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Valid");
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@NotNull");
    }

    @Test
    public void doNotUseValidationParam() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.USE_BEANVALIDATION, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileNotContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Valid");
        assertFileNotContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@NotNull");
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

    @Test
    public void doGenerateRequiredPropertiesInConstructor() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        // Constructor should have properties
        String modelPath = outputPath + "src/main/java/org/openapitools/model/";
        assertFileContains(modelPath + "Pet.java", "public Pet(String name, List<String> photoUrls)");
        assertFileNotContains(modelPath + "Pet.java", "public Pet()");
        assertFileContains(modelPath + "User.java", "public User()");
        assertFileContains(modelPath + "Order.java", "public Order()");
    }

    @Test
    public void doNotGenerateRequiredPropertiesInConstructor() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        // Constructor should have properties
        String modelPath = outputPath + "src/main/java/org/openapitools/model/";
        assertFileContains(modelPath + "Pet.java", "public Pet()");
        assertFileNotContainsRegex(modelPath + "Pet.java", "public Pet\\([^)]+\\)");
        assertFileContains(modelPath + "User.java", "public User()");
        assertFileNotContainsRegex(modelPath + "User.java", "public User\\([^)]+\\)");
        assertFileContains(modelPath + "Order.java", "public Order()");
        assertFileNotContainsRegex(modelPath + "Order.java", "public Order\\([^)]+\\)");
    }
}
