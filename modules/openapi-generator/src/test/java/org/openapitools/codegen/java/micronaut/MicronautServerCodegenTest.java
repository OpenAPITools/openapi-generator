package org.openapitools.codegen.java.micronaut;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaMicronautServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import static java.util.stream.Collectors.groupingBy;
import static org.testng.Assert.assertEquals;

public class MicronautServerCodegenTest extends AbstractMicronautCodegenTest {
    protected static String ROLES_EXTENSION_TEST_PATH = "src/test/resources/3_0/micronaut/roles-extension-test.yaml";
    protected static String MULTI_TAGS_TEST_PATH = "src/test/resources/3_0/micronaut/multi-tags-test.yaml";

    @Test
    public void clientOptsUnicity() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.cliOptions()
                .stream()
                .collect(groupingBy(CliOption::getOpt))
                .forEach((k, v) -> assertEquals(v.size(), 1, k + " is described multiple times"));
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://one.com/v2"));
        openAPI.setInfo(new Info());
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.controller");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.controller");
        Assert.assertEquals(codegen.additionalProperties().get(JavaMicronautServerCodegen.OPT_CONTROLLER_PACKAGE), "org.openapitools.controller");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools");
    }

    @Test
    public void testApiAndModelFilesPresent() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "org.test.test");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.test.test.model");
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_CONTROLLER_PACKAGE, "org.test.test.controller");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.APIS,
                CodegenConstants.MODELS);

        String invokerFolder = outputPath + "src/main/java/org/test/test/";
        assertFileExists(invokerFolder + "Application.java");

        String controllerFolder = outputPath + "src/main/java/org/test/test/controller/";
        assertFileExists(controllerFolder + "PetController.java");
        assertFileExists(controllerFolder + "StoreController.java");
        assertFileExists(controllerFolder + "UserController.java");

        String modelFolder = outputPath + "src/main/java/org/test/test/model/";
        assertFileExists(modelFolder + "Pet.java");
        assertFileExists(modelFolder + "User.java");
        assertFileExists(modelFolder + "Order.java");

        String resources = outputPath + "src/main/resources/";
        assertFileExists(resources + "application.yml");
    }

    @Test
    public void doUseValidationParam() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.USE_BEANVALIDATION, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileContains(outputPath + "/src/main/java/org/openapitools/controller/PetController.java", "@Valid");
        assertFileContains(outputPath + "/src/main/java/org/openapitools/controller/PetController.java", "@NotNull");
    }

    @Test
    public void doNotUseValidationParam() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.USE_BEANVALIDATION, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Files are not generated
        assertFileNotContains(outputPath + "/src/main/java/org/openapitools/controller/PetController.java", "@Valid");
        assertFileNotContains(outputPath + "/src/main/java/org/openapitools/controller/PetController.java", "@NotNull");
    }

    @Test
    public void doGenerateForMaven() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_BUILD,
                JavaMicronautServerCodegen.OPT_BUILD_MAVEN);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES);

        // Files are not generated
        assertFileExists(outputPath + "/pom.xml");
        assertFileNotExists(outputPath + "/build.gradle");
    }

    @Test
    public void doGenerateForGradle() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_BUILD,
                JavaMicronautServerCodegen.OPT_BUILD_GRADLE);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES);

        // Files are not generated
        assertFileExists(outputPath + "/build.gradle");
        assertFileNotExists(outputPath + "/pom.xml");
    }

    @Test
    public void doGenerateForTestJUnit() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_BUILD,
                JavaMicronautServerCodegen.OPT_BUILD_ALL);
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_TEST,
                JavaMicronautServerCodegen.OPT_TEST_JUNIT);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.API_TESTS, CodegenConstants.APIS, CodegenConstants.MODELS);

        // Files are not generated
        assertFileContains(outputPath + "build.gradle", "testRuntime(\"junit");
        assertFileContains(outputPath + "pom.xml", "<artifactId>micronaut-test-junit");
        assertFileNotContains(outputPath + "build.gradle", "testRuntime(\"spock");
        assertFileNotContains(outputPath + "pom.xml", "<artifactId>micronaut-test-spock");
        assertFileExists(outputPath + "src/test/java/");
        assertFileExists(outputPath + "src/test/java/org/openapitools/controller/PetControllerTest.java");
        assertFileContains(outputPath + "src/test/java/org/openapitools/controller/PetControllerTest.java", "PetControllerTest", "@MicronautTest");
    }

    @Test
    public void doGenerateForTestSpock() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_BUILD,
                JavaMicronautServerCodegen.OPT_BUILD_ALL);
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_TEST,
                JavaMicronautServerCodegen.OPT_TEST_SPOCK);
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.SUPPORTING_FILES,
                CodegenConstants.API_TESTS, CodegenConstants.APIS, CodegenConstants.MODELS);

        // Files are not generated
        assertFileNotContains(outputPath + "build.gradle", "testRuntime(\"junit");
        assertFileNotContains(outputPath + "pom.xml", "<artifactId>micronaut-test-junit");
        assertFileContains(outputPath + "build.gradle", "testRuntime(\"spock");
        assertFileContains(outputPath + "pom.xml", "<artifactId>micronaut-test-spock");
        assertFileExists(outputPath + "src/test/groovy");
        assertFileExists(outputPath + "src/test/groovy/org/openapitools/controller/PetControllerSpec.groovy");
        assertFileContains(outputPath + "src/test/groovy/org/openapitools/controller/PetControllerSpec.groovy", "PetControllerSpec", "@MicronautTest");
    }

    @Test
    public void doGenerateRequiredPropertiesInConstructor() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "true");
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
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "false");
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

    @Test
    public void testExtraAnnotations() throws Exception {
        
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        String outputPath = generateFiles(codegen, "src/test/resources/3_0/issue_11772.yml", CodegenConstants.MODELS);

        TestUtils.assertExtraAnnotationFiles(outputPath + "/src/main/java/org/openapitools/model");

    }

    @Test
    public void doNotGenerateAuthRolesWithExtensionWhenNotUseAuth() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_USE_AUTH, false);
        String outputPath = generateFiles(codegen, ROLES_EXTENSION_TEST_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileNotContains(controllerPath + "BooksController.java", "@Secured");
        assertFileNotContains(controllerPath + "UsersController.java", "@Secured");
        assertFileNotContains(controllerPath + "ReviewsController.java", "@Secured");
    }

    @Test
    public void generateAuthRolesWithExtension() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_USE_AUTH, true);
        String outputPath = generateFiles(codegen, ROLES_EXTENSION_TEST_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileContainsRegex(controllerPath + "BooksController.java", "IS_ANONYMOUS[^;]{0,100}bookSearchGet");
        assertFileContainsRegex(controllerPath + "BooksController.java", "@Secured\\(\\{\"admin\"\\}\\)[^;]{0,100}createBook");
        assertFileContainsRegex(controllerPath + "BooksController.java", "IS_ANONYMOUS[^;]{0,100}getBook");
        assertFileContainsRegex(controllerPath + "BooksController.java", "IS_AUTHENTICATED[^;]{0,100}reserveBook");

        assertFileContainsRegex(controllerPath + "ReviewsController.java", "IS_AUTHENTICATED[^;]{0,100}bookSendReviewPost");
        assertFileContainsRegex(controllerPath + "ReviewsController.java", "IS_ANONYMOUS[^;]{0,100}bookViewReviewsGet");

        assertFileContainsRegex(controllerPath + "UsersController.java", "IS_ANONYMOUS[^;]{0,100}getUserProfile");
        assertFileContainsRegex(controllerPath + "UsersController.java", "IS_AUTHENTICATED[^;]{0,100}updateProfile");
    }

    @Test
    public void doGenerateMonoWrapHttpResponse() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REACTIVE, "true");
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_WRAP_IN_HTTP_RESPONSE, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileContains(controllerPath + "PetController.java", "Mono<HttpResponse<Pet>>");
    }

    @Test
    public void doGenerateMono() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REACTIVE, "true");
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_WRAP_IN_HTTP_RESPONSE, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileContains(controllerPath + "PetController.java", "Mono<Pet>");
        assertFileNotContains(controllerPath + "PetController.java", "HttpResponse");
    }

    @Test
    public void doGenerateWrapHttpResponse() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REACTIVE, "false");
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_WRAP_IN_HTTP_RESPONSE, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileContains(controllerPath + "PetController.java", "HttpResponse<Pet>");
        assertFileNotContains(controllerPath + "PetController.java", "Mono");
    }

    @Test
    public void doGenerateNoMonoNoWrapHttpResponse() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_REACTIVE, "false");
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_WRAP_IN_HTTP_RESPONSE, "false");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.MODELS, CodegenConstants.APIS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        assertFileContains(controllerPath + "PetController.java", "Pet");
        assertFileNotContains(controllerPath + "PetController.java", "Mono");
        assertFileNotContains(controllerPath + "PetController.java", "HttpResponse");
    }

    @Test
    public void doGenerateOperationOnlyForFirstTag() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        String outputPath = generateFiles(codegen, MULTI_TAGS_TEST_PATH, CodegenConstants.MODELS,
                CodegenConstants.APIS, CodegenConstants.API_TESTS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        String controllerTestPath = outputPath + "/src/test/java/org/openapitools/controller/";

        // Verify files are generated only for the required tags
        assertFileExists(controllerPath + "AuthorsController.java");
        assertFileExists(controllerPath + "BooksController.java");
        assertFileNotExists(controllerPath + "SearchController.java");

        // Verify the same for test files
        assertFileExists(controllerTestPath + "AuthorsControllerTest.java");
        assertFileExists(controllerTestPath + "BooksControllerTest.java");
        assertFileNotExists(controllerTestPath + "SearchControllerTest.java");

        // Verify all the methods are generated only ones
        assertFileContains(controllerPath + "AuthorsController.java",
                "authorSearchGet", "getAuthor", "getAuthorBooks");
        assertFileContains(controllerPath + "BooksController.java",
                "bookCreateEntryPost", "bookSearchGet", "bookSendReviewPost", "getBook", "isBookAvailable");
        assertFileNotContains(controllerPath + "BooksController.java", "getAuthorBooks");
    }

    @Test
    public void doRepeatOperationForAllTags() {
        JavaMicronautServerCodegen codegen = new JavaMicronautServerCodegen();
        codegen.additionalProperties().put(JavaMicronautServerCodegen.OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG, "false");
        String outputPath = generateFiles(codegen, MULTI_TAGS_TEST_PATH, CodegenConstants.MODELS,
                CodegenConstants.APIS, CodegenConstants.API_TESTS);

        String controllerPath = outputPath + "src/main/java/org/openapitools/controller/";
        String controllerTestPath = outputPath + "/src/test/java/org/openapitools/controller/";

        // Verify all the tags created
        assertFileExists(controllerPath + "AuthorsController.java");
        assertFileExists(controllerPath + "BooksController.java");
        assertFileExists(controllerPath + "SearchController.java");

        // Verify the same for test files
        assertFileExists(controllerTestPath + "AuthorsControllerTest.java");
        assertFileExists(controllerTestPath + "BooksControllerTest.java");
        assertFileExists(controllerTestPath + "SearchControllerTest.java");

        // Verify all the methods are repeated for each of the tags
        assertFileContains(controllerPath + "AuthorsController.java",
                "authorSearchGet", "getAuthor", "getAuthorBooks");
        assertFileContains(controllerPath + "BooksController.java",
                "bookCreateEntryPost", "bookSearchGet", "bookSendReviewPost", "getBook", "isBookAvailable", "getAuthorBooks");
        assertFileContains(controllerPath + "SearchController.java",
                "authorSearchGet", "bookSearchGet");
    }
}
