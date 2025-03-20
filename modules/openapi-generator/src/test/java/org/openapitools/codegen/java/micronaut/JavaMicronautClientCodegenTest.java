package org.openapitools.codegen.java.micronaut;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaMicronautClientCodegen;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.groupingBy;
import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.assertEquals;


public class JavaMicronautClientCodegenTest extends AbstractMicronautCodegenTest {
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
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.FALSE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "org.openapitools.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "org.openapitools.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "org.openapitools");
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

    @Test
    public void doGenerateMultipleContentTypes() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();

        String outputPath = generateFiles(codegen, "src/test/resources/3_0/micronaut/content-type.yaml", CodegenConstants.APIS);

        // body and response content types should be properly annotated using @Consumes and @Produces micronaut annotations
        String apiPath = outputPath + "src/main/java/org/openapitools/api/";
        assertFileContains(apiPath + "DefaultApi.java", "@Consumes({\"application/vnd.oracle.resource+json; type=collection\", \"application/vnd.oracle.resource+json; type=error\"})");
        assertFileContains(apiPath + "DefaultApi.java", "@Produces({\"application/vnd.oracle.resource+json; type=singular\"})");
    }

    @Test
    public void doGenerateOauth2InApplicationConfig() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "true");

        String outputPath = generateFiles(codegen, "src/test/resources/3_0/micronaut/oauth2.yaml", CodegenConstants.SUPPORTING_FILES);

        // micronaut yaml property names shouldn't contain any dots
        String resourcesPath = outputPath + "src/main/resources/";
        assertFileContains(resourcesPath + "application.yml", "OAuth_2_0_Client_Credentials:");
    }

    @Test
    public void testAdditionalClientTypeAnnotations() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.ADDITIONAL_CLIENT_TYPE_ANNOTATIONS, "MyAdditionalAnnotation1(1,${param1});MyAdditionalAnnotation2(2,${param2});");
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                CodegenConstants.APIS);

        // Micronaut declarative http client should contain custom added annotations
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "MyAdditionalAnnotation1(1,${param1})");
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "MyAdditionalAnnotation2(2,${param2})");
    }

    @Test
    public void testDefaultAuthorizationFilterPattern() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "true");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.SUPPORTING_FILES, CodegenConstants.APIS);

        // Micronaut AuthorizationFilter should default to match all patterns
        assertFileContains(outputPath + "/src/main/java/org/openapitools/auth/AuthorizationFilter.java", "@Filter(Filter.MATCH_ALL_PATTERN)");
    }

    @Test
    public void testAuthorizationFilterPattern() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.OPT_CONFIGURE_AUTH, "true");
        codegen.additionalProperties().put(JavaMicronautClientCodegen.AUTHORIZATION_FILTER_PATTERN, "pet/**");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.SUPPORTING_FILES, CodegenConstants.APIS);

        // Micronaut AuthorizationFilter should match the provided pattern
        assertFileContains(outputPath + "/src/main/java/org/openapitools/auth/AuthorizationFilter.java", "@Filter(\"pet/**\")");
    }

    @Test
    public void testNoConfigureClientId() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.APIS);

        // Micronaut declarative http client should not specify a Client id
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Client(\"${openapi-micronaut-client-base-path}\")");
    }

    @Test
    public void testConfigureClientId() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.CLIENT_ID, "unit-test");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.APIS);

        // Micronaut declarative http client should use the provided Client id
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Client( id = \"unit-test\", path = \"${openapi-micronaut-client-base-path}\")");
    }

    @Test
    public void testDefaultPathSeparator() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.APIS);

        // Micronaut declarative http client should use the default path separator
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Client(\"${openapi-micronaut-client-base-path}\")");
    }

    @Test
    public void testConfigurePathSeparator() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(JavaMicronautClientCodegen.BASE_PATH_SEPARATOR, ".");
        String outputPath = generateFiles(codegen, PETSTORE_PATH, CodegenConstants.APIS);

        // Micronaut declarative http client should use the provided path separator
        assertFileContains(outputPath + "/src/main/java/org/openapitools/api/PetApi.java", "@Client(\"${openapi-micronaut-client.base-path}\")");
    }

    /**
     * General XML annotations test (both JAXB and Jackson)
     * <br>
     * Includes regression tests for:
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/2417">Correct Jackson annotation when `wrapped: false`</a>
     */
    @Test
    public void shouldGenerateCorrectXmlAnnotations() {
        // Arrange
        final CodegenConfigurator config = new CodegenConfigurator()
                .addAdditionalProperty(CodegenConstants.WITH_XML, true)
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, null)
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, null)
                .setGeneratorName(JavaMicronautClientCodegen.NAME)
                .setInputSpec("src/test/resources/3_0/java/xml-annotations-test.yaml")
                .setOutputDir(newTempFolder().toString());

        // Act
        final List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Assert
        JavaFileAssert.assertThat(files.get(0))
                .assertTypeAnnotations()
                .containsWithNameAndAttributes("JacksonXmlRootElement", Map.of("localName", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
                .toType()

                // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                .assertProperty("tags").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Tag\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"TagList\""))
                .toProperty().toType()
                .assertMethod("getTags")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Tag\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"TagList\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("friends").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"friend-pet\""))
                .toProperty().toType()
                .assertMethod("getFriends")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"friend-pet\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)
                .assertProperty("status").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"PetStatus\""))
                .toProperty().toType()
                .assertMethod("getStatus")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"PetStatus\""))
                .toFileAssert()

                // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
                //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("photoUrls").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"photoUrls\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"photoUrls\""))
                .toProperty().toType()
                .assertMethod("getPhotoUrls")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"photoUrls\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"photoUrls\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
                .assertProperty("name").assertPropertyAnnotations()
                .doesNotContainWithName("XmlElement")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlAttribute", Map.of("name", "\"name\""))
                .toProperty().toType()
                .assertMethod("getName")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("isAttribute", "true", "localName", "\"name\""))
                .toFileAssert()

                // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
                .assertProperty("id").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toProperty().toType()
                .assertMethod("getId")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toFileAssert()

                // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("foods").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"yummy-yummy\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"yummy-yummy\""))
                .toProperty().toType()
                .assertMethod("getFoods")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"yummy-yummy\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"yummy-yummy\""))
                .toFileAssert()

                // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("colors").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"color\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"colors\""))
                .toProperty().toType()
                .assertMethod("getColors")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"color\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"colors\""))
                .toFileAssert()

                // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("categories").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Category\""))
                .toProperty().toType()
                .assertMethod("getCategories")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Category\""))
                // ↓ specific regression test for #2417: (useWrapping=false) needs to be present
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("activities").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"item\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"activities-array\""))
                .toProperty().toType()
                .assertMethod("getActivities")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"item\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"activities-array\""));
    }
}
