package org.openapitools.codegen.java.jaxrs;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaJerseyServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.openapitools.codegen.TestUtils.assertFileContains;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaJerseyServerCodegenTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void before() {
        codegen = new JavaJerseyServerCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        Assert.assertEquals(codegen.getTag(), CodegenType.SERVER);
        Assert.assertEquals(codegen.getName(), "jaxrs-jersey");
        Assert.assertEquals(codegen.getTemplatingEngine().getClass(), MustacheEngineAdapter.class);
        Assert.assertEquals(codegen.getDateLibrary(), "legacy");
        Assert.assertNull(codegen.getInputSpec());

        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(JavaJerseyServerCodegen.SERVER_PORT), "8082");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.setDateLibrary("java8");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.getDateLibrary(), "java8");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        codegen.additionalProperties().put("serverPort", "8088");
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(JavaJerseyServerCodegen.SERVER_PORT), "8088");
    }

    // Helper function, intended to reduce boilerplate @ copied from ../spring/SpringCodegenTest.java
    static private Map<String, File> generateFiles(DefaultCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        Assert.assertTrue(files.size() > 0);
        TestUtils.validateJavaSourceFiles(files);
        TestUtils.validatePomXmlFiles(files);

        return files.stream().collect(Collectors.toMap(e -> e.getName().replace(outputPath, ""), i -> i));
    }

    @DataProvider(name = "codegenParameterMatrix")
    public Object[][] codegenParameterMatrix() {
        final List<Object[]> rows = new ArrayList<Object[]>();
        for (final String jerseyLibrary: ImmutableList.of("jersey1", "jersey2")) {
            for (final String dateLibrary: ImmutableList.of("joda", "java8")) {
                rows.add(new Object[] { jerseyLibrary, dateLibrary });
            }
        }
        return rows.toArray(new Object[0][0]);
    }

    // almost same test as issue #3139 on Spring
    @Test(dataProvider = "codegenParameterMatrix")
    public void testMultipartJerseyServer(final String jerseyLibrary, final String dateLibrary) throws Exception {
        codegen.setLibrary(jerseyLibrary);
        codegen.setDateLibrary(dateLibrary);

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");

        // Check files for Single, Mixed
        String[] fileS = {
                               "MultipartSingleApi.java", "MultipartSingleApiService.java", "MultipartSingleApiServiceImpl.java",
                               "MultipartMixedApi.java",  "MultipartMixedApiService.java",  "MultipartMixedApiServiceImpl.java"    };

        // UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
        // We will contact the contributor of the following test to see if the fix will break their use cases and
        // how we can fix it accordingly.
        //for (String f : fileS){
        //    assertFileContains( files.get(f).toPath(), "FormDataBodyPart file" );
        //}

        // Check files for Array
        final String[] fileA = { "MultipartArrayApiService.java", "MultipartArrayApi.java", "MultipartArrayApiServiceImpl.java"};
        for (String f : fileA) {
            assertFileContains( files.get(f).toPath(), "List<FormDataBodyPart> files");
        }
    }

    @Test
    public void testHandleDefaultValue_issue8535() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
            .readLocation("src/test/resources/3_0/issue_8535.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestHeadersApi.java"))
            .assertMethod("headersTest")
                .hasParameter("headerNumber").withType("BigDecimal")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .hasParameter("headerString").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("headerStringWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("headerStringQuotes").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("headerStringQuotesWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("headerBoolean").withType("Boolean")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"true\""));

        JavaFileAssert.assertThat(files.get("TestQueryParamsApi.java"))
            .assertMethod("queryParamsTest")
                .hasParameter("queryNumber").withType("BigDecimal")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .hasParameter("queryString").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotes").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotesWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryBoolean").withType("Boolean")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("defaultValue", "\"true\""));
    }

}
