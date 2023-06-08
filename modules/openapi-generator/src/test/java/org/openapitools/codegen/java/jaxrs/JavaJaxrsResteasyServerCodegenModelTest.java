package org.openapitools.codegen.java.jaxrs;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaResteasyServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableMap;

public class JavaJaxrsResteasyServerCodegenModelTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void beforeMethod() {
        codegen = new JavaResteasyServerCodegen();
    }

    @Test(description = "convert a simple java model with java8 types")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("A model with a map")
                .addProperties("map", new MapSchema());

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.vars.get(0).baseType, "Map");
        assertTrue(cm.imports.contains("HashMap"));
    }

    @Test(description = "remove suffix for int64, float and double types")
    public void testDefaultValuesFixed() {
        // we had an issue where int64, float, and double values were having single character string suffixes
        // included in their defaultValues
        // This test verifies that those characters are no longer present
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue8986.yaml");
        final JavaResteasyServerCodegen codegen = new JavaResteasyServerCodegen();
        codegen.setOpenAPI(openAPI);

        String int64Val = "100";
        String floatVal = "3.14159";
        String doubleVal = "3.14159";
        // make sure that the operation parameters omit character suffixes.
        String route = "/numericqueryparams";
        Operation op = openAPI.getPaths().get(route).getGet();
        CodegenOperation co = codegen.fromOperation(route, "GET", op, null);
        CodegenParameter int64Param = co.queryParams.get(0);
        CodegenParameter floatParam = co.queryParams.get(1);
        CodegenParameter doubleParam = co.queryParams.get(2);
        Assert.assertEquals(int64Param.defaultValue, int64Val);
        Assert.assertEquals(floatParam.defaultValue, floatVal);
        Assert.assertEquals(doubleParam.defaultValue, doubleVal);
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
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"11.2\""))
                .toParameter().toMethod()
                .hasParameter("queryString").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotes").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotesWrapped").withType("String")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryBoolean").withType("Boolean")
                    .assertParameterAnnotations()
                    .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"true\""));
    }
}
