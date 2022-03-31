package org.openapitools.codegen.java.jaxrs;

import java.io.File;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;

public class JavaJAXRSCXFCDIServerCodegenTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void beforeMethod() {
        codegen = new JavaJAXRSCXFCDIServerCodegen();
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
