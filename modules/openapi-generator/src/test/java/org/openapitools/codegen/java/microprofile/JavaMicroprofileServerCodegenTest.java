package org.openapitools.codegen.java.microprofile;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.JavaMicroprofileServerCodegen;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.validateJavaSourceFiles;

public class JavaMicroprofileServerCodegenTest {

    protected JavaMicroprofileServerCodegen codegen;

    @BeforeMethod
    public void before() {
        codegen = new JavaMicroprofileServerCodegen();
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationTrue_issue19674() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_19674.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("return UNKNOWN_DEFAULT_OPEN_API");

    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationNotSet_issue19674() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_19674.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("throw new IllegalArgumentException(\"Unexpected value '\" + text + \"'\");");

    }

    @Test
    public void testMicroprofileCanHandleCookieParams() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/microprofile_cookie.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        List<File> files = new DefaultGenerator().opts(input).generate();

        Map<String, File> filesMap = files.stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        validateJavaSourceFiles(files);

        JavaFileAssert.assertThat(filesMap.get("DefaultApi.java"))
                .assertMethod("getCustomer").assertParameter("cookieParameter");
    }

    @Test
    public void testMicroprofileCanHandleCookieParamsSingleRequest() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/microprofile_cookie.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        List<File> files = new DefaultGenerator().opts(input).generate();

        Map<String, File> filesMap = files.stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        validateJavaSourceFiles(files);

        JavaFileAssert.assertThat(filesMap.get("DefaultApi.java"))
                .assertInnerClass("GetCustomerRequest")
                .assertMethod("cookieParameter");
    }
}