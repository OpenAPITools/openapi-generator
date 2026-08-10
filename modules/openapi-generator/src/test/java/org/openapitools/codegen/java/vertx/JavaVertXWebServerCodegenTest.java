package org.openapitools.codegen.java.vertx;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaVertXWebServerCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.INTERFACE_ONLY;

public class JavaVertXWebServerCodegenTest {

    private JavaVertXWebServerCodegen underTest;

    @BeforeMethod
    public void setup() {
        this.underTest = new JavaVertXWebServerCodegen();
    }

    @Test
    public void itShouldSetTheDefaultTemplateKeys() {
        underTest.processOpts();

        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("api.mustache"));
        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("apiHandler.mustache"));
        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("apiImpl.mustache"));
    }

    @Test
    public void itShouldNotSetApiImplMustacheKeyWhenInterfaceOnlyIsTrue() {
        underTest.additionalProperties().put(INTERFACE_ONLY, "true");
        underTest.processOpts();

        Assert.assertFalse(underTest.apiTemplateFiles().containsKey("apiImpl.mustache"));
    }

    @Test
    public void itShouldRedactCredentialsInBodyParams() throws IOException {
        Map<String, File> files = generatePetstoreServer();
        String apiHandlerPath = files.keySet().stream()
                .filter(path -> path.endsWith("UserApiHandler.java"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("UserApiHandler.java not found"));

        File userApiHandler = files.get(apiHandlerPath);
        
        TestUtils.assertFileContains(userApiHandler.toPath(), "logger.debug(\"Parameter user is (body omitted)\");");
    }

    @Test
    public void itShouldCheckFileUploadEmptiness() throws IOException {
        Map<String, File> files = generatePetstoreServer();
        String apiHandlerPath = files.keySet().stream()
                .filter(path -> path.endsWith("PetApiHandler.java"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("PetApiHandler.java not found"));

        File petApiHandler = files.get(apiHandlerPath);

        TestUtils.assertFileContains(petApiHandler.toPath(), "if (routingContext.fileUploads().isEmpty()) {");
        TestUtils.assertFileContains(petApiHandler.toPath(), "} else {");
        TestUtils.assertFileContains(petApiHandler.toPath(), "_file = routingContext.fileUploads().iterator().next();");
    }

    private Map<String, File> generatePetstoreServer() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        DefaultGenerator defaultGenerator = new DefaultGenerator();
        ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);

        JavaVertXWebServerCodegen codegen = new JavaVertXWebServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        clientOptInput.config(codegen);
        defaultGenerator.opts(clientOptInput);

        return defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
    }
}
