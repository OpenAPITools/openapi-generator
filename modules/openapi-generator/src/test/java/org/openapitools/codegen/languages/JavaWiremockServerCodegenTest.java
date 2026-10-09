package org.openapitools.codegen.languages;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class JavaWiremockServerCodegenTest {

    @Test
    public void exampleStringEscapesQuotesAndBackslashes() throws IOException {
        final JavaWiremockServerCodegen codegen = new JavaWiremockServerCodegen();
        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/java-wiremock/example-string-escaping.yaml");

        assertFileContains(files.get("DefaultApiMockServer.java").toPath(),
                "return \"{ \\\"note\\\" : \\\"has \\\\\\\"quote\\\\\\\" inside\\\" }\";",
                "return \"{ \\\"windowsPath\\\" : \\\"C:\\\\\\\\temp\\\\\\\\file.txt\\\", \\\"pattern\\\" : \\\"\\\\\\\\d+\\\" }\";");
    }

    private Map<String, File> generateFiles(JavaWiremockServerCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        List<File> files = generator.opts(input).generate();

        return files.stream().collect(Collectors.toMap(e -> e.getName().replace(outputPath, ""), i -> i));
    }
}
