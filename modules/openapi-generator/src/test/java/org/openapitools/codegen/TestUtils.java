package org.openapitools.codegen;

import static org.testng.Assert.assertTrue;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.testng.Assert;

import java.io.File;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class TestUtils {

    public static OpenAPI parseSpec(String specFilePath) {
        return new OpenAPIParser().readLocation(specFilePath, null, new ParseOptions()).getOpenAPI();
    }

    public static OpenAPI parseContent(String jsonOrYaml) {
        return new OpenAPIParser().readContents(jsonOrYaml, null, null).getOpenAPI();
    }

    public static OpenAPI createOpenAPI() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.setPaths(new Paths());

        final Info info = new Info();
        info.setDescription("API under test");
        info.setVersion("1.0.7");
        info.setTitle("My title");
        openAPI.setInfo(info);

        final Server server = new Server();
        server.setUrl("https://localhost:9999/root");
        openAPI.setServers(Collections.singletonList(server));
        return openAPI;
    }

    public static OpenAPI createOpenAPIWithOneSchema(String name, Schema schema) {
        OpenAPI openAPI = createOpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas(name, schema);
        return openAPI;
    }

    public static WrittenTemplateBasedFile getTemplateBasedFile(MockDefaultGenerator generator, File root, String filename) {
        String defaultApiFilename = new File(root, filename).getAbsolutePath().replace("\\", "/");
        Optional<WrittenTemplateBasedFile> optional = generator.getTemplateBasedFiles().stream().filter(f -> defaultApiFilename.equals(f.getOutputFilename())).findFirst();
        Assert.assertTrue(optional.isPresent());
        return optional.get();
    }

    public static void ensureContainsFile(Map<String, String> generatedFiles, File root, String filename) {
        File file = new File(root, filename);
        String absoluteFilename = file.getAbsolutePath().replace("\\", "/");
        if (!generatedFiles.containsKey(absoluteFilename)) {
            Assert.fail("Could not find '" + absoluteFilename + "' file in list:\n" +
                    generatedFiles.keySet().stream().sorted().collect(Collectors.joining(",\n")));
        }
        assertTrue(generatedFiles.containsKey(absoluteFilename), "File '" + absoluteFilename + "' was not fould in the list of generated files");
    }
}
