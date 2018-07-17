package org.openapitools.codegen;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;

import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.testng.Assert;

import java.io.File;
import java.util.Collections;
import java.util.Optional;

public class TestUtils {

    public static OpenAPI createOpenAPI() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());

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

    public static WrittenTemplateBasedFile getTemplateBasedFile(MockDefaultGenerator generator, File root, String filename) {
        String defaultApiFilename = new File(root, filename).getAbsolutePath().replace("\\", "/");
        Optional<WrittenTemplateBasedFile> optional = generator.getTemplateBasedFiles().stream().filter(f -> defaultApiFilename.equals(f.getOutputFilename())).findFirst();
        Assert.assertTrue(optional.isPresent());
        return optional.get();
    }
}
