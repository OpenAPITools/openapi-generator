package org.openapitools.codegen.erlang;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.newTempFolder;

public class ErlangServerCodegenTest {

    @Test
    public void testCharsetInContentTypeCorrectlyEncodedForErlangServer() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("erlang-server")
                .setInputSpec("src/test/resources/3_0/issue_19895.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        var defaultApiFile = output.resolve("src/openapi_default_handler.erl");
        assertThat(files).contains(defaultApiFile.toFile());
        assertThat(defaultApiFile).content()
                .doesNotContain(
                        "application/json;charset&#x3D;utf-8")
                .contains(
                        "application/json;charset=utf-8"
                );
    }

}
