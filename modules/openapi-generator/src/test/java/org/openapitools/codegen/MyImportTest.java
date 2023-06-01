package org.openapitools.codegen;

import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MyImportTest {

    @Test
    public void testGenerate() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("hideGenerationTimestamp","true");

        File output = new File("C:\\Users\\margebe\\Desktop\\openapi-generator-output");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary("apache-httpclient")
                .setAdditionalProperties(properties)
                .setInputSpec("C:\\Users\\margebe\\Desktop\\openapi-generator-input.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        output.deleteOnExit();

        TestUtils.ensureContainsFile(files, output, "README.md");
    }

}
