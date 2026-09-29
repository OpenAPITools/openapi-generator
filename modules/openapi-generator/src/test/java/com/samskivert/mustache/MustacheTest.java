package com.samskivert.mustache;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.TemplateManager;
import org.openapitools.codegen.VendorExtensionCollector;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MustacheTest {

    TemplateManagerOptions templateManagerOptions = new TemplateManagerOptions(false, false);
    MustacheEngineAdapter mustacheAdapter = new MustacheEngineAdapter();

    {
        mustacheAdapter.setCompiler(mustacheAdapter.getCompiler().withCollector(new VendorExtensionCollector()));
    }

    ModelMap modelMap;

    {
        modelMap = new ModelMap();
        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.baseName = "baseName";
        codegenProperty.vendorExtensions.put("x-data", "DATA");
        codegenProperty.vendorExtensions.put("library", "JAVA");
        codegenModel.setAllVars(List.of(codegenProperty));
        modelMap.setModel(codegenModel);
    }

    @Test
    void testGetter() throws IOException {

        String content = generate("{{#model}}{{#allVars}}{{baseName}}{{/allVars}}{{/model}}", modelMap);
        assertEquals("baseName", content);
    }

    @Test
    void testVendorExtensions() throws IOException {
        String content = generate("{{#model}}{{#allVars}}{{vendorExtensions.x-data}}{{/allVars}}{{/model}}", modelMap);
        assertEquals("DATA", content);
    }

    @Test
    void testVendorExtensions2() throws IOException {
        String content = generate("{{#model}}{{#allVars}}{{library}}{{/allVars}}{{/model}}", modelMap);
        assertEquals("JAVA", content);
    }
    private String generate(String templateContent, ModelMap modelMap) throws IOException {
        File directory = Files.createTempDirectory("mustachetest").toFile();

        try {
            TemplateManager templateManager = createTemplateManager(directory, templateContent);
            File file = File.createTempFile("gen", ".txt", directory);
            templateManager.write(modelMap, "tpl.mustache", file);

            return FileUtils.readFileToString(file, StandardCharsets.UTF_8);
        } finally {
            FileUtils.deleteDirectory(directory);
        }
    }

    private TemplateManager createTemplateManager(File directory, String templateContent) throws IOException {
        File mustacheFile = File.createTempFile("gen", ".mustache", directory);

        FileUtils.writeStringToFile(mustacheFile, templateContent, StandardCharsets.UTF_8);
        return new TemplateManager(templateManagerOptions, mustacheAdapter,
                new TemplatePathLocator[]{tempalteName -> mustacheFile.getAbsolutePath()});
    }
}
