package org.openapitools.codegen.cmd;

import com.fasterxml.jackson.databind.module.SimpleModule;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.Context;
import org.openapitools.codegen.config.GeneratorSettings;
import org.openapitools.codegen.config.WorkflowSettings;
import org.testng.ITestContext;
import org.testng.TestRunner;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.testng.Assert.*;

@SuppressWarnings("RedundantThrows")
public class GenerateBatchTest {
    private static final String SPEC_FILE = "batch/specs/petstore.yaml";
    private static final String JAXRS_DATELIB_J8_JSON = "jaxrs-datelib-j8.json";
    private static final String JAXRS_DATELIB_J8_YAML = "jaxrs-datelib-j8.yaml";
    private static final String JAXRS_DATELIB_J8_YAML_INCLUDE_JSON = "jaxrs-datelib-j8-yaml-include.json";
    private static final String JAXRS_DATELIB_J8_JSON_INCLUDE_YAML = "jaxrs-datelib-j8-json-include.yaml";
    private static final String JAXRS_DATELIB_J8_DOUBLE_JSON = "jaxrs-datelib-j8-double.json";
    private static final String JAXRS_DATELIB_J8_DOUBLE_YAML = "jaxrs-datelib-j8-double.yaml";
    private static final String JAXRS_DATELIB_J8_NESTED_JSON = "jaxrs-datelib-j8-nested.json";
    private static final String JAXRS_DATELIB_J8_NESTED_YAML = "jaxrs-datelib-j8-nested.yaml";
    private static final String JAXRS_DATELIB_J8_NESTED_PROPERTY_MERGE_YAML = "jaxrs-datelib-j8-nested-property-merge.yaml";

    Path workingDirectory;

    @BeforeTest
    public void setUp(ITestContext ctx) throws IOException {
        workingDirectory = Paths.get("src", "test", "resources", "batch");
    }

    @DataProvider(name = "customIncludeDeserializerFiles")
    public Object[][] customIncludeDeserializerFiles() {
        return new Object[][] {
                {JAXRS_DATELIB_J8_JSON},
                {JAXRS_DATELIB_J8_YAML},
                {JAXRS_DATELIB_J8_JSON_INCLUDE_YAML},
                {JAXRS_DATELIB_J8_DOUBLE_JSON},
                {JAXRS_DATELIB_J8_DOUBLE_YAML},
                {JAXRS_DATELIB_J8_NESTED_JSON},
                {JAXRS_DATELIB_J8_NESTED_YAML},
                {JAXRS_DATELIB_J8_NESTED_PROPERTY_MERGE_YAML}
        };
    }

    @Test(dataProvider = "customIncludeDeserializerFiles")
    public void testDeserializerWithJsonInclude(String file) throws IOException {
        String config = getTargetResourceAsFile(file).toString();
        SimpleModule module = GenerateBatch.getCustomDeserializationModel(getIncludesDir());
        CodegenConfigurator loaded = CodegenConfigurator.fromFile(config, module);

        Map<String, Object> expectedAdditionalProperties = new HashMap<>();
        expectedAdditionalProperties.put("serverPort", "8082");
        expectedAdditionalProperties.put("dateLibrary", "java8");
        expectedAdditionalProperties.put("hideGenerationTimestamp", true);
        expectedAdditionalProperties.put("serializableModel", true);
        expectedAdditionalProperties.put("withXml", true);
        expectedAdditionalProperties.put("java8", true);
        expectedAdditionalProperties.put("useBeanValidation", true);

        assertNotNull(loaded);

        Context<?> context = loaded.toContext();
        WorkflowSettings workflowSettings = context.getWorkflowSettings();
        GeneratorSettings generatorSettings = context.getGeneratorSettings();

        assertNotNull(workflowSettings);
        assertNotNull(generatorSettings);

        assertEquals(generatorSettings.getGeneratorName(), "jaxrs-jersey");
        assertEquals(workflowSettings.getOutputDir(), "outputDir");
        assertEquals(workflowSettings.getInputSpec(), SPEC_FILE);
        assertTrue(generatorSettings.getAdditionalProperties().size() >= 7);

        Set<Map.Entry<String, Object>> actualSet = generatorSettings.getAdditionalProperties().entrySet();
        assertTrue(actualSet.containsAll(expectedAdditionalProperties.entrySet()));
    }

    @SuppressWarnings("unused")
    @Test(
            expectedExceptions = { RuntimeException.class },
            expectedExceptionsMessageRegExp = "Unable to deserialize config file: .*"
    )
    public void testInvalidDeserializerWithIncludeOption() {
        // JSON is valid YAML, but not the other way around, so we can't load a YAML include from a JSON config
        // to do so would require additional work.
        String config = getTargetResourceAsFile(JAXRS_DATELIB_J8_YAML_INCLUDE_JSON).toString();
        SimpleModule module = GenerateBatch.getCustomDeserializationModel(getIncludesDir());
        CodegenConfigurator loaded = CodegenConfigurator.fromFile(config, module);
        fail("Expected an exception when trying to load a YAML include from a JSON file");
    }

    private File getIncludesDir() {
        // The includes directory would be "batch" under resources here, as everything is relative to this directory.
        return workingDirectory.toFile();
    }

    private File getTargetResourceAsFile(String relative) {
        return workingDirectory.resolve(relative).toAbsolutePath().toFile();
    }
}