package org.openapitools.codegen.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.guava.GuavaModule;
import io.swagger.v3.core.util.Yaml;
import org.testng.annotations.Test;

import java.io.File;

import static org.testng.Assert.*;

public class DynamicSettingsTest {

    @Test
    public void tesDynamicSettingsWithDynamicProperties() throws Exception {
        ObjectMapper mapper = Yaml.mapper();
        mapper.registerModule(new GuavaModule());

        String spec =
                "gemName: 'petstore'" + System.lineSeparator() +
                "moduleName: 'Petstore'" + System.lineSeparator() +
                "gemVersion: '1.0.0'" + System.lineSeparator() +
                "apiPackage: 'testing'" + System.lineSeparator();

        DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
        GeneratorSettings generatorSettings = dynamicSettings.getGeneratorSettings();
        WorkflowSettings workflowSettings = dynamicSettings.getWorkflowSettings();

        assertNotNull(dynamicSettings);
        assertNotNull(generatorSettings);
        assertNotNull(workflowSettings);

        assertEquals(generatorSettings.getApiPackage(), "testing");
        assertEquals(generatorSettings.getAdditionalProperties().size(), 8);
        assertEquals(generatorSettings.getAdditionalProperties().get("gemName"), "petstore");
        assertEquals(generatorSettings.getAdditionalProperties().get("moduleName"), "Petstore");
        assertEquals(generatorSettings.getAdditionalProperties().get("gemVersion"), "1.0.0");
        assertEquals(generatorSettings.getAdditionalProperties().get("apiPackage"), "testing");
        assertEquals(generatorSettings.getAdditionalProperties().get("gitHost"), "github.com");
        assertEquals(generatorSettings.getAdditionalProperties().get("gitUserId"), "GIT_USER_ID");
        assertEquals(generatorSettings.getAdditionalProperties().get("gitRepoId"), "GIT_REPO_ID");
        assertEquals(generatorSettings.getAdditionalProperties().get("releaseNote"), "Minor update");
    }

    @Test
    public void testDynamicSettingsWithBuilderSideEffects() throws Exception {
        ObjectMapper mapper = Yaml.mapper();
        mapper.registerModule(new GuavaModule());

        // example here is that templateDir is intended to provide the _full_ absolute path
        String input = ".";
        File current = new File(input);

        // sanity
        assertTrue(current.exists());
        assertTrue(current.isDirectory());

        String spec =
                "generatorName: none" + System.lineSeparator() +
                "templateDir: '" + input + "'" + System.lineSeparator();

        DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
        GeneratorSettings generatorSettings = dynamicSettings.getGeneratorSettings();
        WorkflowSettings workflowSettings = dynamicSettings.getWorkflowSettings();

        assertNotNull(dynamicSettings);
        assertNotNull(generatorSettings);
        assertNotNull(workflowSettings);

        assertEquals(generatorSettings.getGeneratorName(), "none");
        assertEquals(workflowSettings.getTemplateDir(), current.getAbsolutePath());
        assertNotEquals(workflowSettings.getTemplateDir(), input);

        assertEquals(generatorSettings.getAdditionalProperties().size(), 4);
        assertEquals(generatorSettings.getAdditionalProperties().get("gitHost"), "github.com");
        assertEquals(generatorSettings.getAdditionalProperties().get("gitUserId"), "GIT_USER_ID");
        assertEquals(generatorSettings.getAdditionalProperties().get("gitRepoId"), "GIT_REPO_ID");
        assertEquals(generatorSettings.getAdditionalProperties().get("releaseNote"), "Minor update");
    }

    @Test
    public void testDynamicSettingsSetsConstructorDefaultsOnDeserialization() throws Exception {
        ObjectMapper mapper = Yaml.mapper();
        mapper.registerModule(new GuavaModule());

        String gitHost = "test.com";
        String gitUserId = "openapitools";
        String spec =
                "supportPython2: true" + System.lineSeparator() +
                "gitHost: '" + gitHost + "'" + System.lineSeparator() +
                "gitUserId: '" + gitUserId + "'" + System.lineSeparator();

        DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
        GeneratorSettings generatorSettings = dynamicSettings.getGeneratorSettings();
        WorkflowSettings workflowSettings = dynamicSettings.getWorkflowSettings();

        assertNotNull(dynamicSettings);
        assertNotNull(generatorSettings);
        assertNotNull(workflowSettings);

        assertEquals(generatorSettings.getGitHost(), gitHost);
        assertEquals(generatorSettings.getGitUserId(), gitUserId);
        assertEquals(generatorSettings.getGitRepoId(), "GIT_REPO_ID");
        assertEquals(generatorSettings.getReleaseNote(), "Minor update");

        assertEquals(generatorSettings.getAdditionalProperties().size(), 5);
        assertEquals(generatorSettings.getAdditionalProperties().get("supportPython2"), true);
        assertEquals(generatorSettings.getAdditionalProperties().get("gitHost"), gitHost);
        assertEquals(generatorSettings.getAdditionalProperties().get("gitUserId"), gitUserId);
        assertEquals(generatorSettings.getAdditionalProperties().get("gitRepoId"), "GIT_REPO_ID");
        assertEquals(generatorSettings.getAdditionalProperties().get("releaseNote"), "Minor update");
    }
}
