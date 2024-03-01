package org.openapitools.codegen.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.guava.GuavaModule;
import io.swagger.v3.core.util.Yaml;
import org.openapitools.codegen.api.TemplateDefinition;
import org.openapitools.codegen.api.TemplateFileType;
import org.testng.annotations.Test;

import java.io.File;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

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
        assertEquals(workflowSettings.getTemplateDir(), current.getCanonicalPath());
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

    @Test
    public void testFullConfigWithFilesMap() throws JsonProcessingException {
        ObjectMapper mapper = Yaml.mapper();
        mapper.registerModule(new GuavaModule());

        String gitUserId = "openapitools";

        String spec = new StringJoiner(System.lineSeparator(), "", "")
                .add("supportPython2: true") // unwrapped additional property
                .add("gitUserId: '" + gitUserId + "'") // unwrapped additional property
                .add("generatorName: java")
                .add("outputDir: samples/client/petstore/java/feign")
                .add("library: feign")
                .add("inputSpec: modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml")
                .add("additionalProperties:")
                .add("  booleanGetterPrefix: is")
                .add("  artifactId: petstore-feign")
                .add("  hideGenerationTimestamp: \"true\"")
                .add("files:")
                .add("  README.mustache:")
                .add("    folder: ''")
                .add("    destinationFilename: README.md")
                .add("  build.sh:")
                .add("    destinationFilename: build.sh")
                .add("  additional/README.mustache:")
                .add("    folder: additional")
                .add("    destinationFilename: README.md")
                .add("  custom/api.mustache:")
                .add("    templateType: API")
                .add("  custom/api_doc.mustache:")
                .add("    templateType: APIDocs")
                .add("  custom/api_test.mustache:")
                .add("    templateType: APITests")
                .add("  custom/model.mustache:")
                .add("    templateType: Model")
                .add("  custom/model_doc.mustache:")
                .add("    templateType: ModelDocs")
                .add("  custom/model_test.mustache:")
                .add("    templateType: ModelTests")
                .add("  additional/build.mustache:")
                .add("    folder: build")
                .add("    destinationFilename: build.gradle")
                .add("    templateType: SupportingFiles")
                .add("  LICENSE: {}")
                .toString();

        DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
        GeneratorSettings generatorSettings = dynamicSettings.getGeneratorSettings();
        WorkflowSettings workflowSettings = dynamicSettings.getWorkflowSettings();
        List<TemplateDefinition> files = dynamicSettings.getFiles();

        assertNotNull(dynamicSettings);
        assertNotNull(generatorSettings);
        assertNotNull(workflowSettings);
        assertNotNull(files);

        Map<String, Object> addlProps = generatorSettings.getAdditionalProperties();
        assertEquals(addlProps.get("supportPython2"), true);
        assertEquals(addlProps.get("gitUserId"), gitUserId);
        assertEquals(addlProps.get("booleanGetterPrefix"), "is");
        assertEquals(addlProps.get("artifactId"), "petstore-feign");
        assertEquals(addlProps.get("hideGenerationTimestamp"), "true");
        assertEquals(generatorSettings.getGeneratorName(), "java");
        assertEquals(workflowSettings.getOutputDir(), "samples/client/petstore/java/feign");
        assertEquals(generatorSettings.getLibrary(), "feign");
        assertEquals(workflowSettings.getInputSpec(), "modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");

        assertEquals(files.size(), 11);

        Map<String, TemplateDefinition> mapped = files.stream().collect(Collectors.toMap(TemplateDefinition::getTemplateFile, Function.identity(), (a, b) -> a, TreeMap::new));

        assertEquals(mapped.get("README.mustache").getTemplateFile(), "README.mustache");
        assertEquals(mapped.get("README.mustache").getFolder(), "");
        assertEquals(mapped.get("README.mustache").getDestinationFilename(), "README.md");
        assertEquals(mapped.get("README.mustache").getTemplateType(), TemplateFileType.SupportingFiles);

        assertEquals(mapped.get("build.sh").getTemplateFile(), "build.sh");
        assertEquals(mapped.get("build.sh").getFolder(), "");
        assertEquals(mapped.get("build.sh").getDestinationFilename(), "build.sh");
        assertEquals(mapped.get("build.sh").getTemplateType(), TemplateFileType.SupportingFiles);

        assertEquals(mapped.get("additional/README.mustache").getTemplateFile(), "additional/README.mustache");
        assertEquals(mapped.get("additional/README.mustache").getFolder(), "additional");
        assertEquals(mapped.get("additional/README.mustache").getDestinationFilename(), "README.md");
        assertEquals(mapped.get("additional/README.mustache").getTemplateType(), TemplateFileType.SupportingFiles);

        assertEquals(mapped.get("custom/api.mustache").getTemplateFile(), "custom/api.mustache");
        assertEquals(mapped.get("custom/api.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/api.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/api.mustache").getTemplateType(), TemplateFileType.API);

        assertEquals(mapped.get("custom/api_doc.mustache").getTemplateFile(), "custom/api_doc.mustache");
        assertEquals(mapped.get("custom/api_doc.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/api_doc.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/api_doc.mustache").getTemplateType(), TemplateFileType.APIDocs);

        assertEquals(mapped.get("custom/api_test.mustache").getTemplateFile(), "custom/api_test.mustache");
        assertEquals(mapped.get("custom/api_test.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/api_test.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/api_test.mustache").getTemplateType(), TemplateFileType.APITests);

        assertEquals(mapped.get("custom/model.mustache").getTemplateFile(), "custom/model.mustache");
        assertEquals(mapped.get("custom/model.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/model.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/model.mustache").getTemplateType(), TemplateFileType.Model);

        assertEquals(mapped.get("custom/model_doc.mustache").getTemplateFile(), "custom/model_doc.mustache");
        assertEquals(mapped.get("custom/model_doc.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/model_doc.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/model_doc.mustache").getTemplateType(), TemplateFileType.ModelDocs);

        assertEquals(mapped.get("custom/model_test.mustache").getTemplateFile(), "custom/model_test.mustache");
        assertEquals(mapped.get("custom/model_test.mustache").getFolder(), "");
        assertEquals(mapped.get("custom/model_test.mustache").getDestinationFilename(), "");
        assertEquals(mapped.get("custom/model_test.mustache").getTemplateType(), TemplateFileType.ModelTests);

        assertEquals(mapped.get("additional/build.mustache").getTemplateFile(), "additional/build.mustache");
        assertEquals(mapped.get("additional/build.mustache").getFolder(), "build");
        assertEquals(mapped.get("additional/build.mustache").getDestinationFilename(), "build.gradle");
        assertEquals(mapped.get("additional/build.mustache").getTemplateType(), TemplateFileType.SupportingFiles);

        assertEquals(mapped.get("LICENSE").getTemplateFile(), "LICENSE");
        assertEquals(mapped.get("LICENSE").getFolder(), "");
        assertEquals(mapped.get("LICENSE").getDestinationFilename(), "LICENSE");
        assertEquals(mapped.get("LICENSE").getTemplateType(), TemplateFileType.SupportingFiles);
    }
}
