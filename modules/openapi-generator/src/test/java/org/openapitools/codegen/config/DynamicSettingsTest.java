package org.openapitools.codegen.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.guava.GuavaModule;
import io.swagger.v3.core.util.Yaml;
import org.openapitools.codegen.api.TemplateDefinition;
import org.openapitools.codegen.api.TemplateFileType;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.TreeMap;
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

    @Test
    public void testFilesDirWithNestedStructure() throws IOException, JsonProcessingException {
        // Create a temp directory structure:
        // filesDir/
        //   README.md                     -> SupportingFiles, folder=""
        //   LICENSE.mustache              -> SupportingFiles, folder="", dest="LICENSE"
        //   api/
        //     custom_api.mustache         -> API, dest="custom_api"
        //   model/
        //     validators.mustache         -> Model, dest="validators"
        //   apiDocs/
        //     api_readme.mustache         -> APIDocs
        //   modelDocs/
        //     model_readme.mustache       -> ModelDocs
        //   apiTests/
        //     api_test.mustache           -> APITests
        //   modelTests/
        //     model_test.mustache         -> ModelTests
        //   supportingFiles/
        //     build.gradle.mustache       -> SupportingFiles, folder=""
        //     scripts/
        //       check.sh                  -> SupportingFiles, folder="scripts"
        //   custom_scripts/
        //     deploy.sh                   -> SupportingFiles, folder="custom_scripts" (unrecognized dir)

        Path tempDir = Files.createTempDirectory("filesDir_test");
        try {
            // Root files
            Files.writeString(tempDir.resolve("README.md"), "readme");
            Files.writeString(tempDir.resolve("LICENSE.mustache"), "license template");

            // api/
            Files.createDirectories(tempDir.resolve("api"));
            Files.writeString(tempDir.resolve("api/custom_api.mustache"), "api template");

            // model/
            Files.createDirectories(tempDir.resolve("model"));
            Files.writeString(tempDir.resolve("model/validators.mustache"), "model template");

            // apiDocs/
            Files.createDirectories(tempDir.resolve("apiDocs"));
            Files.writeString(tempDir.resolve("apiDocs/api_readme.mustache"), "api docs");

            // modelDocs/
            Files.createDirectories(tempDir.resolve("modelDocs"));
            Files.writeString(tempDir.resolve("modelDocs/model_readme.mustache"), "model docs");

            // apiTests/
            Files.createDirectories(tempDir.resolve("apiTests"));
            Files.writeString(tempDir.resolve("apiTests/api_test.mustache"), "api test");

            // modelTests/
            Files.createDirectories(tempDir.resolve("modelTests"));
            Files.writeString(tempDir.resolve("modelTests/model_test.mustache"), "model test");

            // supportingFiles/
            Files.createDirectories(tempDir.resolve("supportingFiles/scripts"));
            Files.writeString(tempDir.resolve("supportingFiles/build.gradle.mustache"), "build file");
            Files.writeString(tempDir.resolve("supportingFiles/scripts/check.sh"), "check script");

            // custom_scripts/ (unrecognized dir name)
            Files.createDirectories(tempDir.resolve("custom_scripts"));
            Files.writeString(tempDir.resolve("custom_scripts/deploy.sh"), "deploy script");

            // Parse config with filesDir
            ObjectMapper mapper = Yaml.mapper();
            mapper.registerModule(new GuavaModule());

            String spec = new StringJoiner(System.lineSeparator(), "", "")
                    .add("generatorName: java")
                    .add("filesDir: '" + tempDir.toString().replace('\\', '/') + "'")
                    .toString();

            DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
            List<TemplateDefinition> files = dynamicSettings.getFiles();
            assertNotNull(files);

            Map<String, TemplateDefinition> mapped = files.stream()
                    .collect(Collectors.toMap(TemplateDefinition::getTemplateFile, Function.identity(), (a, b) -> a, TreeMap::new));

            // Root files -> SupportingFiles
            assertTrue(mapped.containsKey("README.md"), "Should discover README.md at root");
            assertEquals(mapped.get("README.md").getTemplateType(), TemplateFileType.SupportingFiles);
            assertEquals(mapped.get("README.md").getFolder(), "");
            assertEquals(mapped.get("README.md").getDestinationFilename(), "README.md");

            assertTrue(mapped.containsKey("LICENSE.mustache"), "Should discover LICENSE.mustache at root");
            assertEquals(mapped.get("LICENSE.mustache").getTemplateType(), TemplateFileType.SupportingFiles);
            assertEquals(mapped.get("LICENSE.mustache").getDestinationFilename(), "LICENSE");

            // api/ -> API
            assertTrue(mapped.containsKey("api/custom_api.mustache"), "Should discover api/custom_api.mustache");
            assertEquals(mapped.get("api/custom_api.mustache").getTemplateType(), TemplateFileType.API);
            assertEquals(mapped.get("api/custom_api.mustache").getDestinationFilename(), "custom_api");
            assertEquals(mapped.get("api/custom_api.mustache").getFolder(), "");

            // model/ -> Model
            assertTrue(mapped.containsKey("model/validators.mustache"), "Should discover model/validators.mustache");
            assertEquals(mapped.get("model/validators.mustache").getTemplateType(), TemplateFileType.Model);

            // apiDocs/ -> APIDocs
            assertTrue(mapped.containsKey("apiDocs/api_readme.mustache"));
            assertEquals(mapped.get("apiDocs/api_readme.mustache").getTemplateType(), TemplateFileType.APIDocs);

            // modelDocs/ -> ModelDocs
            assertTrue(mapped.containsKey("modelDocs/model_readme.mustache"));
            assertEquals(mapped.get("modelDocs/model_readme.mustache").getTemplateType(), TemplateFileType.ModelDocs);

            // apiTests/ -> APITests
            assertTrue(mapped.containsKey("apiTests/api_test.mustache"));
            assertEquals(mapped.get("apiTests/api_test.mustache").getTemplateType(), TemplateFileType.APITests);

            // modelTests/ -> ModelTests
            assertTrue(mapped.containsKey("modelTests/model_test.mustache"));
            assertEquals(mapped.get("modelTests/model_test.mustache").getTemplateType(), TemplateFileType.ModelTests);

            // supportingFiles/ -> SupportingFiles
            assertTrue(mapped.containsKey("supportingFiles/build.gradle.mustache"));
            assertEquals(mapped.get("supportingFiles/build.gradle.mustache").getTemplateType(), TemplateFileType.SupportingFiles);
            assertEquals(mapped.get("supportingFiles/build.gradle.mustache").getFolder(), "");
            assertEquals(mapped.get("supportingFiles/build.gradle.mustache").getDestinationFilename(), "build.gradle");

            // supportingFiles/scripts/ -> SupportingFiles with folder="scripts"
            assertTrue(mapped.containsKey("supportingFiles/scripts/check.sh"));
            assertEquals(mapped.get("supportingFiles/scripts/check.sh").getTemplateType(), TemplateFileType.SupportingFiles);
            assertEquals(mapped.get("supportingFiles/scripts/check.sh").getFolder(), "scripts");
            assertEquals(mapped.get("supportingFiles/scripts/check.sh").getDestinationFilename(), "check.sh");

            // custom_scripts/ (unrecognized) -> SupportingFiles with folder="custom_scripts"
            assertTrue(mapped.containsKey("custom_scripts/deploy.sh"));
            assertEquals(mapped.get("custom_scripts/deploy.sh").getTemplateType(), TemplateFileType.SupportingFiles);
            assertEquals(mapped.get("custom_scripts/deploy.sh").getFolder(), "custom_scripts");
            assertEquals(mapped.get("custom_scripts/deploy.sh").getDestinationFilename(), "deploy.sh");
        } finally {
            // Cleanup temp directory
            deleteRecursively(tempDir);
        }
    }

    @Test
    public void testFilesDirMergedWithFiles() throws IOException, JsonProcessingException {
        // When both files and filesDir are specified, explicit files entries should take precedence

        Path tempDir = Files.createTempDirectory("filesDir_merge_test");
        try {
            Files.createDirectories(tempDir.resolve("api"));
            Files.writeString(tempDir.resolve("api/custom_api.mustache"), "api from dir");
            Files.writeString(tempDir.resolve("README.md"), "readme from dir");

            ObjectMapper mapper = Yaml.mapper();
            mapper.registerModule(new GuavaModule());

            // Explicit files entry for api/custom_api.mustache with a custom destinationFilename
            String spec = new StringJoiner(System.lineSeparator(), "", "")
                    .add("generatorName: java")
                    .add("filesDir: '" + tempDir.toString().replace('\\', '/') + "'")
                    .add("files:")
                    .add("  api/custom_api.mustache:")
                    .add("    templateType: API")
                    .add("    destinationFilename: CustomApi.java")
                    .toString();

            DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
            List<TemplateDefinition> files = dynamicSettings.getFiles();
            assertNotNull(files);

            Map<String, TemplateDefinition> mapped = files.stream()
                    .collect(Collectors.toMap(TemplateDefinition::getTemplateFile, Function.identity(), (a, b) -> a, TreeMap::new));

            // Explicit entry should win over auto-discovered
            assertEquals(mapped.get("api/custom_api.mustache").getDestinationFilename(), "CustomApi.java");
            assertEquals(mapped.get("api/custom_api.mustache").getTemplateType(), TemplateFileType.API);

            // Auto-discovered file that doesn't conflict should still be present
            assertTrue(mapped.containsKey("README.md"));
            assertEquals(mapped.get("README.md").getTemplateType(), TemplateFileType.SupportingFiles);
        } finally {
            deleteRecursively(tempDir);
        }
    }

    @Test
    public void testFilesDirEmptyDirectory() throws IOException, JsonProcessingException {
        Path tempDir = Files.createTempDirectory("filesDir_empty_test");
        try {
            ObjectMapper mapper = Yaml.mapper();
            mapper.registerModule(new GuavaModule());

            String spec = new StringJoiner(System.lineSeparator(), "", "")
                    .add("generatorName: java")
                    .add("filesDir: '" + tempDir.toString().replace('\\', '/') + "'")
                    .toString();

            DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
            List<TemplateDefinition> files = dynamicSettings.getFiles();
            assertNotNull(files);
            assertEquals(files.size(), 0, "Empty directory should produce no template definitions");
        } finally {
            deleteRecursively(tempDir);
        }
    }

    @Test
    public void testFilesDirNonExistentDirectory() throws JsonProcessingException {
        ObjectMapper mapper = Yaml.mapper();
        mapper.registerModule(new GuavaModule());

        String spec = new StringJoiner(System.lineSeparator(), "", "")
                .add("generatorName: java")
                .add("filesDir: '/nonexistent/path/that/should/not/exist'")
                .toString();

        DynamicSettings dynamicSettings = mapper.readValue(spec, DynamicSettings.class);
        List<TemplateDefinition> files = dynamicSettings.getFiles();
        assertNotNull(files);
        assertEquals(files.size(), 0, "Non-existent directory should produce no template definitions");
    }

    @Test
    public void testDiscoverFilesFromDirectoryCaseInsensitive() throws IOException {
        // Verify that directory name matching is case-insensitive
        Path tempDir = Files.createTempDirectory("filesDir_case_test");
        try {
            Files.createDirectories(tempDir.resolve("API"));
            Files.writeString(tempDir.resolve("API/upper.mustache"), "upper");
            Files.createDirectories(tempDir.resolve("Model"));
            Files.writeString(tempDir.resolve("Model/mixed.mustache"), "mixed");

            List<TemplateDefinition> discovered = DynamicSettings.discoverFilesFromDirectory(tempDir.toString());
            Map<String, TemplateDefinition> mapped = discovered.stream()
                    .collect(Collectors.toMap(TemplateDefinition::getTemplateFile, Function.identity()));

            assertEquals(mapped.get("API/upper.mustache").getTemplateType(), TemplateFileType.API);
            assertEquals(mapped.get("Model/mixed.mustache").getTemplateType(), TemplateFileType.Model);
        } finally {
            deleteRecursively(tempDir);
        }
    }

    private static void deleteRecursively(Path path) throws IOException {
        if (Files.isDirectory(path)) {
            try (var entries = Files.list(path)) {
                for (Path entry : entries.collect(Collectors.toList())) {
                    deleteRecursively(entry);
                }
            }
        }
        Files.deleteIfExists(path);
    }
}
