package org.openapitools.codegen.typescript;

import static org.openapitools.codegen.typescript.TypeScriptGroups.TYPESCRIPT;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.Generator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@Test(groups = {TYPESCRIPT})
public class SharedTypeScriptTest {

    @Test(enabled = false)
    public void typesInImportsAreSplitTest() throws IOException {
        Path output = Files.createTempDirectory("test");
        output.toFile().deleteOnExit();

        CodegenConfigurator config =
                new CodegenConfigurator()
                        .setInputSpec("src/test/resources/split-import.json")
                        .setModelPackage("model")
                        .setApiPackage("api")
                        .setOutputDir(output.toString())
                        .addAdditionalProperty(
                                TypeScriptAxiosClientCodegen.SEPARATE_MODELS_AND_API, true);

        config.setGeneratorName("typescript-axios");
        getGenerator(config).generate();
        final String apiFileContent = Files.readString(output.resolve("api/default-api.ts"), StandardCharsets.UTF_8);
        Assert.assertFalse(apiFileContent.contains("import { GetCustomer200Response | PersonWrapper }"));
        Assert.assertEquals(StringUtils.countMatches(apiFileContent, "import type { PersonWrapper }"), 1);
        Assert.assertEquals(StringUtils.countMatches(apiFileContent, "import type { GetCustomer200Response }"), 1);

        config.setGeneratorName("typescript-node");
        checkAPIFile(getGenerator(config).generate(), "defaultApi.ts");

        config.setGeneratorName("typescript-angular");
        checkAPIFile(getGenerator(config).generate(), "default.service.ts");
    }

    private Generator getGenerator(CodegenConfigurator config) {
        return new DefaultGenerator().opts(config.toClientOptInput());
    }

    private void checkAPIFile(List<File> files, String apiFileName) throws IOException {
        File apiFile = files.stream().filter(file -> file.getName().contains(apiFileName)).findFirst().get();
        String apiFileContent = FileUtils.readFileToString(apiFile, StandardCharsets.UTF_8);
        Assert.assertFalse(apiFileContent.contains("import { GetCustomer200Response | PersonWrapper }"));
        Assert.assertEquals(StringUtils.countMatches(apiFileContent, "import { PersonWrapper }"), 1);
        Assert.assertEquals(StringUtils.countMatches(apiFileContent, "import { GetCustomer200Response }"), 1);
    }

        private Path findModelDefinitionFile(Path root, Pattern modelPattern) throws IOException {
                try (Stream<Path> paths = Files.walk(root)) {
                        return paths
                                        .filter(Files::isRegularFile)
                                        .filter(path -> path.toString().endsWith(".ts"))
                                        .filter(path -> {
                                                try {
                                                        String content = Files.readString(path);
                                                        return modelPattern.matcher(content).find();
                                                } catch (IOException e) {
                                                        return false;
                                                }
                                        })
                                        .findFirst()
                                        .orElseThrow(() -> new IOException("Unable to locate model definition in " + root));
                }
        }

    @Test
    public void oldImportsStillPresentTest() throws IOException {
        Path output = Files.createTempDirectory("test");
        output.toFile().deleteOnExit();

        CodegenConfigurator config =
                new CodegenConfigurator()
                        .setInputSpec("petstore.json")
                        .setModelPackage("model")
                        .setApiPackage("api")
                        .setOutputDir(output.toString())
                        .addAdditionalProperty(
                                TypeScriptAxiosClientCodegen.SEPARATE_MODELS_AND_API, true);

        config.setGeneratorName("typescript-angular");
        final List<File> files = getGenerator(config).generate();
        File pets = files.stream().filter(file -> file.getName().contains("pet.ts")).findFirst().get();
        String apiFileContent = FileUtils.readFileToString(pets, StandardCharsets.UTF_8);
        Assert.assertTrue(apiFileContent.contains("import { Category }"));
        Assert.assertTrue(apiFileContent.contains("import { Tag }"));

        FileUtils.deleteDirectory(new File("src/test/resources/oldImportsStillPresentTest/"));
    }

    /*
        #8000
        Test that primitives are not returned by toModelImportMap
    */
    @Test
    public void toModelImportMapTest() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        Map<String, String[]> types = new HashMap<String, String[]>() {{
            put("Schema & AnotherSchema", new String[]{"AnotherSchema", "Schema"});
            put("Schema | AnotherSchema", new String[]{"AnotherSchema", "Schema"});
            put("Schema & object", new String[]{"Schema"});
            put("Schema | object", new String[]{"Schema"});
        }};

        for (Map.Entry<String, String[]> entry : types.entrySet()) {
            String[] mapped = codegen.toModelImportMap(entry.getKey()).values().toArray(new String[0]);
            Arrays.sort(mapped);
            Assert.assertEquals(mapped, entry.getValue());
        }
    }

    @Test(description = "Issue #21317")
    public void givenTypeMappingContainsGenericAndMappedTypeIsUtilityTypeThenTypeIsNotImportedAndTypeAppearsCorrectly() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        File mainOutput = new File(output, "main");

        Generator generator = new DefaultGenerator();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setInputSpec("src/test/resources/3_1/issue_21317.yaml")
                .setGeneratorName("typescript-fetch")
                .addTypeMapping("object", "Record<string,unknown>")
                .setOutputDir(mainOutput.getAbsolutePath());
        ClientOptInput clientOptInput = configurator.toClientOptInput();
        generator.opts(clientOptInput)
                .generate();
        String mainPath = mainOutput.getAbsolutePath();
        File userModel = new File(mainPath, "/models/User.ts");

        TestUtils.assertFileNotContains(userModel.toPath(), "Recordstringunknown");
        TestUtils.assertFileContains(userModel.toPath(), "Record<string,unknown>");
    }

    @Test(description = "Issue #21317")
    public void givenTypeMappingContainsGenericAndMappedTypeIsUtilityAndWithoutRuntimeChecksTrueTypeThenTypeIsNotImportedAndTypeAppearsCorrectly() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Generator generator = new DefaultGenerator();

        CodegenConfigurator noRuntimeConfigurator = new CodegenConfigurator()
                .setInputSpec("src/test/resources/3_1/issue_21317.yaml")
                .setGeneratorName("typescript-fetch")
                .addTypeMapping("object", "Record<string, unknown>")
                .addTypeMapping("UserSummary", "Pick<User, \"email\">")
                .addAdditionalProperty(TypeScriptFetchClientCodegen.WITHOUT_RUNTIME_CHECKS, true)
                .setOutputDir(output.getAbsolutePath());
        ClientOptInput clientOptInput2 = noRuntimeConfigurator.toClientOptInput();
        generator.opts(clientOptInput2)
                .generate();
        String noRuntimePath = output.getAbsolutePath();
        File apiFile = new File(noRuntimePath, "/apis/DefaultApi.ts");
        System.out.println(Files.readString(apiFile.toPath()));

        TestUtils.assertFileContains(apiFile.toPath(), "Promise<Pick<User, \"email\">>");
        TestUtils.assertFileNotContains(apiFile.toPath(), "Promise<Pickuser");
    }


    @Test(description = "Issue #21317")
    public void givenTypeMappingContainsGenericAndMappedTypeIsUtilityTypeThenTypeIsNotImportedAndTypeAppearsCorrectlyAxios() throws Exception {

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Generator generator = new DefaultGenerator();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setInputSpec("src/test/resources/3_1/issue_21317.yaml")
                .setGeneratorName("typescript-axios")
                .addTypeMapping("UserSummary", "Pick<User, \"email\">")
                .addTypeMapping("object", "Record<string,unknown>")
                .setOutputDir(output.getAbsolutePath());
        generator.opts(configurator.toClientOptInput())
                .generate();

        File axiosApiFile = new File(output, "/api.ts");

        TestUtils.assertFileContains(axiosApiFile.toPath(), "AxiosPromise<Pick<User, \"email\">>");
        TestUtils.assertFileNotContains(axiosApiFile.toPath(), "AxiosPromise<UserSummary>");
    }

    @Test(description = "Issue #20877, #22747 - Maps/arrays of inner enums should use correct enum name")
    public void givenMapWithArrayOfEnumsThenCorrectEnumNameIsUsed() throws Exception {
        // This tests the fix for the issue where maps with array of enums generated
        // "InnerEnum" type reference instead of the correct qualified enum name.
        // The fix is in AbstractTypeScriptClientCodegen and applies to all TypeScript generators.
        final String specPath = "src/test/resources/3_0/issue_19393_map_of_inner_enum.yaml";

        List<String> generators = Arrays.asList(
                "typescript",
                "typescript-angular",
                "typescript-axios",
                "typescript-aurelia",
                "typescript-fetch",
                "typescript-inversify",
                "typescript-jquery",
                "typescript-nestjs",
                "typescript-nestjs-server",
                "typescript-node",
                "typescript-redux-query",
                "typescript-rxjs"
        );

        Pattern modelDefinition = Pattern.compile("\\b(interface|type|class)\\s+EmployeeWithMultiMapOfEnum\\b");
        Pattern namespacedEnumRef = Pattern.compile("projectRoles[^\\n]*ProjectRolesEnum");
        Pattern flatEnumRef = Pattern.compile("projectRoles[^\\n]*EmployeeWithMultiMapOfEnumProjectRolesEnum");

        for (String generatorName : generators) {
            File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
            output.deleteOnExit();

            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName(generatorName)
                    .setInputSpec(specPath)
                    .setOutputDir(output.getAbsolutePath());

            Generator generator = new DefaultGenerator();
            generator.opts(configurator.toClientOptInput()).generate();

            Path modelFile = findModelDefinitionFile(output.toPath(), modelDefinition);
            String fileContents = Files.readString(modelFile);

            Assert.assertFalse(fileContents.contains("InnerEnum"),
                    generatorName + ": Should not contain 'InnerEnum' reference in " + modelFile);

            boolean hasEnumRef = namespacedEnumRef.matcher(fileContents).find()
                    || flatEnumRef.matcher(fileContents).find();
            Assert.assertTrue(hasEnumRef,
                    generatorName + ": Expected enum reference not found in " + modelFile);
        }
    }

}
