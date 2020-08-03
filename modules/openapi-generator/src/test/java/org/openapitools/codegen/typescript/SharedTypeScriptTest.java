package org.openapitools.codegen.typescript;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.Generator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class SharedTypeScriptTest {
    @Test
    public void typesInImportsAreSplittedTest() throws IOException {
        CodegenConfigurator config =
                new CodegenConfigurator()
                        .setInputSpec("src/test/resources/split-import.json")
                        .setModelPackage("model")
                        .setApiPackage("api")
                        .setOutputDir("src/test/resources/typesInImportsAreSplittedTest")
                        .addAdditionalProperty(
                                TypeScriptAxiosClientCodegen.SEPARATE_MODELS_AND_API, true);

        config.setGeneratorName("typescript-axios");
        checkAPIFile(getGenerator(config).generate(), "default-api.ts");

        config.setGeneratorName("typescript-node");
        checkAPIFile(getGenerator(config).generate(), "defaultApi.ts");

        config.setGeneratorName("typescript-angular");
        checkAPIFile(getGenerator(config).generate(), "default.service.ts");

        FileUtils.deleteDirectory(new File("src/test/resources/typesInImportsAreSplittedTest"));
    }

    private Generator getGenerator(CodegenConfigurator config) {
        return new DefaultGenerator().opts(config.toClientOptInput());
    }

    private void checkAPIFile(List<File> files, String apiFileName) throws IOException {
        File apiFile = files.stream().filter(file->file.getName().contains(apiFileName)).findFirst().get();
        String apiFileContent = FileUtils.readFileToString(apiFile);
        Assert.assertTrue(!apiFileContent.contains("import { OrganizationWrapper | PersonWrapper }"));
        Assert.assertEquals(StringUtils.countMatches(apiFileContent,"import { PersonWrapper }"),1);
        Assert.assertEquals(StringUtils.countMatches(apiFileContent,"import { OrganizationWrapper }"),1);
    }

    @Test
    public void oldImportsStillPresentTest() throws IOException {
        CodegenConfigurator config =
                new CodegenConfigurator()
                        .setInputSpec("petstore.json")
                        .setModelPackage("model")
                        .setApiPackage("api")
                        .setOutputDir("src/test/resources/oldImportsStillPresentTest/")
                        .addAdditionalProperty(
                                TypeScriptAxiosClientCodegen.SEPARATE_MODELS_AND_API, true);

        config.setGeneratorName("typescript-angular");
        final List<File> files = getGenerator(config).generate();
        File pets = files.stream().filter(file->file.getName().contains("pet.ts")).findFirst().get();
        String apiFileContent = FileUtils.readFileToString(pets);
        Assert.assertTrue(apiFileContent.contains("import { Category }"));
        Assert.assertTrue(apiFileContent.contains("import { Tag }"));

        FileUtils.deleteDirectory(new File("src/test/resources/oldImportsStillPresentTest/"));
    }
}