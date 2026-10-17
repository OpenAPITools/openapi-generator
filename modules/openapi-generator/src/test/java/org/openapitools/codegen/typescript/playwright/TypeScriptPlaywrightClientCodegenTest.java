package org.openapitools.codegen.typescript.playwright;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.Generator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptPlaywrightClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@Test(groups = {TypeScriptGroups.TYPESCRIPT})
public class TypeScriptPlaywrightClientCodegenTest {

    @Test
    public void testGeneratorMetadata() {
        TypeScriptPlaywrightClientCodegen codegen = new TypeScriptPlaywrightClientCodegen();
        Assert.assertEquals(codegen.getName(), "typescript-playwright");
        Assert.assertEquals(codegen.getHelp(), "Generates end-to-end tests using Playwright for TypeScript.");
    }

    @Test
    public void testTestFileNamingAndFolders() {
        TypeScriptPlaywrightClientCodegen codegen = new TypeScriptPlaywrightClientCodegen();
        codegen.setOutputDir("out");
        Assert.assertEquals(codegen.toApiTestFilename("Pet"), "PetApi.spec");
        Assert.assertEquals(codegen.toModelTestFilename("Pet"), "Pet.spec");
        Assert.assertTrue(codegen.apiTestFileFolder().replace('\\', '/').endsWith("out/spec/apis"));
        Assert.assertTrue(codegen.modelTestFileFolder().replace('\\', '/').endsWith("out/spec/models"));
    }

    @Test
    public void testGeneratesPlaywrightProject() throws IOException {
        final String specPath = "src/test/resources/3_0/petstore.yaml";

        File output = Files.createTempDirectory("test-playwright").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-playwright")
                .setInputSpec(specPath)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        Generator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // self-contained Playwright project scaffolding
        TestUtils.assertFileExists(Paths.get(output + "/package.json"));
        TestUtils.assertFileExists(Paths.get(output + "/tsconfig.json"));
        TestUtils.assertFileExists(Paths.get(output + "/playwright.config.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/runtime.ts"));

        // typed client driven by Playwright's APIRequestContext
        TestUtils.assertFileExists(Paths.get(output + "/apis/PetApi.ts"));
        TestUtils.assertFileContains(Paths.get(output + "/runtime.ts"),
                "import type { APIRequestContext, APIResponse } from '@playwright/test';",
                "constructor(protected requestContext: APIRequestContext");

        // generated Playwright tests
        Path petApiSpec = Paths.get(output + "/spec/apis/PetApi.spec.ts");
        TestUtils.assertFileExists(petApiSpec);
        TestUtils.assertFileContains(petApiSpec,
                "import { test, expect } from '@playwright/test';",
                "const api = new PetApi(request);",
                "expect(response.raw.status()).toBe(200);");

        TestUtils.assertFileExists(Paths.get(output + "/spec/models/Pet.spec.ts"));
    }
}
