/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.terraform.provider;

import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TerraformProviderCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TerraformProviderCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final TerraformProviderCodegen codegen = new TerraformProviderCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.getName(), "terraform-provider");
        Assert.assertEquals(codegen.getTag(), CodegenType.CLIENT);
        Assert.assertNotNull(codegen.getHelp());
        Assert.assertTrue(codegen.getHelp().contains("Terraform"));
    }

    @Test
    public void testProviderNameOption() throws Exception {
        final TerraformProviderCodegen codegen = new TerraformProviderCodegen();
        codegen.additionalProperties().put(TerraformProviderCodegen.PROVIDER_NAME, "myapi");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(TerraformProviderCodegen.PROVIDER_NAME), "myapi");
    }

    @Test
    public void testGeneratePetstore() throws Exception {
        File output = Files.createTempDirectory("terraform-provider-test").toFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put(TerraformProviderCodegen.PROVIDER_NAME, "petstore");
        properties.put(TerraformProviderCodegen.PROVIDER_ADDRESS, "registry.terraform.io/example/petstore");
        properties.put("gitHost", "github.com");
        properties.put("gitUserId", "example");
        properties.put("gitRepoId", "terraform-provider-petstore");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("terraform-provider")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify key files exist
        TestUtils.assertFileExists(Paths.get(output + "/main.go"));
        TestUtils.assertFileExists(Paths.get(output + "/go.mod"));
        TestUtils.assertFileExists(Paths.get(output + "/GNUmakefile"));
        TestUtils.assertFileExists(Paths.get(output + "/README.md"));
        TestUtils.assertFileExists(Paths.get(output + "/.gitignore"));
        TestUtils.assertFileExists(Paths.get(output + "/internal/provider/provider.go"));
        TestUtils.assertFileExists(Paths.get(output + "/internal/client/client.go"));
        TestUtils.assertFileExists(Paths.get(output + "/examples/provider/provider.tf"));
    }

    @Test
    public void testMainGoContent() throws Exception {
        File output = Files.createTempDirectory("terraform-provider-test").toFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put(TerraformProviderCodegen.PROVIDER_NAME, "petstore");
        properties.put(TerraformProviderCodegen.PROVIDER_ADDRESS, "registry.terraform.io/example/petstore");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("terraform-provider")
                .setAdditionalProperties(properties)
                .setGitHost("github.com")
                .setGitUserId("example")
                .setGitRepoId("terraform-provider-petstore")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify main.go content
        TestUtils.assertFileContains(Paths.get(output + "/main.go"), "providerserver.Serve");
        TestUtils.assertFileContains(Paths.get(output + "/main.go"), "registry.terraform.io/example/petstore");

        // Verify provider.go content
        TestUtils.assertFileContains(Paths.get(output + "/internal/provider/provider.go"), "petstoreProvider");
        TestUtils.assertFileContains(Paths.get(output + "/internal/provider/provider.go"), "provider.Provider");

        // Verify client.go content
        TestUtils.assertFileContains(Paths.get(output + "/internal/client/client.go"), "type Client struct");
        TestUtils.assertFileContains(Paths.get(output + "/internal/client/client.go"), "func NewClient");

        // Verify go.mod content
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"), "github.com/example/terraform-provider-petstore");
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"), "terraform-plugin-framework");
    }

    @Test
    public void testTypeMapping() throws Exception {
        final TerraformProviderCodegen codegen = new TerraformProviderCodegen();
        codegen.processOpts();

        // Terraform provider should map DateTime to string (not time.Time)
        Assert.assertEquals(codegen.typeMapping().get("DateTime"), "string");
        Assert.assertEquals(codegen.typeMapping().get("File"), "string");
        Assert.assertEquals(codegen.typeMapping().get("binary"), "string");

        // Standard Go type mappings should still work
        Assert.assertEquals(codegen.typeMapping().get("integer"), "int32");
        Assert.assertEquals(codegen.typeMapping().get("long"), "int64");
        Assert.assertEquals(codegen.typeMapping().get("boolean"), "bool");
        Assert.assertEquals(codegen.typeMapping().get("string"), "string");
    }

    @Test
    public void testFilenames() throws Exception {
        final TerraformProviderCodegen codegen = new TerraformProviderCodegen();

        Assert.assertEquals(codegen.toApiFilename("Pet"), "pet");
        Assert.assertEquals(codegen.toApiFilename("UserProfile"), "user_profile");
        Assert.assertEquals(codegen.toModelFilename("Pet"), "model_pet");
        Assert.assertEquals(codegen.toModelFilename("ApiResponse"), "model_api_response");
    }

    @Test
    public void testProviderExampleContent() throws Exception {
        File output = Files.createTempDirectory("terraform-provider-test").toFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put(TerraformProviderCodegen.PROVIDER_NAME, "petstore");
        properties.put(TerraformProviderCodegen.PROVIDER_ADDRESS, "registry.terraform.io/example/petstore");
        properties.put("gitHost", "github.com");
        properties.put("gitUserId", "example");
        properties.put("gitRepoId", "terraform-provider-petstore");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("terraform-provider")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify provider example
        TestUtils.assertFileContains(Paths.get(output + "/examples/provider/provider.tf"), "petstore");
        TestUtils.assertFileContains(Paths.get(output + "/examples/provider/provider.tf"), "registry.terraform.io/example/petstore");
    }
}
