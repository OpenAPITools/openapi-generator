/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.elixir;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ElixirClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ElixirClientCodegenTest {

    @DataProvider
    public Object[][] pathParameterLibraries() {
        return new Object[][]{{"tesla"}, {"req"}};
    }

    @Test(dataProvider = "pathParameterLibraries")
    public void testGeneratedPathParametersAreEncoded(String library) throws Exception {
        File output = Files.createTempDirectory("elixir-" + library + "-path-params").toFile();
        try {
            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("elixir")
                    .setLibrary(library)
                    .addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, "Petstore")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.getAbsolutePath());

            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

            String generatedApi = Files.readString(output.toPath().resolve("lib/petstore/api/pet.ex"), StandardCharsets.UTF_8);
            Assert.assertTrue(generatedApi.contains("|> url(\"/pet/#{encode_path_param(pet_id)}\")"));
            String generatedRequestBuilder = Files.readString(output.toPath().resolve("lib/petstore/request_builder.ex"), StandardCharsets.UTF_8);
            Assert.assertTrue(generatedRequestBuilder.contains("def encode_path_param(value) do"));
        } finally {
            FileUtils.deleteDirectory(output);
        }
    }

    @Test
    public void testDefaultLibraryAndDependencies() {
        ElixirClientCodegen codegen = new ElixirClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.getLibrary(), "tesla");
        Assert.assertEquals(codegen.additionalProperties().get("isTesla"), Boolean.TRUE);
        Assert.assertEquals(codegen.additionalProperties().get("isReq"), Boolean.FALSE);
        Assert.assertTrue(((List<?>) codegen.additionalProperties().get("deps"))
                .contains("{:tesla, \"~> 1.14\"}"));
    }

    @Test
    public void testReqLibraryAndDependencies() {
        ElixirClientCodegen codegen = new ElixirClientCodegen();
        codegen.setLibrary("req");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("isTesla"), Boolean.FALSE);
        Assert.assertEquals(codegen.additionalProperties().get("isReq"), Boolean.TRUE);
        Assert.assertTrue(((List<?>) codegen.additionalProperties().get("deps"))
                .contains("{:req, \"~> 0.6.0 or ~> 0.7.3\"}"));
    }

    @Test
    public void testCustomTemplateDirectoryFallsBackToEmbeddedTemplates() throws Exception {
        File output = Files.createTempDirectory("elixir-req-custom-templates").toFile();
        try {
            Path templates = Files.createDirectory(output.toPath().resolve("templates"));
            String marker = "# generated from the custom template directory";
            String modelTemplate = Files.readString(Path.of("src/main/resources/elixir/model.mustache"), StandardCharsets.UTF_8)
                    .replace("{{>licenseInfo}}\n", "{{>licenseInfo}}\n" + marker + "\n");
            Files.writeString(templates.resolve("model.mustache"), modelTemplate, StandardCharsets.UTF_8);

            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("elixir")
                    .setLibrary("req")
                    .addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, "Petstore")
                    .setTemplateDir(templates.toString())
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.toPath().resolve("generated").toString());

            ClientOptInput input = configurator.toClientOptInput();
            new DefaultGenerator().opts(input).generate();

            Assert.assertEquals(input.getConfig().embeddedTemplateDir(), "elixir");
            Assert.assertEquals(input.getConfig().templateDir(), templates.toString());
            Path generated = output.toPath().resolve("generated");
            Path model = generated.resolve("lib/petstore/model/pet.ex");
            Assert.assertTrue(Files.exists(model));
            Assert.assertTrue(Files.readString(model, StandardCharsets.UTF_8).contains(marker));
            Path api = generated.resolve("lib/petstore/api/pet.ex");
            Assert.assertTrue(Files.exists(api));
            Assert.assertFalse(Files.readString(api, StandardCharsets.UTF_8).contains(marker));
            Path readme = generated.resolve("README.md");
            Assert.assertTrue(Files.exists(readme));
            Assert.assertFalse(Files.readString(readme, StandardCharsets.UTF_8).contains(marker));
        } finally {
            FileUtils.deleteDirectory(output);
        }
    }

    @Test
    public void testReqLibraryAndGeneratedTypespec() throws Exception {
        File output = Files.createTempDirectory("elixir-req-codegen").toFile();
        try {
            Map<String, Object> additionalProperties = new HashMap<>();
            additionalProperties.put("library", "req");

            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("elixir")
                    .setAdditionalProperties(additionalProperties)
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.getAbsolutePath());

            ClientOptInput input = configurator.toClientOptInput();
            new DefaultGenerator().opts(input).generate();

            String generatedApi;
            try (Stream<Path> paths = Files.walk(output.toPath())) {
                generatedApi = paths
                        .filter(path -> path.toString().endsWith(".ex"))
                        .filter(path -> path.getParent() != null
                                && "api".equals(path.getParent().getFileName().toString()))
                        .map(path -> {
                            try {
                                return Files.readString(path, StandardCharsets.UTF_8);
                            } catch (Exception e) {
                                throw new RuntimeException(e);
                            }
                        })
                        .collect(Collectors.joining("\n"));
            }

            String generatedMix = Files.readString(output.toPath().resolve("mix.exs"), StandardCharsets.UTF_8);
            Assert.assertTrue(generatedMix.contains("{:req, \"~> 0.6.0 or ~> 0.7.3\"}"));
            Assert.assertTrue(generatedApi.contains("Req.Request.t()"));
            Assert.assertTrue(generatedApi.contains("{:error, term()}"));
            Assert.assertFalse(generatedApi.contains("{:error, Req.Response.t() | Exception.t() | term()}"));
            Assert.assertFalse(generatedApi.contains("Tesla."));

            String deleteOrderSpec = generatedApi.lines()
                    .filter(line -> line.startsWith("  @spec delete_order("))
                    .findFirst()
                    .orElse("");
            Assert.assertTrue(deleteOrderSpec.contains("{:ok, Req.Response.t()}"), deleteOrderSpec);

            int deleteOrderSpecIndex = generatedApi.indexOf(deleteOrderSpec);
            String deleteOrderSuccessTypes = deleteOrderSpec
                    .substring(deleteOrderSpec.indexOf(" :: ") + 4)
                    .replaceFirst(" \\| \\{:error, term\\(\\)\\}.*$", "");
            String deleteOrderDoc = generatedApi.substring(0, deleteOrderSpecIndex)
                    .lines()
                    .filter(line -> line.startsWith("  - `{:ok,") && line.endsWith("` on success"))
                    .reduce((first, second) -> second)
                    .orElse("");
            Assert.assertEquals(deleteOrderDoc, "  - `" + deleteOrderSuccessTypes + "` on success");
        } finally {
            FileUtils.deleteDirectory(output);
        }
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void testUnknownLibrary() {
        new ElixirClientCodegen().setLibrary("unknown");
    }

    @Test
    public void testFalseMappedResponsesUseResponseStructInTypespec() throws Exception {
        File output = Files.createTempDirectory("elixir-codegen").toFile();
        try {
            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("elixir")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.getAbsolutePath());

            ClientOptInput input = configurator.toClientOptInput();
            new DefaultGenerator().opts(input).generate();

            String generatedApi;
            try (Stream<Path> paths = Files.walk(output.toPath())) {
                generatedApi = paths
                        .filter(path -> path.toString().endsWith(".ex"))
                        .filter(path -> path.getParent() != null
                                && "api".equals(path.getParent().getFileName().toString()))
                        .map(path -> {
                            try {
                                return Files.readString(path, StandardCharsets.UTF_8);
                            } catch (Exception e) {
                                throw new RuntimeException(e);
                            }
                        })
                        .collect(Collectors.joining("\n"));
            }

            String deleteOrderSpec = generatedApi.lines()
                    .filter(line -> line.startsWith("  @spec delete_order("))
                    .findFirst()
                    .orElse("");
            String loginUserSpec = generatedApi.lines()
                    .filter(line -> line.startsWith("  @spec login_user("))
                    .findFirst()
                    .orElse("");
            Assert.assertTrue(deleteOrderSpec.contains("{:ok, Tesla.Env.t}"), deleteOrderSpec);
            Assert.assertTrue(loginUserSpec.contains("{:ok, Tesla.Env.t}"), loginUserSpec);

            int deleteOrderSpecIndex = generatedApi.indexOf(deleteOrderSpec);
            String deleteOrderSuccessTypes = deleteOrderSpec
                    .substring(deleteOrderSpec.indexOf(" :: ") + 4)
                    .replaceFirst(" \\| \\{:error, Tesla\\.Env\\.t\\}.*$", "");
            String deleteOrderDoc = generatedApi.substring(0, deleteOrderSpecIndex)
                    .lines()
                    .filter(line -> line.startsWith("  - `{:ok,") && line.endsWith("` on success"))
                    .reduce((first, second) -> second)
                    .orElse("");
            Assert.assertEquals(deleteOrderDoc, "  - `" + deleteOrderSuccessTypes + "` on success");
        } finally {
            FileUtils.deleteDirectory(output);
        }
    }
}
