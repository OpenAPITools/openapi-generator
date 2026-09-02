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
            Assert.assertTrue(generatedApi.contains("{:error, Req.Response.t() | Exception.t() | term()}"));
            Assert.assertFalse(generatedApi.contains("Tesla."));

            String deleteOrderSpec = generatedApi.lines()
                    .filter(line -> line.startsWith("  @spec delete_order("))
                    .findFirst()
                    .orElse("");
            Assert.assertTrue(deleteOrderSpec.contains("{:ok, Req.Response.t()}"), deleteOrderSpec);

            int deleteOrderSpecIndex = generatedApi.indexOf(deleteOrderSpec);
            String deleteOrderSuccessTypes = deleteOrderSpec
                    .substring(deleteOrderSpec.indexOf(" :: ") + 4)
                    .replaceFirst(" \\| \\{:error, Req\\.Response\\.t\\(\\) \\| Exception\\.t\\(\\) \\| term\\(\\)\\}.*$", "");
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
}
