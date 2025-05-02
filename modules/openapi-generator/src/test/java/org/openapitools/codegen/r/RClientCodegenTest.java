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

package org.openapitools.codegen.r;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.RClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Pattern;

public class RClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testNullCheckOnEnumValues() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_18016.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(rClientCodegen);
        defaultGenerator.opts(clientOptInput);

        var petsApi = defaultGenerator.generate().stream()
                .filter(file -> "pets_api.R".equals(file.getName())).findFirst();
        if (petsApi.isEmpty()) {
            Assert.fail("`pets_api.R` have not been generated");
        }
        var isIfCondition = Pattern.compile("^\\s*(?!<#)\\s*if.*\\s%in%\\s.*").asPredicate();
        var containsNullCheck = Pattern.compile("![(\\s]*is\\.null").asPredicate();
        var hit = false;
        for (var line : Files.readAllLines(Paths.get(petsApi.get().getAbsolutePath()))) {
            if (isIfCondition.test(line)) {
                hit = true;
                Assert.assertTrue(containsNullCheck.test(line), "Null check is missing in line: " + line);
            }
        }
        Assert.assertTrue(hit, "No if statement for enum found");
    }
}
