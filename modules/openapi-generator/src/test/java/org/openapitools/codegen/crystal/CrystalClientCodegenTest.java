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

package org.openapitools.codegen.crystal;

import io.swagger.v3.oas.models.OpenAPI;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CrystalClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;

import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Tests for CrystalClientCodegen-generated templates
 */
public class CrystalClientCodegenTest {

    @Test
    public void testGenerateCrystalClientWithHtmlEntity() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/pathWithHtmlEntity.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("default_api.cr")) {
                apiFileGenerated = true;
                // Crystal client should set the path unescaped in the api file
                assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8)
                        .contains("local_var_path = \"/foo=bar\""));
            }
        }
        if (!apiFileGenerated) {
            fail("Default api file is not generated!");
        }
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assert.assertEquals(codegen.apiPackage(), "api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "crystal-models");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "crystal-api");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "crystal-models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "crystal-api");
    }

    @Test
    public void testBooleanDefaultValue() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/npe1.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("default_api.cr")) {
                apiFileGenerated = true;
                // Crystal client should set the path unescaped in the api file
                assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8)
                        .contains("local_var_path = \"/default/Resources/{id}\""));
            }
        }
        if (!apiFileGenerated) {
            fail("Default api file is not generated!");
        }
    }

    @Test
    public void testSanitizeModelName() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.sanitizeModelName("JSON::Any"), "JSON::Any");
        // Disallows single colons
        Assert.assertEquals(codegen.sanitizeModelName("JSON:Any"), "JSONAny");
    }
}
