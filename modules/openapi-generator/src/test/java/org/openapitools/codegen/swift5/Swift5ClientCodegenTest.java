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

package org.openapitools.codegen.swift5;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.Swift5ClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Swift5ClientCodegenTest {

    Swift5ClientCodegen swiftCodegen = new Swift5ClientCodegen();

    @Test(enabled = true)
    public void testCapitalizedReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("AS", null), "_as");
    }

    @Test(enabled = true)
    public void testReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Public", null), "_public");
    }

    @Test(enabled = true)
    public void shouldNotBreakNonReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Error", null), "error");
    }

    @Test(enabled = true)
    public void shouldNotBreakCorrectName() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("EntryName", null), "entryName");
    }

    @Test(enabled = true)
    public void testSingleWordAllCaps() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("VALUE", null), "value");
    }

    @Test(enabled = true)
    public void testSingleWordLowercase() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("value", null), "value");
    }

    @Test(enabled = true)
    public void testCapitalsWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY_NAME", null), "entryName");
    }

    @Test(enabled = true)
    public void testCapitalsWithDash() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY-NAME", null), "entryName");
    }

    @Test(enabled = true)
    public void testCapitalsWithSpace() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY NAME", null), "entryName");
    }

    @Test(enabled = true)
    public void testLowercaseWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("entry_name", null), "entryName");
    }

    @Test(enabled = true)
    public void testStartingWithNumber() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123Entry_name", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName123", null), "_123entryName123");
    }

    @Test(enabled = true)
    public void testSpecialCharacters() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("1:1", null), "_1Colon1");
        Assert.assertEquals(swiftCodegen.toEnumVarName("1:One", null), "_1ColonOne");
        Assert.assertEquals(swiftCodegen.toEnumVarName("Apple&Swift", null), "appleAmpersandSwift");
        Assert.assertEquals(swiftCodegen.toEnumVarName("$", null), "dollar");
        Assert.assertEquals(swiftCodegen.toEnumVarName("+1", null), "plus1");
        Assert.assertEquals(swiftCodegen.toEnumVarName(">=", null), "greaterThanOrEqualTo");
    }

    @Test(description = "returns Data when response format is binary", enabled = true)
    public void binaryDataTest() {
        // TODO update json file

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/tests/binaryResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.returnType, "URL");
        Assert.assertEquals(op.bodyParam.dataType, "URL");
        Assert.assertTrue(op.bodyParam.isBinary);
        Assert.assertTrue(op.responses.get(0).isBinary);
    }

    @Test(description = "returns Date when response format is date per default", enabled = true)
    public void dateDefaultTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.returnType, "Date");
        Assert.assertEquals(op.bodyParam.dataType, "Date");
    }

    @Test(description = "returns Date when response format is date and cli option is disabled", enabled = true)
    public void dateDisabledCLITest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.additionalProperties().put(Swift5ClientCodegen.USE_CUSTOM_DATE_WITHOUT_TIME, false);
        codegen.processOpts();
        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.returnType, "Date");
        Assert.assertEquals(op.bodyParam.dataType, "Date");
    }

    @Test(description = "returns OpenAPIDateWithoutTime when response format is date and cli option is enabled", enabled = true)
    public void dateWithoutTimeTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.additionalProperties().put(Swift5ClientCodegen.USE_CUSTOM_DATE_WITHOUT_TIME, true);
        codegen.processOpts();

        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.returnType, "OpenAPIDateWithoutTime");
        Assert.assertEquals(op.bodyParam.dataType, "OpenAPIDateWithoutTime");
    }

    @Test(description = "type from languageSpecificPrimitives should not be prefixed", enabled = true)
    public void prefixExceptionTest() {
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setModelNamePrefix("API");

        final String result = codegen.toModelName("AnyCodable");
        Assert.assertEquals(result, "AnyCodable");
    }

    @Test(description = "type from languageSpecificPrimitives should not be suffixed", enabled = true)
    public void suffixExceptionTest() {
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setModelNameSuffix("API");

        final String result = codegen.toModelName("AnyCodable");
        Assert.assertEquals(result, "AnyCodable");
    }

    @Test(description = "Other types should be prefixed", enabled = true)
    public void prefixTest() {
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setModelNamePrefix("API");

        final String result = codegen.toModelName("MyType");
        Assert.assertEquals(result, "APIMyType");
    }

    @Test(description = "Other types should be suffixed", enabled = true)
    public void suffixTest() {
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setModelNameSuffix("API");

        final String result = codegen.toModelName("MyType");
        Assert.assertEquals(result, "MyTypeAPI");
    }

    @Test(enabled = true)
    public void testDefaultPodAuthors() throws Exception {
        // Given

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift5ClientCodegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, Swift5ClientCodegen.DEFAULT_POD_AUTHORS);
    }

    @Test(enabled = true)
    public void testPodAuthors() throws Exception {
        // Given
        final String openAPIDevs = "OpenAPI Devs";
        swiftCodegen.additionalProperties().put(Swift5ClientCodegen.POD_AUTHORS, openAPIDevs);

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift5ClientCodegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, openAPIDevs);
    }

    @Test(description = "Bug example code generation", enabled = true)
    public void crashSwift5ExampleCodeGenerationStackOverflowTest() throws IOException {
        //final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/Swift5CodeGenerationStackOverflow#2966.yaml");
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("swift5")
                    .setValidateSpec(false)
                    .setInputSpec("src/test/resources/bugs/Swift5CodeGenerationStackOverflow#2966.yaml")
                    .setEnablePostProcessFile(true)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.ENABLE_POST_PROCESS_FILE, "true");

            List<File> files = generator.opts(clientOptInput).generate();
            Assert.assertTrue(files.size() > 0, "No files generated");
        } finally {
           output.deleteOnExit();
        }
    }

    @Test(description = "Bug example code generation 2", enabled = true)
    public void crashSwift5ExampleCodeGenerationStackOverflowBug_2Test() throws IOException {
        //final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/Swift5CodeGenerationStackOverflow#2966.yaml");
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("swift5")
                    .setValidateSpec(false)
//                    .setInputSpec("http://localhost:8080/api/openapi.yaml")
                    .setInputSpec("src/test/resources/bugs/Swift5CodeGenerationBug2.yaml")
                    //.setInputSpec("http://localhost:8080/api/openapi.yaml")
                    .setEnablePostProcessFile(true)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.ENABLE_POST_PROCESS_FILE, "true");

            List<File> files = generator.opts(clientOptInput).generate();
            Assert.assertTrue(files.size() > 0, "No files generated");
        } finally {
            output.deleteOnExit();
        }
    }

    @Test(description = "optional form parameters when using oneOf schema", enabled = true)
    public void oneOfFormParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_15511.yaml");
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final String path = "/as/token.oauth2";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.formParams.size(), 6);

        Assert.assertEquals(op.formParams.get(0).baseName, "client_id");
        Assert.assertEquals(op.formParams.get(1).baseName, "grant_type");
        Assert.assertEquals(op.formParams.get(2).baseName, "password");
        Assert.assertEquals(op.formParams.get(3).baseName, "scope");
        Assert.assertEquals(op.formParams.get(4).baseName, "username");
        Assert.assertEquals(op.formParams.get(5).baseName, "refresh_token");

        Assert.assertEquals(op.formParams.get(0).required, false);
        Assert.assertEquals(op.formParams.get(1).required, false);
        Assert.assertEquals(op.formParams.get(2).required, false);
        Assert.assertEquals(op.formParams.get(3).required, false);
        Assert.assertEquals(op.formParams.get(4).required, false);
        Assert.assertEquals(op.formParams.get(5).required, false);

    }

}
