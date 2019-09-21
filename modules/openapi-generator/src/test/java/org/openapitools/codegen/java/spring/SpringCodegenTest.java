/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java.spring;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static java.util.stream.Collectors.groupingBy;
import static org.openapitools.codegen.languages.SpringCodegen.RESPONSE_WRAPPER;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;

import static org.openapitools.codegen.languages.SpringCodegen.RESPONSE_WRAPPER;
import static org.testng.Assert.assertTrue;

public class SpringCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        openAPI.setInfo(new Info());
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getBasePackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.BASE_PACKAGE), "org.openapitools");
        Assert.assertEquals(codegen.getConfigPackage(), "org.openapitools.configuration");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.CONFIG_PACKAGE), "org.openapitools.configuration");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.SERVER_PORT), "8082");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.setBasePackage("xx.yyyyyyyy.base");
        codegen.setConfigPackage("xx.yyyyyyyy.config");
        codegen.setUnhandledException(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.getBasePackage(), "xx.yyyyyyyy.base");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.BASE_PACKAGE), "xx.yyyyyyyy.base");
        Assert.assertEquals(codegen.getConfigPackage(), "xx.yyyyyyyy.config");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.CONFIG_PACKAGE), "xx.yyyyyyyy.config");
        Assert.assertEquals(codegen.isUnhandledException(), true);
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        codegen.additionalProperties().put(SpringCodegen.BASE_PACKAGE, "xyz.yyyyy.bbbb.base");
        codegen.additionalProperties().put(SpringCodegen.CONFIG_PACKAGE, "xyz.yyyyy.cccc.config");
        codegen.additionalProperties().put(SpringCodegen.SERVER_PORT, "8088");
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        openAPI.setInfo(new Info());
        openAPI.getInfo().setTitle("Some test API");
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.getBasePackage(), "xyz.yyyyy.bbbb.base");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.BASE_PACKAGE), "xyz.yyyyy.bbbb.base");
        Assert.assertEquals(codegen.getConfigPackage(), "xyz.yyyyy.cccc.config");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.CONFIG_PACKAGE), "xyz.yyyyy.cccc.config");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.TITLE), "someTest");
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.SERVER_PORT), "8088");
    }

    @Test
    public void interfaceDefaultImplDisableWithReponseWrapper() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.JAVA_8, true);
        codegen.additionalProperties().put(RESPONSE_WRAPPER, "aWrapper");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("jdk8"), false);
    }

    @Test
    public void doNotGenerateRequestParamForObjectQueryParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/objectQueryParam.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileNotContains(generator, outputPath + "/src/main/java/org/openapitools/api/PonyApi.java",  "@RequestParam");
    }

    @Test
    public void generateFormatForDateAndDateTimeQueryParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_2053.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileContains(
                generator,
                outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java",
                "@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)"
        );
        checkFileContains(
                generator,
                outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java",
                "@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE_TIME)"
        );
    }

    @Test
    public void shouldGenerateRequestParamForRefParams_3248_Regression() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/3248-regression.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java",
                "@RequestParam(value = \"format\"",
                "@RequestParam(value = \"query\"");
    }

    @Test
    public void shouldGenerateRequestParamForRefParams_3248_RegressionDates() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/3248-regression-dates.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java",
                "@RequestParam(value = \"start\"");
    }

    @Test
    public void doGenerateRequestParamForSimpleParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_3248.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/MonkeysApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/BearsApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/PandasApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/CrocodilesApi.java",  "@RequestParam");
        checkFileContains(generator, outputPath + "/src/main/java/org/openapitools/api/PolarBearsApi.java",  "@RequestParam");

    }

    private void checkFileNotContains(MockDefaultGenerator generator, String path, String... lines) {
        String file = generator.getFiles().get(path);
        assertNotNull(file);
        for (String line : lines)
            assertFalse(file.contains(line));
    }

    private void checkFileContains(MockDefaultGenerator generator, String path, String... lines) {
        String file = generator.getFiles().get(path);
        assertNotNull(file);
        int expectedCount = lines.length;
        int actualCount = 0;
        for (String line : lines) {
            if (file.contains(line)) {
                actualCount++;
            }
        }
        assertEquals(actualCount, expectedCount, "File is missing " + (expectedCount - actualCount) + " expected lines.");
    }

    @Test
    public void clientOptsUnicity() {
        SpringCodegen codegen = new SpringCodegen();
        codegen.cliOptions()
                .stream()
                .collect(groupingBy(CliOption::getOpt))
                .forEach((k,v) -> assertEquals(v.size(), 1, k + " is described multiple times"));
    }

    @Test
    public void springcloudWithJava8DisabeJdk8() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.JAVA_8, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("jdk8-default-interface"), false);
    }

    @Test
    public void springcloudWithAsyncHasResponseWrapperCallable() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.JAVA_8, false);
        codegen.additionalProperties().put(SpringCodegen.ASYNC, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();

        Assert.assertNull(codegen.additionalProperties().get("jdk8-default-interface"));
        Assert.assertEquals(codegen.additionalProperties().get(RESPONSE_WRAPPER), "Callable");
    }

    @Test
    public void springcloudWithAsyncAndJava8HasResponseWrapperCompletableFuture() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.JAVA_8, true);
        codegen.additionalProperties().put(SpringCodegen.ASYNC, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("jdk8-default-interface"), false);
        Assert.assertEquals(codegen.additionalProperties().get(RESPONSE_WRAPPER), "CompletableFuture");
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void reactiveRequiredSpringBoot() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();
    }
}
