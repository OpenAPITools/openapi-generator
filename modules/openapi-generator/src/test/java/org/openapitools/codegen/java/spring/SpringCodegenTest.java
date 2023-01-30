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

package org.openapitools.codegen.java.spring;

import static java.util.stream.Collectors.groupingBy;
import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.SpringCodegen.INTERFACE_ONLY;
import static org.openapitools.codegen.languages.SpringCodegen.REQUEST_MAPPING_OPTION;
import static org.openapitools.codegen.languages.SpringCodegen.RESPONSE_WRAPPER;
import static org.openapitools.codegen.languages.SpringCodegen.RETURN_SUCCESS_CODE;
import static org.openapitools.codegen.languages.SpringCodegen.SPRING_BOOT;
import static org.openapitools.codegen.languages.SpringCodegen.USE_SPRING_BOOT3;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.ANNOTATION_LIBRARY;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DOCUMENTATION_PROVIDER;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;

import com.github.javaparser.ast.nodeTypes.NodeWithName;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Ignore;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

public class SpringCodegenTest {

    @Test
    public void clientOptsUnicity() {
        SpringCodegen codegen = new SpringCodegen();
        codegen.cliOptions()
                .stream()
                .collect(groupingBy(CliOption::getOpt))
                .forEach((k, v) -> assertEquals(v.size(), 1, k + " is described multiple times"));
    }

    @Test
    public void doAnnotateDatesOnModelParameters() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_5436.yml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .assertTypeAnnotations()
                .hasSize(3)
                .containsWithName("Validated")
                .containsWithName("Generated")
                .containsWithNameAndAttributes("Generated", ImmutableMap.of(
                        "value", "\"org.openapitools.codegen.languages.SpringCodegen\""
                ))
                .containsWithNameAndAttributes("Tag", ImmutableMap.of(
                        "name", "\"zebras\""
                ))
                .toType()
                .assertMethod("getZebras")
                .hasReturnType("ResponseEntity<Void>")
                .assertMethodAnnotations()
                .hasSize(2)
                .containsWithNameAndAttributes("Operation", ImmutableMap.of("operationId", "\"getZebras\""))
                .containsWithNameAndAttributes("RequestMapping", ImmutableMap.of(
                        "method", "RequestMethod.GET",
                        "value", "\"/zebras\""
                ))
                .toMethod()
                .hasParameter("limit").withType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("Parameter", ImmutableMap.of("name", "\"limit\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("required", "false", "value", "\"limit\""))
                .toParameter()
                .toMethod()
                .hasParameter("animalParams").withType("AnimalParams")
                .toMethod()
                .commentContainsLines("GET /zebras", "@param limit  (optional)")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED)");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/AnimalParams.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .hasProperty("born").withType("LocalDate")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"))
                .toProperty()
                .toType()
                .hasProperty("lastSeen").withType("OffsetDateTime")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"))
                .toProperty().toType()
                .assertMethod("born", "LocalDate")
                .bodyContainsLines("this.born = born")
                .doesNotHaveComment();
    }

    @Test
    public void doGenerateCookieParams() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_5386.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java"))
                .assertMethod("getElephants", "String", "BigDecimal")
                .hasParameter("userToken")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("CookieValue", ImmutableMap.of("name", "\"userToken\""));

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .assertMethod("getZebras", "String")
                .hasParameter("userToken")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("CookieValue", ImmutableMap.of("name", "\"userToken\""));

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BirdsApi.java"))
                .assertMethod("getBirds", "BigDecimal")
                .doesNotHaveParameter("userToken")
                .noneOfParameterHasAnnotation("CookieValue");
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

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/MonkeysApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BearsApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PandasApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CrocodilesApi.java"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PolarBearsApi.java"), "@RequestParam");
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
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PonyApi.java"), "@RequestParam");
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

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertMethod("getElephants", "LocalDate")
                .hasParameter("startDate")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"));

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertMethod("getZebras", "OffsetDateTime")
                .hasParameter("startDateTime")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"));
    }

    @Test
    public void interfaceDefaultImplDisableWithResponseWrapper() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(RESPONSE_WRAPPER, "aWrapper");
        codegen.processOpts();

        // jdk8 tag has been removed
        Assert.assertEquals(codegen.additionalProperties().get("jdk8"), null);
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void reactiveRequiredSpringBoot() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();
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
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiGet", "String", "Format")
                .hasParameter("query")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"query\""))
                .toParameter().toMethod()
                .hasParameter("format")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"format\""));
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
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiGet", "OffsetDateTime")
                .hasParameter("start")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"start\""))
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"));
    }

    @Test
    public void springcloudWithAsyncAndJava8HasResponseWrapperCompletableFuture() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.ASYNC, true);
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("jdk8-default-interface"), false);
        Assert.assertEquals(codegen.additionalProperties().get(RESPONSE_WRAPPER), "CompletableFuture");
    }

    @Test
    public void springcloudWithJava8DisableJdk8() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-cloud");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("jdk8-default-interface"), false);
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
    public void testDefaultValuesFixed() {
        // we had an issue where int64, float, and double values were having single character string suffixes
        // included in their defaultValues
        // This test verifies that those characters are no longer present
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue1226.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);

        String int64Val = "9223372036854775807l";
        String floatVal = "3.14159f";
        String doubleVal = "3.14159d";

        // make sure that the model properties include character suffixes
        String modelName = "NumberHolder";
        Schema nhSchema = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel cm = codegen.fromModel(modelName, nhSchema);
        CodegenProperty int64Prop = cm.vars.get(0);
        CodegenProperty floatProp = cm.vars.get(1);
        CodegenProperty doubleProp = cm.vars.get(2);
        Assert.assertEquals(int64Prop.defaultValue, int64Val);
        Assert.assertEquals(floatProp.defaultValue, floatVal);
        Assert.assertEquals(doubleProp.defaultValue, doubleVal);

        int64Val = "9223372036854775807";
        floatVal = "3.14159";
        doubleVal = "3.14159";

        // make sure that the operation parameters omit character suffixes
        String route = "/numericqueryparams";
        Operation op = openAPI.getPaths().get(route).getGet();
        CodegenOperation co = codegen.fromOperation(route, "GET", op, null);
        CodegenParameter int64Param = co.queryParams.get(0);
        CodegenParameter floatParam = co.queryParams.get(1);
        CodegenParameter doubleParam = co.queryParams.get(2);
        Assert.assertEquals(int64Param.defaultValue, int64Val);
        Assert.assertEquals(floatParam.defaultValue, floatVal);
        Assert.assertEquals(doubleParam.defaultValue, doubleVal);
    }

    @Test
    public void testDoGenerateRequestBodyRequiredAttribute_3134_Regression() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/3134-regression.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiPost", "ExampleApiPostRequest")
                .hasParameter("exampleApiPostRequest")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestBody", ImmutableMap.of("required", "false"));

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"),
                "@RequestBody(required = false");
    }

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
    public void testMultipartBoot() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-boot");
        codegen.setDelegatePattern(true);
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, "springfox");

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");

        // Check that the delegate handles the array
        JavaFileAssert.assertThat(files.get("MultipartArrayApiDelegate.java"))
                .assertMethod("multipartArray", "List<MultipartFile>")
                .hasParameter("files").withType("List<MultipartFile>");

        // Check that the api handles the array
        JavaFileAssert.assertThat(files.get("MultipartArrayApi.java"))
                .assertMethod("multipartArray", "List<MultipartFile>")
                .hasParameter("files").withType("List<MultipartFile>")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"Many files\""))
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"files\"", "required", "false"));

        // UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
        // We will contact the contributor of the following test to see if the fix will break their use cases and
        // how we can fix it accordingly.
        //// Check that the delegate handles the single file
        // final File multipartSingleApiDelegate = files.get("MultipartSingleApiDelegate.java");
        // assertFileContains(multipartSingleApiDelegate.toPath(), "MultipartFile file");

        // Check that the api handles the single file
        JavaFileAssert.assertThat(files.get("MultipartSingleApi.java"))
                .assertMethod("multipartSingle", "MultipartFile")
                .hasParameter("file").withType("MultipartFile")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"One file\""))
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "false"));

        // Check that api validates mixed multipart request
        JavaFileAssert.assertThat(files.get("MultipartMixedApi.java"))
                .assertMethod("multipartMixed", "MultipartMixedStatus", "MultipartFile", "MultipartMixedRequestMarker", "List<MultipartMixedStatus>")
                .hasParameter("status").withType("MultipartMixedStatus")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"status\"", "required", "true"))
                .toParameter().toMethod()
                .hasParameter("file").withType("MultipartFile")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "true"))
                .toParameter().toMethod()
                .hasParameter("marker").withType("MultipartMixedRequestMarker")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"marker\"", "required", "false"))
                .toParameter().toMethod()
                .hasParameter("statusArray").withType("List<MultipartMixedStatus>")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"statusArray\"", "required", "false"));
    }

    @Test
    public void shouldAddParameterWithInHeaderWhenImplicitHeadersIsTrue_issue14418() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
            .readLocation("src/test/resources/bugs/issue_14418.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(SpringCodegen.IMPLICIT_HEADERS, "true");

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestApi.java"))
            .isInterface()
            .hasImports("io.swagger.v3.oas.annotations.enums.ParameterIn")
            .assertMethod("test")
            .assertMethodAnnotations()
            .containsWithNameAndAttributes("Parameters", ImmutableMap.of(
                "value", "{ @Parameter(name = \"testHeader\", description = \"Test header\", required = true, in = ParameterIn.HEADER) }"
                // in = ParameterIn.HEADER is missing?!
            ));
    }

    // Helper function, intended to reduce boilerplate
    private Map<String, File> generateFiles(SpringCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(input).generate();

        return files.stream().collect(Collectors.toMap(e -> e.getName().replace(outputPath, ""), i -> i));
    }

    /*
     * UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
     * We will contact the contributor of the following test to see if the fix will break their use cases and
     * how we can fix it accordingly.
     */
    @Test
    @Ignore
    public void testMultipartCloud() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-cloud");
        codegen.setDelegatePattern(true);

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");

        // Check that the delegate handles the array and the file
        final File multipartApiDelegate = files.get("MultipartApiDelegate.java");
        assertFileContains(multipartApiDelegate.toPath(),
                "List<MultipartFile> files",
                "MultipartFile file");

        // Check that the api handles the array and the file
        final File multipartApi = files.get("MultipartApi.java");
        assertFileContains(multipartApi.toPath(),
                "List<MultipartFile> files",
                "MultipartFile file");
    }

    @Test
    public void testRequestMappingAnnotation() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-boot");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, SpringCodegen.RequestMappingMode.api_interface);

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/2_0/petstore.yaml");

        // Check that the @RequestMapping annotation is generated in the Api file
        final File petApiFile = files.get("PetApi.java");
        assertFileContains(petApiFile.toPath(), "@RequestMapping(\"${openapi.openAPIPetstore.base-path:/v2}\")");

        // Check that the @RequestMapping annotation is not generated in the Controller file
        final File petApiControllerFile = files.get("PetApiController.java");
        assertFileNotContains(petApiControllerFile.toPath(), "@RequestMapping(\"${openapi.openAPIPetstore.base-path:/v2}\")");
    }

   @Test
   public void testNoRequestMappingAnnotation_spring_cloud_default() throws IOException {
      final SpringCodegen codegen = new SpringCodegen();
      codegen.setLibrary( "spring-cloud" );

      final Map<String, File> files = generateFiles( codegen, "src/test/resources/2_0/petstore.yaml" );

      // Check that the @RequestMapping annotation is not generated in the Api file
      final File petApiFile = files.get( "PetApi.java" );
      JavaFileAssert.assertThat( petApiFile ).assertTypeAnnotations().hasSize( 3 ).containsWithName( "Validated" )
            .containsWithName( "Generated" ).containsWithName( "Tag" );

   }

    @Test
    public void testNoRequestMappingAnnotation() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary( "spring-cloud" );
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, SpringCodegen.RequestMappingMode.none);

        final Map<String, File> files = generateFiles( codegen, "src/test/resources/2_0/petstore.yaml" );

        // Check that the @RequestMapping annotation is not generated in the Api file
        final File petApiFile = files.get( "PetApi.java" );
        JavaFileAssert.assertThat( petApiFile ).assertTypeAnnotations().hasSize( 3 ).containsWithName( "Validated" )
            .containsWithName( "Generated" ).containsWithName( "Tag" );
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
    public void useBeanValidationTruePerformBeanValidationFalseJava8TrueForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, false, true, "@Email", "@org.hibernate.validator.constraints.Email");
    }

    @Test
    public void useBeanValidationTruePerformBeanValidationTrueJava8FalseForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, true, false, "@Email", "@org.hibernate.validator.constraints.Email");
    }

    // note: java8 option/mustache tag has been removed and default to true
    private void beanValidationForFormatEmail(boolean useBeanValidation, boolean performBeanValidation, boolean java8, String contains, String notContains) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(useBeanValidation);
        codegen.setPerformBeanValidation(performBeanValidation);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        List<File> files = generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PersonWithEmail.java"), contains);
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PersonWithEmail.java"), notContains);
    }

    @Test
    public void useBeanValidationTruePerformBeanValidationTrueJava8TrueForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, true, true, "@Email", "@org.hibernate.validator.constraints.Email");
    }

    @Test
    public void reactiveMapTypeRequestMonoTest() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/issue_8045.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApi.java"), "Mono<Map<String, DummyRequest>>");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApiDelegate.java"), "Mono<Map<String, DummyRequest>>");
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApi.java"), "Mono<DummyRequest>");
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApiDelegate.java"), "Mono<DummyRequest>");
    }

    @Test
    public void shouldEscapeReservedKeyWordsForRequestParameters_7506_Regression() throws Exception {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-boot");
        codegen.setDelegatePattern(true);

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/issue7506.yaml");

        final File multipartArrayApiDelegate = files.get("ExampleApi.java");
        assertFileContains(multipartArrayApiDelegate.toPath(), "@RequestPart(value = \"super\", required = false) MultipartFile _super");
        assertFileContains(multipartArrayApiDelegate.toPath(), "@RequestPart(value = \"package\", required = false) MultipartFile _package");
    }

    @Test
    public void doGeneratePathVariableForSimpleParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_6762.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, "springfox");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"), "allowableValues = \"0, 1\"");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"), "@PathVariable");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BearsApi.java"), "allowableValues = \"sleeping, awake\"");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BearsApi.java"), "@PathVariable");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java"), "allowableValues = \"sleeping, awake\"");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java"), "@PathVariable");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/GiraffesApi.java"), "allowableValues = \"0, 1\"");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/GiraffesApi.java"), "@PathVariable");
    }

    @Test
    public void shouldGenerateDefaultValueForEnumRequestParameter() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/issue_10278.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/GetApi.java"),
                "@RequestParam(value = \"testParameter1\", required = false, defaultValue = \"BAR\")",
                "@RequestParam(value = \"TestParameter2\", required = false, defaultValue = \"BAR\")");

    }

    /**
     * Define documentation providers to test
     */
    private final static String SPRINGFOX = "springfox";
    private final static String SPRINGFOX_DESTINATIONFILE = "SpringFoxConfiguration.java";
    private final static String SPRINGFOX_TEMPLATEFILE = "openapiDocumentationConfig.mustache";
    private final static String SPRINGDOC = "springdoc";
    private final static String SPRINGDOC_DESTINATIONFILE = "SpringDocConfiguration.java";
    private final static String SPRINGDOC_TEMPLATEFILE = "springdocDocumentationConfig.mustache";

    /**
     * test whether OpenAPIDocumentationConfig.java is generated
     * fix issue #10287
     */
    @Test
    public void testConfigFileGeneration_springfox() {
        testConfigFileCommon(SPRINGFOX, SPRINGFOX_DESTINATIONFILE, SPRINGFOX_TEMPLATEFILE);
    }

    /**
     * test whether SpringDocDocumentationConfig.java is generated
     * fix issue #12220
     */
    @Test
    public void testConfigFileGeneration_springdoc() {
        testConfigFileCommon(SPRINGDOC, SPRINGDOC_DESTINATIONFILE, SPRINGDOC_TEMPLATEFILE);
    }

    private void testConfigFileCommon(String documentationProvider, String destinationFile, String templateFileName) {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, documentationProvider);
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, false);
        codegen.additionalProperties().put(SpringCodegen.SPRING_CLOUD_LIBRARY, "spring-cloud");
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, false);
        codegen.additionalProperties().put(SpringCodegen.API_FIRST, false);

        codegen.processOpts();

        final List<SupportingFile> supList = codegen.supportingFiles();
        String tmpFile;
        String desFile;
        boolean flag = false;
        for (final SupportingFile s : supList) {
            tmpFile = s.getTemplateFile();
            desFile = s.getDestinationFilename();

            if (templateFileName.equals(tmpFile)) {
                flag = true;
                assertEquals(desFile, destinationFile);
            }
        }
        if (!flag) {
            fail(templateFileName + " not generated");
        }
    }

    @Test
    public void shouldAddNotNullOnRequiredAttributes() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/issue_5026-b.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"), "status");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"), "@NotNull");
        Files.readAllLines(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java")).forEach(System.out::println);

    }

    @Test
    public void shouldNotAddNotNullOnReadOnlyAttributes() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/issue_5026.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"), "status");
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"), "@NotNull");
        Files.readAllLines(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java")).forEach(System.out::println);

    }

    @Test
    public void testOneOf5381() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_5381.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        //generator.setGeneratorPropertyDefault(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Foo.java"), "public class Foo implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRef.java"), "public class FooRef implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRefOrValue.java"), "public interface FooRefOrValue");
    }

    @Test
    public void testOneOfAndAllOf() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/oneof_polymorphism_and_inheritance.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        //generator.setGeneratorPropertyDefault(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Foo.java"), "public class Foo extends Entity implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRef.java"), "public class FooRef extends EntityRef implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRefOrValue.java"), "public interface FooRefOrValue");
        // previous bugs
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/BarRef.java"), "atTypesuper.hashCode");
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/BarRef.java"), "private String atBaseType");
        // imports for inherited properties
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PizzaSpeziale.java"), "import java.math.BigDecimal");
    }

    @Test
    public void testTypeMappings() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.typeMapping().get("file"), "org.springframework.core.io.Resource");
    }

    @Test
    public void testImportMappings() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.importMapping().get("org.springframework.core.io.Resource"), "org.springframework.core.io.Resource");
        Assert.assertEquals(codegen.importMapping().get("Pageable"), "org.springframework.data.domain.Pageable");
        Assert.assertEquals(codegen.importMapping().get("DateTimeFormat"), "org.springframework.format.annotation.DateTimeFormat");
        Assert.assertEquals(codegen.importMapping().get("ApiIgnore"), "springfox.documentation.annotations.ApiIgnore");
        Assert.assertEquals(codegen.importMapping().get("ParameterObject"), "org.springdoc.api.annotations.ParameterObject");
    }

    @Test(dataProvider = "issue11464TestCases")
    public void shouldGenerateOneTagAttributeForMultipleTags_Regression11464(String documentProvider, Consumer<String> assertFunction) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_11464.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, documentProvider);
        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFunction.accept(outputPath);
    }

    @DataProvider
    public Object[][] issue11464TestCases() {
        return new Object[][]{
                {DocumentationProviderFeatures.DocumentationProvider.SPRINGDOC.name(), (Consumer<String>) outputPath -> {
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/NoneApi.java"),
                            "@Operation( operationId = \"getNone\", summary = \"No Tag\", responses = {");
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SingleApi.java"),
                            "@Operation( operationId = \"getSingleTag\", summary = \"Single Tag\", tags = { \"tag1\" }, responses = {");
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/MultipleApi.java"),
                            "@Operation( operationId = \"getMultipleTags\", summary = \"Multiple Tags\", tags = { \"tag1\", \"tag2\" }, responses = {");
                }},
                {DocumentationProviderFeatures.DocumentationProvider.SPRINGFOX.name(), (Consumer<String>) outputPath -> {
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/NoneApi.java"),
                            "@ApiOperation( value = \"No Tag\", nickname = \"getNone\", notes = \"\", response = ");
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SingleApi.java"),
                            "@ApiOperation( tags = { \"tag1\" }, value = \"Single Tag\", nickname = \"getSingleTag\", notes = \"\", response = ");
                    assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/MultipleApi.java"),
                            "@ApiOperation( tags = { \"tag1\", \"tag2\" }, value = \"Multiple Tags\", nickname = \"getMultipleTags\", notes = \"\", response = ");
                }},
        };
    }

    @Test
    public void apiFirstShouldNotGenerateApiOrModel() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(SpringCodegen.API_FIRST, true);
        codegen.processOpts();
        Assert.assertTrue(codegen.modelTemplateFiles().isEmpty());
        Assert.assertTrue(codegen.apiTemplateFiles().isEmpty());
    }

    @Test
    public void testIssue11323() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/spring/issue_11323.yml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        //codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Address.java"),
                "@JsonValue", "import com.fasterxml.jackson.annotation.JsonValue;");
    }

    @Test
    public void shouldPurAdditionalModelTypesOverAllModels() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@path.Annotation(param1 = \"test1\", param2 = 3);@path.Annotation2;@custom.Annotation");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        File[] generatedModels = new File(outputPath + "/src/main/java/org/openapitools/model").listFiles();
        assertThat(generatedModels).isNotEmpty();

        for (File modelPath : generatedModels) {
            JavaFileAssert.assertThat(modelPath)
                    .assertTypeAnnotations()
                    .containsWithName("custom.Annotation")
                    .containsWithName("path.Annotation2")
                    .containsWithNameAndAttributes("path.Annotation", ImmutableMap.of("param1", "\"test1\"", "param2", "3"));
        }
    }

    @Test
    public void shouldGenerateExternalDocs() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
            .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");
        codegen.additionalProperties().put(BeanValidationFeatures.USE_BEANVALIDATION, "true");

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetApi.java"))
            .printFileContent()
            .hasImports("io.swagger.v3.oas.annotations.ExternalDocumentation")
            .assertMethod("updatePet")
            .assertMethodAnnotations()
            .containsWithName("Operation")
            .containsWithNameAndAttributes("Operation",
                ImmutableMap.of(
                    "operationId", "\"updatePet\"",
                    //"security", "{ @SecurityRequirement(name = \"petstore_auth\", scopes = { \"write:pets\", \"read:pets\" }) }",
                    "externalDocs", "@ExternalDocumentation(description = \"API documentation for the updatePet operation\", url = \"http://petstore.swagger.io/v2/doc/updatePet\")"
                )
                );
    }

    @Test
    public void testHandleDefaultValue_issue8535() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        Map<String, File> files = generateFromContract("src/test/resources/3_0/issue_8535.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("TestHeadersApi.java"))
                .assertMethod("headersTest")
                .hasParameter("headerNumber").withType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .hasParameter("headerString").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("headerStringWrapped").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("headerStringQuotes").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("headerStringQuotesWrapped").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("headerBoolean").withType("Boolean")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"true\""));

        JavaFileAssert.assertThat(files.get("TestQueryParamsApi.java"))
                .assertMethod("queryParamsTest")
                .hasParameter("queryNumber").withType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .hasParameter("queryString").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringWrapped").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotes").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryStringQuotesWrapped").withType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .hasParameter("queryBoolean").withType("Boolean")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"true\""));
    }

    @Test
    public void testExtraAnnotations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_11772.yml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        TestUtils.assertExtraAnnotationFiles(outputPath + "/src/main/java/org/openapitools/model");

    }

    @Test
    public void testResponseWithArray_issue11897() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(AbstractJavaCodegen.FULL_JAVA_UTIL, "true");
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11897.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("MetadataApi.java"))
                .assertMethod("getWithArrayOfObjects").hasReturnType("ResponseEntity<List<TestResponse>>")
                .toFileAssert()
                .assertMethod("getWithArrayOfString").hasReturnType("ResponseEntity<List<String>>")
                .toFileAssert()
                .assertMethod("getWithSetOfObjects").hasReturnType("ResponseEntity<Set<TestResponse>>")
                .toFileAssert()
                .assertMethod("getWithSetOfStrings").hasReturnType("ResponseEntity<Set<String>>")
                .toFileAssert()
                .assertMethod("getWithMapOfObjects").hasReturnType("ResponseEntity<Map<String, TestResponse>>")
                .toFileAssert()
                .assertMethod("getWithMapOfStrings").hasReturnType("ResponseEntity<Map<String, String>>");
    }

    @Test
    public void testResponseWithArray_issue12524() throws Exception {
        GlobalSettings.setProperty("skipFormModel", "true");

        try {
            Map<String, Object> additionalProperties = new HashMap<>();
            additionalProperties.put(DOCUMENTATION_PROVIDER, "none");
            additionalProperties.put(ANNOTATION_LIBRARY, "none");
            additionalProperties.put(RETURN_SUCCESS_CODE, "true");
            Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_12524.json", SPRING_BOOT, additionalProperties);

            JavaFileAssert.assertThat(files.get("API01ListOfStuff.java"))
                .hasImports("com.fasterxml.jackson.annotation.JsonTypeName");
        } finally {
            GlobalSettings.reset();
        }
    }

    @Test
    public void paramObjectImportForDifferentSpringBootVersions_issue14077() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        Map<String, File> files = generateFromContract("src/test/resources/2_0/petstore-with-spring-pageable.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
            .hasImports("org.springdoc.api.annotations.ParameterObject")
            .assertMethod("findPetsByStatus")
            .hasParameter("pageable").withType("Pageable")
            .assertParameterAnnotations()
            .containsWithName("ParameterObject");


        // different import for SB3
        additionalProperties.put(USE_SPRING_BOOT3, "true");
        files = generateFromContract("src/test/resources/2_0/petstore-with-spring-pageable.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
            .hasImports("org.springdoc.core.annotations.ParameterObject")
            .assertMethod("findPetsByStatus")
            .hasParameter("pageable").withType("Pageable")
            .assertParameterAnnotations()
            .containsWithName("ParameterObject");
    }

    @Test
    public void shouldSetDefaultValueForMultipleArrayItems() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(AbstractJavaCodegen.FULL_JAVA_UTIL, "true");
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");

        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11957.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("SearchApi.java"))
                .assertMethod("defaultList")
                .hasParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"updatedAt:DESC,createdAt:DESC\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("defaultSet")
                .hasParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"updatedAt:DESC,createdAt:DESC\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("emptyDefaultList")
                .hasParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("emptyDefaultSet")
                .hasParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"\""));
    }

    @Test
    public void testPutItemsMethodContainsKeyInSuperClassMethodCall_issue12494() throws IOException {
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_12494.yaml", null);

        JavaFileAssert.assertThat(files.get("ChildClass.java"))
                .assertMethod("putSomeMapItem")
                .bodyContainsLines("super.putSomeMapItem(key, someMapItem);");
    }

    @Test
    public void shouldHandleCustomResponseType_issue11731() throws IOException {
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11731.yaml", SPRING_BOOT);

        JavaFileAssert.assertThat(files.get("CustomersApi.java"))
                .assertMethod("getAllUsingGET1")
                .bodyContainsLines("if (mediaType.isCompatibleWith(MediaType.valueOf(\"application/hal+json\"))) {");
    }

    @Test
    public void shouldHandleContentTypeWithSecondWildcardSubtype_issue12457() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_12457.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("UsersApi.java"))
                .assertMethod("wildcardSubTypeForContentType")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("RequestMapping", ImmutableMap.of(
                        "produces", "{ \"application/json\", \"application/*\" }",
                        "consumes", "{ \"application/octet-stream\", \"application/*\" }"
                ));
    }

    @Test
    public void shouldGenerateDiscriminatorFromAllOfWhenUsingLegacyDiscriminatorBehaviour_issue12692() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true");
        Map<String, File> output = generateFromContract("src/test/resources/bugs/issue_12692.yml", SPRING_BOOT, additionalProperties);

        String jsonTypeInfo = "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"type\", visible = true)";
        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = Cat.class, name = \"cat\")" +
                "})";
        assertFileContains(output.get("Pet.java").toPath(), jsonTypeInfo, jsonSubType);
    }

    @Test
    public void shouldGenerateBeanValidationOnHeaderParams() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_7125.json", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");
        codegen.additionalProperties().put(BeanValidationFeatures.USE_BEANVALIDATION, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("SomeMethodApi.java"))
                .printFileContent()
                .assertMethod("methodWithValidation")
                .hasParameter("headerOne")
                .assertParameterAnnotations()
                .containsWithName("RequestHeader")
                .containsWithName("NotNull")
                .containsWithNameAndAttributes("Size", ImmutableMap.of(
                        "min", "1",
                        "max", "10"
                ))
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of("regexp", "\"\\\\d+\""))
                .toParameter()
                .toMethod()
                .hasParameter("headerTwo")
                .assertParameterAnnotations()
                .containsWithName("RequestHeader")
                .containsWithName("NotNull")
                .containsWithNameAndAttributes("Min", ImmutableMap.of("value", "500"))
                .containsWithNameAndAttributes("Max", ImmutableMap.of("value", "10000"));
    }

    @Test
    public void requiredFieldShouldIncludeNotNullAnnotation_issue13365() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "false");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "false");
        codegen.additionalProperties().put(SpringCodegen.OPENAPI_NULLABLE, "false");
        codegen.additionalProperties().put(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        //Assert that NotNull annotation exists alone with no other BeanValidation annotations
        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"))
                .printFileContent();
        javaFileAssert.assertMethod("getName").assertMethodAnnotations()
                .containsWithName("NotNull").anyMatch(annotation ->
                        !annotation.getNameAsString().equals("Valid") ||
                                !annotation.getNameAsString().equals("Pattern") ||
                                !annotation.getNameAsString().equals("Email") ||
                                !annotation.getNameAsString().equals("Size"));
        javaFileAssert.hasImports("javax.validation.constraints.NotNull");
    }

    @Test
    public void requiredFieldShouldIncludeNotNullAnnotationJakarta_issue13365_issue13885() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "false");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "false");
        codegen.additionalProperties().put(SpringCodegen.USE_SPRING_BOOT3, "true");
        codegen.additionalProperties().put(SpringCodegen.OPENAPI_NULLABLE, "false");
        codegen.additionalProperties().put(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        //Assert that NotNull annotation exists alone with no other BeanValidation annotations
        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"))
                .printFileContent();
        javaFileAssert.assertMethod("getName").assertMethodAnnotations()
                .containsWithName("NotNull").anyMatch(annotation ->
                        !annotation.getNameAsString().equals("Valid") ||
                                !annotation.getNameAsString().equals("Pattern") ||
                                !annotation.getNameAsString().equals("Email") ||
                                !annotation.getNameAsString().equals("Size"));
        javaFileAssert.hasImports("jakarta.validation.constraints.NotNull");
    }

    @Test
    public void nonRequiredFieldShouldNotIncludeNotNullAnnotation_issue13365() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.OPENAPI_NULLABLE, "false");
        codegen.additionalProperties().put(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Alien.java"))
                .printFileContent();
        javaFileAssert.assertMethod("getName")
                .assertMethodAnnotations().anyMatch(annotation -> !annotation.getNameAsString().equals("NotNull"));
        javaFileAssert.hasNoImports("javax.validation.constraints.NotNull");
    }

    @Test
    public void requiredFieldShouldIncludeNotNullAnnotationWithBeanValidationTrue_issue14252() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_14252.yaml");

        JavaFileAssert.assertThat(files.get("MyResponse.java"))
            .printFileContent()
            .hasImports("com.fasterxml.jackson.databind.annotation.JsonSerialize", "com.fasterxml.jackson.databind.ser.std.ToStringSerializer")
            .assertMethod("getMyPropTypeNumber")
            .assertMethodAnnotations()
            .containsWithNameAndAttributes("JsonSerialize", ImmutableMap.of(
                "using", "ToStringSerializer.class"
            ));
    }

    @Test
    public void requiredFieldShouldIncludeNotNullAnnotationWithBeanValidationTrue_issue13365() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "false");
        codegen.additionalProperties().put(SpringCodegen.OPENAPI_NULLABLE, "false");
        codegen.additionalProperties().put(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"))
                .printFileContent();
        javaFileAssert.assertMethod("getName").assertMethodAnnotations()
                .containsWithName("NotNull").containsWithName("Size").containsWithName("Email");
        javaFileAssert
            .hasNoImports("javax.validation.constraints.NotNull")
            .hasImports("javax.validation.constraints");
    }

    public void shouldUseEqualsNullableForArrayWhenSetInConfig_issue13385() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13385.yml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
                .printFileContent()
                .assertMethod("equals")
                .bodyContainsLines("return equalsNullable(this.picture, testObject.picture);");

    }

    @Test
    public void shouldNotUseEqualsNullableForArrayWhenNotSetInConfig_issue13385() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13385_2.yml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
                .printFileContent()
                .assertMethod("equals")
                .bodyContainsLines("return Arrays.equals(this.picture, testObject.picture);");
    }

    @Test
    public void shouldHandleSeparatelyInterfaceAndModelAdditionalAnnotations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
            .readLocation("src/test/resources/bugs/issue_13917.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@marker.Class1;@marker.Class2;@marker.Common");
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_ONE_OF_TYPE_ANNOTATIONS, "@marker.Interface1;@marker.Common");

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PatchRequestInner.java"))
            .isInterface()
            .assertTypeAnnotations()
            .containsWithName("marker.Interface1")
            .containsWithName("marker.Common");

        JavaFileAssert.assertThat(files.get("JSONPatchRequestRemove.java"))
            .isNormalClass()
            .assertTypeAnnotations()
            .containsWithName("marker.Class1")
            .containsWithName("marker.Class2")
            .containsWithName("marker.Common");
    }

    @Test
    public void contractWithoutEnumDoesNotContainsEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/generic.yaml", SPRING_BOOT);

        assertThat(output).doesNotContainKey("EnumConverterConfiguration.java");
    }

    @Test
    public void contractWithEnumContainsEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/enum.yaml", SPRING_BOOT);

        JavaFileAssert.assertThat(output.get("EnumConverterConfiguration.java"))
                .assertMethod("typeConverter");
    }

    private Map<String, File> generateFromContract(String url, String library) throws IOException {
        return generateFromContract(url, library, new HashMap<>());
    }
    private Map<String, File> generateFromContract(String url, String library, Map<String, Object> additionalProperties) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(url, null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        if (null != library) {
            codegen.setLibrary(library);
        }
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().putAll(additionalProperties);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        return generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));
    }
}
