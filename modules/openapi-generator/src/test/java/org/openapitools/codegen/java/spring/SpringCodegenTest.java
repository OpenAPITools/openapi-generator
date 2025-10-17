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

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.lang3.StringUtils;
import org.assertj.core.api.MapAssert;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Ignore;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.*;
import static org.openapitools.codegen.languages.AbstractJavaCodegen.GENERATE_BUILDERS;
import static org.openapitools.codegen.languages.AbstractJavaCodegen.GENERATE_CONSTRUCTOR_WITH_ALL_ARGS;
import static org.openapitools.codegen.languages.SpringCodegen.*;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.ANNOTATION_LIBRARY;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DOCUMENTATION_PROVIDER;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.fail;

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
        generator.setGenerateMetadata(false);
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
                        "value", "ZebrasApi.PATH_GET_ZEBRAS"
                ))
                .toMethod()
                .assertParameter("limit").hasType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("Parameter", ImmutableMap.of("name", "\"limit\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("required", "false", "value", "\"limit\""))
                .toParameter()
                .toMethod()
                .assertParameter("animalParams").hasType("AnimalParams")
                .toMethod()
                .commentContainsLines("GET /zebras", "@param limit  (optional)")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED)");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/AnimalParams.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertProperty("born").withType("LocalDate")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"))
                .toProperty()
                .toType()
                .assertProperty("lastSeen").withType("OffsetDateTime")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"))
                .toProperty().toType()
                .assertMethod("born", "LocalDate")
                .bodyContainsLines("this.born = born")
                .doesNotHaveComment();
    }

    @Test
    public void doAnnotateDatesOnModelParametersWithOptionalAndJsonNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_5436.yml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenApiNullable(true);
        codegen.setUseOptional(true);
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
        generator.setGenerateMetadata(false);
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
                        "value", "ZebrasApi.PATH_GET_ZEBRAS"
                ))
                .toMethod()
                .assertParameter("limit").hasType("Optional<BigDecimal>")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("Parameter", ImmutableMap.of("name", "\"limit\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("required", "false", "value", "\"limit\""))
                .toParameter()
                .toMethod()
                .assertParameter("animalParams").hasType("Optional<AnimalParams>")
                .toMethod()
                .commentContainsLines("GET /zebras", "@param limit  (optional)")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED)");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/AnimalParams.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertProperty("born").withType("Optional<LocalDate>")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"))
                .toProperty()
                .toType()
                .assertProperty("lastSeen").withType("Optional<OffsetDateTime>")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"))
                .toProperty().toType()
                .assertMethod("born", "LocalDate")
                .bodyContainsLines("this.born = Optional.ofNullable(born)")
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java"))
                .assertMethod("getElephants", "String", "BigDecimal")
                .assertParameter("userToken")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("CookieValue", ImmutableMap.of("name", "\"userToken\""));

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .assertMethod("getZebras", "String")
                .assertParameter("userToken")
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

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
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ElephantsApi.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertMethod("getElephants", "LocalDate")
                .assertParameter("startDate")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"));

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .assertMethod("getZebras", "OffsetDateTime")
                .assertParameter("startDateTime")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"));
    }

    @Test
    public void interfaceDefaultImplDisableWithResponseWrapper() {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(RESPONSE_WRAPPER, "aWrapper");
        codegen.processOpts();

        // jdk8 tag has been removed
        Assert.assertNull(codegen.additionalProperties().get("jdk8"));
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiGet", "String", "Format")
                .assertParameter("query")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"query\""))
                .toParameter().toMethod()
                .assertParameter("format")
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiGet", "OffsetDateTime")
                .assertParameter("start")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"start\""))
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"));
    }

    @Test
    public void testJavaClientCorrectConstructorOrderForRequiredFields_issue15825() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(properties)
                .setGeneratorName("spring")
                .setLibrary(SPRING_BOOT)
                .setInputSpec("src/test/resources/bugs/issue_constructor-required-values-with-multiple-inheritance.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        Map<String, File> files = generator.opts(clientOptInput).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("SubType.java"))
                .assertConstructor("TypeEnum", "SchemaVersion", "UUID", "Boolean", "Boolean", "SomeEnum")
                .bodyContainsLines("super(someBoolean, someEnum, schemaVersion, id, oneBoolean);",
                        "this.type = type;");
        JavaFileAssert.assertThat(files.get("IntermediateSubType.java"))
                .assertConstructor("Boolean", "SomeEnum", "SchemaVersion", "UUID", "Boolean")
                .bodyContainsLines("super(oneBoolean, schemaVersion, id);",
                        "this.someBoolean = someBoolean;",
                        "this.someEnum = someEnum");
        JavaFileAssert.assertThat(files.get("IntermediateType.java"))
                .assertConstructor("Boolean", "SchemaVersion", "UUID")
                .bodyContainsLines("super(schemaVersion, id);",
                        "this.oneBoolean = oneBoolean;");
        JavaFileAssert.assertThat(files.get("BaseType.java"))
                .assertConstructor("SchemaVersion", "UUID")
                .bodyContainsLines(
                        "this.schemaVersion = schemaVersion;",
                        "this.id = id;");
    }

    @Test
    public void springcloudWithAsyncAndJava8HasResponseWrapperCompletableFuture() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.ASYNC, "true");
        additionalProperties.put(CodegenConstants.LIBRARY, "spring-cloud");
        additionalProperties.put(CodegenConstants.MODEL_TESTS, "false");
        additionalProperties.put(CodegenConstants.MODEL_DOCS, "false");
        additionalProperties.put(CodegenConstants.APIS, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);

        assertFileContains(files.get("PetApi.java").toPath(), "CompletableFuture<ResponseEntity<Void>> deletePet");
        assertFileNotContains(files.get("PetApi.java").toPath(), "default CompletableFuture<ResponseEntity<Void>> deletePet");
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

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.TRUE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xyz.yyyyy.mmmmm.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xyz.yyyyy.aaaaa.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xyz.yyyyy.iiii.invoker");
        configAssert.assertValue(SpringCodegen.BASE_PACKAGE, "xyz.yyyyy.bbbb.base");
        configAssert.assertValue(SpringCodegen.CONFIG_PACKAGE, "xyz.yyyyy.cccc.config");
        configAssert.assertValue(SpringCodegen.TITLE, "someTest");
        configAssert.assertValue(SpringCodegen.SERVER_PORT, "8088");
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .fileContains("@RequestBody(required = false")
                .assertMethod("exampleApiPost", "ExampleApiPostRequest")
                .assertParameter("exampleApiPostRequest")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestBody", ImmutableMap.of("required", "false"));
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        openAPI.setInfo(new Info());
        codegen.preprocessOpenAPI(openAPI);

//        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
//        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
//        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.FALSE);
//        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
//        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "org.openapitools.model");
//        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
//        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "org.openapitools.api");
//        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
//        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "org.openapitools.api");
//        Assert.assertEquals(codegen.getBasePackage(), "org.openapitools");
//        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.BASE_PACKAGE), "org.openapitools");
        configAssert.assertValue(SpringCodegen.BASE_PACKAGE, "org.openapitools");
//        Assert.assertEquals(codegen.getConfigPackage(), "org.openapitools.configuration");
//        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.CONFIG_PACKAGE), "org.openapitools.configuration");
        configAssert.assertValue(SpringCodegen.CONFIG_PACKAGE, "org.openapitools.configuration");
//        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.SERVER_PORT), "8082");
        configAssert.assertValue(SpringCodegen.SERVER_PORT, "8082");
//        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING), false);
        configAssert.assertValue(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, false);
        configAssert.assertValue(SpringCodegen.USE_RESPONSE_ENTITY, true);
//        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.USE_RESPONSE_ENTITY), true);
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
                .assertParameter("files").hasType("List<MultipartFile>");

        // Check that the api handles the array
        JavaFileAssert.assertThat(files.get("MultipartArrayApi.java"))
                .assertMethod("multipartArray", "List<MultipartFile>")
                .assertParameter("files").hasType("List<MultipartFile>")
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
                .assertParameter("file").hasType("MultipartFile")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"One file\""))
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "false"));

        // Check that api validates mixed multipart request
        JavaFileAssert.assertThat(files.get("MultipartMixedApi.java"))
                .assertMethod("multipartMixed", "MultipartMixedStatus", "MultipartFile", "MultipartMixedRequestMarker", "List<MultipartMixedStatus>")
                .assertParameter("status").hasType("MultipartMixedStatus")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("value", "\"status\"", "required", "true"))
                .toParameter().toMethod()
                .assertParameter("file").hasType("MultipartFile")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "true"))
                .toParameter().toMethod()
                .assertParameter("marker").hasType("MultipartMixedRequestMarker")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"marker\"", "required", "false"))
                .toParameter().toMethod()
                .assertParameter("statusArray").hasType("List<MultipartMixedStatus>")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"statusArray\"", "required", "false"));
    }

    @Test
    public void testReactiveMultipartBoot() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-boot");
        codegen.setDelegatePattern(true);
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, "springfox");
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");

        // Check that the delegate handles the array
        JavaFileAssert.assertThat(files.get("MultipartArrayApiDelegate.java"))
            .assertMethod("multipartArray", "Flux<Part>", "ServerWebExchange")
            .assertParameter("files").hasType("Flux<Part>");

        // Check that the api handles the array
        JavaFileAssert.assertThat(files.get("MultipartArrayApi.java"))
            .assertMethod("multipartArray", "Flux<Part>", "ServerWebExchange")
            .assertParameter("files").hasType("Flux<Part>")
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
            .assertMethod("multipartSingle", "Part", "ServerWebExchange")
            .assertParameter("file").hasType("Part")
            .assertParameterAnnotations()
            .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"One file\""))
            .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "false"));

        // Check that api validates mixed multipart request
        JavaFileAssert.assertThat(files.get("MultipartMixedApi.java"))
            .assertMethod("multipartMixed", "MultipartMixedStatus", "Part", "MultipartMixedRequestMarker", "List<MultipartMixedStatus>", "ServerWebExchange")
            .assertParameter("status").hasType("MultipartMixedStatus")
            .assertParameterAnnotations()
            .containsWithName("Valid")
            .containsWithNameAndAttributes("ApiParam", ImmutableMap.of("value", "\"\""))
            .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"status\"", "required", "true"))
            .toParameter().toMethod()
            .assertParameter("file").hasType("Part")
            .assertParameterAnnotations()
            .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"file\"", "required", "true"))
            .toParameter().toMethod()
            .assertParameter("marker").hasType("MultipartMixedRequestMarker")
            .assertParameterAnnotations()
            .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"marker\"", "required", "false"))
            .toParameter().toMethod()
            .assertParameter("statusArray").hasType("List<MultipartMixedStatus>")
            .assertParameterAnnotations()
            .containsWithNameAndAttributes("RequestPart", ImmutableMap.of("value", "\"statusArray\"", "required", "false"));
    }

    @Test
    public void testAdditionalProperties_issue1466() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/spring/petstore-with-fake-endpoints-models-for-testing.yaml");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesAnyType.java"))
                .assertProperty("additionalProperties").withType("Map<String, Object>")
                .toType()
                .assertMethod("putAdditionalProperty", "String", "Object")
                .toFileAssert()
                .assertMethod("getAdditionalProperty", "String").hasReturnType("Object");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesArray.java"))
                .assertProperty("additionalProperties").withType("Map<String, List>")
                .toType()
                .assertMethod("putAdditionalProperty", "String", "List")
                .toFileAssert()
                .assertMethod("getAdditionalProperty", "String").hasReturnType("List");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesInteger.java"))
                .assertProperty("additionalProperties").withType("Map<String, Integer>")
                .toType()
                .assertMethod("putAdditionalProperty", "String", "Integer")
                .toFileAssert()
                .assertMethod("getAdditionalProperty", "String").hasReturnType("Integer");
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
        generator.setGenerateMetadata(false);
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

    @Test
    public void shouldApiNameSuffixForApiClassname() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_1/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_NAME_SUFFIX, "Controller");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");


        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetController.java"))
                .isInterface();

        File notExisting = files.get("PetApi.java");
        assertThat(notExisting).isNull();
    }

    @Test
    public void shouldUseTagsForClassname() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_15933.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_TAGS, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");


        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetTagApi.java"))
                .isInterface();

        File notExisting = files.get("PetApi.java");
        assertThat(notExisting).isNull();
    }

    @Test
    public void shouldNotUseTagsForClassname() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_15933.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_TAGS, "false");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");


        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .isInterface();

        File notExisting = files.get("PetTagApi.java");
        assertThat(notExisting).isNull();
    }

    @Test
    public void shouldAddValidAnnotationIntoCollectionWhenBeanValidationIsEnabled_issue14723() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14723.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ResponseTest.java"))
                .isNormalClass()
                .hasImports("javax.validation.Valid")
                .assertProperty("details")
                .withType("Map<String, Object>")
                .toType()
                .assertProperty("response")
                .withType("JsonNullable<Set<@Valid ResponseTest2>>")
                .toType()
                .assertProperty("nullableDtos")
                .withType("JsonNullable<Set<@Valid ResponseTest2>>")
                .toType()
                .assertProperty("dtos")
                .withType("Set<@Valid ResponseTest2>")
                .toType()
                .assertProperty("listNullableDtos")
                .withType("JsonNullable<List<@Valid ResponseTest2>>")
                .toType()
                .assertProperty("listDtos")
                .withType("List<@Valid ResponseTest2>")
                .toType()
                .assertProperty("nullableStrings")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .assertProperty("strings")
                .withType("Set<String>")
                .toType()
                .assertProperty("nullableInts")
                .withType("JsonNullable<Set<Integer>>")
                .toType()
                .assertProperty("ints")
                .withType("Set<Integer>");
    }

    @Test
    public void shouldAddValidAnnotationIntoCollectionWhenBeanValidationIsEnabled_issue17150() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/spring/issue_17150.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        // codegen.additionalProperties().put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.setUseSpringBoot3(true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .hasImports("jakarta.validation.Valid")
                .hasImports("jakarta.validation.constraints")
                .assertProperty("stringPattern")
                .withType("Set<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("List<@Min(1) Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("List<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()

                .assertProperty("stringPatternNullable")
                .withType("JsonNullable<Set<@Pattern(regexp = \"[a-z]\") String>>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<Set<@Size(min = 1, max = 10) String>>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("JsonNullable<List<@Size(min = 1) String>>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("JsonNullable<Set<@Size(max = 1) String>>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("JsonNullable<List<@Min(1) @Max(10) Integer>>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("JsonNullable<List<@Min(1) Integer>>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("JsonNullable<List<@Max(10) Integer>>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>>")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("JsonNullable<List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>>")
                .toType()
        ;
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
        generator.setGenerateMetadata(false); // skip metadata generation
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
        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .fileContains("@RequestMapping(\"${openapi.openAPIPetstore.base-path:/v2}\")",
                        "public static final String PATH_ADD_PET = \"/pet\";",
                        "value = PetApi.PATH_ADD_PET");

        // Check that the @RequestMapping annotation is not generated in the Controller file
        final File petApiControllerFile = files.get("PetApiController.java");
        assertFileNotContains(petApiControllerFile.toPath(), "@RequestMapping(\"${openapi.openAPIPetstore.base-path:/v2}\")");
    }

    @Test
    public void testNoRequestMappingAnnotation_spring_cloud_default() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-cloud");

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/2_0/petstore.yaml");

        // Check that the @RequestMapping annotation is not generated in the Api file
        final File petApiFile = files.get("PetApi.java");
        JavaFileAssert.assertThat(petApiFile).assertTypeAnnotations().hasSize(3).containsWithName("Validated")
                .containsWithName("Generated").containsWithName("Tag");

    }

    @Test
    public void testNoRequestMappingAnnotation() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary("spring-cloud");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, SpringCodegen.RequestMappingMode.none);

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/2_0/petstore.yaml");

        // Check that the @RequestMapping annotation is not generated in the Api file
        final File petApiFile = files.get("PetApi.java");
        JavaFileAssert.assertThat(petApiFile).assertTypeAnnotations().hasSize(3).containsWithName("Validated")
                .containsWithName("Generated").containsWithName("Tag");
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

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.TRUE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xx.yyyyyyyy.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xx.yyyyyyyy.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xx.yyyyyyyy.invoker");
        configAssert.assertValue(SpringCodegen.BASE_PACKAGE, codegen::getBasePackage, "xx.yyyyyyyy.base");
        configAssert.assertValue(SpringCodegen.CONFIG_PACKAGE, codegen::getConfigPackage, "xx.yyyyyyyy.config");
        configAssert.assertValue(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING, codegen::isUnhandledException, true);
    }

    @Test
    public void testGenerationOfClientPropertiesConfigurationForOAuth() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/spring/petstore-auth.yaml", null, new ParseOptions()).getOpenAPI();

        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.setHideGenerationTimestamp(true);
        codegen.setInterfaceOnly(false);
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);

        codegen.processOpts();

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
        generator.setGenerateMetadata(false); // skip metadata generation

        generator.opts(input).generate();

        Path filePath = Paths.get(output.getAbsolutePath(), "src/main/java/org/openapitools/configuration/ClientPropertiesConfiguration.java");


        assertFileContains(filePath,
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.registration.oAuth2AccessCode.redirect-uri\", \"set-oAuth2AccessCode-redirect-uri\" );",
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.registration.oAuth2AccessCode.authorization-grant-type\", \"authorization_code\" );",
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.registration.oAuth2AccessCode.client-id\", \"set-oAuth2AccessCode-client-id\" );",
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.registration.oAuth2AccessCode.scope\", \"openid,profile,aud\" );",
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.provider.oAuth2AccessCode.token-uri\", \"${tokenUrl}\" );",
                "oAuth2AccessCode.put(\"spring.security.oauth2.client.provider.oAuth2AccessCode.authorization-uri\", \"${authorizationUrl}\" );",


                "oAuth2Application.put(\"spring.security.oauth2.client.registration.oAuth2Application.client-id\", \"set-oAuth2Application-client-id\" );",
                "oAuth2Application.put(\"spring.security.oauth2.client.registration.oAuth2Application.authorization-grant-type\", \"client_credentials\" );",
                "oAuth2Application.put(\"spring.security.oauth2.client.provider.oAuth2Application.token-uri\", \"/openid-connect/token\" );"

        );

        assertFileNotContains(filePath, "spring.security.oauth2.client.registration.oAuth2Application.scope");
    }

    @Test
    public void useBeanValidationTruePerformBeanValidationFalseForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, false, "@javax.validation.constraints.Email", "@org.hibernate.validator.constraints.Email");
    }

    @Test
    public void useBeanValidationTruePerformBeanValidationTrueForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, true, "@javax.validation.constraints.Email", "@org.hibernate.validator.constraints.Email");
    }

    @Test
    public void useBeanValidationTruePerformBeanValidationFalseJakartaeeTrueForFormatEmail() throws IOException {
        beanValidationForFormatEmail(true, false, true, "@jakarta.validation.constraints.Email", "@javax.validation.constraints.Email");
    }

    // note: java8 option/mustache tag has been removed and default to true
    private void beanValidationForFormatEmail(boolean useBeanValidation, boolean performBeanValidation, String contains, String notContains) throws IOException {
        this.beanValidationForFormatEmail(useBeanValidation, performBeanValidation, false, contains, notContains);
    }

    private void beanValidationForFormatEmail(boolean useBeanValidation, boolean performBeanValidation, boolean useJakarta, String contains, String notContains) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(useBeanValidation);
        codegen.setPerformBeanValidation(performBeanValidation);
        codegen.setUseSpringBoot3(useJakarta);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("PersonWithEmail.java"));
        if (useBeanValidation)
            javaFileAssert.hasImports((useJakarta ? "jakarta" : "javax") + ".validation.constraints");
        if (performBeanValidation) javaFileAssert.hasImports("org.hibernate.validator.constraints");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PersonWithEmail.java"))
                .fileContains(contains)
                .fileDoesNotContain(notContains);
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate API files
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApi.java"))
                .fileContains("Mono<Map<String, DummyRequest>>")
                .fileDoesNotContain("Mono<DummyRequest>");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApiDelegate.java"))
                .fileContains("Mono<Map<String, DummyRequest>>")
                .fileDoesNotContain("Mono<DummyRequest>");
    }

    @Test
    public void reactiveArrayShouldBeWrappedInFluxWithoutMono() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.DELEGATE_PATTERN, "false");
        additionalProperties.put(SpringCodegen.REACTIVE, "true");
        additionalProperties.put(SpringCodegen.USE_RESPONSE_ENTITY, "false");
        additionalProperties.put(SpringCodegen.USE_SPRING_BOOT3, "true");
        additionalProperties.put(CodegenConstants.APIS, "true");
        Map<String, File> files = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert
                .assertThat(files.get("PetApi.java"))
                .assertMethod("addPet").hasReturnType("Mono<Pet>")
                .toFileAssert()
                .assertMethod("findPetsByStatus").hasReturnType("Flux<Pet>")
                .toFileAssert()
                .assertMethod("deletePet").hasReturnType("Mono<Void>");
    }

    @Test
    public void reactiveArrayShouldBeWrappedInMonoFluxWhenUsingResponseEntity() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.DELEGATE_PATTERN, "false");
        additionalProperties.put(SpringCodegen.REACTIVE, "true");
        additionalProperties.put(SpringCodegen.USE_RESPONSE_ENTITY, "true");
        additionalProperties.put(SpringCodegen.USE_SPRING_BOOT3, "true");
        additionalProperties.put(CodegenConstants.MODEL_TESTS, "false");
        additionalProperties.put(CodegenConstants.MODEL_DOCS, "false");
        additionalProperties.put(CodegenConstants.APIS, "true");
        additionalProperties.put(CodegenConstants.SUPPORTING_FILES, "false");
        Map<String, File> files = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert
                .assertThat(files.get("PetApi.java"))
                .assertMethod("addPet").hasReturnType("Mono<ResponseEntity<Pet>>")
                .toFileAssert()
                .assertMethod("findPetsByStatus").hasReturnType("Mono<ResponseEntity<Flux<Pet>>>")
                .toFileAssert()
                .assertMethod("deletePet").hasReturnType("Mono<ResponseEntity<Void>>");
    }

    @Test
    public void shouldGenerateValidCodeForReactiveControllerWithoutParams_issue14907() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_14907.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");
        codegen.additionalProperties().put(USE_TAGS, "true");
        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SKIP_DEFAULT_INTERFACE, "true");
        codegen.additionalProperties().put(IMPLICIT_HEADERS, "true");
        codegen.additionalProperties().put(OPENAPI_NULLABLE, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ConsentControllerApi.java"))
                .assertMethod("readAgreements", "ServerWebExchange");
    }

    @Test
    public void shouldGenerateValidCodeWithPaginated_reactive_issue15265() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_15265.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");
        codegen.additionalProperties().put(USE_TAGS, "true");
        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SKIP_DEFAULT_INTERFACE, "true");
        codegen.additionalProperties().put(IMPLICIT_HEADERS, "true");
        codegen.additionalProperties().put(OPENAPI_NULLABLE, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ConsentControllerApi.java"))
                .assertMethod("paginated", "ServerWebExchange", "Pageable")
                .toFileAssert()
                .assertMethod("paginatedWithParams", "String", "ServerWebExchange", "Pageable");
    }

    @Test
    public void shouldGenerateValidCodeWithPaginated_nonReactive_issue15265() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_15265.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(USE_TAGS, "true");
        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SKIP_DEFAULT_INTERFACE, "true");
        codegen.additionalProperties().put(IMPLICIT_HEADERS, "true");
        codegen.additionalProperties().put(OPENAPI_NULLABLE, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ConsentControllerApi.java"))
                .assertMethod("paginated", "Pageable")
                .toFileAssert()
                .assertMethod("paginatedWithParams", "String", "Pageable");
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate API
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ZebrasApi.java"))
                .fileContains("allowableValues = \"0, 1\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BearsApi.java"))
                .fileContains("allowableValues = \"sleeping, awake\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java"))
                .fileContains("allowableValues = \"sleeping, awake\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/GiraffesApi.java"))
                .fileContains("allowableValues = \"0, 1\"", "@PathVariable");
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate API
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"))
                .fileContains("status", "@NotNull");
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Dummy.java"))
                .fileContains("status")
                .fileDoesNotContain("@NotNull");
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
        //       codegen.setHateoas(true);
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setUseDeductionForOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.opts(input).generate();

        // test deduction
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"), "@JsonTypeInfo(use = JsonTypeInfo.Id.DEDUCTION)", "@JsonSubTypes.Type(value = Dog.class),", "@JsonSubTypes.Type(value = Cat.class)");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Foo.java"), "public class Foo extends Entity implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRef.java"), "public class FooRef extends EntityRef implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRefOrValue.java"), "public interface FooRefOrValue");
        // previous bugs
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/BarRef.java"))
                .fileDoesNotContain("atTypesuper.hashCode", "private String atBaseType");
        // imports for inherited properties
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PizzaSpeziale.java"), "import java.math.BigDecimal");
    }

    @Test
    public void testDiscriminatorWithMappingIssue14731() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14731.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");


        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setUseSpringBoot3(true);
        codegen.setModelNameSuffix("DTO");

        generator.opts(input).generate();

        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/ChildWithMappingADTO.java"), "@JsonTypeName");
        assertFileNotContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/ChildWithMappingBDTO.java"), "@JsonTypeName");
    }

    @Test
    public void testDiscriminatorWithoutMappingIssue14731() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14731.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");


        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setUseSpringBoot3(true);
        codegen.setModelNameSuffix("DTO");

        generator.opts(input).generate();


        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/ChildWithoutMappingADTO.java"), "@JsonTypeName");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/ChildWithoutMappingBDTO.java"), "@JsonTypeName");
    }

    @Test
    void testOneOfWithEnumDiscriminator() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/oneOfDiscriminator.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.opts(input).generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/java/org/openapitools/model/FruitOneOfEnumMappingDisc.java"),
                "public FruitTypeEnum getFruitType();"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/java/org/openapitools/model/AppleOneOfEnumMappingDisc.java"),
                "private FruitTypeEnum fruitType;",
                "public FruitTypeEnum getFruitType() {"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/java/org/openapitools/model/BananaOneOfEnumMappingDisc.java"),
                "private FruitTypeEnum fruitType;",
                "public FruitTypeEnum getFruitType() {"
        );
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate API
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate API
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetApi.java"))
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
                .assertParameter("headerNumber").hasType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .assertParameter("headerString").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("headerStringWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("headerStringQuotes").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("headerStringQuotesWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("headerBoolean").hasType("Boolean")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestHeader", ImmutableMap.of("defaultValue", "\"true\""));

        JavaFileAssert.assertThat(files.get("TestQueryParamsApi.java"))
                .assertMethod("queryParamsTest")
                .assertParameter("queryNumber").hasType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"11.2\""))
                .toParameter().toMethod()
                .assertParameter("queryString").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("queryStringWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("queryStringQuotes").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("queryStringQuotesWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("queryBoolean").hasType("Boolean")
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.opts(input).generate();

        TestUtils.assertExtraAnnotationFiles(outputPath + "/src/main/java/org/openapitools/model");

    }

    @Test
    public void testResponseWithArray_issue11897() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
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
    public void shouldGenerateMethodsWithoutUsingResponseEntityAndWithoutDelegation_issue11537() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        additionalProperties.put(USE_RESPONSE_ENTITY, "false");
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11537.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("MetadataApi.java"))
                .assertMethod("getSomething")
                .hasReturnType("List<String>")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes(
                        "ResponseStatus",
                        ImmutableMap.of("value", "HttpStatus.OK")
                )
                .toMethod()
                .toFileAssert()
                .assertMethod("putSomething")
                .hasReturnType("String")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes(
                        "ResponseStatus",
                        ImmutableMap.of("value", "HttpStatus.CREATED")
                );
    }

    @Test
    public void shouldGenerateMethodsWithoutUsingResponseEntityAndDelegation_issue11537() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        additionalProperties.put(USE_RESPONSE_ENTITY, "false");
        additionalProperties.put(DELEGATE_PATTERN, "true");
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11537.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("MetadataApiDelegate.java"))
                .assertMethod("getSomething").hasReturnType("List<String>")
                .toFileAssert()
                .assertMethod("putSomething").hasReturnType("String");

        JavaFileAssert.assertThat(files.get("MetadataApi.java"))
                .assertMethod("getSomething")
                .hasReturnType("List<String>")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes(
                        "ResponseStatus",
                        ImmutableMap.of("value", "HttpStatus.OK")
                )
                .toMethod()
                .toFileAssert()
                .assertMethod("putSomething")
                .hasReturnType("String")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes(
                        "ResponseStatus",
                        ImmutableMap.of("value", "HttpStatus.CREATED")
                );
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
                .assertParameter("pageable").hasType("Pageable")
                .assertParameterAnnotations()
                .containsWithName("ParameterObject");


        // different import for SB3
        additionalProperties.put(USE_SPRING_BOOT3, "true");
        files = generateFromContract("src/test/resources/2_0/petstore-with-spring-pageable.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .hasImports("org.springdoc.core.annotations.ParameterObject", "org.springframework.data.domain.Pageable")
                .assertMethod("findPetsByStatus")
                .assertParameter("pageable").hasType("Pageable")
                .assertParameterAnnotations()
                .containsWithName("ParameterObject");
    }

    @Test
    public void paramPageableIsNotSpringPaginated_issue13052() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(USE_SPRING_BOOT3, "true");

        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_13052.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .hasImports("org.openapitools.model.Pageable")
                .hasNoImports("org.springframework.data.domain.Pageable", "org.springdoc.core.annotations.ParameterObject")
                .assertMethod("findPageable")
                .assertParameter("pageable").hasType("Pageable");
    }

    @DataProvider(name = "sealedScenarios")
    public static Object[][] sealedScenarios() {
        return new Object[][]{
                {"oneof_polymorphism_and_inheritance.yaml", Map.of(
                        "Foo.java", "public final class Foo extends Entity implements FooRefOrValue",
                        "FooRef.java", "public final class FooRef extends EntityRef implements FooRefOrValue",
                        "FooRefOrValue.java", "public sealed interface FooRefOrValue permits Foo, FooRef ",
                        "Entity.java", "public sealed class Entity extends RepresentationModel<Entity> permits Bar, BarCreate, Foo, Pasta, Pizza {")},
                {"oneOf_additionalProperties.yaml", Map.of(
                        "SchemaA.java", "public final class SchemaA extends RepresentationModel<SchemaA>  implements PostRequest {",
                        "PostRequest.java", "public sealed interface PostRequest permits SchemaA {")},
                {"oneOf_array.yaml", Map.of(
                        "MyExampleGet200Response.java", "public sealed interface MyExampleGet200Response")},
                {"oneOf_duplicateArray.yaml", Map.of(
                        "Example.java", "public interface Example  {")},
                {"oneOf_nonPrimitive.yaml", Map.of(
                        "Example.java", "public interface Example  {")},
                {"oneOf_primitive.yaml", Map.of(
                        "Child.java", "public final class Child extends RepresentationModel<Child>  implements Example {",
                        "Example.java", "public sealed interface Example permits Child {")},
                {"oneOf_primitiveAndArray.yaml", Map.of(
                        "Example.java", "public interface Example  {")},
                {"oneOf_reuseRef.yaml", Map.of(
                        "Fruit.java", "public sealed interface Fruit permits Apple, Banana {",
                        "Banana.java", "public final class Banana extends RepresentationModel<Banana>  implements Fruit {",
                        "Apple.java", "public final class Apple extends RepresentationModel<Apple>  implements Fruit {")},
                {"oneOf_twoPrimitives.yaml", Map.of(
                        "MyExamplePostRequest.java", "public interface MyExamplePostRequest {")},
                {"oneOfArrayMapImport.yaml", Map.of(
                        "Fruit.java", "public interface Fruit  {",
                        "Grape.java", "public final class Grape extends RepresentationModel<Grape>  {",
                        "Apple.java", "public final class Apple extends RepresentationModel<Apple>  {")},
                {"oneOfDiscriminator.yaml", Map.of(
                        "FruitAllOfDisc.java", "public sealed interface FruitAllOfDisc permits AppleAllOfDisc, BananaAllOfDisc {",
                        "AppleAllOfDisc.java", "public final class AppleAllOfDisc extends RepresentationModel<AppleAllOfDisc>  implements FruitAllOfDisc {",
                        "BananaAllOfDisc.java", "public final class BananaAllOfDisc extends RepresentationModel<BananaAllOfDisc>  implements FruitAllOfDisc {",
                        "FruitReqDisc.java", "public sealed interface FruitReqDisc permits AppleReqDisc, BananaReqDisc {",
                        "AppleReqDisc.java", "public final class AppleReqDisc extends RepresentationModel<AppleReqDisc>  implements FruitReqDisc {",
                        "BananaReqDisc.java", "public final class BananaReqDisc extends RepresentationModel<BananaReqDisc>  implements FruitReqDisc {")}
        };
    }

    @Test(dataProvider = "sealedScenarios", description = "sealed scenarios")
    public void sealedScenarios(String apiFile, Map<String, String> definitions) {
        Path output = newTempFolder();
        String outputPath = output.toString().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/" + apiFile, null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(outputPath);
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);
        codegen.setUseSealed(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        codegen.setHateoas(true);
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        codegen.setLegacyDiscriminatorBehavior(false);

        generator.opts(input).generate();

        definitions.forEach((file, check) ->
                assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/" + file), check));
    }

    @Test
    public void shouldSetDefaultValueForMultipleArrayItems() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.INTERFACE_ONLY, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");

        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_11957.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("SearchApi.java"))
                .assertMethod("defaultList")
                .assertParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"updatedAt:DESC,createdAt:DESC\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("defaultSet")
                .assertParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"updatedAt:DESC,createdAt:DESC\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("emptyDefaultList")
                .assertParameter("orderBy")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("defaultValue", "\"\""))
                .toParameter().toMethod().toFileAssert()
                .assertMethod("emptyDefaultSet")
                .assertParameter("orderBy")
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
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");
        codegen.additionalProperties().put(BeanValidationFeatures.USE_BEANVALIDATION, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("SomeMethodApi.java"))
                .assertMethod("methodWithValidation")
                .assertParameter("headerOne")
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
                .assertParameter("headerTwo")
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
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        //Assert that NotNull annotation exists alone with no other BeanValidation annotations
        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"));
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
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        //Assert that NotNull annotation exists alone with no other BeanValidation annotations
        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"));
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
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Alien.java"));
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
                .hasImports("com.fasterxml.jackson.annotation.JsonFormat")
                .assertMethod("getMyPropTypeNumber")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("JsonFormat", ImmutableMap.of(
                        "shape", "JsonFormat.Shape.STRING"
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
        codegen.additionalProperties().put(SpringCodegen.USE_TAGS, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13365.yml");

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("Person.java"));
        javaFileAssert.assertMethod("getName").assertMethodAnnotations()
                .containsWithName("NotNull").containsWithName("Size").containsWithName("javax.validation.constraints.Email");
        javaFileAssert
                .hasNoImports("javax.validation.constraints.NotNull")
                .hasImports("javax.validation.constraints");
    }

    @Test
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
                .assertMethod("equals")
                .bodyContainsLines("return Arrays.equals(this.picture, testObject.picture);");
    }

    @Test
    public void useBeanValidationGenerateAnnotationsForRequestBody_issue13932() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13932.yml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("AddApi.java"))
                .assertMethod("addPost")
                .assertParameter("body")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Min", ImmutableMap.of("value", "2"));
    }

    @Test
    public void useBeanValidationGenerateAnnotationsForFormsRequestBody() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/spring/form-requestbody-params-with-constraints.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("AddApi.java"))
                .assertMethod("addPost")
                .assertParameter("name")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of("regexp", "\"^[[:print:]]+$\""))
                .toParameter()
                .toMethod()
                .assertParameter("quantity")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Min", ImmutableMap.of("value", "1"));
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
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
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
    public void contractWithoutEnumDoesNotContainEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/generic.yaml", SPRING_BOOT);

        assertThat(output).doesNotContainKey("EnumConverterConfiguration.java");
    }

    @Test
    public void contractWithEnumContainsEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/enum.yaml", SPRING_BOOT);

        JavaFileAssert.assertThat(output.get("EnumConverterConfiguration.java"))
                .assertMethod("typeConverter");
    }

    @Test
    public void contractWithResolvedInnerEnumContainsEnumConverter() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("spring")
                .setInputSpec("src/test/resources/3_0/inner_enum.yaml")
                .addInlineSchemaOption("RESOLVE_INLINE_ENUMS", "true")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        Map<String, File> files = generator.opts(clientOptInput).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("EnumConverterConfiguration.java"))
                .assertMethod("ponyTypeConverter");
    }

    @Test
    public void shouldUseTheSameTagNameForTheInterfaceAndTheMethod_issue11570() throws IOException {
        final Map<String, File> output = generateFromContract(
                "src/test/resources/bugs/issue_11570.yml", SPRING_BOOT, Map.of(INTERFACE_ONLY, "true")
        );

        final String expectedTagName = "\"personTagWithExclamation!\"";
        final String expectedTagDescription = "\"the personTagWithExclamation! API\"";

        final String interfaceTag = "@Tag(name = " + expectedTagName + ", description = " + expectedTagDescription + ")";
        final String methodTag = "tags = { " + expectedTagName + " }";
        assertFileContains(output.get("PersonApi.java").toPath(), interfaceTag, methodTag);
    }

    @Test
    public void shouldGenerateConstructorWithOnlyRequiredParameters() throws IOException {
        final Map<String, File> output = generateFromContract(
                "src/test/resources/3_0/spring/issue_9789.yml",
                SPRING_BOOT,
                Map.of(GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, "false")
        );

        JavaFileAssert.assertThat(output.get("ObjectWithNoRequiredParameter.java")).hasNoConstructor("String");

        JavaFileAssert.assertThat(output.get("ObjectWithRequiredParameter.java")).assertConstructor();
        JavaFileAssert.assertThat(output.get("ObjectWithRequiredParameter.java")).assertConstructor("String", "String")
                .hasParameter("param2").toConstructor()
                .hasParameter("param3");

        JavaFileAssert.assertThat(output.get("ObjectWithInheritedRequiredParameter.java")).assertConstructor();
        JavaFileAssert.assertThat(output.get("ObjectWithInheritedRequiredParameter.java")).assertConstructor("Integer", "String", "String")
                .hasParameter("param2").toConstructor()
                .hasParameter("param3").toConstructor()
                .hasParameter("param6").toConstructor()
                .bodyContainsLines("super(param2, param3)", "this.param6 = param6");
    }

    private Map<String, File> generateFromContract(String url, String library) throws IOException {
        return generateFromContract(url, library, new HashMap<>());
    }

    private Map<String, File> generateFromContract(String url, String library, Map<String, Object> additionalProperties) throws IOException {
        return generateFromContract(url, library, additionalProperties, codegen -> {
        });
    }

    /**
     * Generate the contract with additional configuration.
     * <p>
     * use CodegenConfigurator instead of CodegenConfig for easier configuration like in JavaClientCodeGenTest
     */
    private Map<String, File> generateFromContract(String url, String library, Map<String, Object> additionalProperties,
                                                   Consumer<CodegenConfigurator> consumer) throws IOException {

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("spring")
                .setAdditionalProperties(additionalProperties)
                .setValidateSpec(false)
                .setInputSpec(url)
                .setOutputDir(output.getAbsolutePath());
        if (null != library) {
            configurator.setLibrary(library);
        }
        consumer.accept(configurator);

        ClientOptInput input = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        return generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));
    }

    @Test
    public void testMappingSubtypesIssue13150() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13150.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);
        codegen.setHateoas(true);
        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        generator.opts(input).generate();

        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = Foo.class, name = \"foo\")\n" +
                "})";
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Parent.java"), jsonSubType);
    }

    @Test
    public void shouldGenerateJsonPropertyAnnotationLocatedInGetters_issue5705() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/spring/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setWithXml(true);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ResponseObjectWithDifferentFieldNames.java"))
                .assertProperty("normalPropertyName")
                .assertPropertyAnnotations()
                .doesNotContainWithName("JsonProperty")
                .doesNotContainWithName("JacksonXmlProperty")
                .toProperty().toType()
                .assertProperty("UPPER_CASE_PROPERTY_SNAKE")
                .assertPropertyAnnotations()
                .doesNotContainWithName("JsonProperty")
                .doesNotContainWithName("JacksonXmlProperty")
                .toProperty().toType()
                .assertProperty("lowerCasePropertyDashes")
                .assertPropertyAnnotations()
                .doesNotContainWithName("JsonProperty")
                .doesNotContainWithName("JacksonXmlProperty")
                .toProperty().toType()
                .assertProperty("propertyNameWithSpaces")
                .assertPropertyAnnotations()
                .doesNotContainWithName("JsonProperty")
                .doesNotContainWithName("JacksonXmlProperty")
                .toProperty().toType()
                .assertMethod("getNormalPropertyName")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"normalPropertyName\""))
                .containsWithNameAndAttributes("JacksonXmlProperty", ImmutableMap.of("localName", "\"normalPropertyName\""))
                .toMethod().toFileAssert()
                .assertMethod("getUPPERCASEPROPERTYSNAKE")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"UPPER_CASE_PROPERTY_SNAKE\""))
                .containsWithNameAndAttributes("JacksonXmlProperty", ImmutableMap.of("localName", "\"UPPER_CASE_PROPERTY_SNAKE\""))
                .toMethod().toFileAssert()
                .assertMethod("getLowerCasePropertyDashes")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"lower-case-property-dashes\""))
                .containsWithNameAndAttributes("JacksonXmlProperty", ImmutableMap.of("localName", "\"lower-case-property-dashes\""))
                .toMethod().toFileAssert()
                .assertMethod("getPropertyNameWithSpaces")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"property name with spaces\""))
                .containsWithNameAndAttributes("JacksonXmlProperty", ImmutableMap.of("localName", "\"property name with spaces\""));
    }

    @Test
    public void testReturnTypeVoidWithResponseEntity_issue12341() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12341.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "true");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest", "ObjTest")
                .hasReturnType("ResponseEntity<ObjTest>")
                .bodyContainsLines("return postToTest(objToTest);");
        javaFileAssert
                .assertMethod("postToTest", "ObjTest")
                .hasReturnType("ResponseEntity<ObjTest>")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);");
        javaFileAssert
                .assertMethod("_putToTest", "ObjTest")
                .hasReturnType("ResponseEntity<Void>")
                .bodyContainsLines("return putToTest(objToTest);");
        javaFileAssert
                .assertMethod("putToTest", "ObjTest")
                .hasReturnType("ResponseEntity<Void>")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);");
    }

    @Test
    public void testReturnTypeVoidWithoutResponseEntityWithDelegate_issue12341() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12341.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest", "ObjTest")
                .hasReturnType("ObjTest")
                .bodyContainsLines("return postToTest(objToTest);");
        javaFileAssert
                .assertMethod("postToTest", "ObjTest")
                .hasReturnType("ObjTest")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
        javaFileAssert
                .assertMethod("_putToTest", "ObjTest")
                .hasReturnType("void")
                .bodyContainsLines("putToTest(objToTest);")
                .bodyNotContainsLines("return putToTest(objToTest);");
        javaFileAssert
                .assertMethod("putToTest", "ObjTest")
                .hasReturnType("void")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
    }

    @Test
    public void testReturnTypeVoidWithoutResponseEntityWithoutDelegateWithAsync_issue12341() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12341.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "false");
        codegen.additionalProperties().put(ASYNC, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("postToTest", "ObjTest")
                .hasReturnType("CompletableFuture<ObjTest>")
                .bodyContainsLines("return CompletableFuture.supplyAsync(()-> {")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
        javaFileAssert
                .assertMethod("putToTest", "ObjTest")
                .hasReturnType("CompletableFuture<Void>")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
    }

    @Test
    public void testReturnTypeVoidWithoutResponseEntityWithoutDelegateWithoutAsync_issue12341() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12341.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "false");
        codegen.additionalProperties().put(ASYNC, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("postToTest", "ObjTest")
                .hasReturnType("ObjTest")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
        javaFileAssert
                .assertMethod("putToTest", "ObjTest")
                .hasReturnType("void")
                .bodyContainsLines("throw new IllegalArgumentException(\"Not implemented\");");
    }

    @Test
    public void testHasRestControllerDoesNotHaveController_issue15264() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue15264.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .isInterface()
                .hasImports("org.springframework.web.bind.annotation.RestController")
                .hasNoImports("org.springframework.stereotype.Controller")
                .assertTypeAnnotations()
                .containsWithName("RestController")
                .doesNotContainWithName("Controller");
    }

    @Test
    public void testDoesNotHasRestControllerHaveController_issue15264() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue15264.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "true");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .isInterface()
                .hasImports("org.springframework.stereotype.Controller")
                .hasNoImports("org.springframework.web.bind.annotation.RestController")
                .assertTypeAnnotations()
                .containsWithName("Controller")
                .doesNotContainWithName("RestController");
    }

    @Test
    public void testXPatternMessage_issue5857() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue5857.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("ObjTest.java"));
        javaFileAssert
                .assertMethod("getField2")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of(
                        "regexp", "\"\\\\w\"",
                        "message", "\"Only letters, numbers and underscore\""
                ));
        javaFileAssert
                .assertMethod("getField3")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of(
                        "regexp", "\"\\\\w\""
                ));
    }

    @Test
    public void testXPatternMessage_issue18959() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_18959.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(PERFORM_BEANVALIDATION, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest")
                .assertParameter("groupObj")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of(
                        "regexp", "\"[a-zA-Z]\"",
                        "message", "\"Only letters\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of(
                        "regexp", "\"[0-9a-fA-F]\"",
                        "message", "\"Only numbers and letters a-f\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientId")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Pattern", ImmutableMap.of(
                        "regexp", "\"\\\\d\"",
                        "message", "\"Only numbers\""
                ));
    }

    @Test
    public void testEnumCaseInsensitive_issue8084() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue8084.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(USE_ENUM_CASE_INSENSITIVE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("EnumTest.java"));
        javaFileAssert
                .assertMethod("fromValue")
                .bodyContainsLines("if (b.value.equalsIgnoreCase(value)) {");
    }

    @Test
    public void testEnumCaseSensitive_issue8084() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue8084.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(USE_ENUM_CASE_INSENSITIVE, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("EnumTest.java"));
        javaFileAssert
                .assertMethod("fromValue")
                .bodyContainsLines("if (b.value.equals(value)) {");
    }

    @Test
    public void testHasOperationParameterExtraAnnotation_issue18224() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_18224.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest")
                .assertParameter("groupObj")
                .assertParameterAnnotations()
                .containsWithName("com.test.MyAnnotationInPath")
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotations()
                .containsWithName("com.test.MyAnnotationInQuery")
                .toParameter()
                .toMethod()
                .assertParameter("clientId")
                .assertParameterAnnotations()
                .containsWithName("com.test.MyAnnotationInHeader");
    }

    @Test
    public void testHasOperationExtraAnnotation_issue15822() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue15822.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest")
                .assertMethodAnnotations()
                .containsWithName("javax.annotation.security.RolesAllowed");
    }

    @Test
    public void testHasOperationExtraAnnotation_issue12219() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12219.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest")
                .assertMethodAnnotations()
                .containsWithName("javax.annotation.security.RolesAllowed")
                .containsWithName("org.springframework.security.access.annotation.Secured")
                .containsWithName("org.springframework.security.access.prepost.PreAuthorize");
    }

    @Test
    public void testHasOperationExtraAnnotation_issue12219_array() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue12219_array.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SpringCodegen.DATE_LIBRARY, "java8-localdatetime");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApi.java"));
        javaFileAssert
                .assertMethod("_postToTest")
                .assertMethodAnnotations()
                .containsWithName("javax.annotation.security.RolesAllowed")
                .containsWithName("org.springframework.security.access.annotation.Secured")
                .containsWithName("org.springframework.security.access.prepost.PreAuthorize");
    }

    @Test
    public void doCallFluentParentSettersFromChildModel() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_16496.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenApiNullable(true);
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.opts(input).generate();


        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"))
                // Fluent method assertions
                .assertMethod("alias")
                .hasReturnType("Animal")
                .bodyContainsLines("this.alias = JsonNullable.of(alias);", "return this;")
                .assertParameter("alias")
                .hasType("String")
                .toMethod()
                .toFileAssert()

                // Setter method assertions
                .assertMethod("setAlias")
                .hasReturnType("void")
                .assertParameter("alias")
                .hasType("JsonNullable<String>");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Zebra.java"))
                // Fluent method assertions
                .assertMethod("alias")
                .hasReturnType("Zebra")
                .bodyContainsLines("super.alias(alias);", "return this;")
                .assertParameter("alias")
                .hasType("String")
                .toMethod()
                .toFileAssert()

                // No overridden setter on child object
                .hasNoMethod("setAlias");
    }

    @Test
    public void testModelsWithNoneOptionalAndJsonNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_14765.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenApiNullable(true);
        codegen.setUseOptional(false);
        codegen.setUseSpringBoot3(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.opts(input).generate();


        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"))
                .hasImports("jakarta.validation.Valid")
                .hasImports("jakarta.validation.constraints")

                .assertProperty("name")
                .withType("String")
                .toType()
                .assertProperty("age")
                .withType("JsonNullable<Integer>")
                .toType()
                .assertProperty("alias")
                .withType("JsonNullable<String>")
                .toType()
                .assertProperty("color")
                .withType("String")
                .toType()
                .assertProperty("names")
                .withType("List<String>")
                .toType()
                .assertProperty("colors")
                .withType("JsonNullable<List<String>>")
                .toType()
                .assertProperty("stringPattern")
                .withType("String")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("String")
                .toType()
                .assertProperty("stringEmail")
                .withType("String")
                .toType()
                .assertProperty("intMinMax")
                .withType("Integer")
                .toType()
                .assertProperty("intMin")
                .withType("Integer")
                .toType()
                .assertProperty("intMax")
                .withType("Integer")
                .toType()
                .assertProperty("numberMinMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMin")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("stringDefault")
                .withType("String")
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .assertProperty("zebra")
                .withType("Zebra")
                .toType()

                .assertProperty("stringPatternNullable")
                .withType("JsonNullable<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("JsonNullable<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("JsonNullable<@Size(max = 1) String>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("JsonNullable<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("JsonNullable<@Min(1) Integer>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("JsonNullable<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("JsonNullable<@DecimalMin(\"1\") BigDecimal>")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("JsonNullable<@DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("stringDefaultNullable")
                .withType("JsonNullable<@Size(max = 1) String>")
                .toType()
                .fileContains("stringDefaultNullable = JsonNullable.<String>undefined();")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getName")
                .hasReturnType("String")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("colors")
                .hasReturnType("Animal")
                .bodyContainsLines("this.colors = JsonNullable.of(colors);", "return this;")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .assertParameter("colors")
                .hasType("JsonNullable<List<String>>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getColors")
                .hasReturnType("JsonNullable<List<String>>")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("names")
                .hasReturnType("Animal")
                .bodyContainsLines("this.names = names;", "return this;")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getNames")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert();

        assertJsonNullableMethod(javaFileAssert, Integer.class, "age", "JsonNullable<Integer>");
        assertJsonNullableMethod(javaFileAssert, String.class, "alias", "JsonNullable<String>");
        assertMethod(javaFileAssert, String.class, "color");

        assertMethod(javaFileAssert, String.class, "stringPattern");
        assertMethod(javaFileAssert, String.class, "stringMaxMinLength");
        assertMethod(javaFileAssert, String.class, "stringMinLength");
        assertMethod(javaFileAssert, String.class, "stringMaxLength");
        assertMethod(javaFileAssert, String.class, "stringEmail");
        assertMethod(javaFileAssert, Integer.class, "intMinMax");
        assertMethod(javaFileAssert, Integer.class, "intMin");
        assertMethod(javaFileAssert, Integer.class, "intMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMin");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMax");
        assertMethod(javaFileAssert, "Zebra", "zebra");

        assertJsonNullableMethod(javaFileAssert, String.class, "stringPatternNullable", "JsonNullable<@Pattern(regexp = \"[a-z]\") String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMaxMinLengthNullable", "JsonNullable<@Size(min = 1, max = 10) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMinLengthNullable", "JsonNullable<@Size(min = 1) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMaxLengthNullable", "JsonNullable<@Size(max = 1) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringEmailNullable", "JsonNullable<@jakarta.validation.constraints.Email String>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMinMaxNullable", "JsonNullable<@Min(1) @Max(10) Integer>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMinNullable", "JsonNullable<@Min(1) Integer>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMaxNullable", "JsonNullable<@Max(10) Integer>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMinMaxNullable", "JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMinNullable", "JsonNullable<@DecimalMin(\"1\") BigDecimal>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMaxNullable", "JsonNullable<@DecimalMax(\"10\") BigDecimal>");

    }

    @Test
    public void testModelsWithOptionalAndJsonNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_14765.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenApiNullable(true);
        codegen.setUseOptional(true);
        codegen.setUseSpringBoot3(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.opts(input).generate();


        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"))
                .hasImports("jakarta.validation.Valid")
                .hasImports("jakarta.validation.constraints")

                .assertProperty("name")
                .withType("String")
                .toType()
                .assertProperty("age")
                .withType("JsonNullable<Integer>")
                .toType()
                .assertProperty("alias")
                .withType("JsonNullable<String>")
                .toType()
                .assertProperty("color")
                .withType("Optional<String>")
                .toType()
                .assertProperty("names")
                .withType("List<String>")
                .toType()
                .assertProperty("colors")
                .withType("JsonNullable<List<String>>")
                .toType()
                .assertProperty("stringPattern")
                .withType("Optional<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Optional<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("Optional<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Optional<@Size(max = 1) String>")
                .toType()
                .assertProperty("stringEmail")
                .withType("Optional<@jakarta.validation.constraints.Email String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("Optional<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("Optional<@Min(1) Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("Optional<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("Optional<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("Optional<@DecimalMin(\"1\") BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("Optional<@DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("stringDefault")
                .withType("Optional<@Size(max = 1) String>")
                .toType()
                .fileContains("stringDefault = Optional.of(\"ABC\")")
                .assertProperty("zebra")
                .withType("Optional<Zebra>")
                .toType()

                .assertProperty("stringPatternNullable")
                .withType("JsonNullable<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("JsonNullable<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("JsonNullable<@Size(max = 1) String>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("JsonNullable<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("JsonNullable<@Min(1) Integer>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("JsonNullable<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("JsonNullable<@DecimalMin(\"1\") BigDecimal>")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("JsonNullable<@DecimalMax(\"10\") BigDecimal>")
                .toType()
                .assertProperty("stringDefaultNullable")
                .withType("JsonNullable<@Size(max = 1) String>")
                .toType()
                .fileContains("stringDefaultNullable = JsonNullable.<String>undefined();")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getName")
                .hasReturnType("String")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("colors")
                .hasReturnType("Animal")
                .bodyContainsLines("this.colors = JsonNullable.of(colors);", "return this;")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .assertParameter("colors")
                .hasType("JsonNullable<List<String>>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getColors")
                .hasReturnType("JsonNullable<List<String>>")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("names")
                .hasReturnType("Animal")
                .bodyContainsLines("this.names = names;", "return this;")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getNames")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert();

        assertJsonNullableMethod(javaFileAssert, String.class, "alias", "JsonNullable<String>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "age", "JsonNullable<Integer>");
        assertOptionalMethod(javaFileAssert, String.class, "color", "Optional<String>");

        assertOptionalMethod(javaFileAssert, String.class, "stringPattern", "Optional<@Pattern(regexp = \"[a-z]\") String>");
        assertOptionalMethod(javaFileAssert, String.class, "stringMaxMinLength", "Optional<@Size(min = 1, max = 10) String>");
        assertOptionalMethod(javaFileAssert, String.class, "stringMinLength", "Optional<@Size(min = 1) String>");
        assertOptionalMethod(javaFileAssert, String.class, "stringMaxLength", "Optional<@Size(max = 1) String>");
        assertOptionalMethod(javaFileAssert, String.class, "stringEmail", "Optional<@jakarta.validation.constraints.Email String>");
        assertOptionalMethod(javaFileAssert, Integer.class, "intMinMax", "Optional<@Min(1) @Max(10) Integer>");
        assertOptionalMethod(javaFileAssert, Integer.class, "intMin", "Optional<@Min(1) Integer>");
        assertOptionalMethod(javaFileAssert, Integer.class, "intMax", "Optional<@Max(10) Integer>");
        assertOptionalMethod(javaFileAssert, BigDecimal.class, "numberMinMax", "Optional<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>");
        assertOptionalMethod(javaFileAssert, BigDecimal.class, "numberMin", "Optional<@DecimalMin(\"1\") BigDecimal>");
        assertOptionalMethod(javaFileAssert, BigDecimal.class, "numberMax", "Optional<@DecimalMax(\"10\") BigDecimal>");
        assertOptionalMethod(javaFileAssert, "Zebra", "zebra", "Optional<Zebra>");

        assertJsonNullableMethod(javaFileAssert, String.class, "stringPatternNullable", "JsonNullable<@Pattern(regexp = \"[a-z]\") String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMaxMinLengthNullable", "JsonNullable<@Size(min = 1, max = 10) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMinLengthNullable", "JsonNullable<@Size(min = 1) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringMaxLengthNullable", "JsonNullable<@Size(max = 1) String>");
        assertJsonNullableMethod(javaFileAssert, String.class, "stringEmailNullable", "JsonNullable<@jakarta.validation.constraints.Email String>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMinMaxNullable", "JsonNullable<@Min(1) @Max(10) Integer>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMinNullable", "JsonNullable<@Min(1) Integer>");
        assertJsonNullableMethod(javaFileAssert, Integer.class, "intMaxNullable", "JsonNullable<@Max(10) Integer>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMinMaxNullable", "JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMinNullable", "JsonNullable<@DecimalMin(\"1\") BigDecimal>");
        assertJsonNullableMethod(javaFileAssert, BigDecimal.class, "numberMaxNullable", "JsonNullable<@DecimalMax(\"10\") BigDecimal>");

    }

    @Test
    public void testModelsWithOptionalAndNoneJsonNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_14765.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenApiNullable(false);
        codegen.setUseOptional(true);
        codegen.setUseSpringBoot3(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.opts(input).generate();


        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"))
                .hasImports("jakarta.validation.Valid")
                .hasImports("jakarta.validation.constraints")

                .assertProperty("name")
                .withType("String")
                .toType()
                .assertProperty("age")
                .withType("Integer")
                .toType()
                .assertProperty("alias")
                .withType("String")
                .toType()
                .assertProperty("color")
                .withType("String")
                .toType()
                .assertProperty("names")
                .withType("List<String>")
                .toType()
                .assertProperty("colors")
                .withType("List<String>")
                .toType()
                .assertProperty("stringPattern")
                .withType("String")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("String")
                .toType()
                .assertProperty("stringEmail")
                .withType("String")
                .toType()
                .assertProperty("intMinMax")
                .withType("Integer")
                .toType()
                .assertProperty("intMin")
                .withType("Integer")
                .toType()
                .assertProperty("intMax")
                .withType("Integer")
                .toType()
                .assertProperty("numberMinMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMin")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("stringDefault")
                .withType("String")
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .assertProperty("zebra")
                .withType("Zebra")
                .toType()

                .assertProperty("stringPatternNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("Integer")
                .toType()
                .assertProperty("intMinNullable")
                .withType("Integer")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("Integer")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("stringDefaultNullable")
                .withType("String")
                .toType()
                .fileContains("stringDefaultNullable = null;")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getName")
                .hasReturnType("String")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("age")
                .hasReturnType("Animal")
                .bodyContainsLines("this.age = age;", "return this;")
                .assertParameter("age")
                .hasType("Integer")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setAge")
                .hasReturnType("void")
                .assertParameter("age")
                .hasType("Integer")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getAge")
                .hasReturnType("Integer")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("colors")
                .hasReturnType("Animal")
                .bodyContainsLines("this.colors = colors;", "return this;")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getColors")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("names")
                .hasReturnType("Animal")
                .bodyContainsLines("this.names = names;", "return this;")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getNames")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert();

        assertMethod(javaFileAssert, String.class, "alias");
        assertMethod(javaFileAssert, String.class, "color");

        assertMethod(javaFileAssert, String.class, "stringPattern");
        assertMethod(javaFileAssert, String.class, "stringMaxMinLength");
        assertMethod(javaFileAssert, String.class, "stringMinLength");
        assertMethod(javaFileAssert, String.class, "stringMaxLength");
        assertMethod(javaFileAssert, String.class, "stringEmail");
        assertMethod(javaFileAssert, Integer.class, "intMinMax");
        assertMethod(javaFileAssert, Integer.class, "intMin");
        assertMethod(javaFileAssert, Integer.class, "intMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMin");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMax");
        assertMethod(javaFileAssert, "Zebra", "zebra");

        assertMethod(javaFileAssert, String.class, "stringPatternNullable");
        assertMethod(javaFileAssert, String.class, "stringMaxMinLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringMinLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringMaxLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringEmailNullable");
        assertMethod(javaFileAssert, Integer.class, "intMinMaxNullable");
        assertMethod(javaFileAssert, Integer.class, "intMinNullable");
        assertMethod(javaFileAssert, Integer.class, "intMaxNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinMaxNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMaxNullable");

    }

    @Test
    public void testModelsWithNoneOptionalAndNoneOpenApiNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_14765.yaml", null, new ParseOptions()).getOpenAPI();

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenApiNullable(false);
        codegen.setUseOptional(false);
        codegen.setUseSpringBoot3(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.opts(input).generate();


        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Animal.java"))
                .hasImports("jakarta.validation.Valid")
                .hasImports("jakarta.validation.constraints")

                .assertProperty("name")
                .withType("String")
                .toType()
                .assertProperty("age")
                .withType("Integer")
                .toType()
                .assertProperty("alias")
                .withType("String")
                .toType()
                .assertProperty("color")
                .withType("String")
                .toType()
                .assertProperty("names")
                .withType("List<String>")
                .toType()
                .assertProperty("colors")
                .withType("List<String>")
                .toType()
                .assertProperty("stringPattern")
                .withType("String")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMinLength")
                .withType("String")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("String")
                .toType()
                .assertProperty("stringEmail")
                .withType("String")
                .toType()
                .assertProperty("intMinMax")
                .withType("Integer")
                .toType()
                .assertProperty("intMin")
                .withType("Integer")
                .toType()
                .assertProperty("intMax")
                .withType("Integer")
                .toType()
                .assertProperty("numberMinMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMin")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMax")
                .withType("BigDecimal")
                .toType()
                .assertProperty("stringDefault")
                .withType("String")
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .assertProperty("zebra")
                .withType("Zebra")
                .toType()

                .assertProperty("stringPatternNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("String")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("Integer")
                .toType()
                .assertProperty("intMinNullable")
                .withType("Integer")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("Integer")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("BigDecimal")
                .toType()
                .assertProperty("stringDefaultNullable")
                .withType("String")
                .toType()
                .fileContains("stringDefaultNullable = null;")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .assertParameter("name")
                .hasType("String")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getName")
                .hasReturnType("String")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("age")
                .hasReturnType("Animal")
                .bodyContainsLines("this.age = age;", "return this;")
                .assertParameter("age")
                .hasType("Integer")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setAge")
                .hasReturnType("void")
                .assertParameter("age")
                .hasType("Integer")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getAge")
                .hasReturnType("Integer")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("colors")
                .hasReturnType("Animal")
                .bodyContainsLines("this.colors = colors;", "return this;")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .assertParameter("colors")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getColors")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert()

                .assertMethod("names")
                .hasReturnType("Animal")
                .bodyContainsLines("this.names = names;", "return this;")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .assertParameter("names")
                .hasType("List<String>")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("getNames")
                .hasReturnType("List<String>")
                .doesNotHaveParameters()
                .toFileAssert();

        assertMethod(javaFileAssert, String.class, "alias");
        assertMethod(javaFileAssert, String.class, "color");

        assertMethod(javaFileAssert, String.class, "stringPattern");
        assertMethod(javaFileAssert, String.class, "stringMaxMinLength");
        assertMethod(javaFileAssert, String.class, "stringMinLength");
        assertMethod(javaFileAssert, String.class, "stringMaxLength");
        assertMethod(javaFileAssert, String.class, "stringEmail");
        assertMethod(javaFileAssert, Integer.class, "intMinMax");
        assertMethod(javaFileAssert, Integer.class, "intMin");
        assertMethod(javaFileAssert, Integer.class, "intMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinMax");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMin");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMax");
        assertMethod(javaFileAssert, "Zebra", "zebra");

        assertMethod(javaFileAssert, String.class, "stringPatternNullable");
        assertMethod(javaFileAssert, String.class, "stringMaxMinLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringMinLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringMaxLengthNullable");
        assertMethod(javaFileAssert, String.class, "stringEmailNullable");
        assertMethod(javaFileAssert, Integer.class, "intMinMaxNullable");
        assertMethod(javaFileAssert, Integer.class, "intMinNullable");
        assertMethod(javaFileAssert, Integer.class, "intMaxNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinMaxNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMinNullable");
        assertMethod(javaFileAssert, BigDecimal.class, "numberMaxNullable");

    }

    private void assertOptionalMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName, String getterReturnType) {
        assertOptionalMethod(javaFileAssert, type.getSimpleName(), expectedName, getterReturnType);
    }

    private void assertOptionalMethod(JavaFileAssert javaFileAssert, String type, String expectedName, String getterReturnType) {
        assertWrapperMethod(javaFileAssert, "Optional", type, expectedName, getterReturnType);
    }

    private void assertJsonNullableMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName, String getterReturnType) {
        assertJsonNullableMethod(javaFileAssert, type.getSimpleName(), expectedName, getterReturnType);
    }

    private void assertJsonNullableMethod(JavaFileAssert javaFileAssert, String type, String expectedName, String getterReturnType) {
        assertWrapperMethod(javaFileAssert, "JsonNullable", type, expectedName, getterReturnType);
    }

    private void assertWrapperMethod(JavaFileAssert javaFileAssert, String wrapperType, String type, String expectedName, String getterReturnType) {
        String methodName = StringUtils.capitalize(expectedName);
        var of = wrapperType.equals("Optional") ? "ofNullable" : "of";
        javaFileAssert.assertMethod(expectedName)
                .hasReturnType("Animal")
                .bodyContainsLines("this." + expectedName + " = " + wrapperType + "." + of + "(" + expectedName + ");", "return this;")
                .assertParameter(expectedName)
                .hasType(type)
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("set" + methodName)
                .hasReturnType("void")
                .assertParameter(expectedName)
                .hasType(wrapperType + "<" + type + ">")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("get" + methodName)
                .hasReturnType(getterReturnType)
                .doesNotHaveParameters()
                .toFileAssert();
    }

    private void assertMethod(JavaFileAssert javaFileAssert, String type, String expectedName) {
        String methodName = StringUtils.capitalize(expectedName);
        javaFileAssert.assertMethod(expectedName)
                .hasReturnType("Animal")
                .bodyContainsLines("this." + expectedName + " = " + expectedName + ";", "return this;")
                .assertParameter(expectedName)
                .hasType(type)
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("set" + methodName)
                .hasReturnType("void")
                .assertParameter(expectedName)
                .hasType(type)
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("get" + methodName)
                .hasReturnType(type)
                .doesNotHaveParameters()
                .toFileAssert();
    }

    private void assertMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName) {
        assertMethod(javaFileAssert, type.getSimpleName(), expectedName);
    }


    @Test
    public void multiLineOperationDescription() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, DocumentationProvider.SPRINGDOC.name());

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/issue12474-multiline-description.yaml", SPRING_BOOT, additionalProperties);

        String expectedDescription = "# Multi-line descriptions  This is an example of a multi-line description.  It: - has multiple lines - uses Markdown (CommonMark) for rich text representation";
        JavaFileAssert.assertThat(files.get("PingTagApi.java"))
                .fileContains(expectedDescription);
    }

    @Test
    public void multiLineTagDescription() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, DocumentationProvider.SPRINGDOC.name());

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/issue12474-multiline-description.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PingTagApi.java"))
                .fileContains("This is a multine tag : * tag item 1 * tag item 2 ");
    }

    @Test
    public void testSSEOperationSupport() throws Exception {

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/sse.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(SSE, "true");
        codegen.additionalProperties().put(REACTIVE, "true");
        codegen.additionalProperties().put(INTERFACE_ONLY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        MapAssert.assertThatMap(files).isNotEmpty();
        File api = files.get("PathApi.java");
        File delegate = files.get("PathApiDelegate.java");

        JavaFileAssert.assertThat(api)
                .assertMethod("sseVariant1", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Flux<String>")
                .toFileAssert()
                .assertMethod("sseVariant2", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Flux<EventType>")
                .toFileAssert()
                .assertMethod("nonSSE", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Mono<ResponseEntity<String>>");

        JavaFileAssert.assertThat(delegate)
                .assertMethod("sseVariant1", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Flux<String>")
                .bodyContainsLines("return Flux.empty();")
                .toFileAssert()
                .assertMethod("sseVariant2", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Flux<EventType>")
                .bodyContainsLines("return Flux.empty();")
                .toFileAssert()
                .assertMethod("nonSSE", "ServerWebExchange")
                .isNotNull()
                .hasReturnType("Mono<ResponseEntity<String>>")
                .bodyContainsLines("return result.then(Mono.empty());")
        ;

    }

    @Test
    public void givenMultipartForm_whenGenerateReactiveServer_thenParameterAreCreatedAsRequestPart() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/petstore-with-tags.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.REACTIVE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PetApi.java"),
                "@Valid @RequestPart(value = \"additionalMetadata\", required = false) String additionalMetadata");
    }

    @Test
    public void givenMultipartForm_whenGenerateBlockedServer_thenParameterAreCreatedAsRequestPart() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/petstore-with-tags.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PetApi.java"),
                "@Valid @RequestParam(value = \"additionalMetadata\", required = false) String additionalMetadata");
    }

    @Test
    public void testAllArgsConstructor_16797() throws IOException {
        final Map<String, File> output = generateFromContract("src/test/resources/3_0/spring/issue_16797.yaml", SPRING_BOOT,
                Map.of(GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE, INTERFACE_ONLY, "true"),
                codegen -> codegen.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "false"));
        JavaFileAssert.assertThat(output.get("Object4.java"))
                .assertConstructor("String", "Type1", "String", "String", "Boolean")
                .hasParameter("responseType").toConstructor()
                .hasParameter("requestId").toConstructor()
                .hasParameter("success").toConstructor()
                .hasParameter("pageInfo")
        ;

    }

    @Test
    public void testAllArgsConstructor_16797_REFACTOR_ALLOF_WITH_PROPERTIES_ONLY() throws IOException {
        final Map<String, File> output = generateFromContract("src/test/resources/3_0/spring/issue_16797.yaml", SPRING_BOOT,
                Map.of(GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE, INTERFACE_ONLY, "true"),
                codegen -> codegen.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "true"));
        JavaFileAssert.assertThat(output.get("Object4.java"))
                .assertConstructor("String", "Type1", "String", "String", "Boolean")
                .hasParameter("responseType").toConstructor()
                .hasParameter("requestId").toConstructor()
                .hasParameter("success").toConstructor()
                .hasParameter("pageInfo")
        ;
    }

    @Test
    public void testMultiInheritanceParentRequiredParams_issue16797() throws IOException {
        final Map<String, File> output = generateFromContract(
                "src/test/resources/3_0/spring/issue_16797.yaml", SPRING_BOOT, Map.of(INTERFACE_ONLY, "true")
        );
        // constructor should as
        //       public Object4(Type1 pageInfo, String responseType, String requestId, Boolean success) {
        //            super(responseType, requestId, success, pageInfo);
        //        }
        JavaFileAssert.assertThat(output.get("Object4.java"))
                .assertConstructor("Type1", "String", "String", "Boolean")
                .hasParameter("responseType").toConstructor()
                .hasParameter("requestId").toConstructor()
                .hasParameter("success").toConstructor()
                .hasParameter("pageInfo").toConstructor()
        ;
    }

    @Test
    public void testMultiInheritanceParentRequiredParams_issue15796() throws IOException {
        final Map<String, File> output = generateFromContract(
                "src/test/resources/3_0/spring/issue_15796.yaml", SPRING_BOOT, Map.of(INTERFACE_ONLY, "true")
        );
        // constructor should as this
        //public Poodle(String race, String type) {
        //    super(race, type);
        //}
        JavaFileAssert.assertThat(output.get("Poodle.java"))
                .assertConstructor("String", "String")
                .hasParameter("type").toConstructor()
                .hasParameter("race").toConstructor()
        ;
    }

    @Test
    public void testAllArgsConstructor_defaultOrder_15796() throws IOException {
        final Map<String, File> output = generateFromContract("src/test/resources/3_0/spring/issue_15796.yaml", SPRING_BOOT,
                Map.of(GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE, INTERFACE_ONLY, "true"),
                config -> config.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", " true"));
        // constructors should as this
        //public Poodle(String race, String type) {
        //    super(race, type);
        //}
        // and
        //public Poodle(String hairType, Integer tails, String race, String name, String type) {
        //  super(tails, race, name, type);
        //  this.hairType = hairType;
        //}
        JavaFileAssert.assertThat(output.get("Poodle.java"))
                .assertConstructor("String", "String")
                .hasParameter("type").toConstructor()
                .hasParameter("race").toConstructor()
                .toFileAssert()
                .assertConstructor("String", "Integer", "String", "String", "String")
                .hasParameter("tails").toConstructor()
                .hasParameter("race").toConstructor()
                .hasParameter("name").toConstructor()
                .hasParameter("type").toConstructor()
                .hasParameter("hairType").toConstructor()
        ;
    }

    @Test
    public void generateAllArgsConstructor() throws IOException {
        Map<String, File> files = generateFromContract("src/test/resources/3_0/java/all_args_constructor.yaml", null,
                Map.of(AbstractJavaCodegen.GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE, INTERFACE_ONLY, "true"),
                codegenConfig -> codegenConfig.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", " true"));
        JavaFileAssert.assertThat(files.get("Pet.java"))
                .assertConstructor("String")
                .hasParameter("type").toConstructor()
                .toFileAssert()
                .assertConstructor("LocalDate", "String", "String")
                .hasParameter("dateOfBirth").toConstructor()
                .hasParameter("name").toConstructor()
                .hasParameter("type").toConstructor();
        JavaFileAssert.assertThat(files.get("Cat.java"))
                .assertConstructor("Integer", "String", "LocalDate", "String", "String");

        // test required constructor
        JavaFileAssert.assertThat(files.get("Page.java"))
                .assertConstructor("Integer")
                .toFileAssert()
                .fileContains("Constructor with only required parameters and all parameters");

        JavaFileAssert.assertThat(files.get("PageOfPets.java"))
                .assertConstructor("Integer", "List<Pet>")
                .hasParameter("count").toConstructor()
                .hasParameter("_list").toConstructor()
                .toFileAssert()
                .assertConstructor("Integer")
                .hasParameter("count").toConstructor();
    }

    @Test
    public void allOfDuplicatedProperties() throws IOException {
        Map<String, File> output = generateFromContract(
                "src/test/resources/3_0/allOfDuplicatedProperties.yaml",
                SPRING_BOOT,
                Map.of(GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, true, INTERFACE_ONLY, "true")
        );

        JavaFileAssert.assertThat(output.get("ModelC.java"))
                .assertConstructor("String", "Integer", "Integer", "String", "String");
    }

    @Test
    public void testLombokAnnotations() throws IOException {
        final Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@lombok.Data;@lombok.NoArgsConstructor;@lombok.AllArgsConstructor");
        additionalProperties.put(INTERFACE_ONLY, "true");
        Map<String, File> output = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);
        JavaFileAssert.assertThat(output.get("Pet.java"))
                .hasNoConstructor()
                .hasNoMethod("toString")
                .hasNoMethod("hashCode")
                .hasNoMethod("equals")
                .hasNoMethod("getId")
                .hasNoMethod("setId")
                .hasNoMethod("getName")
                .hasNoMethod("setName")
        ;
        additionalProperties.put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@lombok.ToString");
        output = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);
        JavaFileAssert.assertThat(output.get("Pet.java"))
                .assertConstructor().toFileAssert()
                .hasNoMethod("toString")
                .assertMethod("hashCode")
                .toFileAssert()
                .assertMethod("equals")
                .toFileAssert()
                .assertMethod("getId")
                .toFileAssert()
                .assertMethod("setId")
                .toFileAssert()
                .assertMethod("getName")
                .toFileAssert()
                .assertMethod("setName")
        ;
        additionalProperties.put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@lombok.Getter;@lombok.Setter");
        output = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);
        JavaFileAssert.assertThat(output.get("Pet.java"))
                .assertConstructor().toFileAssert()
                .assertMethod("toString")
                .toFileAssert()
                .assertMethod("hashCode")
                .toFileAssert()
                .assertMethod("equals")
        ;
    }

    @Test
    void testBuilderJavaSpring_noOptional() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/builder.yaml",
                SPRING_BOOT,
                Map.of(
                        GENERATE_BUILDERS, true,
                        SpringCodegen.OPENAPI_NULLABLE, false,
                        SpringCodegen.USE_OPTIONAL, false,
                        INTERFACE_ONLY, "true"
                )
        );

        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains("toBuilder()",
                        "builder()",
                        "public static class Builder {");
        JavaFileAssert.assertThat(files.get("Snake.java"))
                .fileContains("toBuilder()",
                        "builder()",
                        "public static class Builder extends Reptile.Builder {",
                        "return builder.copyOf(this);");
        JavaFileAssert.assertThat(files.get("SimpleObject.java"))
                .fileContains("public SimpleObject.Builder additionalProperties(Map<String, Integer> additionalProperties) {",
                        "SimpleObject.Builder nullableObject(String nullableObject) {",
                        "SimpleObject.Builder nb(BigDecimal nb) {")
                .fileDoesNotContain("SimpleObject.Builder nullableObject(JsonNullable<String> nullableObject) {");
    }

    @Test
    void testBuilderJavaSpring_useOptional() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/builder.yaml",
                SPRING_BOOT,
                Map.of(
                        GENERATE_BUILDERS, true,
                        SpringCodegen.OPENAPI_NULLABLE, true,
                        SpringCodegen.USE_OPTIONAL, true,
                        INTERFACE_ONLY, "true"
                )
        );

        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains("toBuilder()",
                        "builder()",
                        "public static class Builder {");
        JavaFileAssert.assertThat(files.get("Snake.java"))
                .fileContains("toBuilder()",
                        "builder()",
                        "public static class Builder extends Reptile.Builder {",
                        "return builder.copyOf(this);");
        JavaFileAssert.assertThat(files.get("SimpleObject.java"))
                .fileContains("public SimpleObject.Builder additionalProperties(Map<String, Integer> additionalProperties) {",
                        "SimpleObject.Builder nullableObject(String nullableObject) {",
                        "SimpleObject.Builder nullableObject(JsonNullable<String> nullableObject) {",
                        "SimpleObject.Builder nb(BigDecimal nb) {");
    }

    @Test
    public void optionalListShouldBeEmpty() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_1/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_NAME_SUFFIX, "Controller");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");


        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetDto.java"))
                .fileContains("private List<@Valid TagDto> tags = new ArrayList<>();")
                .fileContains("private List<String> photoUrls = new ArrayList<>();");

    }

    @Test
    public void testCollectionTypesWithDefaults_issue_18102() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_1/java/issue_18102.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_NAME_SUFFIX, "Controller");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");
        codegen.setContainerDefaultToNull(true);


        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata and ↓ only generate models
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetDto.java"))
                .fileContains("private @Nullable List<@Valid TagDto> tags")
                .fileContains("private List<@Valid TagDto> tagsDefaultList = new ArrayList<>()")
                .fileContains("private @Nullable Set<@Valid TagDto> tagsUnique")
                .fileContains("private Set<@Valid TagDto> tagsDefaultSet = new LinkedHashSet<>();")
                .fileContains("private @Nullable List<String> stringList")
                .fileContains("private List<String> stringDefaultList = new ArrayList<>(Arrays.asList(\"A\", \"B\"));")
                .fileContains("private List<String> stringEmptyDefaultList = new ArrayList<>();")
                .fileContains("@Nullable Set<String> stringSet")
                .fileContains("private Set<String> stringDefaultSet = new LinkedHashSet<>(Arrays.asList(\"A\", \"B\"));")
                .fileContains("private Set<String> stringEmptyDefaultSet = new LinkedHashSet<>();")
                .fileDoesNotContain("private List<@Valid TagDto> tags = new ArrayList<>()")
                .fileDoesNotContain("private Set<@Valid TagDto> tagsUnique = new LinkedHashSet<>()")
                .fileDoesNotContain("private List<String> stringList = new ArrayList<>()")
                .fileDoesNotContain("private Set<String> stringSet = new LinkedHashSet<>()");
    }

    @Test
    public void shouldGenerateOptionalParameterTypesWhenUsingOptionalAndDelegate_issue17768() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_TAGS, "true");
        additionalProperties.put(SpringCodegen.SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(SpringCodegen.PERFORM_BEANVALIDATION, "true");
        additionalProperties.put(SpringCodegen.SPRING_CONTROLLER, "true");
        additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
        additionalProperties.put(SpringCodegen.USE_OPTIONAL, "true");
        additionalProperties.put(DELEGATE_PATTERN, "true");
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_17768.yaml", SPRING_BOOT, additionalProperties);
        JavaFileAssert.assertThat(files.get("TestApiDelegate.java"))
                .assertMethod("updatePost")
                .assertParameter("updateRequest")
                .hasType("Optional<UpdateRequest>")
                .toMethod()
                .toFileAssert();
        JavaFileAssert.assertThat(files.get("TestApi.java"))
                .assertMethod("updatePost")
                .assertParameter("updateRequest")
                .hasType("Optional<UpdateRequest>")
                .toMethod()
                .toFileAssert();
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationTrue_issue13241() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13241.yaml");

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("return UNKNOWN_DEFAULT_OPEN_API");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationNotSet_issue13241() throws IOException {

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_13241.yaml");

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("throw new IllegalArgumentException(\"Unexpected value '\" + value + \"'\");");
    }

    /**
     * General XML annotations test (both JAXB and Jackson)
     * <br>
     * Includes regression tests for:
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/2417">Correct Jackson annotation when `wrapped: false`</a>
     */
    @Test
    void shouldGenerateCorrectXmlAnnotations() {
        // Arrange
        final CodegenConfigurator config = new CodegenConfigurator()
                .addAdditionalProperty(CodegenConstants.WITH_XML, true)
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .setGeneratorName("spring")
                .setInputSpec("src/test/resources/3_0/java/xml-annotations-test.yaml")
                .setLibrary(SPRING_BOOT)
                .setOutputDir(newTempFolder().toString());

        // Act
        final List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Assert
        JavaFileAssert.assertThat(files.get(0))
                .assertTypeAnnotations()
                .containsWithNameAndAttributes("JacksonXmlRootElement", Map.of("localName", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
                .toType()

                // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                .assertMethod("getTags")
                .doesNotHaveAnnotation("XmlAttribute")
                .hasAnnotation("XmlElement", Map.of("name", "\"Tag\""))
                .hasAnnotation("XmlElementWrapper", Map.of("name", "\"TagList\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Tag\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"TagList\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertMethod("getFriends")
                .doesNotHaveAnnotation("XmlAttribute")
                .doesNotHaveAnnotation("XmlElementWrapper")
                .hasAnnotation("XmlElement", Map.of("name", "\"friend-pet\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"friend-pet\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)
                .assertMethod("getStatus")
                .doesNotHaveAnnotation("XmlAttribute")
                .doesNotHaveAnnotation("XmlElementWrapper")
                .hasAnnotation("XmlElement", Map.of("name", "\"PetStatus\""))
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"PetStatus\""))
                .toFileAssert()

                // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
                //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertMethod("getPhotoUrls")
                .doesNotHaveAnnotation("XmlAttribute")
                .hasAnnotation("XmlElement", Map.of("name", "\"photoUrls\""))
                .hasAnnotation("XmlElementWrapper", Map.of("name", "\"photoUrls\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"photoUrls\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"photoUrls\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
                .assertMethod("getName")
                .doesNotHaveAnnotation("XmlElement")
                .doesNotHaveAnnotation("XmlElementWrapper")
                .hasAnnotation("XmlAttribute", Map.of("name", "\"name\""))
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("isAttribute", "true", "localName", "\"name\""))
                .toFileAssert()

                // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
                .assertMethod("getId")
                .doesNotHaveAnnotation("XmlAttribute")
                .doesNotHaveAnnotation("XmlElementWrapper")
                .hasAnnotation("XmlElement", Map.of("name", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toFileAssert()

                // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertMethod("getFoods")
                .doesNotHaveAnnotation("XmlAttribute")
                .hasAnnotation("XmlElement", Map.of("name", "\"yummy-yummy\""))
                .hasAnnotation("XmlElementWrapper", Map.of("name", "\"yummy-yummy\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"yummy-yummy\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"yummy-yummy\""))
                .toFileAssert()

                // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertMethod("getColors")
                .doesNotHaveAnnotation("XmlAttribute")
                .hasAnnotation("XmlElement", Map.of("name", "\"color\""))
                .hasAnnotation("XmlElementWrapper", Map.of("name", "\"colors\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"color\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"colors\""))
                .toFileAssert()

                // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertMethod("getCategories")
                .doesNotHaveAnnotation("XmlAttribute")
                .doesNotHaveAnnotation("XmlElementWrapper")
                .hasAnnotation("XmlElement", Map.of("name", "\"Category\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Category\""))
                // ↓ specific regression test for #2417: (useWrapping=false) needs to be present
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertMethod("getActivities")
                .doesNotHaveAnnotation("XmlAttribute")
                .hasAnnotation("XmlElement", Map.of("name", "\"item\""))
                .hasAnnotation("XmlElementWrapper", Map.of("name", "\"activities-array\""))
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"item\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"activities-array\""));
    }

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/12804">#12804</a>
     */
    @Test
    public void shouldGenerateSingleDeprecatedAnnotation() {
        final var tempDir = TestUtils.newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .addAdditionalProperty(GENERATE_BUILDERS, true)
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setGeneratorName("spring")
                .setOutputDir(tempDir.toString());

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        JavaFileAssert.assertThat(tempDir.resolve("src/main/java/org/openapitools/model/Pet.java"))
                .assertInnerClass("Builder")
                .assertMethod("status").hasAnnotation("Deprecated")
                .toInnerClassAssert()
                .assertMethod("build")
                .doesNotHaveAnnotation("Deprecated");
    }

    @Test
    public void shouldAnnotateNonRequiredFieldsAsNullable() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setGenerateConstructorWithAllArgs(true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/nullable-annotation.yaml");
        var file = files.get("Item.java");

        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryName")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalDescription")
                .hasAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalOneWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableStr")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainerWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .fileContains(
                        "public Item(" +
                                "String mandatoryName," +
                                " @Nullable String optionalDescription," +
                                " String optionalOneWithDefault," +
                                " String nullableStr," +
                                " List<String> mandatoryContainer," +
                                " List<String> optionalContainer," +
                                " List<String> optionalContainerWithDefault," +
                                " List<String> nullableContainer)"
                );
    }

    @Test
    public void shouldAnnotateNonRequiredFieldsAsNullableWhenSetContainerDefaultToNull() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setGenerateConstructorWithAllArgs(true);
        codegen.setContainerDefaultToNull(true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/nullable-annotation.yaml");
        var file = files.get("Item.java");

        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainer")
                .hasAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainerWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .fileContains(
                        ", List<String> mandatoryContainer," +
                                " @Nullable List<String> optionalContainer," +
                                " List<String> optionalContainerWithDefault," +
                                " List<String> nullableContainer)"
                );
    }

    @Test
    public void shouldNotAnnotateNonRequiredFieldsAsNullableWhileUseOptional() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setGenerateConstructorWithAllArgs(true);
        codegen.setUseOptional(true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/nullable-annotation.yaml");
        var file = files.get("Item.java");

        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryName")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalDescription")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalOneWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableStr")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .fileContains(
                        "public Item(String mandatoryName, String optionalDescription," +
                                " String optionalOneWithDefault, String nullableStr"
                );
    }

    @Test
    public void shouldAnnotateNonRequiredFieldsAsNullableWhileNotUsingOpenApiNullableAndContainerDefaultToNullSet() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setGenerateConstructorWithAllArgs(true);
        codegen.setOpenApiNullable(false);
        codegen.setContainerDefaultToNull(true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/nullable-annotation.yaml");
        var file = files.get("Item.java");

        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryName")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalDescription")
                .hasAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalOneWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableStr")
                .hasAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("mandatoryContainer")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainer")
                .hasAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("optionalContainerWithDefault")
                .doesNotHaveAnnotation("Nullable");
        JavaFileAssert.assertThat(file)
                .assertProperty("nullableContainer")
                .hasAnnotation("Nullable");

        JavaFileAssert.assertThat(file)
                .fileContains(
                        " List<String> mandatoryContainer," +
                                " @Nullable List<String> optionalContainer," +
                                " List<String> optionalContainerWithDefault," +
                                " @Nullable List<String> nullableContainer)"
                );
    }

    @Test
    public void shouldNotAcceptNullValues() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setUseSpringBoot3(true);
        codegen.setUseOptional(true);
        codegen.setOptionalAcceptNullable(false);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/petstore.yaml");
        var file = files.get("Category.java");

        JavaFileAssert.assertThat(file)
                .fileContains(
                        "this.name = Optional.of(name);"
                );
        JavaFileAssert.assertThat(file)
                .fileDoesNotContain(
                        "this.name = Optional.ofNullable(name);"
                );
    }

    @Test
    public void shouldAcceptNullValues() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setUseSpringBoot3(true);
        codegen.setUseOptional(true);
        //codegen.setOptionalAcceptNullable(true); // default to true

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/petstore.yaml");
        var file = files.get("Category.java");

        JavaFileAssert.assertThat(file)
                .fileContains(
                        "this.name = Optional.ofNullable(name);"
                );
        JavaFileAssert.assertThat(file)
                .fileDoesNotContain(
                        "this.name = Optional.of(name);"
                );
    }

    @Test
    public void testEnumWithImplements() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/enum-implements.yaml");
        SpringCodegen codegen = new SpringCodegen();
        codegen.setOutputDir(output.toString());

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Type.java")).fileContains("Type implements java.io.Serializable {");
    }

    @Test
    public void givenMultipartForm_whenGenerateUsingOptional_thenParameterAreCreatedAsOptional() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/issue_9530.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(SpringCodegen.USE_OPTIONAL, "true");
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);


        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/api/PetApi.java"),
                "@Valid @RequestParam(value = \"additionalMetadata\", required = false) Optional<String> additionalMetadata",
                "@Valid @RequestParam(value = \"length\", required = true) Integer length");
    }

    @Test
    public void shouldEnableBuiltInValidationOptionWhenSetToTrue() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setUseSpringBoot3(true);
        codegen.setUseOptional(true);
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, true);
        codegen.additionalProperties().put(SpringCodegen.USE_SPRING_BUILT_IN_VALIDATION, true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/petstore.yaml");
        var file = files.get("UserApi.java");

        JavaFileAssert.assertThat(file)
                .hasNoImports("org.springframework.validation.annotation.Validated")
                .assertTypeAnnotations()
                .doesNotContainWithName("Validated");
    }

    @Test
    public void shouldDisableBuiltInValidationOptionWhenSetToFalse() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setUseSpringBoot3(true);
        codegen.setUseOptional(true);
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, true);
        codegen.additionalProperties().put(SpringCodegen.USE_SPRING_BUILT_IN_VALIDATION, false);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/petstore.yaml");
        var file = files.get("UserApi.java");

        JavaFileAssert.assertThat(file)
                .hasImports("org.springframework.validation.annotation.Validated")
                .assertTypeAnnotations()
                .containsWithName("Validated");
    }

    @Test
    public void shouldDisableBuiltInValidationOptionByDefault() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setUseSpringBoot3(true);
        codegen.setUseOptional(true);
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, true);

        Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/petstore.yaml");
        var file = files.get("UserApi.java");

        JavaFileAssert.assertThat(file)
                .hasImports("org.springframework.validation.annotation.Validated")
                .assertTypeAnnotations()
                .containsWithName("Validated");
    }

    @Test
    public void testExampleAnnotationGeneration_issue17610() throws IOException {
        final Map<String, File> generatedCodeFiles = generateFromContract("src/test/resources/3_0/spring/api-response-examples_issue17610.yaml", SPRING_BOOT);

        JavaFileAssert.assertThat(generatedCodeFiles.get("DogsApi.java"))
                .assertMethod("createDog")
                .assertMethodAnnotations()
                .recursivelyContainsWithName("ExampleObject");
    }

    @Test
    public void testExampleAnnotationGeneration_issue17610_2() throws IOException {
        final Map<String, File> generatedCodeFiles = generateFromContract("src/test/resources/3_0/spring/petstore_with_api_response_examples.yaml", SPRING_BOOT);

        JavaFileAssert.assertThat(generatedCodeFiles.get("PetApi.java"))
                .assertMethod("addPet")
                .assertMethodAnnotations()
                .recursivelyContainsWithName("ExampleObject")
                .toMethod().toFileAssert()
                .assertMethod("findPetsByStatus")
                .assertMethodAnnotations()
                .recursivelyContainsWithName("ExampleObject");
    }

    @Test
    public void testEnumFieldShouldBeFinal_issue21018() throws IOException {
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        Map<String, File> files = generateFiles(codegen, "src/test/resources/bugs/issue_21018.yaml");

        JavaFileAssert.assertThat(files.get("SomeEnum.java"))
                .fileContains("private final String value;");

        JavaFileAssert.assertThat(files.get("SomeObject.java"))
                .fileContains("private final String value");
    }

    @Test
    public void testCollectionTypesWithDefaults_issue_collection() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/java/issue_collection.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_NAME_SUFFIX, "Controller");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, "Dto");
        codegen.additionalProperties().put("defaultToEmptyContainer", "array");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("PetDto.java"))
                .fileContains("private @Nullable List<@Valid TagDto> tags;")
                .fileContains("private List<@Valid TagDto> tagsRequiredList = new ArrayList<>();")
                .fileContains("private @Nullable List<String> stringList;")
                .fileContains("private List<String> stringRequiredList = new ArrayList<>();");
    }

    @Test
    public void testDefaultForRequiredNonNullableMap() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/java/issue_21890.yaml", null, new ParseOptions()).getOpenAPI();
        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("defaultToEmptyContainer", "map");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains("private Map<String, String> requiredNonNullableMap = new HashMap<>();");
    }

    @Test
    public void testGenericReturnTypeWhenUsingResponseEntity_issue1096() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_RESPONSE_ENTITY, "true");
        additionalProperties.put(SpringCodegen.GENERATE_GENERIC_RESPONSE_ENTITY, "true");
        additionalProperties.put(SpringCodegen.USE_SPRING_BOOT3, "true");
        additionalProperties.put(CodegenConstants.MODEL_TESTS, "false");
        additionalProperties.put(CodegenConstants.MODEL_DOCS, "false");
        additionalProperties.put(CodegenConstants.APIS, "true");
        additionalProperties.put(CodegenConstants.SUPPORTING_FILES, "false");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .assertMethod("getPetById").hasReturnType("ResponseEntity<?>")
                .toFileAssert()
                .assertMethod("findPetsByStatus").hasReturnType("ResponseEntity<?>");
    }

    @Test
    public void testGenericReturnTypeWhenNotUsingResponseEntity_issue1096() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(SpringCodegen.USE_RESPONSE_ENTITY, "false");
        additionalProperties.put(SpringCodegen.GENERATE_GENERIC_RESPONSE_ENTITY, "true");
        additionalProperties.put(SpringCodegen.USE_SPRING_BOOT3, "true");
        additionalProperties.put(CodegenConstants.MODEL_TESTS, "false");
        additionalProperties.put(CodegenConstants.MODEL_DOCS, "false");
        additionalProperties.put(CodegenConstants.APIS, "true");
        additionalProperties.put(CodegenConstants.SUPPORTING_FILES, "false");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .assertMethod("getPetById").hasReturnType("Pet")
                .toFileAssert()
                .assertMethod("findPetsByStatus").hasReturnType("List<Pet>");
    }

    @Test
    public void testHasRestControllerDoesNotHaveController_issue21156() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/issue_21156.yaml");
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary("spring-boot");

        codegen.additionalProperties().put(INTERFACE_ONLY, "false");
        codegen.additionalProperties().put(DELEGATE_PATTERN, "true");
        codegen.additionalProperties().put(SPRING_CONTROLLER, "true");
        codegen.additionalProperties().put(RETURN_SUCCESS_CODE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false); // skip metadata generation
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert javaFileAssert = JavaFileAssert.assertThat(files.get("TestApiDelegate.java"));
        javaFileAssert
                .hasImports("java.util.concurrent.atomic.AtomicInteger");
    }

    @Test
    public void testOneOfInterfaceWithAnnotation() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/java/oneOf-with-annotations.yaml", SPRING_BOOT);
        JavaFileAssert.assertThat(files.get("Fruit.java"))
                .isInterface()
                .assertTypeAnnotations().containsWithName("SuppressWarnings");
    }

    @Test
    public void testApiVersion() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/apiVersion.yaml", SPRING_BOOT,
                Map.of(SpringCodegen.SPRING_API_VERSION, "v1",
                        USE_TAGS, true));
        JavaFileAssert.assertThat(files.get("TestApi.java"))
                .assertMethod("getVersions")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("RequestMapping", Map.of("version", "\"v1\""))
                .toMethod().toFileAssert()

                .assertMethod("getOverrides")
                .assertMethodAnnotations()
                .containsWithNameAndAttributes("RequestMapping", Map.of("version", "\"2+\""))
                .toMethod().toFileAssert()

                .assertMethod("getNones")
                .assertMethodAnnotations()
                .containsWithNameAndDoesContainAttributes("RequestMapping", List.of("version"));
    }

    @Test
    public void annotationLibraryDoesNotCauseImportConflictsInSpring() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("documentationProvider", "source");
        properties.put("annotationLibrary", "none");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/native/issue21991.yaml");

        SpringCodegen codegen = new SpringCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().putAll(properties);

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        File apiFile = files.get("Schema.java");
        assertNotNull(apiFile);

        JavaFileAssert.assertThat(apiFile).fileDoesNotContain(
            "import io.swagger.v3.oas.annotations.media.Schema;"
        );
    }

    @Test
    public void annotationLibraryDoesNotCauseImportConflictsInSpringWithAnnotationLibrary() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("documentationProvider", "source");
        properties.put("annotationLibrary", "swagger2");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/native/issue21991.yaml");

        SpringCodegen codegen = new SpringCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().putAll(properties);

        ClientOptInput input = new ClientOptInput()
            .openAPI(openAPI)
            .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        Map<String, File> files = generator.opts(input).generate().stream()
            .collect(Collectors.toMap(File::getName, Function.identity()));

        File apiFile = files.get("Schema.java");
        assertNotNull(apiFile);

        JavaFileAssert.assertThat(apiFile).fileContains(
            "import io.swagger.v3.oas.annotations.media.Schema;"
        );
    }
}
