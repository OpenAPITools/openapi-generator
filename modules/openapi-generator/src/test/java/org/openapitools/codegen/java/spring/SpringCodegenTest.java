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
import static org.openapitools.codegen.languages.AbstractJavaCodegen.GENERATE_BUILDERS;
import static org.openapitools.codegen.languages.AbstractJavaCodegen.GENERATE_CONSTRUCTOR_WITH_ALL_ARGS;
import static org.openapitools.codegen.languages.SpringCodegen.ASYNC;
import static org.openapitools.codegen.languages.SpringCodegen.DELEGATE_PATTERN;
import static org.openapitools.codegen.languages.SpringCodegen.DocumentationProvider;
import static org.openapitools.codegen.languages.SpringCodegen.IMPLICIT_HEADERS;
import static org.openapitools.codegen.languages.SpringCodegen.INTERFACE_ONLY;
import static org.openapitools.codegen.languages.SpringCodegen.OPENAPI_NULLABLE;
import static org.openapitools.codegen.languages.SpringCodegen.REACTIVE;
import static org.openapitools.codegen.languages.SpringCodegen.REQUEST_MAPPING_OPTION;
import static org.openapitools.codegen.languages.SpringCodegen.RESPONSE_WRAPPER;
import static org.openapitools.codegen.languages.SpringCodegen.RETURN_SUCCESS_CODE;
import static org.openapitools.codegen.languages.SpringCodegen.SKIP_DEFAULT_INTERFACE;
import static org.openapitools.codegen.languages.SpringCodegen.SPRING_BOOT;
import static org.openapitools.codegen.languages.SpringCodegen.SPRING_CLOUD_LIBRARY;
import static org.openapitools.codegen.languages.SpringCodegen.SPRING_CONTROLLER;
import static org.openapitools.codegen.languages.SpringCodegen.SSE;
import static org.openapitools.codegen.languages.SpringCodegen.USE_ENUM_CASE_INSENSITIVE;
import static org.openapitools.codegen.languages.SpringCodegen.USE_RESPONSE_ENTITY;
import static org.openapitools.codegen.languages.SpringCodegen.USE_SPRING_BOOT3;
import static org.openapitools.codegen.languages.SpringCodegen.USE_TAGS;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.ANNOTATION_LIBRARY;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DOCUMENTATION_PROVIDER;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;

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
import org.apache.commons.lang3.StringUtils;
import org.assertj.core.api.MapAssert;
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
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Ignore;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

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
                        "value", "\"/zebras\""
                ))
                .toMethod()
                .hasParameter("limit").withType("Optional<BigDecimal>")
                .assertParameterAnnotations()
                .containsWithName("Valid")
                .containsWithNameAndAttributes("Parameter", ImmutableMap.of("name", "\"limit\""))
                .containsWithNameAndAttributes("RequestParam", ImmutableMap.of("required", "false", "value", "\"limit\""))
                .toParameter()
                .toMethod()
                .hasParameter("animalParams").withType("Optional<AnimalParams>")
                .toMethod()
                .commentContainsLines("GET /zebras", "@param limit  (optional)")
                .bodyContainsLines("return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED)");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/AnimalParams.java"))
                .hasImports("org.springframework.format.annotation.DateTimeFormat")
                .hasProperty("born").withType("Optional<LocalDate>")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE"))
                .toProperty()
                .toType()
                .hasProperty("lastSeen").withType("Optional<OffsetDateTime>")
                .assertPropertyAnnotations()
                .containsWithNameAndAttributes("DateTimeFormat", ImmutableMap.of("iso", "DateTimeFormat.ISO.DATE_TIME"))
                .toProperty().toType()
                .assertMethod("born", "LocalDate")
                .bodyContainsLines("this.born = Optional.of(born)")
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .assertMethod("exampleApiGet", "OffsetDateTime")
                .hasParameter("start")
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

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
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

        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGenerateMetadata(false);

        generator.opts(input).generate();

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/ExampleApi.java"))
                .fileContains("@RequestBody(required = false")
                .assertMethod("exampleApiPost", "ExampleApiPostRequest")
                .hasParameter("exampleApiPostRequest")
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

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
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
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.USE_RESPONSE_ENTITY), true);
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
    public void testAdditionalProperties_issue1466() throws IOException {
        final SpringCodegen codegen = new SpringCodegen();

        final Map<String, File> files = generateFiles(codegen, "src/test/resources/3_0/spring/petstore-with-fake-endpoints-models-for-testing.yaml");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesAnyType.java"))
            .hasProperty("additionalProperties").withType("Map<String, Object>")
            .toType()
            .assertMethod("putAdditionalProperty", "String", "Object")
            .toFileAssert()
            .assertMethod("getAdditionalProperty", "String").hasReturnType("Object");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesArray.java"))
            .hasProperty("additionalProperties").withType("Map<String, List>")
            .toType()
            .assertMethod("putAdditionalProperty", "String", "List")
            .toFileAssert()
            .assertMethod("getAdditionalProperty", "String").hasReturnType("List");

        JavaFileAssert.assertThat(files.get("AdditionalPropertiesInteger.java"))
            .hasProperty("additionalProperties").withType("Map<String, Integer>")
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
              .hasProperty("details")
              .withType( "Map<String, Object>" )
              .toType()
              .hasProperty("response")
              .withType( "JsonNullable<Set<@Valid ResponseTest2>>" )
              .toType()
              .hasProperty("nullableDtos")
              .withType( "JsonNullable<Set<@Valid ResponseTest2>>" )
              .toType()
              .hasProperty("dtos")
              .withType( "Set<@Valid ResponseTest2>" )
              .toType()
              .hasProperty("listNullableDtos")
              .withType( "JsonNullable<List<@Valid ResponseTest2>>" )
              .toType()
              .hasProperty("listDtos")
              .withType( "List<@Valid ResponseTest2>" )
              .toType()
              .hasProperty("nullableStrings")
              .withType( "JsonNullable<Set<String>>" )
              .toType()
              .hasProperty("strings")
              .withType( "Set<String>" )
              .toType()
              .hasProperty("nullableInts")
              .withType( "JsonNullable<Set<Integer>>" )
              .toType()
              .hasProperty("ints")
              .withType( "Set<Integer>" );
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
                .hasProperty("stringPattern")
                .withType( "Set<@Pattern(regexp = \"[a-z]\") String>" )
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType( "Set<@Size(min = 1, max = 10) String>" )
                .toType()
                .hasProperty("stringMinLength")
                .withType( "List<@Size(min = 1) String>" )
                .toType()
                .hasProperty("stringMaxLength")
                .withType( "Set<@Size(max = 1) String>" )
                .toType()
                .hasProperty("intMinMax")
                .withType( "List<@Min(1) @Max(10) Integer>" )
                .toType()
                .hasProperty("intMin")
                .withType( "List<@Min(1) Integer>" )
                .toType()
                .hasProperty("intMax")
                .withType( "List<@Max(10) Integer>" )
                .toType()
                .hasProperty("numberMinMax")
                .withType( "List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>" )
                .toType()
                .hasProperty("numberMin")
                .withType( "List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>" )
                .toType()
                .hasProperty("numberMax")
                .withType( "List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>" )
                .toType()

                .hasProperty("stringPatternNullable")
                .withType( "JsonNullable<Set<@Pattern(regexp = \"[a-z]\") String>>" )
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType( "JsonNullable<Set<@Size(min = 1, max = 10) String>>" )
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType( "JsonNullable<List<@Size(min = 1) String>>" )
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType( "JsonNullable<Set<@Size(max = 1) String>>" )
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType( "JsonNullable<List<@Min(1) @Max(10) Integer>>" )
                .toType()
                .hasProperty("intMinNullable")
                .withType( "JsonNullable<List<@Min(1) Integer>>" )
                .toType()
                .hasProperty("intMaxNullable")
                .withType( "JsonNullable<List<@Max(10) Integer>>" )
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType( "JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>>" )
                .toType()
                .hasProperty("numberMinNullable")
                .withType( "JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>>" )
                .toType()
                .hasProperty("numberMaxNullable")
                .withType( "JsonNullable<List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>>" )
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
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
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
        Assert.assertTrue(codegen.isUnhandledException());
        Assert.assertEquals(codegen.additionalProperties().get(SpringCodegen.UNHANDLED_EXCEPTION_HANDLING), true);
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

        assertFileNotContains(filePath,"spring.security.oauth2.client.registration.oAuth2Application.scope");
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
        beanValidationForFormatEmail(true, false, true,"@jakarta.validation.constraints.Email", "@javax.validation.constraints.Email");
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
        if (useBeanValidation) javaFileAssert.hasImports((useJakarta? "jakarta" : "javax") + ".validation.constraints");
        if (performBeanValidation) javaFileAssert.hasImports("org.hibernate.validator.constraints");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/PersonWithEmail.java"))
            .fileContains(contains)
            .fileDoesNotContains(notContains);
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
            .fileDoesNotContains("Mono<DummyRequest>");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/SomeApiDelegate.java"))
            .fileContains("Mono<Map<String, DummyRequest>>")
            .fileDoesNotContains("Mono<DummyRequest>");
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
            .fileContains( "allowableValues = \"0, 1\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/BearsApi.java"))
            .fileContains( "allowableValues = \"sleeping, awake\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/CamelsApi.java"))
            .fileContains( "allowableValues = \"sleeping, awake\"", "@PathVariable");
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/api/GiraffesApi.java"))
            .fileContains( "allowableValues = \"0, 1\"", "@PathVariable");
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
            .fileContains( "status", "@NotNull");
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
            .fileContains( "status")
            .fileDoesNotContains("@NotNull");
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
        codegen.setLegacyDiscriminatorBehavior(false);

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Foo.java"), "public class Foo extends Entity implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRef.java"), "public class FooRef extends EntityRef implements FooRefOrValue");
        assertFileContains(Paths.get(outputPath + "/src/main/java/org/openapitools/model/FooRefOrValue.java"), "public interface FooRefOrValue");
        // previous bugs
        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/BarRef.java"))
            .fileDoesNotContains( "atTypesuper.hashCode", "private String atBaseType");
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
            .hasParameter("pageable").withType("Pageable")
            .assertParameterAnnotations()
            .containsWithName("ParameterObject");


        // different import for SB3
        additionalProperties.put(USE_SPRING_BOOT3, "true");
        files = generateFromContract("src/test/resources/2_0/petstore-with-spring-pageable.yaml", SPRING_BOOT, additionalProperties);

        JavaFileAssert.assertThat(files.get("PetApi.java"))
            .hasImports("org.springdoc.core.annotations.ParameterObject", "org.springframework.data.domain.Pageable")
            .assertMethod("findPetsByStatus")
            .hasParameter("pageable").withType("Pageable")
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
                .hasParameter("pageable").withType("Pageable");
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
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
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
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "PascalCase");
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
                .hasParameter("body")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("Min", ImmutableMap.of("value", "2"));
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

        JavaFileAssert.assertThat(output.get("ObjectWithNoRequiredParameter.java")).assertNoConstructor("String");

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
        return generateFromContract(url, library, additionalProperties, codegen -> {});
    }

    /**
     * Generate the contract with additional configuration.
     *
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
            .hasProperty("normalPropertyName")
                .assertPropertyAnnotations()
                .doesNotContainsWithName("JsonProperty")
                .doesNotContainsWithName("JacksonXmlProperty")
                .toProperty().toType()
            .hasProperty("UPPER_CASE_PROPERTY_SNAKE")
                .assertPropertyAnnotations()
                .doesNotContainsWithName("JsonProperty")
                .doesNotContainsWithName("JacksonXmlProperty")
                .toProperty().toType()
            .hasProperty("lowerCasePropertyDashes")
                .assertPropertyAnnotations()
                .doesNotContainsWithName("JsonProperty")
                .doesNotContainsWithName("JacksonXmlProperty")
                .toProperty().toType()
            .hasProperty("propertyNameWithSpaces")
                .assertPropertyAnnotations()
                .doesNotContainsWithName("JsonProperty")
                .doesNotContainsWithName("JacksonXmlProperty")
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
                .doesNotContainsWithName("Controller");
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
                .doesNotContainsWithName("RestController");
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
            .hasParameter("alias")
            .withType("String")
            .toMethod()
            .toFileAssert()

            // Setter method assertions
            .assertMethod("setAlias")
            .hasReturnType("void")
            .hasParameter("alias")
            .withType("JsonNullable<String>");

        JavaFileAssert.assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/model/Zebra.java"))
            // Fluent method assertions
            .assertMethod("alias")
            .hasReturnType("Zebra")
            .bodyContainsLines("super.alias(alias);", "return this;")
            .hasParameter("alias")
            .withType("String")
            .toMethod()
            .toFileAssert()

            // No overridden setter on child object
            .assertNoMethod("setAlias");
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

                .hasProperty("name")
                .withType( "String" )
                .toType()
                .hasProperty("age")
                .withType( "JsonNullable<Integer>" )
                .toType()
                .hasProperty("alias")
                .withType( "JsonNullable<String>" )
                .toType()
                .hasProperty("color")
                .withType( "String" )
                .toType()
                .hasProperty("names")
                .withType( "List<String>" )
                .toType()
                .hasProperty("colors")
                .withType( "JsonNullable<List<String>>" )
                .toType()
                .hasProperty("stringPattern")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringEmail")
                .withType( "String" )
                .toType()
                .hasProperty("intMinMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMin")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("numberMinMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMin")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("stringDefault")
                .withType( "String" )
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .hasProperty("zebra")
                .withType( "Zebra" )
                .toType()

                .hasProperty("stringPatternNullable")
                .withType( "JsonNullable<@Pattern(regexp = \"[a-z]\") String>" )
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType( "JsonNullable<@Size(min = 1, max = 10) String>" )
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType( "JsonNullable<@Size(min = 1) String>" )
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType( "JsonNullable<@Size(max = 1) String>" )
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType( "JsonNullable<@Min(1) @Max(10) Integer>" )
                .toType()
                .hasProperty("intMinNullable")
                .withType( "JsonNullable<@Min(1) Integer>" )
                .toType()
                .hasProperty("intMaxNullable")
                .withType( "JsonNullable<@Max(10) Integer>" )
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType( "JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("numberMinNullable")
                .withType( "JsonNullable<@DecimalMin(\"1\") BigDecimal>" )
                .toType()
                .hasProperty("numberMaxNullable")
                .withType( "JsonNullable<@DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("stringDefaultNullable")
                .withType( "JsonNullable<@Size(max = 1) String>" )
                .toType()
                .fileContains("stringDefaultNullable = JsonNullable.<String>undefined();")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .hasParameter("name")
                .withType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .hasParameter("name")
                .withType("String")
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
                .hasParameter("colors")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .hasParameter("colors")
                .withType("JsonNullable<List<String>>")
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
                .hasParameter("names")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .hasParameter("names")
                .withType("List<String>")
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

                .hasProperty("name")
                .withType( "String" )
                .toType()
                .hasProperty("age")
                .withType( "JsonNullable<Integer>" )
                .toType()
                .hasProperty("alias")
                .withType( "JsonNullable<String>" )
                .toType()
                .hasProperty("color")
                .withType( "Optional<String>" )
                .toType()
                .hasProperty("names")
                .withType( "List<String>" )
                .toType()
                .hasProperty("colors")
                .withType( "JsonNullable<List<String>>" )
                .toType()
                .hasProperty("stringPattern")
                .withType( "Optional<@Pattern(regexp = \"[a-z]\") String>" )
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType( "Optional<@Size(min = 1, max = 10) String>" )
                .toType()
                .hasProperty("stringMinLength")
                .withType( "Optional<@Size(min = 1) String>" )
                .toType()
                .hasProperty("stringMaxLength")
                .withType( "Optional<@Size(max = 1) String>" )
                .toType()
                .hasProperty("stringEmail")
                .withType( "Optional<@jakarta.validation.constraints.Email String>" )
                .toType()
                .hasProperty("intMinMax")
                .withType( "Optional<@Min(1) @Max(10) Integer>" )
                .toType()
                .hasProperty("intMin")
                .withType( "Optional<@Min(1) Integer>" )
                .toType()
                .hasProperty("intMax")
                .withType( "Optional<@Max(10) Integer>" )
                .toType()
                .hasProperty("numberMinMax")
                .withType( "Optional<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("numberMin")
                .withType( "Optional<@DecimalMin(\"1\") BigDecimal>" )
                .toType()
                .hasProperty("numberMax")
                .withType( "Optional<@DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("stringDefault")
                .withType( "Optional<@Size(max = 1) String>" )
                .toType()
                .fileContains("stringDefault = Optional.of(\"ABC\")")
                .hasProperty("zebra")
                .withType( "Optional<Zebra>" )
                .toType()

                .hasProperty("stringPatternNullable")
                .withType( "JsonNullable<@Pattern(regexp = \"[a-z]\") String>" )
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType( "JsonNullable<@Size(min = 1, max = 10) String>" )
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType( "JsonNullable<@Size(min = 1) String>" )
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType( "JsonNullable<@Size(max = 1) String>" )
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType( "JsonNullable<@Min(1) @Max(10) Integer>" )
                .toType()
                .hasProperty("intMinNullable")
                .withType( "JsonNullable<@Min(1) Integer>" )
                .toType()
                .hasProperty("intMaxNullable")
                .withType( "JsonNullable<@Max(10) Integer>" )
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType( "JsonNullable<@DecimalMin(\"1\") @DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("numberMinNullable")
                .withType( "JsonNullable<@DecimalMin(\"1\") BigDecimal>" )
                .toType()
                .hasProperty("numberMaxNullable")
                .withType( "JsonNullable<@DecimalMax(\"10\") BigDecimal>" )
                .toType()
                .hasProperty("stringDefaultNullable")
                .withType( "JsonNullable<@Size(max = 1) String>" )
                .toType()
                .fileContains("stringDefaultNullable = JsonNullable.<String>undefined();")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .hasParameter("name")
                .withType("String")
                .toMethod()
                .toFileAssert()
                 // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .hasParameter("name")
                .withType("String")
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
                .hasParameter("colors")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .hasParameter("colors")
                .withType("JsonNullable<List<String>>")
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
                .hasParameter("names")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .hasParameter("names")
                .withType("List<String>")
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
            assertOptionalMethod(javaFileAssert,"Zebra", "zebra", "Optional<Zebra>");

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

                .hasProperty("name")
                .withType( "String" )
                .toType()
                .hasProperty("age")
                .withType( "Integer" )
                .toType()
                .hasProperty("alias")
                .withType( "String" )
                .toType()
                .hasProperty("color")
                .withType( "String" )
                .toType()
                .hasProperty("names")
                .withType( "List<String>" )
                .toType()
                .hasProperty("colors")
                .withType( "List<String>" )
                .toType()
                .hasProperty("stringPattern")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringEmail")
                .withType( "String" )
                .toType()
                .hasProperty("intMinMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMin")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("numberMinMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMin")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("stringDefault")
                .withType( "String" )
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .hasProperty("zebra")
                .withType( "Zebra" )
                .toType()

                .hasProperty("stringPatternNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMinNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMaxNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMinNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMaxNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("stringDefaultNullable")
                .withType( "String" )
                .toType()
                .fileContains("stringDefaultNullable = null;")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .hasParameter("name")
                .withType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .hasParameter("name")
                .withType("String")
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
                .hasParameter("age")
                .withType("Integer")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setAge")
                .hasReturnType("void")
                .hasParameter("age")
                .withType("Integer")
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
                .hasParameter("colors")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .hasParameter("colors")
                .withType("List<String>")
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
                .hasParameter("names")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .hasParameter("names")
                .withType("List<String>")
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

                .hasProperty("name")
                .withType( "String" )
                .toType()
                .hasProperty("age")
                .withType( "Integer" )
                .toType()
                .hasProperty("alias")
                .withType( "String" )
                .toType()
                .hasProperty("color")
                .withType( "String" )
                .toType()
                .hasProperty("names")
                .withType( "List<String>" )
                .toType()
                .hasProperty("colors")
                .withType( "List<String>" )
                .toType()
                .hasProperty("stringPattern")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMinLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxLength")
                .withType( "String" )
                .toType()
                .hasProperty("stringEmail")
                .withType( "String" )
                .toType()
                .hasProperty("intMinMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMin")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMax")
                .withType( "Integer" )
                .toType()
                .hasProperty("numberMinMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMin")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMax")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("stringDefault")
                .withType( "String" )
                .toType()
                .fileContains("stringDefault = \"ABC\"")
                .hasProperty("zebra")
                .withType( "Zebra" )
                .toType()

                .hasProperty("stringPatternNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType( "String" )
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMinNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("intMaxNullable")
                .withType( "Integer" )
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMinNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("numberMaxNullable")
                .withType( "BigDecimal" )
                .toType()
                .hasProperty("stringDefaultNullable")
                .withType( "String" )
                .toType()
                .fileContains("stringDefaultNullable = null;")

                .assertMethod("name")
                .hasReturnType("Animal")
                .bodyContainsLines("this.name = name;", "return this;")
                .hasParameter("name")
                .withType("String")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setName")
                .hasReturnType("void")
                .hasParameter("name")
                .withType("String")
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
                .hasParameter("age")
                .withType("Integer")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setAge")
                .hasReturnType("void")
                .hasParameter("age")
                .withType("Integer")
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
                .hasParameter("colors")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setColors")
                .hasReturnType("void")
                .hasParameter("colors")
                .withType("List<String>")
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
                .hasParameter("names")
                .withType("List<String>")
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("setNames")
                .hasReturnType("void")
                .hasParameter("names")
                .withType("List<String>")
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
    
    private void assertOptionalMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName, String getterReturnType){
        assertOptionalMethod(javaFileAssert, type.getSimpleName(), expectedName, getterReturnType);
    }

    private void assertOptionalMethod(JavaFileAssert javaFileAssert, String type, String expectedName, String getterReturnType){
        assertWrapperMethod(javaFileAssert, "Optional", type, expectedName, getterReturnType);
    }

    private void assertJsonNullableMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName, String getterReturnType){
        assertJsonNullableMethod(javaFileAssert, type.getSimpleName(), expectedName, getterReturnType);
    }

    private void assertJsonNullableMethod(JavaFileAssert javaFileAssert, String type, String expectedName, String getterReturnType){
        assertWrapperMethod(javaFileAssert, "JsonNullable", type, expectedName, getterReturnType);
    }

    private void assertWrapperMethod(JavaFileAssert javaFileAssert, String wrapperType, String type, String expectedName, String getterReturnType){
        String methodName = StringUtils.capitalize(expectedName);
        javaFileAssert.assertMethod(expectedName)
                .hasReturnType("Animal")
                .bodyContainsLines("this."+expectedName+" = "+wrapperType+".of("+expectedName+");", "return this;")
                .hasParameter(expectedName)
                .withType(type)
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("set"+methodName)
                .hasReturnType("void")
                .hasParameter(expectedName)
                .withType(wrapperType+"<"+type+">")
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("get"+methodName)
                .hasReturnType(getterReturnType)
                .doesNotHaveParameters()
                .toFileAssert();
    }

    private void assertMethod(JavaFileAssert javaFileAssert, String type, String expectedName){
        String methodName = StringUtils.capitalize(expectedName);
        javaFileAssert.assertMethod(expectedName)
                .hasReturnType("Animal")
                .bodyContainsLines("this."+expectedName+" = "+ expectedName + ";", "return this;")
                .hasParameter(expectedName)
                .withType(type)
                .toMethod()
                .toFileAssert()
                // Setter method assertions
                .assertMethod("set"+methodName)
                .hasReturnType("void")
                .hasParameter(expectedName)
                .withType(type)
                .toMethod()
                .toFileAssert()
                // Getter method assertions
                .assertMethod("get"+methodName)
                .hasReturnType(type)
                .doesNotHaveParameters()
                .toFileAssert();
    }

    private void assertMethod(JavaFileAssert javaFileAssert, Class<?> type, String expectedName){
        assertMethod(javaFileAssert,type.getSimpleName(), expectedName);
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
                .assertNoConstructor()
                .assertNoMethod("toString")
                .assertNoMethod("hashCode")
                .assertNoMethod("equals")
                .assertNoMethod("getId")
                .assertNoMethod("setId")
                .assertNoMethod("getName")
                .assertNoMethod("setName")
        ;
        additionalProperties.put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@lombok.ToString");
        output = generateFromContract("src/test/resources/3_0/petstore.yaml", SPRING_BOOT, additionalProperties);
        JavaFileAssert.assertThat(output.get("Pet.java"))
                .assertConstructor().toFileAssert()
                .assertNoMethod("toString")
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
                .fileDoesNotContains("SimpleObject.Builder nullableObject(JsonNullable<String> nullableObject) {");
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
                .fileContains("private List<@Valid TagDto> tags")
                .fileContains("private List<@Valid TagDto> tagsDefaultList = new ArrayList<>()")
                .fileContains("private Set<@Valid TagDto> tagsUnique")
                .fileContains("private Set<@Valid TagDto> tagsDefaultSet = new LinkedHashSet<>();")
                .fileContains("private List<String> stringList")
                .fileContains("private List<String> stringDefaultList = new ArrayList<>(Arrays.asList(\"A\", \"B\"));")
                .fileContains("private List<String> stringEmptyDefaultList = new ArrayList<>();")
                .fileContains("Set<String> stringSet")
                .fileContains("private Set<String> stringDefaultSet = new LinkedHashSet<>(Arrays.asList(\"A\", \"B\"));")
                .fileContains("private Set<String> stringEmptyDefaultSet = new LinkedHashSet<>();")
                .fileDoesNotContains("private List<@Valid TagDto> tags = new ArrayList<>()")
                .fileDoesNotContains("private Set<@Valid TagDto> tagsUnique = new LinkedHashSet<>()")
                .fileDoesNotContains("private List<String> stringList = new ArrayList<>()")
                .fileDoesNotContains("private Set<String> stringSet = new LinkedHashSet<>()");
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
                .hasParameter("updateRequest")
                .withType("Optional<UpdateRequest>")
                .toMethod()
                .toFileAssert();
        JavaFileAssert.assertThat(files.get("TestApi.java"))
                .assertMethod("updatePost")
                .hasParameter("updateRequest")
                .withType("Optional<UpdateRequest>")
                .toMethod()
                .toFileAssert();
    }
}
