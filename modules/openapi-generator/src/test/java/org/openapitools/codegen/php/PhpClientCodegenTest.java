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

package org.openapitools.codegen.php;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class PhpClientCodegenTest {

    protected PhpClientCodegen codegen;

    @BeforeMethod
    public void before() {
        codegen = new PhpClientCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpClientCodegen codegen = new PhpClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "OpenAPI\\Client\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "OpenAPI\\Client\\Model");
        Assert.assertEquals(codegen.apiPackage(), "OpenAPI\\Client\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "OpenAPI\\Client\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "OpenAPI\\Client");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpClientCodegen codegen = new PhpClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.setModelPackage("My\\Client\\Model");
        codegen.setApiPackage("My\\Client\\Api");
        codegen.setInvokerPackage("My\\Client\\Invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "My\\Client\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "My\\Client\\Model");
        Assert.assertEquals(codegen.apiPackage(), "My\\Client\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "My\\Client\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "My\\Client\\Invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "My\\Client\\Invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PhpClientCodegen codegen = new PhpClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "Xmodel");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "Xapi");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "Xinvoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "Xinvoker\\Xmodel");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "Xinvoker\\Xmodel");
        Assert.assertEquals(codegen.apiPackage(), "Xinvoker\\Xapi");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "Xinvoker\\Xapi");
        Assert.assertEquals(codegen.getInvokerPackage(), "Xinvoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "Xinvoker");
    }

    @Test(description = "convert a model with dollar signs")
    public void modelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/dollar-in-names-pull14359.yaml");
        final PhpClientCodegen codegen = new PhpClientCodegen();

        codegen.setOpenAPI(openAPI);
        final CodegenModel simpleName = codegen.fromModel("$DollarModel$", openAPI.getComponents().getSchemas().get("$DollarModel$"));
        Assert.assertEquals(simpleName.name, "$DollarModel$");
        Assert.assertEquals(simpleName.classname, "DollarModel");
        Assert.assertEquals(simpleName.classVarName, "dollar_model");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationEnabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListNotContains(modelContent, a -> a.equals("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");

        List<String> serializerContent = Files
                .readAllLines(files.get("ObjectSerializer.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListContains(serializerContent, a -> a.equals("if (defined(\"$class::UNKNOWN_DEFAULT_OPEN_API\")) {"), "");
        Assert.assertListContains(serializerContent, a -> a.equals("return constant(\"$class::UNKNOWN_DEFAULT_OPEN_API\");"), "");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationDisabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListNotContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListContains(modelContent, a -> a.equalsIgnoreCase("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");

        List<String> serializerContent = Files
                .readAllLines(files.get("ObjectSerializer.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListNotContains(serializerContent, a -> a.equals("return constant(\"$class::UNKNOWN_DEFAULT_OPEN_API\");"), "");
    }
    @Test
    public void testDateTimeLengthValidationIsNotGenerated() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        ObjectSchema model = new ObjectSchema();
        model.addProperties("startsAt", new DateTimeSchema().minLength(20).maxLength(25));
        model.addProperties("title", new StringSchema().minLength(2).maxLength(10));

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("ProductDeal", model);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        String modelPhp = String.join("\n", Files.readAllLines(files.get("ProductDeal.php").toPath()));

        Assert.assertFalse(modelPhp.contains("mb_strlen($this->container['starts_at'])"), modelPhp);
        Assert.assertFalse(modelPhp.contains("mb_strlen($starts_at)"), modelPhp);
        Assert.assertTrue(modelPhp.contains("mb_strlen($this->container['title']) > 10"), modelPhp);
        Assert.assertTrue(modelPhp.contains("mb_strlen($title) > 10"), modelPhp);
    }

    @Test
    public void testInternalVariableNamesAreRenamed() throws IOException {
        // guzzle + snake_case (default): only the spec params whose final name
        // equals a template local in a position where the local wins are
        // renamed; the same names are kept wherever they are harmless
        Path target = Files.createTempDirectory("test");
        try {
            String generated = generatePhpApi("guzzle", null, target);
            assertSignatureEquals(generated, "useThisRequest",
                    "$param_this, string $contentType = self::contentTypes['useThis'][0]",
                    "param `this` must be renamed; `$this` cannot be used as a parameter");
            assertSignatureEquals(generated, "opWithServersRequest",
                    "$param_variables, $host_index = null, ?int $hostIndex = null, "
                            + "array $variables = [], string $contentType = self::contentTypes['opWithServers'][0]",
                    "`variables` duplicates the appended `array $variables`; "
                            + "`hostIndex` becomes `$host_index` and stays safe under snake_case");
            assertSignatureEquals(generated, "noServersRequest",
                    "$variables = null, $host_index = null, $content_type = null, "
                            + "string $contentType = self::contentTypes['noServers'][0]",
                    "without servers nothing is appended, so `variables`/`hostIndex` keep their names");
            assertSignatureEquals(generated, "localsRequest",
                    "$resource_path, $form_params, $query_params, $header_params, $http_body, "
                            + "$param_multipart, string $contentType = self::contentTypes['locals'][0]",
                    "`multipart` is initialized before the params are read; "
                            + "the camelCase-only names stay untouched under snake_case");
            assertSignatureEquals(generated, "groupAssocRequest", "$associative_array",
                    "the x-group-parameters wrapper argument keeps its name");
            Assert.assertTrue(
                    generated.contains("$param_associative_array = array_key_exists('param_associative_array', $associative_array)"),
                    "spec param `associative_array` must be unboxed into $param_associative_array");
            Assert.assertTrue(generated.contains("$q = array_key_exists('q', $associative_array)"),
                    "the wrapper array must still be intact when the next key is unboxed");
            assertSignatureEquals(generated, "noGroupAssocRequest",
                    "$associative_array = null, string $contentType = self::contentTypes['noGroupAssoc'][0]",
                    "without x-group-parameters `associative_array` is a plain param and stays");
            Assert.assertTrue(generated.contains(
                    "$return_type = array_key_exists('return_type', $associative_array)"),
                    "under x-group-parameters `returnType` stays inside the wrapper and is safe");
            Assert.assertTrue(generated.contains(
                    "$content_type = array_key_exists('content_type', $associative_array)"),
                    "snake_case `contentType` becomes $content_type and never meets the $contentType local");
            assertSignatureEquals(generated, "bodyHeadersRequest",
                    "$param_headers, string $contentType = self::contentTypes['bodyHeaders'][0]",
                    "a body param named `headers` must be renamed so isset() reads the caller's value");
            Assert.assertTrue(generated.contains("isset($param_headers)"),
                    "isset() must read the renamed body param");
            assertSignatureEquals(generated, "queryHeadersRequest",
                    "$headers = null, string $contentType = self::contentTypes['queryHeaders'][0]",
                    "a query param named `headers` is consumed before $headers is assigned");
            assertSignatureEquals(generated, "formProcRequest",
                    "$form_data_processor = null, $headers = null, "
                            + "string $contentType = self::contentTypes['formProc'][0]",
                    "snake_case `formDataProcessor` stays safe; a form field `headers` "
                            + "is consumed before $headers is assigned and stays");
            Assert.assertTrue(generated.contains("'headers' => $headers"),
                    "form field `headers` keeps its name and its wire name");
            assertSignatureEquals(generated, "queryFormProcRequest",
                    "$form_data_processor = null, string $contentType = self::contentTypes['queryFormProc'][0]",
                    "a query param named `formDataProcessor` never meets the local");
            assertSignatureEquals(generated, "retTypeRequest",
                    "$return_type, string $contentType = self::contentTypes['retType'][0]",
                    "`returnType` only collides under camelCase and must keep its name");
            assertSignatureEquals(generated, "sigContentTypeRequest",
                    "$content_type, string $contentType = self::contentTypes['sigContentType'][0]",
                    "`contentType` only collides under camelCase and must keep its name");
            assertSignatureEquals(generated, "keyValueRequest",
                    "$key = null, $value = null, string $contentType = self::contentTypes['keyValue'][0]",
                    "guzzle has no $key/$value loop; these names stay");
            assertSignatureEquals(generated, "dupRenameRequest",
                    "$param_multipart, $param_multipart2, "
                            + "string $contentType = self::contentTypes['dupRename'][0]",
                    "the renamed `multipart` must be numbered, not reuse `param_multipart`");
            // names whose locals are only assigned after the caller's value was
            // consumed must not be renamed
            assertSignatureEquals(generated, "safeParamsRequest",
                    "$query, $request = null, $options = null, $response = null, $status_code = null, "
                            + "$data = null, $content = null, $e = null, $exception = null, "
                            + "string $contentType = self::contentTypes['safeParams'][0]",
                    "harmless names must keep their names");

            Assert.assertFalse(generated.contains("$this,"), "no signature may declare $this");
            // the wire names stay unchanged: the caller's value still reaches the wire
            Assert.assertTrue(generated.contains("'this',"), "wire name `this` must be kept");
            Assert.assertTrue(generated.contains("'multipart',"), "wire name `multipart` must be kept");
            Assert.assertTrue(generated.contains("'param_multipart',"),
                    "wire name `param_multipart` must be kept");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testInternalVariableNamesAreRenamedCamelCase() throws IOException {
        // guzzle + camelCase: the camelCase template locals collide with the
        // spec params directly, but still only where the spec param is read
        // after the local was assigned
        Path target = Files.createTempDirectory("test");
        try {
            String generated = generatePhpApi("guzzle", "camelCase", target);
            assertSignatureEquals(generated, "useThisRequest",
                    "$paramThis, string $contentType = self::contentTypes['useThis'][0]",
                    "param `this` must be renamed under camelCase too");
            assertSignatureEquals(generated, "sigContentTypeRequest",
                    "$paramContentType, string $contentType = self::contentTypes['sigContentType'][0]",
                    "duplicate `$contentType` declaration must be resolved");
            assertSignatureEquals(generated, "opWithServersRequest",
                    "$paramVariables, $paramHostIndex = null, ?int $hostIndex = null, "
                            + "array $variables = [], string $contentType = self::contentTypes['opWithServers'][0]",
                    "duplicate `$hostIndex`/`$variables` declarations must be resolved");
            assertSignatureEquals(generated, "noServersRequest",
                    "$variables = null, $hostIndex = null, $paramContentType = null, "
                            + "string $contentType = self::contentTypes['noServers'][0]",
                    "without servers `variables`/`hostIndex` stay safe even under camelCase");
            assertSignatureEquals(generated, "localsRequest",
                    "$paramResourcePath, $paramFormParams, $paramQueryParams, $paramHeaderParams, "
                            + "$paramHttpBody, $paramMultipart, string $contentType = self::contentTypes['locals'][0]",
                    "locals initialized before the params are read must not shadow them");
            Assert.assertTrue(generated.contains(
                    "$associativeArray = array_key_exists('associativeArray', $associative_array)"),
                    "camelCase `associative_array` becomes $associativeArray and never meets "
                            + "the $associative_array wrapper");
            assertSignatureEquals(generated, "noGroupAssocRequest",
                    "$associativeArray = null, string $contentType = self::contentTypes['noGroupAssoc'][0]",
                    "without x-group-parameters there is no $associative_array local at all");
            Assert.assertTrue(generated.contains(
                    "$returnType = array_key_exists('returnType', $associative_array)"),
                    "under x-group-parameters `returnType` stays inside the wrapper and is safe");
            Assert.assertTrue(generated.contains(
                    "$paramContentType = array_key_exists('paramContentType', $associative_array)"),
                    "camelCase `contentType` collides with the unboxed $contentType and is renamed");
            assertSignatureEquals(generated, "bodyHeadersRequest",
                    "$paramHeaders, string $contentType = self::contentTypes['bodyHeaders'][0]",
                    "a body param named `headers` must be renamed");
            Assert.assertTrue(generated.contains("isset($paramHeaders)"),
                    "isset() must read the renamed body param");
            assertSignatureEquals(generated, "queryHeadersRequest",
                    "$headers = null, string $contentType = self::contentTypes['queryHeaders'][0]",
                    "a query param named `headers` stays safe under camelCase too");
            assertSignatureEquals(generated, "formProcRequest",
                    "$paramFormDataProcessor = null, $headers = null, "
                            + "string $contentType = self::contentTypes['formProc'][0]",
                    "a form field named `formDataProcessor` must be renamed under camelCase; "
                            + "a form field `headers` stays safe");
            assertSignatureEquals(generated, "queryFormProcRequest",
                    "$formDataProcessor = null, string $contentType = self::contentTypes['queryFormProc'][0]",
                    "a query param named `formDataProcessor` stays safe under camelCase too");
            assertSignatureEquals(generated, "retTypeAsyncWithHttpInfo",
                    "$paramReturnType, string $contentType = self::contentTypes['retType'][0]",
                    "the async forwarder assigns $returnType before forwarding the params");
            Assert.assertTrue(generated.contains("$this->retTypeRequest($paramReturnType,"),
                    "the renamed param must be forwarded to the request builder");
            assertSignatureEquals(generated, "keyValueRequest",
                    "$key = null, $value = null, string $contentType = self::contentTypes['keyValue'][0]",
                    "guzzle has no $key/$value loop; these names stay");
            assertSignatureEquals(generated, "dupRenameRequest",
                    "$paramMultipart, $paramMultipart2, "
                            + "string $contentType = self::contentTypes['dupRename'][0]",
                    "the renamed `multipart` must be numbered under camelCase too");
            // harmless names keep their names under camelCase too
            assertSignatureEquals(generated, "safeParamsRequest",
                    "$query, $request = null, $options = null, $response = null, $statusCode = null, "
                            + "$data = null, $content = null, $e = null, $exception = null, "
                            + "string $contentType = self::contentTypes['safeParams'][0]",
                    "harmless names must keep their names");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testInternalVariableNamesAreRenamedOriginal() throws IOException {
        // guzzle + original: spec names are used as-is, so every camelCase
        // local name collides verbatim
        Path target = Files.createTempDirectory("test");
        try {
            String generated = generatePhpApi("guzzle", "original", target);
            assertSignatureEquals(generated, "useThisRequest",
                    "$param_this, string $contentType = self::contentTypes['useThis'][0]",
                    "param `this` must be renamed under original too");
            assertSignatureEquals(generated, "sigContentTypeRequest",
                    "$param_contentType, string $contentType = self::contentTypes['sigContentType'][0]",
                    "duplicate `$contentType` declaration must be resolved");
            assertSignatureEquals(generated, "opWithServersRequest",
                    "$param_variables, $param_hostIndex = null, ?int $hostIndex = null, "
                            + "array $variables = [], string $contentType = self::contentTypes['opWithServers'][0]",
                    "duplicate `$hostIndex`/`$variables` declarations must be resolved");
            assertSignatureEquals(generated, "noServersRequest",
                    "$variables = null, $hostIndex = null, $param_contentType = null, "
                            + "string $contentType = self::contentTypes['noServers'][0]",
                    "without servers `variables`/`hostIndex` stay safe under original too");
            assertSignatureEquals(generated, "localsRequest",
                    "$param_resourcePath, $param_formParams, $param_queryParams, $param_headerParams, "
                            + "$param_httpBody, $param_multipart, string $contentType = self::contentTypes['locals'][0]",
                    "locals initialized before the params are read must not shadow them");
            assertSignatureEquals(generated, "noGroupAssocRequest",
                    "$associative_array = null, string $contentType = self::contentTypes['noGroupAssoc'][0]",
                    "without x-group-parameters `associative_array` is a plain param and stays");
            assertSignatureEquals(generated, "bodyHeadersRequest",
                    "$param_headers, string $contentType = self::contentTypes['bodyHeaders'][0]",
                    "a body param named `headers` must be renamed");
            assertSignatureEquals(generated, "queryHeadersRequest",
                    "$headers = null, string $contentType = self::contentTypes['queryHeaders'][0]",
                    "a query param named `headers` stays safe under original too");
            assertSignatureEquals(generated, "formProcRequest",
                    "$param_formDataProcessor = null, $headers = null, "
                            + "string $contentType = self::contentTypes['formProc'][0]",
                    "a form field named `formDataProcessor` must be renamed under original");
            assertSignatureEquals(generated, "retTypeAsyncWithHttpInfo",
                    "$param_returnType, string $contentType = self::contentTypes['retType'][0]",
                    "the async forwarder assigns $returnType before forwarding the params");
            assertSignatureEquals(generated, "keyValueRequest",
                    "$key = null, $value = null, string $contentType = self::contentTypes['keyValue'][0]",
                    "guzzle has no $key/$value loop; these names stay");
            assertSignatureEquals(generated, "dupRenameRequest",
                    "$param_multipart, $param_multipart2, "
                            + "string $contentType = self::contentTypes['dupRename'][0]",
                    "the renamed `multipart` must be numbered under original too");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testInternalVariableParameterNameMappingsWin() throws IOException {
        // a name the user chose explicitly through parameterNameMappings wins
        // over the collision handling and is never renamed - even when the
        // mapped name itself collides with a template local
        Path target = Files.createTempDirectory("test");
        try {
            Map<String, String> mappings = new java.util.HashMap<>();
            mappings.put("this", "this");
            mappings.put("multipart", "queryParams");
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("guzzle")
                    .setInputSpec("src/test/resources/3_0/php/php-internal-var-collision.yaml")
                    .setParameterNameMappings(mappings)
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            Path apiPath = target.resolve("lib/Api/DefaultApi.php");
            TestUtils.assertFileExists(apiPath);
            String generated = new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);

            assertSignatureEquals(generated, "useThisRequest",
                    "$this, string $contentType = self::contentTypes['useThis'][0]",
                    "the parameterNameMappings result `this` must be kept, "
                            + "not renamed to param_this");
            assertSignatureEquals(generated, "dupRenameRequest",
                    "$param_multipart, $queryParams, "
                            + "string $contentType = self::contentTypes['dupRename'][0]",
                    "the parameterNameMappings result `queryParams` must be kept "
                            + "even though it collides with the $queryParams local");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testPsr18InternalVariableNamesAreRenamed() throws IOException {
        // psr-18 shares the xxxRequest() locals and the x-group unboxing with
        // guzzle but appends no signature parameters; the explode foreach uses
        // collision-proof loop variables, so `key`/`value` keep their names
        Path target = Files.createTempDirectory("test");
        try {
            String generated = generatePhpApi("psr-18", null, target);
            assertSignatureEquals(generated, "psrSigRequest",
                    "$content_type, $host_index = null, $variables = null",
                    "psr-18 has no appended signature params; these names must stay");
            assertSignatureEquals(generated, "psrKvRequest",
                    "$aaa = null, $key = null, $value = null",
                    "the explode foreach uses $localVariableKey/$localVariableValue, "
                            + "so `key`/`value` keep their names");
            Assert.assertTrue(generated.contains("as $localVariableKey => $localVariableValue"),
                    "the explode foreach must use collision-proof loop variables");
            Assert.assertTrue(generated.contains("$queryParams['key'] = $key"),
                    "param `key` must be sent under its own name");
            Assert.assertFalse(generated.contains("$queryParams[$key] = $value"),
                    "the explode foreach must not feed $queryParams from $key/$value "
                            + "loop variables");
            assertSignatureEquals(generated, "psrLocalsRequest",
                    "$param_this, $param_multipart, $resource_path",
                    "psr-18 shares the xxxRequest() locals with the guzzle template");
            assertSignatureEquals(generated, "psrBodyHeadersRequest", "$param_headers",
                    "a body param named `headers` must be renamed");
            assertSignatureEquals(generated, "psrFormProcRequest",
                    "$form_data_processor = null, $headers = null",
                    "snake_case `formDataProcessor` stays safe; a form field `headers` stays");
            assertSignatureEquals(generated, "psrRetTypeRequest", "$return_type",
                    "snake_case `returnType` stays safe");
            assertSignatureEquals(generated, "psrGroupAssocRequest", "$associative_array",
                    "the x-group-parameters wrapper argument keeps its name");
            Assert.assertTrue(generated.contains(
                    "$param_associative_array = array_key_exists('param_associative_array', $associative_array)"),
                    "spec param `associative_array` must be unboxed into $param_associative_array");
            Assert.assertTrue(generated.contains("$q = array_key_exists('q', $associative_array)"),
                    "the param unboxed after `associative_array` must read the intact wrapper");
            assertSignatureEquals(generated, "psrGroupAssocLastRequest", "$associative_array",
                    "the x-group-parameters wrapper argument keeps its name");
            Assert.assertTrue(generated.contains(
                    "$associative_array = array_key_exists('associative_array', $associative_array)"),
                    "psr-18 never reads the wrapper after the unboxing, so the param unboxed "
                            + "last must keep its name");
            assertSignatureEquals(generated, "psrLoopVarRequest",
                    "$aaa = null, $local_variable_key = null, $local_variable_value = null",
                    "snake_case `localVariableKey`/`localVariableValue` never meet the "
                            + "$localVariableKey/$localVariableValue loop variables");
            assertSignatureEquals(generated, "psrNoGroupAssocRequest", "$associative_array = null",
                    "without x-group-parameters `associative_array` is a plain param and stays");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testPsr18InternalVariableNamesAreRenamedCamelCase() throws IOException {
        // under camelCase the guzzle-only names (contentType/hostIndex/
        // variables) must still NOT be renamed in psr-18, while the shared
        // locals do collide
        Path target = Files.createTempDirectory("test");
        try {
            String generated = generatePhpApi("psr-18", "camelCase", target);
            assertSignatureEquals(generated, "psrSigRequest",
                    "$contentType, $hostIndex = null, $variables = null",
                    "psr-18 has no appended signature params; even under camelCase "
                            + "the guzzle-only names must stay");
            assertSignatureEquals(generated, "psrKvRequest",
                    "$aaa = null, $key = null, $value = null",
                    "`key`/`value` keep their names under camelCase too");
            Assert.assertTrue(generated.contains("as $localVariableKey => $localVariableValue"),
                    "the explode foreach must use collision-proof loop variables");
            assertSignatureEquals(generated, "psrLocalsRequest",
                    "$paramThis, $paramMultipart, $paramResourcePath",
                    "the shared xxxRequest() locals collide under camelCase");
            assertSignatureEquals(generated, "psrBodyHeadersRequest", "$paramHeaders",
                    "a body param named `headers` must be renamed");
            assertSignatureEquals(generated, "psrFormProcRequest",
                    "$paramFormDataProcessor = null, $headers = null",
                    "a form field `formDataProcessor` collides under camelCase; "
                            + "a form field `headers` stays");
            assertSignatureEquals(generated, "psrRetTypeAsyncWithHttpInfo", "$paramReturnType",
                    "psr-18 emits the async forwarder too, so `returnType` collides");
            Assert.assertTrue(generated.contains(
                    "$associativeArray = array_key_exists('associativeArray', $associative_array)"),
                    "camelCase `associative_array` stays safe inside the wrapper");
            assertSignatureEquals(generated, "psrLoopVarRequest",
                    "$aaa = null, $paramLocalVariableKey = null, $paramLocalVariableValue = null",
                    "camelCase `localVariableKey`/`localVariableValue` collide with the "
                            + "explode foreach loop variables and must be renamed");
            assertSignatureEquals(generated, "psrGroupAssocLastRequest", "$associative_array",
                    "the x-group-parameters wrapper argument keeps its name");
            assertSignatureEquals(generated, "psrNoGroupAssocRequest", "$associativeArray = null",
                    "without x-group-parameters `associative_array` stays");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    /**
     * Generates the php api class for {@code php-internal-var-collision.yaml}
     * (guzzle) or {@code php-psr18-internal-var-collision.yaml} (psr-18) into
     * {@code target} and returns the generated DefaultApi.php contents.
     */
    private static String generatePhpApi(String library, String namingConvention, Path target)
            throws IOException {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php")
                .setLibrary(library)
                .setInputSpec("src/test/resources/3_0/php/"
                        + ("psr-18".equals(library)
                            ? "php-psr18-internal-var-collision.yaml"
                            : "php-internal-var-collision.yaml"))
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        if (namingConvention != null) {
            configurator.addAdditionalProperty("variableNamingConvention", namingConvention);
        }
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path apiPath = target.resolve("lib/Api/DefaultApi.php");
        TestUtils.assertFileExists(apiPath);
        return new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);
    }

    /**
     * Asserts the exact parameter list of {@code function <name>(...)} in the
     * generated api class. Matching the full signature guards both the renames
     * and the absence of duplicates.
     */
    private static void assertSignatureEquals(String generated, String functionName,
            String expectedParams, String message) {
        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("function " + functionName + "\\(([^)]*)\\)")
                .matcher(generated);
        Assert.assertTrue(m.find(), functionName + " signature should exist");
        Assert.assertEquals(m.group(1), expectedParams, message + " — signature was: " + m.group(1));
    }
}