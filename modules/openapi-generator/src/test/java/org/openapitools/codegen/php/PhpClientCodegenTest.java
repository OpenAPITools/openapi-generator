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
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

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
    public void testGuzzleOpenApi32OperationsAndQueryStringParam() throws IOException {
        Path target = Files.createTempDirectory("test");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("guzzle")
                    .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            Path apiPath = target.resolve("lib/Api/DefaultApi.php");
            TestUtils.assertFileExists(apiPath);
            String generated = new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);
            // non-standard methods go through a verbatim subclass so psr7's
            // strtoupper cannot mangle them
            for (String method : new String[]{"QUERY", "PURGE", "customMethod", "CHECK&FETCH", "X#Y"}) {
                Assert.assertTrue(generated.contains("'" + method + "'"),
                        "expected verbatim method literal for " + method);
            }
            Assert.assertTrue(generated.contains("new class("), "verbatim Request subclass expected");
            Assert.assertTrue(generated.contains("'GET'"), "standard method kept on plain Request");
            // `in: querystring` appends verbatim with ?/& handling, not via query params;
            // the accumulator is $__-prefixed so a parameter named `uri` cannot shadow it
            Assert.assertTrue(generated.contains("(str_contains($__requestUri, '?') ? '&' : '?') . $qs"),
                    "querystring param should be appended verbatim");
            Assert.assertTrue(generated.contains(". $uri;"),
                    "a querystring parameter literally named `uri` must still reach the wire");
            Assert.assertFalse(generated.contains("toQueryValue(\n            $qs"),
                    "querystring param must not be serialized as a name=value pair");
            // QUERY may carry a body; REPORT/PROPPATCH need one on some stacks
            Assert.assertTrue(generated.contains("'REPORT'"), "REPORT should emit a verbatim literal");
            Path composerPath = target.resolve("composer.json");
            String composer = new String(Files.readAllBytes(composerPath), StandardCharsets.UTF_8);
            // psr7 < 2.10 Utils::modifyRequest() rebuilds a plain Request and
            // drops the verbatim subclass - pin the floor high enough
            Assert.assertTrue(composer.contains("\"guzzlehttp/psr7\": \"^2.10\""),
                    "psr7 constraint must exclude versions that rebuild plain Requests");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testPsr18SkipsOpenApi32Operations() throws IOException {
        Path target = Files.createTempDirectory("test");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("psr-18")
                    .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            Path apiPath = target.resolve("lib/Api/DefaultApi.php");
            TestUtils.assertFileExists(apiPath);
            String generated = new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);
            Assert.assertTrue(generated.contains("function listPets"), "GET operation should be kept");
            for (String op : new String[]{"queryPets", "purgePets", "customPets", "checkFetchPets", "hashPets"}) {
                Assert.assertFalse(generated.contains("function " + op),
                        "psr-18 must skip unsupported 3.2 operation " + op);
            }
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testGuzzleSkipsInvalidHttpMethodToken() throws IOException {
        Path target = Files.createTempDirectory("test");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("guzzle")
                    .setInputSpec("src/test/resources/3_2/rust-invalid-method.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            Path apiPath = target.resolve("lib/Api/DefaultApi.php");
            TestUtils.assertFileExists(apiPath);
            String generated = new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);
            Assert.assertTrue(generated.contains("function listPets"), "GET operation should be kept");
            Assert.assertFalse(generated.contains("badMethod"),
                    "invalid RFC 9110 method token must be skipped");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    @Test
    public void testGuzzleWebhookOperationsEmitVerbatimMethods() throws IOException {
        Path target = Files.createTempDirectory("test");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("guzzle")
                    .setInputSpec("src/test/resources/3_2/go-webhook-operations.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            Path apiPath = target.resolve("lib/Api/DefaultApi.php");
            TestUtils.assertFileExists(apiPath);
            String generated = new String(Files.readAllBytes(apiPath), StandardCharsets.UTF_8);
            Assert.assertTrue(generated.contains("'QUERY'"),
                    "webhook query operation should emit a verbatim QUERY method");
            Assert.assertTrue(generated.contains("'customMethod'"),
                    "webhook additionalOperations should emit a verbatim method");
        } finally {
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    /**
     * End-to-end check: runs the generated guzzle client against a raw TCP
     * capture listener, verifying query/additionalOperations methods and
     * `in: querystring` reach the wire verbatim. Skipped when php or composer
     * is unavailable, or when packagist cannot be reached.
     */
    @Test
    public void testGuzzleGeneratedClientSendsVerbatimMethods() throws IOException, InterruptedException {
        if (!isCommandAvailable("php", "--version")) {
            throw new org.testng.SkipException("php is not on PATH; skipping generated-client verification");
        }
        Path target = Files.createTempDirectory("php32-verify");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("php")
                    .setLibrary("guzzle")
                    .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

            ProcessBuilder composer = composerInstallCommand(target.toFile());
            if (composer == null || runAndCheck(composer, 300) != 0) {
                throw new org.testng.SkipException(
                        "composer install failed (composer missing or packagist unreachable)");
            }

            Path capture = target.resolve("capture.php");
            Files.copy(Path.of("src/test/resources/3_2/php-guzzle-capture/capture.php"), capture);

            Process p = new ProcessBuilder("php", "capture.php",
                            target.toAbsolutePath().toString())
                    .directory(target.toFile())
                    .redirectErrorStream(true)
                    .start();
            boolean finished = p.waitFor(90, java.util.concurrent.TimeUnit.SECONDS);
            if (!finished) {
                p.destroyForcibly();
            }
            String output = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            Assert.assertTrue(finished, "php capture timed out:\n" + output);
            Assert.assertTrue(output.contains("CAPTURE-PASS"),
                    "generated client did not send verbatim 3.2 methods/querystring:\n" + output);
        } finally {
            // composer's vendor dir is tens of MB; deleteOnExit cannot remove non-empty dirs
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    private ProcessBuilder composerInstallCommand(File dir) {
        if (isCommandAvailable("composer", "--version")) {
            return new ProcessBuilder("composer", "install", "--quiet", "--no-interaction",
                    "--no-dev", "--prefer-dist")
                    .directory(dir).redirectErrorStream(true);
        }
        String phar = System.getenv("COMPOSER_PHAR");
        if (phar != null && Files.isRegularFile(Path.of(phar))) {
            return new ProcessBuilder("php", phar, "install", "--quiet", "--no-interaction",
                    "--no-dev", "--prefer-dist")
                    .directory(dir).redirectErrorStream(true);
        }
        return null;
    }

    private int runAndCheck(ProcessBuilder pb, long timeoutSeconds) throws IOException, InterruptedException {
        Process p = pb.start();
        if (!p.waitFor(timeoutSeconds, java.util.concurrent.TimeUnit.SECONDS)) {
            p.destroyForcibly();
            return -1;
        }
        p.getInputStream().transferTo(java.io.OutputStream.nullOutputStream());
        return p.exitValue();
    }

    private boolean isCommandAvailable(String... command) {
        try {
            Process p = new ProcessBuilder(command)
                    .redirectErrorStream(true).start();
            // wait before draining: a child that never exits would otherwise block
            // the stream read forever
            if (!p.waitFor(10, java.util.concurrent.TimeUnit.SECONDS)) {
                p.destroyForcibly();
                return false;
            }
            p.getInputStream().transferTo(java.io.OutputStream.nullOutputStream());
            return p.exitValue() == 0;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }
}