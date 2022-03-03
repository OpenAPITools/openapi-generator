package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.*;

public class DefaultGeneratorTest {

    @Test
    public void testIgnoreFileProcessing() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            List<String> ignoreFile = Arrays.asList(
                    ".travis.yml",
                    "build.sbt",
                    "src/main/AndroidManifest.xml",
                    "pom.xml",
                    "src/test/**",
                    "src/main/java/org/openapitools/client/api/UserApi.java"
            );
            File ignorePath = new File(output, ".openapi-generator-ignore");
            Files.write(ignorePath.toPath(),
                    String.join(System.lineSeparator(), ignoreFile).getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("java")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 44);

            // Check expected generated files
            // api sanity check
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/api/PetApi.java");
            Assert.assertTrue(new File(output, "src/main/java/org/openapitools/client/api/PetApi.java").exists());

            // model sanity check
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/model/Category.java");
            Assert.assertTrue(new File(output, "src/main/java/org/openapitools/client/model/Category.java").exists());

            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/model/ModelApiResponse.java");
            Assert.assertTrue(new File(output, "src/main/java/org/openapitools/client/model/ModelApiResponse.java").exists());

            // supporting files sanity check
            TestUtils.ensureContainsFile(files, output, "build.gradle");
            Assert.assertTrue(new File(output, "build.gradle").exists());

            TestUtils.ensureContainsFile(files, output, "api/openapi.yaml");
            Assert.assertTrue(new File(output, "build.gradle").exists());

            // Check excluded files
            TestUtils.ensureDoesNotContainsFile(files, output, ".travis.yml");
            Assert.assertFalse(new File(output, ".travis.yml").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, "build.sbt");
            Assert.assertFalse(new File(output, "build.sbt").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, "src/main/AndroidManifest.xml");
            Assert.assertFalse(new File(output, "src/main/AndroidManifest.xml").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, "pom.xml");
            Assert.assertFalse(new File(output, "pom.xml").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, "src/test/java/org/openapitools/client/model/CategoryTest.java");
            Assert.assertFalse(new File(output, "src/test/java/org/openapitools/client/model/CategoryTest.java").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, "src/main/java/org/openapitools/client/api/UserApi.java");
            Assert.assertFalse(new File(output, "src/main/java/org/openapitools/client/api/UserApi.java").exists());
        } finally {
            output.delete();
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
    public void testFilesAreNeverOverwritten() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("java")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            // Create "existing" files
            String apiTestRelativePath = "src/test/java/org/openapitools/client/api/PetApiTest.java";
            String modelTestRelativePath = "src/test/java/org/openapitools/client/model/CategoryTest.java";

            File apiTestFile = new File(output, apiTestRelativePath);
            new File(apiTestFile.getParent()).mkdirs();
            Files.write(apiTestFile.toPath(),
                    "empty".getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            File modelTestFile = new File(output, modelTestRelativePath);
            new File(modelTestFile.getParent()).mkdirs();
            Files.write(modelTestFile.toPath(),
                    "empty".getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "true");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 16);

            // Check API is written and Test is not
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/api/PetApi.java");
            Assert.assertTrue(new File(output, "src/main/java/org/openapitools/client/api/PetApi.java").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, apiTestRelativePath);
            Assert.assertTrue(apiTestFile.exists());
            String apiTestContents = Files.readAllLines(apiTestFile.toPath()).get(0);
            Assert.assertEquals(apiTestContents, "empty", "Expected test file to retain original contents.");

            // Check Model is written and Test is not
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/model/Category.java");
            Assert.assertTrue(new File(output, "src/test/java/org/openapitools/client/model/CategoryTest.java").exists());

            TestUtils.ensureDoesNotContainsFile(files, output, modelTestRelativePath);
            Assert.assertTrue(modelTestFile.exists());
            String modelTestContents = Files.readAllLines(modelTestFile.toPath()).get(0);
            Assert.assertEquals(modelTestContents, "empty", "Expected test file to retain original contents.");
        } finally {
            output.delete();
        }
    }

    @Test
    public void dryRunWithApisOnly() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("java")
                    .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(true);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 1);
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/api/PingApi.java");
        } finally {
            output.delete();
        }
    }

    @Test
    public void dryRunWithModelsOnly() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("java")
                    .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(true);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 1);
            TestUtils.ensureContainsFile(files, output, "src/main/java/org/openapitools/client/model/SomeObj.java");
        } finally {
            output.delete();
        }
    }

    @Test
    public void dryRunWithSupportFilesSelections() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("java")
                    .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(true);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<String> filesToGenerate = Arrays.asList(
                    "pom.xml",
                    ".travis.yml",
                    ".gitignore",
                    "git_push.sh"
            );
            GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, String.join(",", filesToGenerate));

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 5);

            TestUtils.ensureContainsFile(files, output, "pom.xml");
            TestUtils.ensureContainsFile(files, output, ".travis.yml");
            TestUtils.ensureContainsFile(files, output, ".gitignore");
            TestUtils.ensureContainsFile(files, output, "git_push.sh");
            TestUtils.ensureContainsFile(files, output, ".openapi-generator/VERSION");
        } finally {
            GlobalSettings.reset();
            output.delete();
        }
    }

    @Test
    public void supportCustomTemplateEngine() throws IOException {
        Path target = Files.createTempDirectory("test");
        File templateDir = new File(target.toFile(), "template");
        Files.createDirectory(templateDir.toPath());
        File output = new File(target.toFile(), "out");
        Files.createDirectory(output.toPath());

        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("jmeter")
                    .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                    .setSkipOverwrite(false)
                    .setTemplateDir(templateDir.toPath().toAbsolutePath().toString())
                    .setTemplatingEngineName("handlebars")
                    .setOutputDir(output.toPath().toAbsolutePath().toString());

            // Create custom template directory
            Files.copy(
                    Objects.requireNonNull(this.getClass().getClassLoader().getResourceAsStream("templating/templates/jmeter/api.hbs")),
                    new File(templateDir, "api.hbs").toPath());
            Files.copy(
                    Objects.requireNonNull(this.getClass().getClassLoader().getResourceAsStream("templating/templates/jmeter/testdata-localhost.hbs")),
                    new File(templateDir, "testdata-localhost.hbs").toPath());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 5);

            // Check API is written and Test is not
            TestUtils.ensureContainsFile(files, output, "PingApi.jmx");
            Assert.assertTrue(new File(output, "PingApi.jmx").exists());

            TestUtils.ensureContainsFile(files, output, "PingApi.csv");
            Assert.assertTrue(new File(output, "PingApi.csv").exists());

            TestUtils.ensureContainsFile(files, output, ".openapi-generator-ignore");
            Assert.assertTrue(new File(output, ".openapi-generator-ignore").exists());

            TestUtils.ensureContainsFile(files, output, ".openapi-generator/VERSION");
            Assert.assertTrue(new File(output, ".openapi-generator/VERSION").exists());

            TestUtils.ensureContainsFile(files, output, ".openapi-generator/FILES");
            Assert.assertTrue(new File(output, ".openapi-generator/FILES").exists());

            TestUtils.assertFileContains(java.nio.file.Paths.get(output + "/PingApi.jmx"), "PingApi Test Plan via Handlebars");
            TestUtils.assertFileContains(java.nio.file.Paths.get(output + "/PingApi.csv"),
                    "testCase,httpStatusCode,someObj",
                    "Success,200,0");
        } finally {
            output.delete();
        }
    }

    @Test
    public void testNonStrictProcessPaths() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.setPaths(new Paths());
        openAPI.getPaths().addPathItem("path1/", new PathItem().get(new Operation().operationId("op1").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));
        openAPI.getPaths().addPathItem("path2/", new PathItem().get(new Operation().operationId("op2").addParametersItem(new QueryParameter().name("p1").schema(new StringSchema())).responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));

        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        CodegenConfig config = new DefaultCodegen();
        config.setStrictSpecBehavior(false);
        opts.config(config);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        Map<String, List<CodegenOperation>> result = generator.processPaths(openAPI.getPaths());
        Assert.assertEquals(result.size(), 1);
        List<CodegenOperation> defaultList = result.get("Default");
        Assert.assertEquals(defaultList.size(), 2);
        Assert.assertEquals(defaultList.get(0).path, "path1/");
        Assert.assertEquals(defaultList.get(0).allParams.size(), 0);
        Assert.assertEquals(defaultList.get(1).path, "path2/");
        Assert.assertEquals(defaultList.get(1).allParams.size(), 1);
    }

    @Test
    public void testProcessPaths() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.setPaths(new Paths());
        openAPI.getPaths().addPathItem("/path1", new PathItem().get(new Operation().operationId("op1").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));
        openAPI.getPaths().addPathItem("/path2", new PathItem().get(new Operation().operationId("op2").addParametersItem(new QueryParameter().name("p1").schema(new StringSchema())).responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));
        openAPI.getPaths().addPathItem("/path3", new PathItem().addParametersItem(new QueryParameter().name("p1").schema(new StringSchema())).get(new Operation().operationId("op3").addParametersItem(new QueryParameter().name("p2").schema(new IntegerSchema())).responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));
        openAPI.getPaths().addPathItem("/path4", new PathItem().addParametersItem(new QueryParameter().name("p1").schema(new StringSchema())).get(new Operation().operationId("op4").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")))));

        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        opts.config(new DefaultCodegen());

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        Map<String, List<CodegenOperation>> result = generator.processPaths(openAPI.getPaths());
        Assert.assertEquals(result.size(), 1);
        List<CodegenOperation> defaultList = result.get("Default");
        Assert.assertEquals(defaultList.size(), 4);
        Assert.assertEquals(defaultList.get(0).path, "/path1");
        Assert.assertEquals(defaultList.get(0).allParams.size(), 0);
        Assert.assertEquals(defaultList.get(1).path, "/path2");
        Assert.assertEquals(defaultList.get(1).allParams.size(), 1);
        Assert.assertEquals(defaultList.get(2).path, "/path3");
        Assert.assertEquals(defaultList.get(2).allParams.size(), 2);
        Assert.assertEquals(defaultList.get(3).path, "/path4");
        Assert.assertEquals(defaultList.get(3).allParams.size(), 1);
    }

    @Test
    public void testRefModelValidationProperties() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/refAliasedPrimitiveWithValidation.yml");
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setStrictSpecBehavior(false);
        opts.config(config);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);

        String expectedPattern = "^\\d{3}-\\d{2}-\\d{4}$";
        // NOTE: double-escaped regex for when the value is intended to be dumped in template into a String location.
        String escapedPattern = config.toRegularExpression(expectedPattern);

        Schema stringRegex = openAPI.getComponents().getSchemas().get("StringRegex");
        // Sanity check.
        Assert.assertEquals(stringRegex.getPattern(), expectedPattern);

        // Validate when we alias/unalias
        Schema unaliasedStringRegex = ModelUtils.unaliasSchema(openAPI, stringRegex);
        Assert.assertEquals(unaliasedStringRegex.getPattern(), expectedPattern);

        // Validate when converting to property
        CodegenProperty stringRegexProperty = config.fromProperty("stringRegex", stringRegex);
        Assert.assertEquals(stringRegexProperty.pattern, escapedPattern);

        // Validate when converting to parameter
        Operation operation = openAPI.getPaths().get("/fake/StringRegex").getPost();
        RequestBody body = operation.getRequestBody();
        CodegenParameter codegenParameter = config.fromRequestBody(body, new HashSet<>(), "body");

        Assert.assertEquals(codegenParameter.pattern, escapedPattern);

        // Validate when converting to response
        ApiResponse response = operation.getResponses().get("200");
        CodegenResponse codegenResponse = config.fromResponse("200", response);

        Assert.assertEquals(((Schema) codegenResponse.schema).getPattern(), expectedPattern);
        Assert.assertEquals(codegenResponse.pattern, escapedPattern);
    }

    @Test
    public void testBuiltinLibraryTemplates() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("kotlin")
                    .setLibrary("jvm-okhttp4")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 26);

            // Generator should report a library templated file as a generated file
            TestUtils.ensureContainsFile(files, output, "src/main/kotlin/org/openapitools/client/infrastructure/Errors.kt");

            // Generated file should exist on the filesystem after generation
            File generatedFile = new File(output, "src/main/kotlin/org/openapitools/client/infrastructure/Errors.kt");
            Assert.assertTrue(generatedFile.exists());

            // Generated file should contain some expected text
            TestUtils.assertFileContains(generatedFile.toPath(), "package org.openapitools.client.infrastructure",
                    "open class ClientException",
                    "open class ServerException");
        } finally {
            output.delete();
        }
    }

    @Test
    public void testBuiltinNonLibraryTemplates() throws IOException {
        Path target = Files.createTempDirectory("test");
        File output = target.toFile();
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("kotlin")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 26);

            // Generator should report README.md as a generated file
            TestUtils.ensureContainsFile(files, output, "README.md");

            // Generated file should exist on the filesystem after generation
            File readme = new File(output, "README.md");
            Assert.assertTrue(readme.exists());

            // README.md should contain some expected text
            TestUtils.assertFileContains(readme.toPath(), "# org.openapitools.client - Kotlin client library for OpenAPI Petstore",
                    "## Requires",
                    "## Build",
                    "## Features/Implementation Notes");
        } finally {
            output.delete();
        }
    }

    @Test
    public void testCustomLibraryTemplates() throws IOException {
        Path target = Files.createTempDirectory("test");
        Path templates = Files.createTempDirectory("templates");
        File output = target.toFile();
        try {
            // Create custom template
            File customTemplate = new File(templates.toFile(), "libraries/jvm-okhttp/infrastructure/Errors.kt.mustache");
            new File(customTemplate.getParent()).mkdirs();
            StringBuilder sb = new StringBuilder();
            sb.append("// {{someKey}}").append("\n");
            sb.append("@file:Suppress(\"unused\")").append("\n");
            sb.append("package org.openapitools.client.infrastructure").append("\n");
            sb.append("import java.lang.RuntimeException").append("\n");
            sb.append("open class CustomException(").append("\n");
            sb.append("  message: kotlin.String? = null, val statusCode: Int = -1, val response: Response? = null) : RuntimeException(message) {").append("\n");
            sb.append("    companion object {").append("\n");
            sb.append("      private const val serialVersionUID: Long = 789L").append("\n");
            sb.append("    }").append("\n");
            sb.append("}").append("\n");
            Files.write(customTemplate.toPath(),
                    sb.toString().getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("kotlin")
                    .addAdditionalProperty("someKey", "testCustomLibraryTemplates")
                    .setTemplateDir(templates.toAbsolutePath().toString())
                    .setLibrary("jvm-okhttp4")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 26);

            // Generator should report a library templated file as a generated file
            TestUtils.ensureContainsFile(files, output, "src/main/kotlin/org/openapitools/client/infrastructure/Errors.kt");

            // Generated file should exist on the filesystem after generation
            File readme = new File(output, "src/main/kotlin/org/openapitools/client/infrastructure/Errors.kt");
            Assert.assertTrue(readme.exists());

            // Generated file should contain our custom templated text
            TestUtils.assertFileContains(readme.toPath(), "// testCustomLibraryTemplates",
                    "package org.openapitools.client.infrastructure",
                    "open class CustomException(",
                    "private const val serialVersionUID: Long = 789L");
        } finally {
            output.delete();
            templates.toFile().delete();
        }
    }

    @Test
    public void testCustomNonLibraryTemplates() throws IOException {
        Path target = Files.createTempDirectory("test");
        Path templates = Files.createTempDirectory("templates");
        File output = target.toFile();
        try {
            // Create custom template
            File customTemplate = new File(templates.toFile(), "README.mustache");
            new File(customTemplate.getParent()).mkdirs();
            Files.write(customTemplate.toPath(),
                    "# {{someKey}}".getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("kotlin")
                    .addAdditionalProperty("someKey", "testCustomNonLibraryTemplates")
                    .setTemplateDir(templates.toAbsolutePath().toString())
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            Assert.assertEquals(files.size(), 26);

            // Generator should report README.md as a generated file
            TestUtils.ensureContainsFile(files, output, "README.md");

            // Generated file should exist on the filesystem after generation
            File readme = new File(output, "README.md");
            Assert.assertTrue(readme.exists());

            // README.md should contain our custom templated text
            TestUtils.assertFileContains(readme.toPath(), "# testCustomNonLibraryTemplates");
        } finally {
            output.delete();
            templates.toFile().delete();
        }
    }

    @Test
    public void testHandlesTrailingSlashInServers() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7533.yaml");
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setStrictSpecBehavior(false);
        opts.config(config);
        final DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        generator.configureGeneratorProperties();

        List<File> files = new ArrayList<>();
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<>();
        generator.generateModels(files, allModels, filteredSchemas);
        List<Object> allOperations = new ArrayList<>();
        generator.generateApis(files, allOperations, allModels);

        Map<String, Object> bundle = generator.buildSupportFileBundle(allOperations, allModels);
        LinkedList<CodegenServer> servers = (LinkedList<CodegenServer>) bundle.get("servers");
        Assert.assertEquals(servers.get(0).url, "");
        Assert.assertEquals(servers.get(1).url, "http://trailingshlash.io:80/v1");
        Assert.assertEquals(servers.get(2).url, "http://notrailingslash.io:80/v2");
    }
    
    @Test
    public void testHandlesRelativeUrlsInServers() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_10056.yaml");
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setStrictSpecBehavior(true);
        opts.config(config);
        final DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        generator.configureGeneratorProperties();

        List<File> files = new ArrayList<>();
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<>();
        generator.generateModels(files, allModels, filteredSchemas);
        List<Object> allOperations = new ArrayList<>();
        generator.generateApis(files, allOperations, allModels);

        Map<String, Object> bundle = generator.buildSupportFileBundle(allOperations, allModels);
        LinkedList<CodegenServer> servers = (LinkedList<CodegenServer>) bundle.get("servers");
        Assert.assertEquals(servers.get(0).url, "/relative/url");
    }

    @Test
    public void testProcessUserDefinedTemplatesWithConfig() throws IOException {
        Path target = Files.createTempDirectory("test");
        Path templates = Files.createTempDirectory("templates");
        File output = target.toFile();
        try {
            // Create custom template
            File customTemplate = new File(templates.toFile(), "README.mustache");
            new File(customTemplate.getParent()).mkdirs();
            Files.write(customTemplate.toPath(),
                    "# {{someKey}}".getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE);

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("python")
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setPackageName("io.something")
                    .setTemplateDir(templates.toAbsolutePath().toString())
                    .addAdditionalProperty("files", "src/test/resources/sampleConfig.json:\n\t folder: supportingjson "+
                    "\n\t destinationFilename: supportingconfig.json \n\t templateType: SupportingFiles")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString());

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator(false);

            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");

            List<File> files = generator.opts(clientOptInput).generate();

            // remove commented code based on review - files does not seem to be supported in CodegenConfigurator
            // supporting files sanity check
            // TestUtils.ensureContainsFile(files, output, "sampleConfig.json");
            // Assert.assertTrue(new File(output, "sampleConfig.json").exists());

            // Generator should report api_client.py as a generated file
            TestUtils.ensureContainsFile(files, output, "io/something/api_client.py");

            // Generated file should exist on the filesystem after generation
            File apiClient = new File(output, "io/something/api_client.py");
            Assert.assertTrue(apiClient.exists());

            // Generated file should contain our custom packageName
            TestUtils.assertFileContains(apiClient.toPath(),
                    "from io.something import rest"
              );
        } finally {
            output.delete();
            templates.toFile().delete();
        }
    }

    @Test
    public void testRecursionBug4650() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/recursion-bug-4650.yaml");
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setStrictSpecBehavior(false);
        opts.config(config);
        final DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        generator.configureGeneratorProperties();

        List<File> files = new ArrayList<>();
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<>();
        // The bug causes a StackOverflowError when calling generateModels
        generator.generateModels(files, allModels, filteredSchemas);
        // all fine, we have passed
    }
}
