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

package org.openapitools.codegen.java;

import com.google.common.collect.ImmutableMap;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.validateJavaSourceFiles;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class JavaClientCodegenTest {

    @Test
    public void arraysInRequestBody() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        RequestBody body1 = new RequestBody();
        body1.setDescription("A list of ids");
        body1.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new StringSchema()))));
        CodegenParameter codegenParameter1 = codegen.fromRequestBody(body1, new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter1.description, "A list of ids");
        Assert.assertEquals(codegenParameter1.dataType, "List<String>");
        Assert.assertEquals(codegenParameter1.baseType, "String");

        RequestBody body2 = new RequestBody();
        body2.setDescription("A list of list of values");
        body2.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new ArraySchema().items(new IntegerSchema())))));
        CodegenParameter codegenParameter2 = codegen.fromRequestBody(body2, new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter2.description, "A list of list of values");
        Assert.assertEquals(codegenParameter2.dataType, "List<List<Integer>>");
        Assert.assertEquals(codegenParameter2.baseType, "List");

        RequestBody body3 = new RequestBody();
        body3.setDescription("A list of points");
        body3.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new ObjectSchema().$ref("#/components/schemas/Point")))));
        ObjectSchema point = new ObjectSchema();
        point.addProperties("message", new StringSchema());
        point.addProperties("x", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        point.addProperties("y", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        CodegenParameter codegenParameter3 = codegen.fromRequestBody(body3, new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter3.description, "A list of points");
        Assert.assertEquals(codegenParameter3.dataType, "List<Point>");
        Assert.assertEquals(codegenParameter3.baseType, "Point");
    }

    @Test
    public void nullValuesInComposedSchema() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        ComposedSchema schema = new ComposedSchema();
        CodegenModel result = codegen.fromModel("CompSche",
                schema);
        Assert.assertEquals(result.name, "CompSche");
    }

    @Test
    public void testParametersAreCorrectlyOrderedWhenUsingRetrofit() {
        JavaClientCodegen javaClientCodegen = new JavaClientCodegen();
        javaClientCodegen.setLibrary(JavaClientCodegen.RETROFIT_2);

        CodegenOperation codegenOperation = new CodegenOperation();
        CodegenParameter queryParamRequired = createQueryParam("queryParam1", true);
        CodegenParameter queryParamOptional = createQueryParam("queryParam2", false);
        CodegenParameter pathParam1 = createPathParam("pathParam1");
        CodegenParameter pathParam2 = createPathParam("pathParam2");

        codegenOperation.allParams = Arrays.asList(queryParamRequired, pathParam1, pathParam2, queryParamOptional);
        Map<String, Object> operations = ImmutableMap.<String, Object>of("operation", Arrays.asList(codegenOperation));

        Map<String, Object> objs = ImmutableMap.of("operations", operations, "imports", new ArrayList<Map<String, String>>());

        javaClientCodegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assert.assertEquals(Arrays.asList(pathParam1, pathParam2, queryParamRequired, queryParamOptional), codegenOperation.allParams);
        assertTrue(pathParam1.hasMore);
        assertTrue(pathParam2.hasMore);
        assertTrue(queryParamRequired.hasMore);
        Assert.assertFalse(queryParamOptional.hasMore);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);

        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.client.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.client.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.client.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.client");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.client");
        Assert.assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_GSON);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xyz.yyyyy.zzzzzzz.model");
        codegen.setApiPackage("xyz.yyyyy.zzzzzzz.api");
        codegen.setInvokerPackage("xyz.yyyyy.zzzzzzz.invoker");
        codegen.setSerializationLibrary("JACKSON");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_GSON); // the library JavaClientCodegen.OKHTTP_GSON only supports GSON
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.zzzzzzz.iiii.invoker");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "JACKSON");
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, JavaClientCodegen.JERSEY2);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.iiii.invoker");
        Assert.assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_JACKSON);
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromApi() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.aaaaa");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromModel() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.client.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.mmmmm");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm");
    }

    @Test
    public void testGetSchemaTypeWithComposedSchemaWithAllOf() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/composed-allof.yaml");
        final JavaClientCodegen codegen = new JavaClientCodegen();

        Operation operation = openAPI.getPaths().get("/ping").getPost();
        CodegenOperation co = codegen.fromOperation("/ping", "POST", operation, null);
        Assert.assertEquals(co.allParams.size(), 1);
        Assert.assertEquals(co.allParams.get(0).baseType, "MessageEventCoreWithTimeListEntries");
    }

    @Test
    public void updateCodegenPropertyEnum() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();

        codegen.updateCodegenPropertyEnum(array);

        List<Map<String, String>> enumVars = (List<Map<String, String>>) array.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Map<String, String> testedEnumVar = enumVars.get(0);
        Assert.assertNotNull(testedEnumVar);
        Assert.assertEquals(testedEnumVar.getOrDefault("name", ""), "NUMBER_1");
        Assert.assertEquals(testedEnumVar.getOrDefault("value", ""), "1");
    }

    @Test
    public void updateCodegenPropertyEnumWithCustomNames() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();
        array.getItems().setVendorExtensions(Collections.singletonMap("x-enum-varnames", Collections.singletonList("ONE")));

        codegen.updateCodegenPropertyEnum(array);

        List<Map<String, String>> enumVars = (List<Map<String, String>>) array.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Map<String, String> testedEnumVar = enumVars.get(0);
        Assert.assertNotNull(testedEnumVar);
        Assert.assertEquals(testedEnumVar.getOrDefault("name", ""), "ONE");
        Assert.assertEquals(testedEnumVar.getOrDefault("value", ""), "1");
    }

    @Test
    public void testGeneratePing() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 37);
        TestUtils.ensureContainsFile(generatedFiles, output, ".gitignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator/VERSION");
        TestUtils.ensureContainsFile(generatedFiles, output, ".travis.yml");
        TestUtils.ensureContainsFile(generatedFiles, output, "build.gradle");
        TestUtils.ensureContainsFile(generatedFiles, output, "build.sbt");
        TestUtils.ensureContainsFile(generatedFiles, output, "docs/DefaultApi.md");
        TestUtils.ensureContainsFile(generatedFiles, output, "git_push.sh");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle.properties");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.jar");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.properties");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradlew.bat");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradlew");
        TestUtils.ensureContainsFile(generatedFiles, output, "pom.xml");
        TestUtils.ensureContainsFile(generatedFiles, output, "README.md");
        TestUtils.ensureContainsFile(generatedFiles, output, "settings.gradle");
        TestUtils.ensureContainsFile(generatedFiles, output, "api/openapi.yaml");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/AndroidManifest.xml");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/api/DefaultApi.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiCallback.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiClient.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiException.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiResponse.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ServerConfiguration.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ServerVariable.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/ApiKeyAuth.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/Authentication.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/HttpBasicAuth.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/HttpBearerAuth.java");
        //TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/OAuth.java");
        //TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/OAuthFlow.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/Configuration.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/GzipRequestInterceptor.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/JSON.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/Pair.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ProgressRequestBody.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ProgressResponseBody.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/StringUtil.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/test/java/xyz/abcdef/api/DefaultApiTest.java");

        validateJavaSourceFiles(generatedFiles);

        String defaultApiFilename = new File(output, "src/main/java/xyz/abcdef/api/DefaultApi.java").getAbsolutePath().replace("\\", "/");
        String defaultApiConent = generatedFiles.get(defaultApiFilename);
        assertTrue(defaultApiConent.contains("public class DefaultApi"));

        WrittenTemplateBasedFile templateBasedFile = TestUtils.getTemplateBasedFile(generator, output, "src/main/java/xyz/abcdef/api/DefaultApi.java");
        Assert.assertEquals(templateBasedFile.getTemplateData().get("classname"), "DefaultApi");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingSomeObj() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(CodegenConstants.MODEL_PACKAGE, "zz.yyyy.model.xxxx");
        properties.put(CodegenConstants.API_PACKAGE, "zz.yyyy.api.xxxx");
        properties.put(CodegenConstants.INVOKER_PACKAGE, "zz.yyyy.invoker.xxxx");
        properties.put(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX, "is");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 40);
        TestUtils.ensureContainsFile(generatedFiles, output, ".gitignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator/VERSION");
        TestUtils.ensureContainsFile(generatedFiles, output, ".travis.yml");
        TestUtils.ensureContainsFile(generatedFiles, output, "build.gradle");
        TestUtils.ensureContainsFile(generatedFiles, output, "build.sbt");
        TestUtils.ensureContainsFile(generatedFiles, output, "docs/PingApi.md");
        TestUtils.ensureContainsFile(generatedFiles, output, "docs/SomeObj.md");
        TestUtils.ensureContainsFile(generatedFiles, output, "git_push.sh");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle.properties");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.jar");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.properties");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradlew.bat");
        TestUtils.ensureContainsFile(generatedFiles, output, "gradlew");
        TestUtils.ensureContainsFile(generatedFiles, output, "pom.xml");
        TestUtils.ensureContainsFile(generatedFiles, output, "README.md");
        TestUtils.ensureContainsFile(generatedFiles, output, "settings.gradle");
        TestUtils.ensureContainsFile(generatedFiles, output, "api/openapi.yaml");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/AndroidManifest.xml");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/api/xxxx/PingApi.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ApiCallback.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ApiClient.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ApiException.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ApiResponse.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ServerConfiguration.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ServerVariable.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/auth/ApiKeyAuth.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/auth/Authentication.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/auth/HttpBasicAuth.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/auth/HttpBearerAuth.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/Configuration.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/GzipRequestInterceptor.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/JSON.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/Pair.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ProgressRequestBody.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/ProgressResponseBody.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/invoker/xxxx/StringUtil.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/zz/yyyy/model/xxxx/SomeObj.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/test/java/zz/yyyy/api/xxxx/PingApiTest.java");
        TestUtils.ensureContainsFile(generatedFiles, output, "src/test/java/zz/yyyy/model/xxxx/SomeObjTest.java");

        validateJavaSourceFiles(generatedFiles);

        String defaultApiFilename = new File(output, "src/main/java/zz/yyyy/model/xxxx/SomeObj.java").getAbsolutePath().replace("\\", "/");
        String defaultApiConent = generatedFiles.get(defaultApiFilename);
        assertTrue(defaultApiConent.contains("public class SomeObj"));
        assertTrue(defaultApiConent.contains("Boolean isActive()"));

        output.deleteOnExit();
    }

    @Test
    public void testJdkHttpClient() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 26);
        validateJavaSourceFiles(generatedFiles);

        String defaultApiFilename = new File(output, "src/main/java/xyz/abcdef/api/DefaultApi.java").getAbsolutePath().replace("\\", "/");
        String defaultApiContent = generatedFiles.get(defaultApiFilename);
        assertTrue(defaultApiContent.contains("public class DefaultApi"));
        assertTrue(defaultApiContent.contains("import java.net.http.HttpClient;"));
        assertTrue(defaultApiContent.contains("import java.net.http.HttpRequest;"));
        assertTrue(defaultApiContent.contains("import java.net.http.HttpResponse;"));

        String apiClientFilename = new File(output, "src/main/java/xyz/abcdef/ApiClient.java").getAbsolutePath().replace("\\", "/");
        String apiClientContent = generatedFiles.get(apiClientFilename);
        assertTrue(apiClientContent.contains("public class ApiClient"));
        assertTrue(apiClientContent.contains("import java.net.http.HttpClient;"));
        assertTrue(apiClientContent.contains("import java.net.http.HttpRequest;"));
    }

    @Test
    public void testReferencedHeader() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue855.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        ApiResponse ok_200 = openAPI.getComponents().getResponses().get("OK_200");
        CodegenResponse response = codegen.fromResponse("200", ok_200);

        Assert.assertEquals(1, response.headers.size());
        CodegenProperty header = response.headers.get(0);
        Assert.assertEquals("UUID", header.dataType);
        Assert.assertEquals("Request", header.baseName);
    }

    @Test
    public void testAuthorizationScopeValues_Issue392() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue392.yaml");

        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.setOpenAPI(openAPI);
        clientOptInput.setConfig(new JavaClientCodegen());

        defaultGenerator.opts(clientOptInput);
        final List<CodegenOperation> codegenOperations = defaultGenerator.processPaths(openAPI.getPaths()).get("Pet");

        // Verify GET only has 'read' scope
        final CodegenOperation getCodegenOperation = codegenOperations.stream().filter(it -> it.httpMethod.equals("GET")).collect(Collectors.toList()).get(0);
        assertTrue(getCodegenOperation.hasAuthMethods);
        assertEquals(getCodegenOperation.authMethods.size(), 1);
        final List<Map<String, Object>> getScopes = getCodegenOperation.authMethods.get(0).scopes;
        assertEquals(getScopes.size(), 1, "GET scopes don't match. actual::" + getScopes);

        // POST operation should have both 'read' and 'write' scope on it
        final CodegenOperation postCodegenOperation = codegenOperations.stream().filter(it -> it.httpMethod.equals("POST")).collect(Collectors.toList()).get(0);
        assertTrue(postCodegenOperation.hasAuthMethods);
        assertEquals(postCodegenOperation.authMethods.size(), 1);
        final List<Map<String, Object>> postScopes = postCodegenOperation.authMethods.get(0).scopes;
        assertEquals(postScopes.size(), 2, "POST scopes don't match. actual:" + postScopes);
    }

    @Test
    public void testAuthorizationsHasMoreWhenFiltered() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue4584.yaml");

        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.setOpenAPI(openAPI);
        clientOptInput.setConfig(new JavaClientCodegen());

        defaultGenerator.opts(clientOptInput);
        final List<CodegenOperation> codegenOperations = defaultGenerator.processPaths(openAPI.getPaths()).get("Pet");

        final CodegenOperation getCodegenOperation = codegenOperations.stream().filter(it -> it.httpMethod.equals("GET")).collect(Collectors.toList()).get(0);
        assertTrue(getCodegenOperation.hasAuthMethods);
        assertEquals(getCodegenOperation.authMethods.size(), 2);
        assertTrue(getCodegenOperation.authMethods.get(0).hasMore);
        Assert.assertFalse(getCodegenOperation.authMethods.get(1).hasMore);
   }

    @Test
    public void testFreeFormObjects() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue796.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();

        Schema test1 = openAPI.getComponents().getSchemas().get("MapTest1");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm1 = codegen.fromModel("MapTest1", test1);
        Assert.assertEquals(cm1.getDataType(), "Map");
        Assert.assertEquals(cm1.getParent(), "HashMap<String, Object>");
        Assert.assertEquals(cm1.getClassname(), "MapTest1");

        Schema test2 = openAPI.getComponents().getSchemas().get("MapTest2");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm2 = codegen.fromModel("MapTest2", test2);
        Assert.assertEquals(cm2.getDataType(), "Map");
        Assert.assertEquals(cm2.getParent(), "HashMap<String, Object>");
        Assert.assertEquals(cm2.getClassname(), "MapTest2");

        Schema test3 = openAPI.getComponents().getSchemas().get("MapTest3");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm3 = codegen.fromModel("MapTest3", test3);
        Assert.assertEquals(cm3.getDataType(), "Map");
        Assert.assertEquals(cm3.getParent(), "HashMap<String, Object>");
        Assert.assertEquals(cm3.getClassname(), "MapTest3");

        Schema other = openAPI.getComponents().getSchemas().get("OtherObj");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("OtherObj", other);
        Assert.assertEquals(cm.getDataType(), "Object");
        Assert.assertEquals(cm.getClassname(), "OtherObj");
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/3589
     */
    @Test
    public void testImportMapping() throws IOException {

        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        Map<String, String> importMappings = new HashMap<>();
        importMappings.put("TypeAlias", "foo.bar.TypeAlias");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTEASY)
                .setAdditionalProperties(properties)
                .setImportMappings(importMappings)
                .setInputSpec("src/test/resources/3_0/type-alias.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        Assert.assertEquals(clientOptInput.getConfig().importMapping().get("TypeAlias"), "foo.bar.TypeAlias");

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 1);
        TestUtils.ensureContainsFile(generatedFiles, output, "src/main/java/org/openapitools/client/model/ParentType.java");

        final String parentTypeContents = generatedFiles.values().iterator().next();

        final Pattern FIELD_PATTERN = Pattern.compile(".* private (.*?) typeAlias;.*", Pattern.DOTALL);
        Matcher fieldMatcher = FIELD_PATTERN.matcher(parentTypeContents);
        Assert.assertTrue(fieldMatcher.matches());

        // this is the type of the field 'typeAlias'. With a working importMapping it should
        // be 'foo.bar.TypeAlias' or just 'TypeAlias'
        Assert.assertEquals(fieldMatcher.group(1), "foo.bar.TypeAlias");
    }

    @Test
    public void testBearerAuth() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/pingBearerAuth.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();

        List<CodegenSecurity> security = codegen.fromSecurity(openAPI.getComponents().getSecuritySchemes());
        Assert.assertEquals(security.size(), 1);
        Assert.assertEquals(security.get(0).isBasic, Boolean.TRUE);
        Assert.assertEquals(security.get(0).isBasicBasic, Boolean.FALSE);
        Assert.assertEquals(security.get(0).isBasicBearer, Boolean.TRUE);
    }

    private CodegenProperty codegenPropertyWithArrayOfIntegerValues() {
        CodegenProperty array = new CodegenProperty();
        final CodegenProperty items = new CodegenProperty();
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", Collections.singletonList(1));
        items.setAllowableValues(allowableValues);
        items.dataType = "Integer";
        array.setItems(items);
        array.dataType = "Array";
        array.mostInnerItems = items;
        return array;
    }

    private CodegenParameter createPathParam(String name) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isPathParam = true;
        return codegenParameter;
    }

    private CodegenParameter createQueryParam(String name, boolean required) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isQueryParam = true;
        codegenParameter.required = required;
        return codegenParameter;
    }

    private CodegenParameter createStringParam(String name) {
        CodegenParameter codegenParameter = new CodegenParameter();
        codegenParameter.paramName = name;
        codegenParameter.baseName = name;
        codegenParameter.dataType = "String";
        return codegenParameter;
    }

    @Test
    public void escapeName() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        assertEquals("_default", codegen.toApiVarName("Default"));
        assertEquals("_int", codegen.toApiVarName("int"));
        assertEquals("pony", codegen.toApiVarName("pony"));
    }
}
