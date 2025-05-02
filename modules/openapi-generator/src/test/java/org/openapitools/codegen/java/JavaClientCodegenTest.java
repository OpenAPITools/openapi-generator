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
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import lombok.Getter;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.assertj.core.api.InstanceOfAssertFactories.FILE;
import static org.openapitools.codegen.CodegenConstants.SERIALIZATION_LIBRARY;
import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.openapitools.codegen.TestUtils.validateJavaSourceFiles;
import static org.openapitools.codegen.languages.JavaClientCodegen.*;
import static org.testng.Assert.*;

public class JavaClientCodegenTest {

    // This is the kind of information that ideally would be defined and available system-wide
    @Getter
    enum Library {
        APACHE_HTTPCLIENT("apache-httpclient", Serializer.JACKSON),
        FEIGN("feign", Serializer.JACKSON, Set.of(Serializer.GSON)),
        GOOGLE_API_CLIENT("google-api-client", Serializer.JACKSON),
        JERSEY_2("jersey2", Serializer.JACKSON),
        JERSEY_3("jersey3", Serializer.JACKSON),
        MICROPROFILE("microprofile", Serializer.JSONB, Set.of(Serializer.JACKSON)),
        NATIVE("native", Serializer.JACKSON),
        OKHTTP("okhttp-gson", Serializer.GSON),
        REST_ASSURED("rest-assured", Serializer.GSON, Set.of(Serializer.JACKSON)),
        RESTEASY("resteasy", Serializer.JACKSON),
        REST_CLIENT("restclient", Serializer.JACKSON),
        REST_TEMPLATE("resttemplate", Serializer.JACKSON),
        RETROFIT_2("retrofit2", Serializer.GSON),
        VERTX("vertx", Serializer.JACKSON),
        WEBCLIENT("webclient", Serializer.JACKSON);

        public final String value;
        public final Set<Serializer> supportedSerializers;
        public final Serializer defaultSerializer;

        Library(String identifier, Serializer defaultSerializer) {
            this(identifier, defaultSerializer, Set.of());
        }

        Library(String identifier, Serializer defaultSerializer, Set<Serializer> otherSupportedSerializers) {
            otherSupportedSerializers = new HashSet<>(otherSupportedSerializers);
            otherSupportedSerializers.add(defaultSerializer);
            this.supportedSerializers = Set.copyOf(otherSupportedSerializers);
            this.defaultSerializer = defaultSerializer;
            this.value = identifier;
        }
    }

    enum Serializer {
        GSON, JACKSON, JSONB;

        public String toString() {
            return this.name().toLowerCase(Locale.ROOT);
        }
    }

    @DataProvider
    Iterator<Library> supportedLibraries() {
        return Arrays.stream(Library.values()).iterator();
    }

    @DataProvider
    Iterator<Library> librariesSupportingGson() {
        return Arrays.stream(Library.values())
                .filter(library -> library.getSupportedSerializers().contains(Serializer.GSON))
                .iterator();
    }

    @DataProvider
    Iterator<Library> librariesSupportingJackson() {
        return Arrays.stream(Library.values())
                .filter(library -> library.getSupportedSerializers().contains(Serializer.JACKSON))
                .iterator();
    }

    @DataProvider
    Iterator<Library> librariesNotSupportingJackson() {
        return Arrays.stream(Library.values())
                .filter(library -> !library.getSupportedSerializers().contains(Serializer.JACKSON))
                .iterator();
    }


    @Test
    public void arraysInRequestBody() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        RequestBody body1 = new RequestBody();
        body1.setDescription("A list of ids");
        body1.setContent(new Content().addMediaType(
                "application/json",
                new MediaType().schema(new ArraySchema().items(new StringSchema()))
        ));
        CodegenParameter codegenParameter1 = codegen.fromRequestBody(body1, new HashSet<>(), null);
        Assertions.assertEquals(codegenParameter1.description, "A list of ids");
        Assertions.assertEquals(codegenParameter1.dataType, "List<String>");
        Assertions.assertEquals(codegenParameter1.baseType, "String");

        RequestBody body2 = new RequestBody();
        body2.setDescription("A list of list of values");
        body2.setContent(new Content().addMediaType(
                "application/json",
                new MediaType().schema(new ArraySchema().items(new ArraySchema().items(new IntegerSchema())))
        ));
        CodegenParameter codegenParameter2 = codegen.fromRequestBody(body2, new HashSet<>(), null);
        Assertions.assertEquals(codegenParameter2.description, "A list of list of values");
        Assertions.assertEquals(codegenParameter2.dataType, "List<List<Integer>>");
        Assertions.assertEquals(codegenParameter2.baseType, "List");

        RequestBody body3 = new RequestBody();
        body3.setDescription("A list of points");
        body3.setContent(
                new Content()
                        .addMediaType(
                                "application/json",
                                new MediaType()
                                        .schema(
                                                new ArraySchema()
                                                        .items(new ObjectSchema().$ref("#/components/schemas/Point")))));
        ObjectSchema point = new ObjectSchema();
        point.addProperty("message", new StringSchema());
        point.addProperty("x", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        point.addProperty("y", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        CodegenParameter codegenParameter3 = codegen.fromRequestBody(body3, new HashSet<>(), null);
        Assertions.assertEquals(codegenParameter3.description, "A list of points");
        Assertions.assertEquals(codegenParameter3.dataType, "List<Point>");
        Assertions.assertEquals(codegenParameter3.baseType, "Point");
    }

    @Test
    public void nullValuesInComposedSchema() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        ComposedSchema schema = new ComposedSchema();
        CodegenModel result = codegen.fromModel("CompSche",
                schema);
        Assertions.assertEquals(result.name, "CompSche");
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

        codegenOperation.allParams.addAll(Arrays.asList(queryParamRequired, pathParam1, pathParam2, queryParamOptional));
        OperationMap operations = new OperationMap();
        operations.setOperation(codegenOperation);

        OperationsMap objs = new OperationsMap();
        objs.setOperation(operations);
        objs.setImports(new ArrayList<>());

        javaClientCodegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assertions.assertEquals(Arrays.asList(pathParam1, pathParam2, queryParamRequired, queryParamOptional), codegenOperation.allParams);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.FALSE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "org.openapitools.client.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "org.openapitools.client.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "org.openapitools.client");
        assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_GSON);
        configAssert.assertValue(JavaClientCodegen.SERIALIZATION_LIBRARY_GSON, "true");
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
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, Boolean.TRUE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xyz.yyyyy.zzzzzzz.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xyz.yyyyy.zzzzzzz.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xyz.yyyyy.zzzzzzz.invoker");
        assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_GSON); // the library JavaClientCodegen.OKHTTP_GSON only supports GSON
    }

    @Test
    public void testFailOnUnknownPropertiesAdditionalProperty() {
        final JavaClientCodegen codegen = new JavaClientCodegen();

        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(JavaClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, codegen::isFailOnUnknownProperties, Boolean.FALSE);

        codegen.additionalProperties().put(JavaClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, true);
        codegen.processOpts();

        configAssert.assertValue(JavaClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, codegen::isFailOnUnknownProperties, Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen
                .additionalProperties()
                .put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen
                .additionalProperties()
                .put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.zzzzzzz.iiii.invoker");
        // this REQUIRES that serialization-library is handled case-insensitively (not sure if that's intentional)
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "JACKSON");
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, JavaClientCodegen.JERSEY2);
        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, Boolean.TRUE);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.zzzzzzz.iiii.invoker");
        assertEquals(codegen.getSerializationLibrary(), JavaClientCodegen.SERIALIZATION_LIBRARY_JACKSON);
    }

    @Test
    public void testGeneratedAuthClassesJersey() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.JERSEY3)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(files).contains(
                output.resolve("src/main/java/xyz/abcdef/auth/ApiKeyAuth.java").toFile(),
                output.resolve("src/main/java/xyz/abcdef/auth/Authentication.java").toFile(),
                output.resolve("src/main/java/xyz/abcdef/auth/HttpBasicAuth.java").toFile(),
                output.resolve("src/main/java/xyz/abcdef/auth/HttpBearerAuth.java").toFile(),
                output.resolve("src/main/java/xyz/abcdef/auth/HttpSignatureAuth.java").toFile()
        );
    }

    @Test
    public void testImportMappingResult() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .addTypeMapping("OffsetDateTime", "Instant")
                .addImportMapping("OffsetDateTime", "java.time.Instant")
                .setGeneratorName("java")
                .setInputSpec("src/test/resources/3_0/echo_api.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/api/QueryApi.java"))
                .content().contains("import java.time.Instant;");
    }

    @Test
    public void testSupportedSecuritySchemesJersey() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, JavaClientCodegen.JERSEY3);
        codegen.processOpts();

        Assertions.assertTrue(codegen.getFeatureSet().getSecurityFeatures().contains(SecurityFeature.SignatureAuth));
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromApi() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");

        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xyz.yyyyy.zzzzzzz.aaaaa");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromModel() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");

        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "org.openapitools.client.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xyz.yyyyy.zzzzzzz.mmmmm");
    }

    @Test
    public void testGetSchemaTypeWithComposedSchemaWithAllOf() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/composed-allof.yaml");
        Operation operation = openAPI.getPaths().get("/ping").getPost();

        CodegenOperation co = new JavaClientCodegen().fromOperation("/ping", "POST", operation, null);

        assertThat(co.allParams).hasSize(1)
                .first().hasFieldOrPropertyWithValue("baseType", "MessageEventCoreWithTimeListEntries");
    }

    @Test
    public void updateCodegenPropertyEnum() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();

        codegen.updateCodegenPropertyEnum(array);

        var enumVars = (List<Map<String, String>>) array.getItems().getAllowableValues().get("enumVars");
        Assertions.assertNotNull(enumVars);
        Map<String, String> testedEnumVar = enumVars.get(0);
        Assertions.assertNotNull(testedEnumVar);
        Assertions.assertEquals(testedEnumVar.getOrDefault("name", ""), "NUMBER_1");
        Assertions.assertEquals(testedEnumVar.getOrDefault("value", ""), "1");
    }

    @Test
    public void updateCodegenPropertyEnumWithCustomNames() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();
        array.getItems().setVendorExtensions(Map.of("x-enum-varnames", Collections.singletonList("ONE")));

        codegen.updateCodegenPropertyEnum(array);

        var enumVars = (List<Map<String, String>>) array.getItems().getAllowableValues().get("enumVars");
        Assertions.assertNotNull(enumVars);
        Map<String, String> testedEnumVar = enumVars.get(0);
        Assertions.assertNotNull(testedEnumVar);
        Assertions.assertEquals(testedEnumVar.getOrDefault("name", ""), "ONE");
        Assertions.assertEquals(testedEnumVar.getOrDefault("value", ""), "1");
    }

    @Test
    public void testGeneratePing() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).hasSize(40).map(File::toPath).contains(
                output.resolve(".gitignore"),
                output.resolve(".openapi-generator-ignore"),
                output.resolve(".openapi-generator/FILES"),
                output.resolve(".openapi-generator/VERSION"),
                output.resolve(".travis.yml"),
                output.resolve("build.gradle"),
                output.resolve("build.sbt"),
                output.resolve("docs/DefaultApi.md"),
                output.resolve("git_push.sh"),
                output.resolve("gradle.properties"),
                output.resolve("gradle/wrapper/gradle-wrapper.jar"),
                output.resolve("gradle/wrapper/gradle-wrapper.properties"),
                output.resolve("gradlew.bat"),
                output.resolve("gradlew"),
                output.resolve("pom.xml"),
                output.resolve("README.md"),
                output.resolve("settings.gradle"),
                output.resolve("api/openapi.yaml"),
                output.resolve("src/main/AndroidManifest.xml"),
                output.resolve("src/main/java/xyz/abcdef/api/DefaultApi.java"),
                output.resolve("src/main/java/xyz/abcdef/ApiCallback.java"),
                output.resolve("src/main/java/xyz/abcdef/ApiClient.java"),
                output.resolve("src/main/java/xyz/abcdef/ApiException.java"),
                output.resolve("src/main/java/xyz/abcdef/ApiResponse.java"),
                output.resolve("src/main/java/xyz/abcdef/ServerVariable.java"),
                output.resolve("src/main/java/xyz/abcdef/auth/ApiKeyAuth.java"),
                output.resolve("src/main/java/xyz/abcdef/ServerConfiguration.java"),
                output.resolve("src/main/java/xyz/abcdef/auth/Authentication.java"),
                output.resolve("src/main/java/xyz/abcdef/auth/HttpBasicAuth.java"),
                output.resolve("src/main/java/xyz/abcdef/auth/HttpBearerAuth.java"),
                output.resolve("src/main/java/xyz/abcdef/Configuration.java"),
                output.resolve("src/main/java/xyz/abcdef/GzipRequestInterceptor.java"),
                output.resolve("src/main/java/xyz/abcdef/JSON.java"),
                output.resolve("src/main/java/xyz/abcdef/ProgressRequestBody.java"),
                output.resolve("src/main/java/xyz/abcdef/Pair.java"),
                output.resolve("src/main/java/xyz/abcdef/ProgressResponseBody.java"),
                output.resolve("src/main/java/xyz/abcdef/StringUtil.java"),
                output.resolve("src/test/java/xyz/abcdef/api/DefaultApiTest.java")
        );
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/DefaultApi.java")).content()
                .contains("public class DefaultApi");
    }

    @Test
    public void testGeneratePingSomeObj() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.MODEL_PACKAGE, "zz.yyyy.model.xxxx")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "zz.yyyy.api.xxxx")
                .addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, "zz.yyyy.invoker.xxxx")
                .addAdditionalProperty(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX, "is")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/zz/yyyy/model/xxxx/SomeObj.java")).content()
                .contains("public class SomeObj", "Boolean isActive()");
        assertThat(files).hasSize(43).map(File::toPath).contains(
                output.resolve(".gitignore"),
                output.resolve(".openapi-generator-ignore"),
                output.resolve(".openapi-generator/FILES"),
                output.resolve(".openapi-generator/VERSION"),
                output.resolve(".travis.yml"),
                output.resolve("build.gradle"),
                output.resolve("build.sbt"),
                output.resolve("docs/PingApi.md"),
                output.resolve("docs/SomeObj.md"),
                output.resolve("git_push.sh"),
                output.resolve("gradle.properties"),
                output.resolve("gradle/wrapper/gradle-wrapper.jar"),
                output.resolve("gradle/wrapper/gradle-wrapper.properties"),
                output.resolve("gradlew.bat"),
                output.resolve("gradlew"),
                output.resolve("pom.xml"),
                output.resolve("README.md"),
                output.resolve("settings.gradle"),
                output.resolve("api/openapi.yaml"),
                output.resolve("src/main/AndroidManifest.xml"),
                output.resolve("src/main/java/zz/yyyy/api/xxxx/PingApi.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ApiCallback.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ApiClient.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ApiException.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ApiResponse.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ServerConfiguration.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ServerVariable.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/auth/ApiKeyAuth.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/auth/Authentication.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/auth/HttpBasicAuth.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/auth/HttpBearerAuth.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/Configuration.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/GzipRequestInterceptor.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/JSON.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/Pair.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ProgressRequestBody.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/ProgressResponseBody.java"),
                output.resolve("src/main/java/zz/yyyy/invoker/xxxx/StringUtil.java"),
                output.resolve("src/main/java/zz/yyyy/model/xxxx/SomeObj.java"),
                output.resolve("src/test/java/zz/yyyy/api/xxxx/PingApiTest.java"),
                output.resolve("src/test/java/zz/yyyy/model/xxxx/SomeObjTest.java")
        );
    }

    @Test
    public void testJdkHttpClient() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(files).hasSize(34);
        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/DefaultApi.java")).content().contains(
                "public class DefaultApi",
                "import java.net.http.HttpClient;",
                "import java.net.http.HttpRequest;",
                "import java.net.http.HttpResponse;"
        );
        assertThat(output.resolve("src/main/java/xyz/abcdef/ApiClient.java")).content().contains(
                "public class ApiClient",
                "import java.net.http.HttpClient;",
                "import java.net.http.HttpRequest;"
        );
    }

    @Test
    public void testJdkHttpClientWithUseBeanValidationEnabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_BEANVALIDATION, true)
                .addAdditionalProperty(JavaClientCodegen.USE_JAKARTA_EE, true)
                .setInputSpec("src/test/resources/3_1/issue-17485.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/UserApi.java")).content()
                .contains("@Pattern", "import jakarta.validation.constraints.*");
    }

    @Test
    public void testJdkHttpClientWithAndWithoutDiscriminator() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(CodegenConstants.MODEL_PACKAGE, "xyz.abcdef.model")
                .addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, "xyz.abcdef.invoker")
                .setInputSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        assertThat(files).hasSize(153);
        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/model/Dog.java")).content()
                .contains("import xyz.abcdef.invoker.JSON;");
    }

    @Test
    public void testJdkHttpAsyncClient() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.ASYNC_NATIVE, true)
                .setLibrary(JavaClientCodegen.NATIVE)
                .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(files).hasSize(37);

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/PingApi.java")).content().contains(
                "public class PingApi",
                "import java.net.http.HttpClient;",
                "import java.net.http.HttpRequest;",
                "import java.net.http.HttpResponse;",
                "import java.util.concurrent.CompletableFuture;"
        );
        assertThat(output.resolve("src/main/java/xyz/abcdef/ApiClient.java")).content().contains(
                "public class ApiClient",
                "import java.net.http.HttpClient;",
                "import java.net.http.HttpRequest;"
        );
    }

    @Test
    public void testReferencedHeader() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue855.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        ApiResponse ok_200 = openAPI.getComponents().getResponses().get("OK_200");
        CodegenResponse response = codegen.fromResponse("200", ok_200);

        Assertions.assertEquals(response.headers.size(), 1);
        CodegenProperty header = response.headers.get(0);
        Assertions.assertEquals(header.dataType, "UUID");
        Assertions.assertEquals(header.baseName, "Request");
    }

    @Test
    public void testAuthorizationScopeValues_Issue392() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue392.yaml");

        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(new JavaClientCodegen());

        defaultGenerator.opts(clientOptInput);
        final List<CodegenOperation> codegenOperations =
                defaultGenerator.processPaths(openAPI.getPaths()).get("Pet");

        // Verify GET only has 'read' scope
        final CodegenOperation getCodegenOperation =
                codegenOperations.stream()
                        .filter(it -> it.httpMethod.equals("GET"))
                        .collect(Collectors.toList())
                        .get(0);
        assertTrue(getCodegenOperation.hasAuthMethods);
        assertEquals(getCodegenOperation.authMethods.size(), 1);
        final List<Map<String, Object>> getScopes = getCodegenOperation.authMethods.get(0).scopes;
        assertEquals(getScopes.size(), 1, "GET scopes don't match. actual::" + getScopes);

        // POST operation should have both 'read' and 'write' scope on it
        final CodegenOperation postCodegenOperation =
                codegenOperations.stream()
                        .filter(it -> it.httpMethod.equals("POST"))
                        .collect(Collectors.toList())
                        .get(0);
        assertTrue(postCodegenOperation.hasAuthMethods);
        assertEquals(postCodegenOperation.authMethods.size(), 1);
        final List<Map<String, Object>> postScopes = postCodegenOperation.authMethods.get(0).scopes;
        assertEquals(postScopes.size(), 2, "POST scopes don't match. actual:" + postScopes);
    }

    @Test
    public void testAuthorizationScopeValues_Issue6733() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTEASY)
                .setValidateSpec(false)
                .setInputSpec("src/test/resources/3_0/regression-6734.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        // tests if NPE will crash generation when path in yaml arent provided
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGenerateMetadata(false);
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        Assertions.assertEquals(files.size(), 1);
    }

    @Test
    public void testTypedAndNonTypedComposedSchemaGeneration_3_1() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTEASY)
                .setValidateSpec(false)
                .setInputSpec("src/test/resources/3_1/composed-schemas-with-and-without-type.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGenerateMetadata(false);
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        Assertions.assertEquals(files.size(), 9);
    }

    @Test
    public void testMultiPartSpecifiesFileName_Issue17367() throws IOException {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTEASY)
                .setValidateSpec(false)
                .setInputSpec("src/test/resources/3_0/issue-17367.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
        generator.setGenerateMetadata(false);
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        File apiClient = files.stream()
                .filter(f -> f.getName().equals("ApiClient.java"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("ApiClient.java not found"));
        var contents = Arrays.stream(Files.readString(apiClient.toPath(), StandardCharsets.UTF_8).split("\n"));
        // https://docs.jboss.org/resteasy/docs/6.2.5.Final/javadocs/org/jboss/resteasy/plugins/providers/multipart/MultipartFormDataOutput.html#addFormData(java.lang.String,java.lang.Object,jakarta.ws.rs.core.MediaType,java.lang.String)
        assertTrue(contents.anyMatch(l -> l.matches(
                ".*multipart\\.addFormData\\(param.getKey\\(\\),\\s*" +
                        "new\\s+FileInputStream\\(file\\),\\s*" +
                        "MediaType\\.APPLICATION_OCTET_STREAM_TYPE,\\s*" +
                        "file.getName\\(\\)\\);.*")));
    }

    @Test
    public void testAuthorizationsMethodsSizeWhenFiltered() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue4584.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(new JavaClientCodegen());
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        defaultGenerator.opts(clientOptInput);

        final List<CodegenOperation> codegenOperations = defaultGenerator.processPaths(openAPI.getPaths()).get("Pet");

        final CodegenOperation getCodegenOperation = codegenOperations.stream()
                .filter(it -> it.httpMethod.equals("GET"))
                .collect(Collectors.toList())
                .get(0);
        assertTrue(getCodegenOperation.hasAuthMethods);
        assertEquals(getCodegenOperation.authMethods.size(), 2);
    }

    @Test
    public void testFreeFormObjects() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue796.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();

        Schema<?> test1 = openAPI.getComponents().getSchemas().get("MapTest1");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm1 = codegen.fromModel("MapTest1", test1);
        Assertions.assertEquals(cm1.getDataType(), "Map");
        Assertions.assertEquals(cm1.getParent(), "HashMap<String, Object>");
        Assertions.assertEquals(cm1.getClassname(), "MapTest1");

        Schema<?> test2 = openAPI.getComponents().getSchemas().get("MapTest2");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm2 = codegen.fromModel("MapTest2", test2);
        Assertions.assertEquals(cm2.getDataType(), "Map");
        Assertions.assertEquals(cm2.getParent(), "HashMap<String, Object>");
        Assertions.assertEquals(cm2.getClassname(), "MapTest2");

        Schema<?> test3 = openAPI.getComponents().getSchemas().get("MapTest3");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm3 = codegen.fromModel("MapTest3", test3);
        Assertions.assertEquals(cm3.getDataType(), "Map");
        Assertions.assertEquals(cm3.getParent(), "HashMap<String, Object>");
        Assertions.assertEquals(cm3.getClassname(), "MapTest3");

        Schema<?> other = openAPI.getComponents().getSchemas().get("OtherObj");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("OtherObj", other);
        Assertions.assertEquals(cm.getDataType(), "Object");
        Assertions.assertEquals(cm.getClassname(), "OtherObj");
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/3589
     */
    @Test
    public void testSchemaMapping() throws IOException {
        final Path output = newTempFolder();
        final ClientOptInput clientOptInput = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTEASY)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setSchemaMappings(Map.of("TypeAlias", "foo.bar.TypeAlias"))
                .setGenerateAliasAsModel(true)
                .setInputSpec("src/test/resources/3_0/type-alias.yaml")
                .setOutputDir(output.toString().replace("\\", "/"))
                .toClientOptInput();

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGenerateMetadata(false);
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);
        Assertions.assertEquals(clientOptInput.getConfig().schemaMapping().get("TypeAlias"), "foo.bar.TypeAlias");
        assertThat(files).hasSize(1)
                .contains(output.resolve("src/main/java/org/openapitools/client/model/ParentType.java").toFile());

        File file = files.stream().filter(f -> f.getName().endsWith("ParentType.java")).findFirst().get();
        String parentTypeContents = Files.readString(file.toPath());

        final Pattern FIELD_PATTERN = Pattern.compile(".* private (.*?) typeAlias;.*", Pattern.DOTALL);
        Matcher fieldMatcher = FIELD_PATTERN.matcher(parentTypeContents);
        Assertions.assertTrue(fieldMatcher.matches());

        // this is the type of the field 'typeAlias'. With a working schemaMapping it should
        // be 'foo.bar.TypeAlias' or just 'TypeAlias'
        Assertions.assertEquals(fieldMatcher.group(1), "foo.bar.TypeAlias");
    }

    @Test
    public void testBearerAuth() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/pingBearerAuth.yaml");

        List<CodegenSecurity> security = new JavaClientCodegen().fromSecurity(openAPI.getComponents().getSecuritySchemes());

        assertThat(security).hasSize(1)
                .first()
                .hasFieldOrPropertyWithValue("isBasic", Boolean.TRUE)
                .hasFieldOrPropertyWithValue("isBasicBasic", Boolean.FALSE)
                .hasFieldOrPropertyWithValue("isBasicBearer", Boolean.TRUE);
    }

    @Test
    public void testVertXAuthInfoWithHyphenSeparatedSecurityScheme() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.VERTX)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/ping-with-hyphen-separated-security-scheme.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        // Test that hyphen-separated security scheme names does not
        // break the Java VertX client code generation
        validateJavaSourceFiles(files);

        // Test that the name was correctly transformed to camelCase
        // starting with an uppercase letter
        assertThat(output.resolve("src/main/java/xyz/abcdef/ApiClient.java")).content()
                .contains(
                        "public static class AuthInfo {",
                        "public void addHyphenatedNameTestAuthentication(String bearerToken) {",
                        "public static AuthInfo forHyphenatedNameTestAuthentication(String bearerToken) {"
                );
    }

    private CodegenProperty codegenPropertyWithArrayOfIntegerValues() {
        CodegenProperty array = new CodegenProperty();
        final CodegenProperty items = new CodegenProperty();
        items.setAllowableValues(new HashMap<>(Map.of("values", Collections.singletonList(1))));
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
        assertEquals(codegen.toApiVarName("Default"), "_default");
        assertEquals(codegen.toApiVarName("int"), "_int");
        assertEquals(codegen.toApiVarName("pony"), "pony");
    }

    @Test
    public void testAnyType() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/any_type.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();

        Schema<?> test1 = openAPI.getComponents().getSchemas().get("AnyValueModel");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm1 = codegen.fromModel("AnyValueModel", test1);
        Assertions.assertEquals(cm1.getClassname(), "AnyValueModel");

        final CodegenProperty property1 = cm1.allVars.get(0);
        Assertions.assertEquals(property1.baseName, "any_value");
        Assertions.assertEquals(property1.dataType, "Object");
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);
        Assertions.assertFalse(property1.isFreeFormObject);
        Assertions.assertTrue(property1.isAnyType);

        final CodegenProperty property2 = cm1.allVars.get(1);
        Assertions.assertEquals(property2.baseName, "any_value_with_desc");
        Assertions.assertEquals(property2.dataType, "Object");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);
        Assertions.assertFalse(property2.isFreeFormObject);
        Assertions.assertTrue(property2.isAnyType);

        final CodegenProperty property3 = cm1.allVars.get(2);
        Assertions.assertEquals(property3.baseName, "any_value_nullable");
        Assertions.assertEquals(property3.dataType, "Object");
        Assertions.assertFalse(property3.required);
        Assertions.assertTrue(property3.isPrimitiveType);
        Assertions.assertFalse(property3.isContainer);
        Assertions.assertFalse(property3.isFreeFormObject);
        Assertions.assertTrue(property3.isAnyType);

        Schema<?> test2 = openAPI.getComponents().getSchemas().get("AnyValueModelInline");
        codegen.setOpenAPI(openAPI);
        CodegenModel cm2 = codegen.fromModel("AnyValueModelInline", test2);
        Assertions.assertEquals(cm2.getClassname(), "AnyValueModelInline");

        final CodegenProperty cp1 = cm2.vars.get(0);
        Assertions.assertEquals(cp1.baseName, "any_value");
        Assertions.assertEquals(cp1.dataType, "Object");
        Assertions.assertFalse(cp1.required);
        Assertions.assertTrue(cp1.isPrimitiveType);
        Assertions.assertFalse(cp1.isContainer);
        Assertions.assertFalse(cp1.isFreeFormObject);
        Assertions.assertTrue(cp1.isAnyType);

        final CodegenProperty cp2 = cm2.vars.get(1);
        Assertions.assertEquals(cp2.baseName, "any_value_with_desc");
        Assertions.assertEquals(cp2.dataType, "Object");
        Assertions.assertFalse(cp2.required);
        Assertions.assertTrue(cp2.isPrimitiveType);
        Assertions.assertFalse(cp2.isContainer);
        Assertions.assertFalse(cp2.isFreeFormObject);
        Assertions.assertTrue(cp2.isAnyType);

        final CodegenProperty cp3 = cm2.vars.get(2);
        Assertions.assertEquals(cp3.baseName, "any_value_nullable");
        Assertions.assertEquals(cp3.dataType, "Object");
        Assertions.assertFalse(cp3.required);
        Assertions.assertTrue(cp3.isPrimitiveType);
        Assertions.assertFalse(cp3.isContainer);
        Assertions.assertFalse(cp3.isFreeFormObject);
        Assertions.assertTrue(cp3.isAnyType);

        // map
        // Should allow in any type including map, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp4 = cm2.vars.get(3);
        Assertions.assertEquals(cp4.baseName, "map_free_form_object");
        Assertions.assertEquals(cp4.dataType, "Map<String, Object>");
        Assertions.assertFalse(cp4.required);
        Assertions.assertTrue(cp4.isPrimitiveType);
        Assertions.assertTrue(cp4.isContainer);
        Assertions.assertTrue(cp4.isMap);
        Assertions.assertTrue(cp4.isFreeFormObject);
        Assertions.assertFalse(cp4.isAnyType);
        Assertions.assertFalse(cp4.isModel);

        // Should allow in any type including map, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp5 = cm2.vars.get(4);
        Assertions.assertEquals(cp5.baseName, "map_any_value_with_desc");
        Assertions.assertEquals(cp5.dataType, "Map<String, Object>");
        Assertions.assertFalse(cp5.required);
        Assertions.assertTrue(cp5.isPrimitiveType);
        Assertions.assertTrue(cp5.isContainer);
        Assertions.assertTrue(cp5.isMap);
        Assertions.assertTrue(cp5.isFreeFormObject);
        Assertions.assertFalse(cp5.isAnyType);
        Assertions.assertFalse(cp5.isModel);

        // Should allow in any type including map, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp6 = cm2.vars.get(5);
        Assertions.assertEquals(cp6.baseName, "map_any_value_nullable");
        Assertions.assertEquals(cp6.dataType, "Map<String, Object>");
        Assertions.assertFalse(cp6.required);
        Assertions.assertTrue(cp6.isPrimitiveType);
        Assertions.assertTrue(cp6.isContainer);
        Assertions.assertTrue(cp6.isMap);
        Assertions.assertTrue(cp6.isFreeFormObject);
        Assertions.assertFalse(cp6.isAnyType);

        // array
        // Should allow in any type including array, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp7 = cm2.vars.get(6);
        Assertions.assertEquals(cp7.baseName, "array_any_value");
        Assertions.assertEquals(cp7.dataType, "List<Object>");
        Assertions.assertFalse(cp7.required);
        Assertions.assertTrue(cp7.isPrimitiveType);
        Assertions.assertTrue(cp7.isContainer);
        Assertions.assertTrue(cp7.isArray);
        Assertions.assertFalse(cp7.isFreeFormObject);
        Assertions.assertFalse(cp7.isAnyType);

        // Should allow in any type including array, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp8 = cm2.vars.get(7);
        Assertions.assertEquals(cp8.baseName, "array_any_value_with_desc");
        Assertions.assertEquals(cp8.dataType, "List<Object>");
        Assertions.assertFalse(cp8.required);
        Assertions.assertTrue(cp8.isPrimitiveType);
        Assertions.assertTrue(cp8.isContainer);
        Assertions.assertTrue(cp8.isArray);
        Assertions.assertFalse(cp8.isFreeFormObject);
        Assertions.assertFalse(cp8.isAnyType);

        // Should allow in any type including array, https://github.com/swagger-api/swagger-parser/issues/1603
        final CodegenProperty cp9 = cm2.vars.get(8);
        Assertions.assertEquals(cp9.baseName, "array_any_value_nullable");
        Assertions.assertEquals(cp9.dataType, "List<Object>");
        Assertions.assertFalse(cp9.required);
        Assertions.assertTrue(cp9.isPrimitiveType);
        Assertions.assertTrue(cp9.isContainer);
        Assertions.assertTrue(cp9.isArray);
        Assertions.assertFalse(cp9.isFreeFormObject);
        Assertions.assertFalse(cp9.isAnyType);
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/4803
     * <p>
     * UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
     * We will contact the contributor of the following test to see if the fix will break their use cases and
     * how we can fix it accordingly.
     */
    @Test(enabled = false)
    public void testRestTemplateFormMultipart() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/MultipartApi.java")).content()
                .contains(
                        "multipartArrayWithHttpInfo(List<File> files)",
                        "formParams.addAll(\"files\","
                                + " files.stream().map(FileSystemResource::new).collect(Collectors.toList()));",

                        // mixed
                        "multipartMixedWithHttpInfo(File file, MultipartMixedMarker marker)",
                        "formParams.add(\"file\", new FileSystemResource(file));",

                        // single file
                        "multipartSingleWithHttpInfo(File file)",
                        "formParams.add(\"file\", new FileSystemResource(file));"
                );
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/4803
     * <p>
     * UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
     * We will contact the contributor of the following test to see if the fix will break their use cases and
     * how we can fix it accordingly.
     */
    @Test(enabled = false)
    public void testWebClientFormMultipart() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/MultipartApi.java")).content()
                .contains(
                        // multiple files
                        "multipartArray(List<File> files)",
                        "formParams.addAll(\"files\","
                                + " files.stream().map(FileSystemResource::new).collect(Collectors.toList()));",

                        // mixed
                        "multipartMixed(File file, MultipartMixedMarker marker)",
                        "formParams.add(\"file\", new FileSystemResource(file));",

                        // single file
                        "multipartSingle(File file)",
                        "formParams.add(\"file\", new FileSystemResource(file));"
                );
    }

    @Test
    public void shouldGenerateBlockingAndNoBlockingOperationsForWebClient() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.WEBCLIENT_BLOCKING_OPERATIONS, true)
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setInputSpec("src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("StoreApi.java"))
                .assertMethod("getInventory")
                .hasReturnType(
                        "Mono<Map<String, Integer>>") // explicit 'x-webclient-blocking: false' which overrides
                // global config
                .toFileAssert()
                .assertMethod("placeOrder")
                .hasReturnType("Order"); // use global config

        JavaFileAssert.assertThat(files.get("PetApi.java"))
                .assertMethod("findPetsByStatus")
                .hasReturnType(
                        "List<Pet>"); // explicit 'x-webclient-blocking: true' which overrides global config
    }

    @Test
    public void testAllowModelWithNoProperties() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setInputSpec("src/test/resources/2_0/emptyBaseModel.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/RealCommand.java"))
                .content().contains("class RealCommand {");
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Command.java"))
                .content().contains("class Command {");
        assertThat(files).hasSize(49).contains(
                output.resolve("src/main/java/org/openapitools/client/model/RealCommand.java").toFile(),
                output.resolve("src/main/java/org/openapitools/client/model/Command.java").toFile()
        );
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/6715
     * <p>
     * UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
     * We will contact the contributor of the following test to see if the fix will break their use cases and
     * how we can fix it accordingly.
     */
    @Test(enabled = false)
    public void testRestTemplateWithUseAbstractionForFiles() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("/src/main/java/xyz/abcdef/api/MultipartApi.java")).content().contains(
                // multiple files
                "multipartArray(java.util.Collection<org.springframework.core.io.Resource> files)",
                "multipartArrayWithHttpInfo(java.util.Collection<org.springframework.core.io.Resource>"
                        + " files)",
                "formParams.addAll(\"files\", files.stream().collect(Collectors.toList()));",

                // mixed
                "multipartMixed(org.springframework.core.io.Resource file, MultipartMixedMarker marker)",
                "multipartMixedWithHttpInfo(org.springframework.core.io.Resource file, MultipartMixedMarker"
                        + " marker)",
                "formParams.add(\"file\", file);",

                // single file
                "multipartSingle(org.springframework.core.io.Resource file)",
                "multipartSingleWithHttpInfo(org.springframework.core.io.Resource file)",
                "formParams.add(\"file\", file);"
        );
    }

    @Test
    void testNotDuplicateOauth2FlowsScopes() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7614.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(new JavaClientCodegen());
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        defaultGenerator.opts(clientOptInput);

        final Map<String, List<CodegenOperation>> paths = defaultGenerator.processPaths(openAPI.getPaths());

        final List<CodegenOperation> codegenOperations = paths.values().stream().flatMap(Collection::stream).collect(Collectors.toList());
        final CodegenOperation getWithBasicAuthAndOauth = getByOperationId(codegenOperations, "getWithBasicAuthAndOauth");
        assertEquals(getWithBasicAuthAndOauth.authMethods.size(), 3);
        assertEquals(getWithBasicAuthAndOauth.authMethods.get(0).name, "basic_auth");

        final Map<String, Object> passwordFlowScope = getWithBasicAuthAndOauth.authMethods.get(1).scopes.get(0);
        assertEquals(passwordFlowScope.get("scope"), "something:create");
        assertEquals(passwordFlowScope.get("description"), "create from password flow");

        final Map<String, Object> clientCredentialsFlow = getWithBasicAuthAndOauth.authMethods.get(2).scopes.get(0);
        assertEquals(clientCredentialsFlow.get("scope"), "something:create");
        assertEquals(clientCredentialsFlow.get("description"), "create from client credentials flow");

        final CodegenOperation getWithOauthAuth = getByOperationId(codegenOperations, "getWithOauthAuth");
        assertEquals(getWithOauthAuth.authMethods.size(), 2);

        final Map<String, Object> passwordFlow = getWithOauthAuth.authMethods.get(0).scopes.get(0);
        assertEquals(passwordFlow.get("scope"), "something:create");
        assertEquals(passwordFlow.get("description"), "create from password flow");

        final Map<String, Object> clientCredentialsCreateFlow = getWithOauthAuth.authMethods.get(1).scopes.get(0);
        assertEquals(clientCredentialsCreateFlow.get("scope"), "something:create");
        assertEquals(clientCredentialsCreateFlow.get("description"), "create from client credentials flow");

        final Map<String, Object> clientCredentialsProcessFlow = getWithOauthAuth.authMethods.get(1).scopes.get(1);
        assertEquals(clientCredentialsProcessFlow.get("scope"), "something:process");
        assertEquals(clientCredentialsProcessFlow.get("description"), "process from client credentials flow");
    }

    private CodegenOperation getByOperationId(List<CodegenOperation> codegenOperations, String operationId) {
        return getByCriteria(codegenOperations, (co) -> co.operationId.equals(operationId))
                .orElseThrow(
                        () -> new IllegalStateException(
                                String.format(Locale.ROOT, "Operation with id [%s] does not exist", operationId)
                        )
                );
    }

    private Optional<CodegenOperation> getByCriteria(List<CodegenOperation> codegenOperations, Predicate<CodegenOperation> filter) {
        return codegenOperations.stream()
                .filter(filter)
                .findFirst();
    }

    @Test
    public void testCustomMethodParamsAreCamelizedWhenUsingFeign() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(FEIGN)
                .setInputSpec("src/test/resources/3_0/issue_7791.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);
        var defaultApiFile = output.resolve("src/main/java/org/openapitools/client/api/DefaultApi.java");
        assertThat(files).contains(defaultApiFile.toFile());
        assertThat(defaultApiFile).content()
                .doesNotContain("event_id")
                .contains(
                        "@RequestLine(\"POST /events/{eventId}:undelete\")",
                        // baseName is kept for form parameters
                        "@Param(\"some_file\") @javax.annotation.Nullable File someFile"
                );
    }

    /**
     * see https://github.com/OpenAPITools/openapi-generator/issues/19895
     */
    @Test
    public void testCharsetInContentTypeCorrectlyEncodedForFeignApi_issue19895() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(FEIGN)
                .setInputSpec("src/test/resources/3_0/issue_19895.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);
        var defaultApiFile = output.resolve("src/main/java/org/openapitools/client/api/DefaultApi.java");
        assertThat(files).contains(defaultApiFile.toFile());
        assertThat(defaultApiFile).content()
                .doesNotContain(
                        "Content-Type: application/json;charset&#x3D;utf-8",
                        "Accept: application/json;charset&#x3D;utf-8")
                .contains(
                        "Content-Type: application/json;charset=utf-8",
                        "Accept: application/json;charset=utf-8"
                );
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/6715
     * <p>
     * UPDATE: the following test has been ignored due to https://github.com/OpenAPITools/openapi-generator/pull/11081/
     * We will contact the contributor of the following test to see if the fix will break their use cases and
     * how we can fix it accordingly.
     */
    @Test(enabled = false)
    public void testWebClientWithUseAbstractionForFiles() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/MultipartApi.java")).content()
                .contains(
                        // multiple files
                        "multipartArray(java.util.Collection<org.springframework.core.io.AbstractResource> files)",
                        "formParams.addAll(\"files\", files.stream().collect(Collectors.toList()));",

                        // mixed
                        "multipartMixed(org.springframework.core.io.AbstractResource file, MultipartMixedMarker"
                                + " marker)",
                        "formParams.add(\"file\", file);",

                        // single file
                        "multipartSingle(org.springframework.core.io.AbstractResource file)",
                        "formParams.add(\"file\", file);"
                );
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/8352
     */
    @Test
    public void testRestTemplateWithFreeFormInQueryParameters() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue8352.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(output.resolve("src/main/java/xyz/abcdef/ApiClient.java")).content()
                .contains("value instanceof Map");
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/8352
     */
    @Test
    public void testWebClientWithFreeFormInQueryParameters() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue8352.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/ApiClient.java")).content()
                .contains("value instanceof Map");
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/11242
     */
    @Test
    public void testNativeClientWhiteSpacePathParamEncoding() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue11242.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).hasSize(37);
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/ApiClient.java"),
                "public static String urlEncode(String s) { return URLEncoder.encode(s,"
                        + " UTF_8).replaceAll(\"\\\\+\", \"%20\"); }"
        );
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/4808
     */
    @Test
    public void testNativeClientExplodedQueryParamObject() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue4808.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).hasSize(40);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/DefaultApi.java")).content()
                .contains(
                        "localVarQueryParams.addAll(ApiClient.parameterToPairs(\"since\", queryObject.getSince()));",
                        "localVarQueryParams.addAll(ApiClient.parameterToPairs(\"sinceBuild\", queryObject.getSinceBuild()));",
                        "localVarQueryParams.addAll(ApiClient.parameterToPairs(\"maxBuilds\", queryObject.getMaxBuilds()));",
                        "localVarQueryParams.addAll(ApiClient.parameterToPairs(\"maxWaitSecs\", queryObject.getMaxWaitSecs()));"
                );
    }

    @Test
    public void testDefaultMicroprofileRestClientVersion() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).contains(output.resolve("pom.xml").toFile());
        assertThat(output.resolve("src/main/java/org/openapitools/client/api/PetApi.java")).content()
                .contains("import javax.");
        assertThat(output.resolve("pom.xml")).content()
                .contains(
                        "<microprofile.rest.client.api.version>2.0</microprofile.rest.client.api.version>",
                        "<smallrye.rest.client.version>1.2.1</smallrye.rest.client.version>",
                        "<java.version>1.8</java.version>"
                );
    }

    @Test
    public void testMicroprofileRestClientVersion_1_4_1() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "1.4.1"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).contains(output.resolve("pom.xml").toFile());
        assertThat(output.resolve("src/main/java/org/openapitools/client/api/PetApi.java")).content()
                .contains("import javax.");
        assertThat(output.resolve("pom.xml")).content()
                .contains(
                        "<microprofile.rest.client.api.version>1.4.1</microprofile.rest.client.api.version>",
                        "<smallrye.rest.client.version>1.2.1</smallrye.rest.client.version>",
                        "<java.version>1.8</java.version>"
                );
    }

    @Test(
            expectedExceptions = IllegalArgumentException.class,
            expectedExceptionsMessageRegExp =
                    "Version incorrectVersion of MicroProfile Rest Client is not supported or incorrect."
                            + " Supported versions are 1.4.1, 2.0, 3.0"
    )
    public void testMicroprofileRestClientIncorrectVersion() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "incorrectVersion"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        fail("Expected an exception that did not occur");
    }

    @Test
    public void testMicroprofileRestClientVersion_3_0() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).contains(output.resolve("pom.xml").toFile());
        assertThat(output.resolve("src/main/java/org/openapitools/client/api/PetApi.java")).content()
                .contains("import jakarta.");
        assertThat(output.resolve("pom.xml")).content()
                .contains(
                        "<microprofile.rest.client.api.version>3.0</microprofile.rest.client.api.version>",
                        "<jersey.mp.rest.client.version>3.0.4</jersey.mp.rest.client.version>",
                        "<java.version>11</java.version>"
                );
    }

    @Test
    public void testMicroprofileGenerateCorrectJsonbCreator_issue12622() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/bugs/issue_12622.json")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Foo.java"))
                .assertConstructor("String", "Integer")
                .hasParameter("b")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes(
                        "JsonbProperty", ImmutableMap.of("value", "\"b\"", "nillable", "true"))
                .toParameter()
                .toConstructor()
                .hasParameter("c")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("JsonbProperty", ImmutableMap.of("value", "\"c\""));
    }

    @Test
    public void testMicroprofileGenerateCorrectJacksonGenerator_issue18336() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0");
        properties.put(CodegenConstants.SERIALIZATION_LIBRARY, JavaClientCodegen.SERIALIZATION_LIBRARY_JACKSON);

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()

                .setAdditionalProperties(properties)
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.MICROPROFILE)
                .setInputSpec("src/test/resources/bugs/issue_18336.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(clientOptInput).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Pet.java"))
                .assertConstructor("String")
                .assertConstructorAnnotations()
                .containsWithName("JsonCreator")
                .toConstructor()
                .hasParameter("name")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "JSON_PROPERTY_NAME", "required", "true"));
    }

    @Test
    public void testJavaClientDefaultValues_issueNoNumber() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(JavaClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setOutputDir(output.toString().replace("\\", "/"))
                .setInputSpec("src/test/resources/bugs/java-codegen-empty-array-as-default-value/issue_wrong-default.yaml");

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("DefaultValuesType.java"))
                .assertProperty("stringDefault")
                .asString().endsWith("= new ArrayList<>();");
        JavaFileAssert.assertThat(files.get("DefaultValuesType.java"))
                .assertProperty("stringDefault2")
                .asString().endsWith("= new ArrayList<>(Arrays.asList(\"Hallo\", \"Huhu\"));");
        JavaFileAssert.assertThat(files.get("DefaultValuesType.java"))
                .assertProperty("objectDefault")
                .asString().endsWith("= new ArrayList<>();");
    }

    @Test
    public void testWebClientJsonCreatorWithNullable_issue12790() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setAdditionalProperties(Map.of(AbstractJavaCodegen.OPENAPI_NULLABLE, "true"))
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setInputSpec("src/test/resources/bugs/issue_12790.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
                .printFileContent()
                .assertConstructor("String", "String")
                .bodyContainsLines(
                        "this.nullableProperty = nullableProperty == null ? JsonNullable.<String>undefined() :"
                                + " JsonNullable.of(nullableProperty);",
                        "this.notNullableProperty = notNullableProperty;");
    }

    @Test
    public void testRestTemplateResponseTypeWithUseAbstractionForFiles() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setInputSpec("src/test/resources/3_0/issue13146_file_abstraction_response.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/ResourceApi.java")).content()
                .contains(
                        "org.springframework.core.io.Resource resourceInResponse()",
                        "ResponseEntity<org.springframework.core.io.Resource> resourceInResponseWithHttpInfo()",
                        "ParameterizedTypeReference<org.springframework.core.io.Resource> localReturnType = new"
                                + " ParameterizedTypeReference<org.springframework.core.io.Resource>()"
                );
    }

    @Test(dataProvider = "supportedLibraries")
    void testExtraAnnotations(Library library) {
        final Path output = newTempFolder();
        final String outputPath = output.toString().replace('\\', '/');
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(library.value)
                .setAdditionalProperties(Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"))
                .setInputSpec("src/test/resources/3_0/issue_11772.yml")
                .setOutputDir(outputPath);

        final DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.assertExtraAnnotationFiles(outputPath + "/src/main/java/org/openapitools/client/model");
    }

    /**
     * See https://github.com/OpenAPITools/openapi-generator/issues/11340
     */
    @Test
    public void testReferencedHeader2() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("java")
                .setAdditionalProperties(Map.of(BeanValidationFeatures.USE_BEANVALIDATION, "true"))
                .setInputSpec("src/test/resources/3_0/issue-11340.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("DefaultApi.java"))
                .assertMethod("operationWithHttpInfo")
                .assertParameter("requestBody")
                .assertParameterAnnotations()
                .containsWithName("NotNull")
                .toParameter().toMethod()
                .assertParameter("xNonNullHeaderParameter")
                .assertParameterAnnotations()
                .containsWithName("NotNull");
    }

    @Test
    public void testReturnTypeMapping() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setInputSpec("src/test/resources/3_0/issue14525.yaml")
                .addTypeMapping("array", "Stack")
                .addImportMapping("Stack", "java.util.Stack")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/api/DefaultApi.java")).content()
                .contains("import java.util.Stack;");
    }

    @Test
    public void testNativeClientExplodedQueryParamWithArrayProperty() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/exploded-query-param-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                Paths.get(output + "/src/main/java/xyz/abcdef/api/DefaultApi.java"),
                "localVarQueryParams.addAll(ApiClient.parameterToPairs(\"multi\", \"values\","
                        + " queryObject.getValues()));");
    }

    @Test
    public void testJdkHttpClientWithAndWithoutParentExtension() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                // use default `okhttp-gson`
                //.setLibrary(JavaClientCodegen.NATIVE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(CodegenConstants.MODEL_PACKAGE, "xyz.abcdef.model")
                .addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, "xyz.abcdef.invoker")
                .setInputSpec("src/test/resources/3_0/allOf_extension_parent.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(files).hasSize(33);
        assertThat(output.resolve("src/main/java/xyz/abcdef/model/Child.java"))
                .content().contains("public class Child extends Person {");
        assertThat(output.resolve("src/main/java/xyz/abcdef/model/Adult.java"))
                .content().contains("public class Adult extends Person {");
        assertThat(output.resolve("src/main/java/xyz/abcdef/model/AnotherChild.java"))
                .content().contains("public class AnotherChild {");
    }

    @Test
    public void testDiscriminatorWithMappingIssue14731() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14731.yaml", null, new ParseOptions())
                .getOpenAPI();

        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);
        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setUseJakartaEe(true);
        codegen.setModelNameSuffix("DTO");

        final ClientOptInput input = new ClientOptInput().openAPI(openAPI).config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/ChildWithMappingADTO.java"))
                .content().doesNotContain("@JsonTypeName");
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/ChildWithMappingBDTO.java"))
                .content().doesNotContain("@JsonTypeName");
    }

    @Test
    public void testDiscriminatorWithoutMappingIssue14731() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14731.yaml", null, new ParseOptions())
                .getOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.setUseOneOfInterfaces(true);
        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setUseJakartaEe(true);
        codegen.setModelNameSuffix("DTO");
        codegen.setLibrary(JavaClientCodegen.RESTTEMPLATE);

        final ClientOptInput input = new ClientOptInput().openAPI(openAPI).config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/ChildWithoutMappingADTO.java"))
                .content().contains("@JsonTypeName");
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/ChildWithoutMappingBDTO.java"))
                .content().contains("@JsonTypeName");
    }

    @Test
    public void testForJavaNativeJsonSubtype() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14917.yaml", null, new ParseOptions())
                .getOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setLibrary(JavaClientCodegen.NATIVE);
        codegen.setOutputDir(output.toString());

        new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Cat.java")).content()
                .contains("mappings.put(\"Cat\", Cat.class)")
                .doesNotContain(
                        "@JsonSubTypes",
                        "mappings.put(\"cat\", Cat.class);",
                        "mappings.put(\"dog\", Dog.class);",
                        "mappings.put(\"lizard\", Lizard.class);"
                );
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Pet.java")).content()
                .contains(
                        "@JsonSubTypes.Type(value = Cat.class, name = \"cat\")",
                        "@JsonSubTypes.Type(value = Dog.class, name = \"dog\")",
                        "@JsonSubTypes.Type(value = Lizard.class, name = \"lizard\")",
                        "mappings.put(\"cat\", Cat.class)",
                        "mappings.put(\"dog\", Dog.class)",
                        "mappings.put(\"lizard\", Lizard.class)",
                        "mappings.put(\"Pet\", Pet.class)"
                ).doesNotContain(
                        "@JsonSubTypes.Type(value = Cat.class, name = \"Cat\")",
                        "@JsonSubTypes.Type(value = Dog.class, name = \"Dog\")",
                        "@JsonSubTypes.Type(value = Lizard.class, name = \"Lizard\")"
                );
    }

    @Test
    public void shouldProperlyExplodeRestTemplateQueryParameters_issue907() {

        final Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/explode-query-parameter.yaml",
                JavaClientCodegen.RESTTEMPLATE
        );

        JavaFileAssert.assertThat(files.get("DefaultApi.java"))
                .printFileContent()
                .assertMethod("searchWithHttpInfo")
                .bodyContainsLines(
                        "localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, \"regular-param\","
                                + " regularParam));")
                .bodyContainsLines(
                        "localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someString\","
                                + " objectParam.getSomeString()));")
                .bodyContainsLines(
                        "localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someBoolean\","
                                + " objectParam.getSomeBoolean()));")
                .bodyContainsLines(
                        "localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someInteger\","
                                + " objectParam.getSomeInteger()));");
    }

    @Test
    public void shouldProperlyExplodeWebClientQueryParameters() {
        final Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/explode-query-parameter.yaml",
                JavaClientCodegen.WEBCLIENT
        );

        JavaFileAssert.assertThat(files.get("DefaultApi.java"))
                .printFileContent()
                .assertMethod("searchRequestCreation")
                .bodyContainsLines(
                        "queryParams.putAll(apiClient.parameterToMultiValueMap(null, \"regular-param\","
                                + " regularParam));")
                .bodyContainsLines(
                        "queryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someString\","
                                + " objectParam.getSomeString()));")
                .bodyContainsLines(
                        "queryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someBoolean\","
                                + " objectParam.getSomeBoolean()));")
                .bodyContainsLines(
                        "queryParams.putAll(apiClient.parameterToMultiValueMap(null, \"someInteger\","
                                + " objectParam.getSomeInteger()));");
    }

    private static Map<String, File> generateFromContract(final String pathToSpecification, final String library) {
        return generateFromContract(pathToSpecification, library, new HashMap<>());
    }

    @SneakyThrows
    private static Map<String, File> generateFromContract(
            final String pathToSpecification,
            final String library,
            final Map<String, Object> properties
    ) {
        return generateFromContract(pathToSpecification, library, properties, configurator -> {
        });
    }

    @SneakyThrows
    private static Map<String, File> generateFromContract(
            final String pathToSpecification,
            final String library,
            final Map<String, Object> properties,
            final Consumer<CodegenConfigurator> consumer
    ) {
        final Path output = newTempFolder();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(library)
                .setAdditionalProperties(properties)
                .setInputSpec(pathToSpecification)
                .setOutputDir(output.toString());
        consumer.accept(configurator);
        return new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .stream().collect(Collectors.toMap(File::getName, Function.identity()));
    }

    @Test
    public void testForJavaApacheHttpClientJsonSubtype() {
        final Path output = newTempFolder();
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14917.yaml", null, new ParseOptions())
                .getOpenAPI();

        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setLibrary(JavaClientCodegen.APACHE);
        codegen.setOutputDir(output.toString());

        new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Cat.java")).content()
                .contains(
                        "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property ="
                                + " \"petType\", visible = true)"
                ).doesNotContain(
                        "mappings.put",
                        "@JsonSubTypes.Type(value = Cat.class, name = \"cat\")",
                        "@JsonSubTypes.Type(value = Dog.class, name = \"dog\")",
                        "@JsonSubTypes.Type(value = Lizard.class, name = \"lizard\")"
                );

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Pet.java")).content()
                .contains(
                        "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property ="
                                + " \"petType\", visible = true)",
                        "@JsonSubTypes.Type(value = Cat.class, name = \"cat\")",
                        "@JsonSubTypes.Type(value = Dog.class, name = \"dog\")",
                        "@JsonSubTypes.Type(value = Lizard.class, name = \"lizard\")"
                ).doesNotContain(
                        "@JsonSubTypes.Type(value = Cat.class, name = \"Cat\")",
                        "@JsonSubTypes.Type(value = Dog.class, name = \"Dog\")",
                        "@JsonSubTypes.Type(value = Lizard.class, name = \"Lizard\")"
                );
    }

    @Test
    public void testIsOverriddenProperty() {
        final OpenAPI openAPI =
                TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenModel cm1 = codegen.fromModel("Cat", openAPI.getComponents().getSchemas().get("Cat"));

        CodegenProperty cp0 = cm1.getAllVars().get(0);
        Assertions.assertEquals(cp0.getName(), "petType");
        Assertions.assertEquals(cp0.isOverridden, true);

        CodegenProperty cp1 = cm1.getAllVars().get(1);
        Assertions.assertEquals(cp1.getName(), "name");
        Assertions.assertEquals(cp1.isOverridden, false);
    }

    @Test
    public void testForJavaApacheHttpClientOverrideSetter() {
        final Path output = newTempFolder();
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/allOf_composition_discriminator.yaml", null, null)
                .getOpenAPI();

        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.setLibrary(JavaClientCodegen.APACHE);

        new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Cat.java")).content()
                .contains("  @Override\n" + "  public Cat petType(@javax.annotation.Nonnull String petType) {");
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Pet.java")).content()
                .contains("  }\n" + "\n" + "  public Pet petType(@javax.annotation.Nonnull String petType) {\n");
    }

    @Test
    public void testForJavaNativeClientOverrideSetter() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/allOf_composition_discriminator.yaml", null, null)
                .getOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.setLibrary(JavaClientCodegen.NATIVE);

        new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Cat.java")).content()
                .contains("  @Override\n" + "  public Cat petType(@javax.annotation.Nonnull String petType) {");
        assertThat(output.resolve("src/main/java/org/openapitools/client/model/Pet.java")).content()
                .contains("  }\n" + "\n" + "  public Pet petType(@javax.annotation.Nonnull String petType) {\n");
    }

    @Test
    public void testDeprecatedProperty() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setInputSpec("src/test/resources/3_0/deprecated-properties.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/org/openapitools/client/model/BigDog.java"),
                "@Deprecated\n public BigDog declawed(@javax.annotation.Nullable Boolean declawed) {", // deprecated builder method
                "@Deprecated\n @javax.annotation.Nullable\n\n public Boolean getDeclawed() {", // deprecated getter
                "@Deprecated\n" + " public void setDeclawed(@javax.annotation.Nullable Boolean declawed) {" // deprecated setter
        );
    }

    @Test
    public void testDeprecatedPropertyJersey3() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.JERSEY3)
                .setInputSpec("src/test/resources/3_0/deprecated-properties.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/org/openapitools/client/model/BigDog.java"),
                "@Deprecated\n public BigDog declawed(@jakarta.annotation.Nullable Boolean declawed) {", // deprecated builder method
                "@Deprecated\n @jakarta.annotation.Nullable\n @JsonProperty(JSON_PROPERTY_DECLAWED)\n"
                        + " @JsonInclude(value = JsonInclude.Include.USE_DEFAULTS)\n\n"
                        + " public Boolean getDeclawed() {", // deprecated getter
                "@Deprecated\n @JsonProperty(JSON_PROPERTY_DECLAWED)\n"
                        + " @JsonInclude(value = JsonInclude.Include.USE_DEFAULTS)\n"
                        + " public void setDeclawed(@jakarta.annotation.Nullable Boolean declawed) {" // deprecated setter
        );
    }

    @DataProvider
    public static Object[][] librariesToRegressionTestForIssue15684() {
        return new Object[][]{{"okhttp-gson"}, {"jersey2"}, {"jersey3"}, {"native"}};
    }

    @Test(dataProvider = "librariesToRegressionTestForIssue15684")
    public void shouldNotAddAdditionalModelAnnotationsToAbstractOpenApiSchema_issue15684(String library) {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(library)
                .addAdditionalProperty(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@annotation1;@annotation2")
                .setInputSpec("src/test/resources/3_0/deprecated-properties.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("AbstractOpenApiSchema.java"))
                .assertTypeAnnotations()
                .doesNotContainWithName("annotation1")
                .doesNotContainWithName("annotation2");
        JavaFileAssert.assertThat(files.get("Animal.java"))
                .assertTypeAnnotations()
                .containsWithName("annotation1")
                .containsWithName("annotation2");
    }

    @Test
    public void testRestTemplateWithGeneratedClientAsBeanDisabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.GENERATE_CLIENT_AS_BEAN, false)
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileNotContains(output.resolve("src/main/java/xyz/abcdef/ApiClient.java"), "@Component");
        TestUtils.assertFileNotContains(output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"), "@Component");
    }

    @Test
    public void testRestTemplateWithGeneratedClientAsBeanEnabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.GENERATE_CLIENT_AS_BEAN, true)
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/ApiClient.java"), "@Component");
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"), "@Component");
    }

    @Test
    public void testRestTemplateWithUseBeanValidationEnabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_BEANVALIDATION, true)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("pom.xml"), "<artifactId>jakarta.validation-api</artifactId>");
        TestUtils.assertFileContains(output.resolve("src/main/java/org/openapitools/client/model/Pet.java"), "@Valid");
    }

    @Test
    public void testRestTemplateWithUseBeanValidationDisabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_BEANVALIDATION, false)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileNotContains(output.resolve("pom.xml"), "<artifactId>jakarta.validation-api</artifactId>");
        TestUtils.assertFileNotContains(output.resolve("src/main/java/org/openapitools/client/model/Pet.java"), "@Valid");
    }

    @Test
    public void testRestTemplateWithPerformBeanValidationEnabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.PERFORM_BEANVALIDATION, true)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("pom.xml"), "<artifactId>hibernate-validator</artifactId>");
        TestUtils.assertFileExists(output.resolve("src/main/java/xyz/abcdef/BeanValidationException.java"));
    }

    @Test
    public void testRestTemplateWithPerformBeanValidationDisabled() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.PERFORM_BEANVALIDATION, false)
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileNotContains(output.resolve("pom.xml"), "<artifactId>hibernate-validator</artifactId>");
        TestUtils.assertFileNotExists(output.resolve("src/main/java/org/openapitools/client/invoker/BeanValidationException.java"));
    }

    @Test
    public void testLogicToAvoidStackOverflow() {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.GENERATE_CLIENT_AS_BEAN, true)
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setInputSpec("src/test/resources/3_0/issue_12929.yaml")
                .setOutputDir(newTempFolder().toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        // shouldn't throw stackoverflow exception
    }

    @Test
    public void testWebClientSupportListOfStringReturnType_issue7118() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setInputSpec("src/test/resources/bugs/issue_7118.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        assertThat(output.resolve("src/main/java/xyz/abcdef/api/UsersApi.java")).content()
                .contains(
                        // set of string
                        "ParameterizedTypeReference<Set<String>> localVarReturnType = new"
                                + " ParameterizedTypeReference<Set<String>>() {};",
                        "getUserIdSetRequestCreation().toEntity(localVarReturnType)",
                        // list of string
                        "ParameterizedTypeReference<List<String>> localVarReturnType = new"
                                + " ParameterizedTypeReference<List<String>>() {};",
                        "getUserIdListRequestCreation().toEntity(localVarReturnType)"
                );
    }

    @Test
    public void testEnumCaseInsensitive_issue8084() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue8084.yaml");
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(newTempFolder().toString());
        codegen.additionalProperties().put(USE_ENUM_CASE_INSENSITIVE, "true");

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("EnumTest.java"))
                .assertMethod("fromValue")
                .bodyContainsLines("if (b.value.equalsIgnoreCase(value)) {");
    }

    @Test
    public void testEnumCaseSensitive_issue8084() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/issue8084.yaml");
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(newTempFolder().toString());
        codegen.additionalProperties().put(USE_ENUM_CASE_INSENSITIVE, "false");

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("EnumTest.java"))
                .assertMethod("fromValue")
                .bodyContainsLines("if (b.value.equals(value)) {");
    }

    @Test
    public void testMapOfInnerEnum_issue19393() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_19393_map_of_inner_enum.yaml");
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(newTempFolder().toString());

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("EmployeeWithMapOfEnum.java"))
                .assertProperty("projectRole")
                .withType("Map<String, InnerEnum>");

        JavaFileAssert.assertThat(files.get("EmployeeWithMultiMapOfEnum.java"))
                .assertProperty("projectRoles")
                .withType("Map<String, Set<InnerEnum>>");
    }

    @Test
    public void testWebClientResponseTypeWithUseAbstractionForFiles_issue16589() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setInputSpec("src/test/resources/3_0/issue13146_file_abstraction_response.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/api/ResourceApi.java"),
                "Mono<org.springframework.core.io.Resource> resourceInResponse()",
                "Mono<ResponseEntity<org.springframework.core.io.Resource>> resourceInResponseWithHttpInfo()",
                "ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<org.springframework.core.io.Resource>()"
        );
    }

    @Test
    public void testHandleConstantParams() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/autoset_constant.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.additionalProperties().put(CodegenConstants.AUTOSET_CONSTANTS, "true");
        codegen.setAutosetConstants(true);

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        File apiFile = files.get("HelloExampleApi.java");
        Assertions.assertNotNull(apiFile);
        JavaFileAssert.assertThat(apiFile)
                .assertMethod("helloCall", "String", "ApiCallback")
                .bodyContainsLines("localVarHeaderParams.put(\"X-CUSTOM_CONSTANT_HEADER\", \"CONSTANT_VALUE\")");
    }

    @Test
    public void testAllOfWithSinglePrimitiveTypeRef() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allof_primitive.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.setAutosetConstants(true);

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        assertNull(files.get("AllOfDatetime.java"));
    }

    @Test
    public void testOpenapiGeneratorIgnoreListOption() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allof_primitive.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());
        codegen.setAutosetConstants(true);
        codegen.openapiGeneratorIgnoreList().add("README.md");
        codegen.openapiGeneratorIgnoreList().add("pom.xml");

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        // make sure README.md and pom.xml are not generated
        assertNull(files.get("README.md"));
        assertNull(files.get("pom.xml"));
    }

    @Test
    public void testEnumDiscriminatorDefaultValueIsNotString() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
                "src/test/resources/3_0/enum_discriminator_inheritance.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        Map<String, String> expectedContents = Map.of(
                "Cat", "this.petType = PetTypeEnum.CATTY",
                "Dog", "this.petType = PetTypeEnum.DOG",
                "Gecko", "this.petType = PetTypeEnum.GECKO",
                "Chameleon", "this.petType = PetTypeEnum.CAMO",
                "MiniVan", "this.carType = CarType.MINI_VAN",
                "CargoVan", "this.carType = CarType.CARGO_VAN",
                "SUV", "this.carType = CarType.SUV",
                "Truck", "this.carType = CarType.TRUCK",
                "Sedan", "this.carType = CarType.SEDAN"

        );
        for (Map.Entry<String, String> e : expectedContents.entrySet()) {
            String modelName = e.getKey();
            String expectedContent = e.getValue();
            File entityFile = files.get(modelName + ".java");
            assertNotNull(entityFile);
            assertThat(entityFile).content().doesNotContain("Type = this.getClass().getSimpleName();");
            assertThat(entityFile).content().contains(expectedContent);
        }
    }

    @Test
    public void testRestTemplateHandleURIEnum() {
        String[] expectedInnerEnumLines = new String[]{
                "V1_SCHEMA_JSON(URI.create(\"https://example.com/v1/schema.json\"))",
                "V2_SCHEMA_JSON(URI.create(\"https://example.com/v2/schema.json\"))"
        };

        String[] expectedEnumLines = new String[]{
                "V1_METADATA_JSON(URI.create(\"https://example.com/v1/metadata.json\"))",
                "V2_METADATA_JSON(URI.create(\"https://example.com/v2/metadata.json\"))"
        };

        testHandleURIEnum(JavaClientCodegen.RESTTEMPLATE, expectedInnerEnumLines, expectedEnumLines);
    }

    @Test
    public void testOkHttpGsonHandleURIEnum() {
        String[] expectedInnerEnumLines = new String[]{
                "V1_SCHEMA_JSON(URI.create(\"https://example.com/v1/schema.json\"))",
                "V2_SCHEMA_JSON(URI.create(\"https://example.com/v2/schema.json\"))",
                "jsonWriter.value(enumeration.getValue().toASCIIString())",
                "URI value =  URI.create(jsonReader.nextString())",
                "URI value = URI.create(jsonElement.getAsString())"
        };

        String[] expectedEnumLines = new String[]{
                "V1_METADATA_JSON(URI.create(\"https://example.com/v1/metadata.json\"))",
                "V2_METADATA_JSON(URI.create(\"https://example.com/v2/metadata.json\"))",
                "jsonWriter.value(enumeration.getValue().toASCIIString())",
                "URI value = URI.create(jsonReader.nextString())",
                "URI value = URI.create(jsonElement.getAsString())"
        };

        testHandleURIEnum(JavaClientCodegen.OKHTTP_GSON, expectedInnerEnumLines, expectedEnumLines);
    }

    @Test
    public void testMicroprofileHandleURIEnum() {
        String[] expectedInnerEnumLines = new String[]{
                "V1_SCHEMA_JSON(URI.create(\"https://example.com/v1/schema.json\"))",
                "V2_SCHEMA_JSON(URI.create(\"https://example.com/v2/schema.json\"))",
                "generator.write(obj.value.toASCIIString())"
        };

        String[] expectedEnumLines = new String[]{
                "V1_METADATA_JSON(URI.create(\"https://example.com/v1/metadata.json\"))",
                "V2_METADATA_JSON(URI.create(\"https://example.com/v2/metadata.json\"))"
        };

        testHandleURIEnum(JavaClientCodegen.MICROPROFILE, expectedInnerEnumLines, expectedEnumLines);
    }

    private void testHandleURIEnum(String library, String[] expectedInnerEnumLines, String[] expectedEnumLines) {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(library)
                .setInputSpec("src/test/resources/3_0/enum-and-inner-enum-uri.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .stream().collect(Collectors.toMap(File::getName, Function.identity()));

        // enum
        File modelFile = files.get("Metadata.java");
        Assertions.assertNotNull(modelFile);
        JavaFileAssert.assertThat(modelFile).fileContains(expectedEnumLines);

        // Inner enum
        File apiFile = files.get("V1SchemasGetDefaultResponse.java");
        Assertions.assertNotNull(apiFile);
        JavaFileAssert.assertThat(apiFile).fileContains(expectedInnerEnumLines);
    }

    @Test
    public void testQueryParamsExploded_whenQueryParamIsNull() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTTEMPLATE)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue_17555.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/api/DepartmentApi.java"), "if (filter != null) {");
    }

    @Test
    public void generateAllArgsConstructor() {
        Map<String, File> files = generateFromContract("src/test/resources/3_0/java/all_args_constructor.yaml", JavaClientCodegen.RESTTEMPLATE,
                Map.of(AbstractJavaCodegen.GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE),
                codegenConfigurator -> codegenConfigurator.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "false"));
        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains("protected String name", "protected String type")
                .assertConstructor("String")
                .hasParameter("type").toConstructor()
                .toFileAssert()
                .assertConstructor("LocalDate", "String", "String")
                .hasParameter("dateOfBirth").toConstructor()
                .hasParameter("name").toConstructor()
                .hasParameter("type").toConstructor();
        JavaFileAssert.assertThat(files.get("Cat.java"))
                .assertConstructor("Integer", "String", "LocalDate", "String", "String");

        // test readonly constructor
        JavaFileAssert.assertThat(files.get("Page.java"))
                .assertConstructor("Integer")
                .toFileAssert()
                .fileContains("Constructor with only readonly parameters and all parameters");

        JavaFileAssert.assertThat(files.get("PageOfPets.java"))
                .assertConstructor("Integer")
                .hasParameter("count").toConstructor()
                .toFileAssert()
                .assertConstructor("Integer", "List<Pet>")
                .hasParameter("count").toConstructor()
                .hasParameter("_list").toConstructor();
    }

    @Test
    public void generateAllArgsConstructor_REFACTOR_ALLOF_WITH_PROPERTIES_ONLY() {
        // try the generation with some additional OpenAPINormalizers

        Map<String, File> files = generateFromContract("src/test/resources/3_0/java/all_args_constructor.yaml", JavaClientCodegen.RESTTEMPLATE,
                Map.of(AbstractJavaCodegen.GENERATE_CONSTRUCTOR_WITH_ALL_ARGS, Boolean.TRUE),
                codegenConfigurator -> codegenConfigurator.addOpenapiNormalizer("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "true"));
        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains("protected String name", "protected String type")
                .assertConstructor("String")
                .hasParameter("type").toConstructor()
                .toFileAssert()
                .assertConstructor("LocalDate", "String", "String")
                .hasParameter("dateOfBirth").toConstructor()
                .hasParameter("name").toConstructor()
                .hasParameter("type").toConstructor();

        JavaFileAssert.assertThat(files.get("PageOfPets.java"))
                .assertConstructor("Integer", "List<Pet>")
                .hasParameter("count").toConstructor()
                .hasParameter("_list").toConstructor()
                .toFileAssert()
                .assertConstructor("Integer")
                .hasParameter("count").toConstructor();

        JavaFileAssert.assertThat(files.get("Cat.java"))
                .assertConstructor("Integer", "String", "LocalDate", "String", "String");
    }

    @Test
    public void testRestClientFormMultipart() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/MultipartApi.java"),
                // multiple files
                "multipartArray(@jakarta.annotation.Nullable List<File> files)",
                "formParams.addAll(\"files\","
                        + " files.stream().map(FileSystemResource::new).collect(Collectors.toList()));",

                // mixed
                "multipartMixed(@jakarta.annotation.Nonnull MultipartMixedStatus status, @jakarta.annotation.Nonnull File _file, @jakarta.annotation.Nullable MultipartMixedRequestMarker marker, @jakarta.annotation.Nullable List<MultipartMixedStatus> statusArray)",
                "formParams.add(\"file\", new FileSystemResource(_file));",

                // single file
                "multipartSingle(@jakarta.annotation.Nullable File _file)",
                "formParams.add(\"file\", new FileSystemResource(_file));"
        );
    }

    @Test
    public void testRestClientWithUseAbstractionForFiles() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/MultipartApi.java"),
                // multiple files
                "multipartArray(java.util.Collection<org.springframework.core.io.AbstractResource> files)",
                "formParams.addAll(\"files\", files.stream().collect(Collectors.toList()));",

                // mixed
                "multipartMixed(@jakarta.annotation.Nonnull MultipartMixedStatus status, org.springframework.core.io.AbstractResource _file, @jakarta.annotation.Nullable MultipartMixedRequestMarker marker, @jakarta.annotation.Nullable List<MultipartMixedStatus> statusArray)",
                "formParams.add(\"file\", _file);",

                // single file
                "multipartSingle(org.springframework.core.io.AbstractResource _file)",
                "formParams.add(\"file\", _file);"
        );
    }

    @Test
    public void testRestClientWithFreeFormInQueryParameters() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(CodegenConstants.API_PACKAGE, "xyz.abcdef.api"))
                .setInputSpec("src/test/resources/3_0/issue8352.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(output.resolve("src/main/java/xyz/abcdef/ApiClient.java"), "value instanceof Map");
    }

    @Test
    public void testRestClientJsonCreatorWithNullable_issue12790() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .addAdditionalProperty(AbstractJavaCodegen.OPENAPI_NULLABLE, "true")
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setInputSpec("src/test/resources/bugs/issue_12790.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestObject.java"))
                .printFileContent()
                .assertConstructor("String", "String")
                .bodyContainsLines(
                        "this.nullableProperty = nullableProperty == null ? JsonNullable.<String>undefined() :"
                                + " JsonNullable.of(nullableProperty);",
                        "this.notNullableProperty = notNullableProperty;"
                );
    }

    @Test
    public void testRestClientSupportListOfStringReturnType_issue7118() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setInputSpec("src/test/resources/bugs/issue_7118.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/UsersApi.java"),
                // set of string
                "ParameterizedTypeReference<Set<String>> localVarReturnType = new"
                        + " ParameterizedTypeReference<>() {};",
                "getUserIdSetRequestCreation().toEntity(localVarReturnType)",
                // list of string
                "ParameterizedTypeReference<List<String>> localVarReturnType = new"
                        + " ParameterizedTypeReference<>() {};",
                "getUserIdListRequestCreation().toEntity(localVarReturnType)"
        );
    }

    @Test
    public void testRestClientResponseTypeWithUseAbstractionForFiles_issue16589() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .addAdditionalProperty(CodegenConstants.API_PACKAGE, "xyz.abcdef.api")
                .addAdditionalProperty(JavaClientCodegen.USE_ABSTRACTION_FOR_FILES, true)
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setInputSpec("src/test/resources/3_0/issue13146_file_abstraction_response.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/ResourceApi.java"),
                "org.springframework.core.io.Resource resourceInResponse()",
                "ResponseEntity<org.springframework.core.io.Resource> resourceInResponseWithHttpInfo()",
                "ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<>()"
        );
    }

    @Test
    void testBuilderJavaClient() {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/builder.yaml",
                JavaClientCodegen.RESTTEMPLATE,
                Map.of(AbstractJavaCodegen.GENERATE_BUILDERS, Boolean.TRUE)
        );

        JavaFileAssert.assertThat(files.get("Pet.java"))
                .fileContains(
                        "protected String petReadonlyProperty", "toBuilder()", "builder()", "public static class Builder {"
                );
        JavaFileAssert.assertThat(files.get("Snake.java"))
                .fileContains(
                        "toBuilder()",
                        "builder()",
                        "public static class Builder extends Reptile.Builder {",
                        ".petType(getPetType())",
                        ".name(getName())",
                        "hasLegs(getHasLegs())"
                );
    }

    @DataProvider
    Iterator<String> serializationLibraries() {
        return new JavaClientCodegen().supportedLibraries().keySet().iterator();
    }

    @Test(dataProvider = "serializationLibraries")
    void setsDefaultSerializationLibrary(String library) {
        var codegen = new JavaClientCodegen();
        codegen.setLibrary(library);
        codegen.processOpts();

        assertThat(codegen.additionalProperties())
                .containsAnyOf(
                        entry(SERIALIZATION_LIBRARY_GSON, "true"),
                        entry(SERIALIZATION_LIBRARY_JACKSON, "true"),
                        entry(SERIALIZATION_LIBRARY_JSONB, "true")
                );
    }

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/18515">#18515</a>:
     * When GSON is selected as serializer, there should not be any jackson references
     * (except jackson-databind-nullable that is, which is only added when openApiNullable=true)
     */
    @Test(dataProvider = "librariesSupportingGson")
    void gsonCodeDoesNotContainJacksonReferences(Library library) {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .addAdditionalProperty(SERIALIZATION_LIBRARY, Serializer.GSON)
                .addAdditionalProperty(OPENAPI_NULLABLE, "false")
                .setGeneratorName("java")
                .setLibrary(library.getValue())
                .setInputSpec("src/test/resources/3_0/java/autoset_constant.yaml")
                .setOutputDir(newTempFolder().toString());
        var generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        assertThat(files).allSatisfy(
                file -> assertThat(file).content().doesNotContainIgnoringCase("jackson")
        );
    }

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/6496">#6496</a>
     */
    @Test
    void doesNotGenerateJacksonJsonFormatAnnotation_whenLibraryIsGson_andSerializeBigDecimalAsStringIsTrue() {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .addAdditionalProperty(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_GSON)
                .addAdditionalProperty(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, true)
                .addGlobalProperty(CodegenConstants.MODELS, "FormatTest")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, "false")
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, "false")
                .setInputSpec("src/test/resources/2_0/java/issue-6496.yaml")
                .setOutputDir(newTempFolder().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(files).hasSize(1).first(FILE).content()
                .doesNotContain(
                        "@JsonDeserialize(as = LinkedHashSet.class)",
                        "@JsonFormat(shape = JsonFormat.Shape.STRING)",
                        "com.fasterxml.jackson.databind.annotation.JsonDeserialize"
                );
    }

    /**
     * Test that fix for <a href="https://github.com/OpenAPITools/openapi-generator/issues/6496">#6496</a> has
     * no unwanted side effects on the existing feature (Jackson + bigDecimalAsString)
     */
    @Test
    void generatesJacksonJsonFormatAnnotation_whenLibraryIsJackson_andSerializeBigDecimalAsStringIsTrue() {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .addAdditionalProperty(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_JACKSON)
                .addAdditionalProperty(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, true)
                .addAdditionalProperty(OPENAPI_NULLABLE, false)
                .addGlobalProperty(CodegenConstants.MODELS, "FormatTest")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, "false")
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, "false")
                .setInputSpec("src/test/resources/2_0/java/issue-6496.yaml")
                .setOutputDir(newTempFolder().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        assertThat(files).hasSize(1).first(FILE).content()
                .contains(
                        "@JsonDeserialize(as = LinkedHashSet.class)",
                        "@JsonFormat(shape = JsonFormat.Shape.STRING)",
                        "com.fasterxml.jackson.databind.annotation.JsonDeserialize"
                );
    }

    /**
     * General XML annotations test (both JAXB and Jackson)
     * <br>
     * Includes regression tests for:
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/2417">Correct Jackson annotation when `wrapped: false`</a>
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/18869">Microprofile generator missing Jackson annotations and namespaces</a>
     */
    @Test(dataProvider = "librariesSupportingJackson")
    void shouldGenerateCorrectXmlAnnotations(Library library) {
        // Arrange
        final CodegenConfigurator config = new CodegenConfigurator()
                .addAdditionalProperty(CodegenConstants.WITH_XML, true)
                .addAdditionalProperty(SERIALIZATION_LIBRARY, "jackson")
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, null)
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, null)
                .setGeneratorName("java")
                .setInputSpec("src/test/resources/3_0/java/xml-annotations-test.yaml")
                .setLibrary(library.value)
                .setOutputDir(newTempFolder().toString());

        // Act
        final List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Assert
        JavaFileAssert.assertThat(files.get(0))
                .assertTypeAnnotations()
                .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
                .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("JacksonXmlRootElement", Map.of("localName", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .toType()

                // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                .assertProperty("tags").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Tag\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"TagList\""))
                .toProperty().toType()
                .assertMethod("getTags")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Tag\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"TagList\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("friends").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"friend-pet\""))
                .toProperty().toType()
                .assertMethod("getFriends")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"friend-pet\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)
                .assertProperty("status").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"PetStatus\""))
                .toProperty().toType()
                .assertMethod("getStatus")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"PetStatus\""))
                .toFileAssert()

                // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
                //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("photoUrls").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"photoUrls\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"photoUrls\""))
                .toProperty().toType()
                .assertMethod("getPhotoUrls")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"photoUrls\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"photoUrls\"", "useWrapping", "true"))
                .toFileAssert()

                // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
                .assertProperty("name").assertPropertyAnnotations()
                .doesNotContainWithName("XmlElement")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlAttribute", Map.of("name", "\"name\""))
                .toProperty().toType()
                .assertMethod("getName")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("isAttribute", "true", "localName", "\"name\""))
                .toFileAssert()

                // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
                .assertProperty("id").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toProperty().toType()
                .assertMethod("getId")
                .doesNotHaveAnnotation("JacksonXmlElementWrapper")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toFileAssert()

                // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("foods").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"yummy-yummy\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"yummy-yummy\""))
                .toProperty().toType()
                .assertMethod("getFoods")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"yummy-yummy\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"yummy-yummy\""))
                .toFileAssert()

                // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("colors").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"color\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"colors\""))
                .toProperty().toType()
                .assertMethod("getColors")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"color\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"colors\""))
                .toFileAssert()

                // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("categories").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Category\""))
                .toProperty().toType()
                .assertMethod("getCategories")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"Category\""))
                // ↓ specific regression test for #2417: (useWrapping=false) needs to be present
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toFileAssert()

                // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("activities").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"item\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"activities-array\""))
                .toProperty().toType()
                .assertMethod("getActivities")
                .hasAnnotation("JacksonXmlProperty", Map.of("localName", "\"item\""))
                .hasAnnotation("JacksonXmlElementWrapper", Map.of("localName", "\"activities-array\""));
    }

    /**
     * General XML annotations test (only JAXB)
     */
    @Test(dataProvider = "librariesNotSupportingJackson")
    void shouldGenerateCorrectJaxbAnnotations(Library library) {
        // Arrange
        final CodegenConfigurator config = new CodegenConfigurator()
                .addAdditionalProperty(CodegenConstants.WITH_XML, true)
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, null)
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, null)
                .setGeneratorName("java")
                .setLibrary(library.value)
                .setInputSpec("src/test/resources/3_0/java/xml-annotations-test.yaml")
                .setOutputDir(newTempFolder().toString());

        // Act
        final List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Assert
        JavaFileAssert.assertThat(files.get(0))
                .assertTypeAnnotations()
                .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
                .toType()

                // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                .assertProperty("tags").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Tag\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"TagList\""))
                .toProperty().toType()

                // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("friends").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"friend-pet\""))
                .toProperty().toType()

                // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)
                .assertProperty("status").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"PetStatus\""))
                .toProperty().toType()

                // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
                //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("photoUrls").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"photoUrls\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"photoUrls\""))
                .toProperty().toType()

                // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
                .assertProperty("name").assertPropertyAnnotations()
                .doesNotContainWithName("XmlElement")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlAttribute", Map.of("name", "\"name\""))
                .toProperty().toType()

                // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
                .assertProperty("id").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toProperty().toType()

                // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("foods").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"yummy-yummy\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"yummy-yummy\""))
                .toProperty().toType()

                // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("colors").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"color\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"colors\""))
                .toProperty().toType()

                // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("categories").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .doesNotContainWithName("XmlElementWrapper")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"Category\""))
                .toProperty().toType()

                // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("activities").assertPropertyAnnotations()
                .doesNotContainWithName("XmlAttribute")
                .containsWithNameAndAttributes("XmlElement", Map.of("name", "\"item\""))
                .containsWithNameAndAttributes("XmlElementWrapper", Map.of("name", "\"activities-array\""));
    }


    @Test
    public void testRestTemplateWithGeneratedOAuthTokenSuppliers() {

        final Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/oauth.yaml",
                JavaClientCodegen.RESTTEMPLATE
        );

        final JavaFileAssert apiClient = JavaFileAssert.assertThat(files.get("ApiClient.java"))
                .printFileContent();
        apiClient
                .assertMethod("setAccessToken", "String")
                .bodyContainsLines("setAccessToken(() -> accessToken);");
        apiClient
                .assertMethod("setAccessToken", "Supplier<String>")
                .bodyContainsLines("((OAuth) auth).setAccessToken(tokenSupplier);");

        final JavaFileAssert oAuth = JavaFileAssert.assertThat(files.get("OAuth.java"))
                .printFileContent();
        oAuth
                .assertMethod("setAccessToken", "String")
                .bodyContainsLines("setAccessToken(() -> accessToken);");
        oAuth
                .assertMethod("setAccessToken", "Supplier<String>")
                .bodyContainsLines("this.tokenSupplier = tokenSupplier;");
        oAuth
                .assertMethod("applyToParams")
                .bodyContainsLines("Optional.ofNullable(tokenSupplier).map(Supplier::get).ifPresent(accessToken ->")
                .bodyContainsLines("headerParams.add(HttpHeaders.AUTHORIZATION, \"Bearer \" + accessToken)");

    }

    @Test
    public void testRestClientWithGeneratedOAuthTokenSuppliers() {
        final Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/java/oauth.yaml",
                JavaClientCodegen.RESTCLIENT
        );

        final JavaFileAssert oAuth = JavaFileAssert.assertThat(files.get("OAuth.java"))
                .printFileContent();
        oAuth
                .assertMethod("setAccessToken", "String")
                .bodyContainsLines("setAccessToken(() -> accessToken);");
        oAuth
                .assertMethod("setAccessToken", "Supplier<String>")
                .bodyContainsLines("this.tokenSupplier = tokenSupplier;");
        oAuth
                .assertMethod("applyToParams")
                .bodyContainsLines("Optional.ofNullable(tokenSupplier).map(Supplier::get).ifPresent(accessToken ->")
                .bodyContainsLines("headerParams.add(HttpHeaders.AUTHORIZATION, \"Bearer \" + accessToken)");
    }

    @Test
    public void testRestClientWithXML_issue_19137() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.WITH_XML, true
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        validateJavaSourceFiles(files);
        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/ApiClient.java"),
                "import com.fasterxml.jackson.dataformat.xml.XmlMapper;",
                "import com.fasterxml.jackson.dataformat.xml.ser.ToXmlGenerator;"
        );
    }

    @Test
    public void testRestClientWithUseSingleRequestParameter_issue_19406() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "true"
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public record DeletePetRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey){}",
                "public void deletePet(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public ResponseEntity<Void> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public void deletePet(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {",
                "public ResponseEntity<Void> deletePetWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {"
        );
    }

    @Test
    public void testRestClientWithUseSingleRequestParameter_static_issue_20668() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "static"
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public static class DeletePetRequest {",
                "DeletePetRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey)",
                "Long petId()",
                "DeletePetRequest petId(@jakarta.annotation.Nonnull Long petId) {",
                "String apiKey()",
                "DeletePetRequest apiKey(@jakarta.annotation.Nullable String apiKey) {",
                "public void deletePet(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public ResponseEntity<Void> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws RestClientResponseException {",
                "public void deletePet(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {",
                "public ResponseEntity<Void> deletePetWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {"
        );
        TestUtils.assertFileNotContains(output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public record DeletePetRequest(Long petId, String apiKey){}");
    }

    @Test
    public void testDuplicatedOperationId() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setValidateSpec(false)
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.RESTCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "true"
                ))
                .setInputSpec("src/test/resources/3_0/duplicated_operationid.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                " void getObject("
        );

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/UserApi.java"),
                " void getObject("
        );

        TestUtils.assertFileNotContains(
                output.resolve("src/main/java/xyz/abcdef/api/UserApi.java"),
                " void getObject_0("
        );

        TestUtils.assertFileNotContains(
                output.resolve("src/main/java/xyz/abcdef/api/UserApi.java"),
                " void getObject0("
        );
    }

    @Test
    public void testWebClientWithUseSingleRequestParameter_issue_19407() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "true"
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public class DeletePetRequest {",
                "DeletePetRequest(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey)",
                "Long petId()",
                "String apiKey()",
                "public Mono<Void> deletePet(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<Void> deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {"
        );
    }

    @Test
    public void testWebClientWithUseSingleRequestParameter_static() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "static"
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public static class DeletePetRequest {",
                "DeletePetRequest(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey)",
                "Long petId()",
                "String apiKey()",
                "public Mono<Void> deletePet(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<Void> deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {"
        );
    }

    @Test
    public void testWebClientWithUseSingleRequestParameter_static_issue_20668() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.WEBCLIENT)
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api",
                        CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "static"
                ))
                .setInputSpec("src/test/resources/3_1/java/petstore.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/PetApi.java"),
                "public static class DeletePetRequest {",
                "DeletePetRequest(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey)",
                "Long petId()",
                "DeletePetRequest petId(@javax.annotation.Nonnull Long petId) {",
                "String apiKey()",
                "DeletePetRequest apiKey(@javax.annotation.Nullable String apiKey) {",
                "public Mono<Void> deletePet(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws WebClientResponseException {",
                "public Mono<Void> deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {",
                "public ResponseSpec deletePetWithResponseSpec(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey) throws WebClientResponseException {"
        );
    }

    @Test
    public void testGenerateParameterId() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setValidateSpec(false)
                .setGeneratorName("java")
                .setAdditionalProperties(Map.of(
                        CodegenConstants.API_PACKAGE, "xyz.abcdef.api"
                ))
                .setInputSpec("src/test/resources/3_1/issue_20239.yaml")
                .setOutputDir(output.toString().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(
                output.resolve("src/main/java/xyz/abcdef/api/ATagApi.java"),
                " getCall(@javax.annotation.Nonnull String pathParameter, @javax.annotation.Nullable Integer queryParameter, final ApiCallback _callback)"
        );

        TestUtils.assertFileNotContains(
                output.resolve("src/main/java/xyz/abcdef/api/ATagApi.java"),
                " getCall(Integer queryParameter, final ApiCallback _callback)"
        );
    }

    @Test
    public void callNativeServiceWithEmptyResponseSync() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");
        properties.put("asyncNative", "false");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/java/native/issue13968.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        Map<String, File> files = generator.opts(clientOptInput).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File apiFile = files.get("DefaultApi.java");
        assertNotNull(apiFile);

        JavaFileAssert.assertThat(apiFile).fileContains(
                //reading the body into a string, then checking if it is blank.
                "String responseBody = new String(localVarResponse.body().readAllBytes());",
                "responseBody.isBlank()? null: memberVarObjectMapper.readValue(responseBody, new TypeReference<LocationData>() {})"
        );
    }


    /**
     * This checks that the async client is not affected by this fix.
     * See https://github.com/OpenAPITools/openapi-generator/issues/13968
     */
    @Test
    public void callNativeServiceWithEmptyResponseAsync() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");
        properties.put("asyncNative", "true");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.NATIVE)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/java/native/issue13968.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        Map<String, File> files = generator.opts(clientOptInput).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File apiFile = files.get("DefaultApi.java");
        assertNotNull(apiFile);

        JavaFileAssert.assertThat(apiFile).fileDoesNotContain(
                //reading the body into a string, then checking if it is blank.
                "String responseBody = new String(localVarResponse.body().readAllBytes());",
                "responseBody.isBlank()? null: memberVarObjectMapper.readValue(responseBody, new TypeReference<LocationData>() {})"
        );
    }

    @Test
    public void testEnumWithImplements() {
        final Path output = newTempFolder();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/enum-implements.yaml");
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOutputDir(output.toString());

        Map<String, File> files = new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen))
                .generate().stream().collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Type.java")).fileContains("Type implements java.io.Serializable {");
    }

    /**
     * This checks bug issue-20718
     * A situation when schemaMapping is used and oneOf also is used with one of the schema-mapped dataTypes and the dataType
     * contains a package definition
     * The dataType needs to be sanitized to generate compileable code
     */
    @Test
    public void testClassesAreValidJavaJersey2() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JERSEY2)
                .setSchemaMappings(Map.of(
                        "A", "some.pkg.A",
                        "B", "some.pkg.B"))
                .setInputSpec("src/test/resources/bugs/issue_20718-dataType_with_schema_mapping.yml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File oneOfFile = files.get("ResultObjectOneOf.java");
        assertNotNull(oneOfFile);

        JavaFileAssert.assertThat(oneOfFile).fileContains(
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );
        File anyOfFile = files.get("ResultObjectAnyOf.java");
        assertNotNull(anyOfFile);

        JavaFileAssert.assertThat(anyOfFile).fileContains(
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );
    }

    /**
     * This checks bug issue-20718
     * A situation when schemaMapping is used and oneOf also is used with one of the schema-mapped dataTypes and the dataType
     * contains a package definition
     * The dataType needs to be sanitized to generate compileable code
     */
    @Test
    public void testClassesAreValidJavaJersey3() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JERSEY3)
                .setSchemaMappings(Map.of(
                        "A", "some.pkg.A",
                        "B", "some.pkg.B"))
                .setInputSpec("src/test/resources/bugs/issue_20718-dataType_with_schema_mapping.yml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File oneOfFile = files.get("ResultObjectOneOf.java");
        assertNotNull(oneOfFile);

        JavaFileAssert.assertThat(oneOfFile).fileContains(
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );
        File anyOfFile = files.get("ResultObjectAnyOf.java");
        assertNotNull(anyOfFile);

        JavaFileAssert.assertThat(anyOfFile).fileContains(
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );
    }

    /**
     * This checks bug issue-20718
     * A situation when schemaMapping is used and oneOf also is used with one of the schema-mapped dataTypes and the dataType
     * contains a package definition
     * The dataType needs to be sanitized to generate compileable code
     */
    @Test
    public void testClassesAreValidJavaOkHttpGson() {
        final Path output = newTempFolder();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(OKHTTP_GSON)
                .setSchemaMappings(Map.of(
                        "A", "some.pkg.A",
                        "B", "some.pkg.B"))
                .setInputSpec("src/test/resources/bugs/issue_20718-dataType_with_schema_mapping.yml")
                .setOutputDir(output.toString().replace("\\", "/"));

        Map<String, File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File oneOfFile = files.get("ResultObjectOneOf.java");
        assertNotNull(oneOfFile);

        JavaFileAssert.assertThat(oneOfFile).fileContains(
                "final TypeAdapter<some.pkg.A> adaptersomepkgA = gson.getDelegateAdapter(this, TypeToken.get(some.pkg.A.class));",
                        "final TypeAdapter<some.pkg.B> adaptersomepkgB = gson.getDelegateAdapter(this, TypeToken.get(some.pkg.B.class));",
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );

        File anyOfFile = files.get("ResultObjectAnyOf.java");
        assertNotNull(anyOfFile);

        JavaFileAssert.assertThat(anyOfFile).fileContains(
                "final TypeAdapter<some.pkg.A> adaptersomepkgA = gson.getDelegateAdapter(this, TypeToken.get(some.pkg.A.class));",
                "final TypeAdapter<some.pkg.B> adaptersomepkgB = gson.getDelegateAdapter(this, TypeToken.get(some.pkg.B.class));",
                "public some.pkg.A getsomepkgA() throws ClassCastException {",
                "public some.pkg.B getsomepkgB() throws ClassCastException {"
        );
    }
}