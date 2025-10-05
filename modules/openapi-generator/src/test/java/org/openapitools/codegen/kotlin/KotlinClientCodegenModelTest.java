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

package org.openapitools.codegen.kotlin;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.openapitools.codegen.*;
import org.openapitools.codegen.antlr4.KotlinLexer;
import org.openapitools.codegen.antlr4.KotlinParser;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.CodegenConstants.*;

@SuppressWarnings("static-method")
public class KotlinClientCodegenModelTest {

    private Schema<?> getArrayTestSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format("int64"))
                .addProperties("examples", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
    }

    private Schema<?> getSimpleSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format("int64"))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
    }

    private Schema<?> getMapSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("mapping", new MapSchema()
                        .additionalProperties(new StringSchema()));
    }

    private Schema<?> getComplexSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("child", new ObjectSchema().$ref("#/components/schemas/Child"));
    }

    @Test(description = "convert a simple model")
    public void simpleModelTest() {
        final Schema<?> schema = getSimpleSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "kotlin.Long");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, null);
        Assert.assertEquals(property1.baseType, "kotlin.Long");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "kotlin.String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, null);
        Assert.assertEquals(property2.baseType, "kotlin.String");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: threetenbp")
    public void selectDateLibraryAsThreetenbp() {
        final Schema<?> schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.THREETENBP.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "org.threeten.bp.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "org.threeten.bp.OffsetDateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: threetenbp-localdatetime")
    public void selectDateLibraryAsThreetenbpLocalDateTime() {
        final Schema<?> schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        String value = KotlinClientCodegen.DateLibrary.THREETENBP_LOCALDATETIME.value;
        Assert.assertEquals(value, "threetenbp-localdatetime");
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.THREETENBP_LOCALDATETIME.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "org.threeten.bp.LocalDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "org.threeten.bp.LocalDateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date string")
    public void selectDateLibraryAsString() {
        final Schema<?> schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.STRING.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "kotlin.String");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "kotlin.String");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date java8")
    public void selectDateLibraryAsJava8() {
        final Schema<?> schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.JAVA8.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date java8-localdatetime")
    public void selectDateLibraryAsJava8LocalDateTime() {
        final Schema<?> schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        String value = KotlinClientCodegen.DateLibrary.JAVA8_LOCALDATETIME.value;
        Assert.assertEquals(value, "java8-localdatetime");
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.JAVA8_LOCALDATETIME.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "java.time.LocalDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "java.time.LocalDateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a model with array property to default kotlin.Array")
    public void arrayPropertyTest() {
        final Schema<?> model = getArrayTestSchema();

        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.classname, "Sample");
        Assert.assertEquals(generated.description, "a sample model");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.getter, "getExamples");
        Assert.assertEquals(property.setter, "setExamples");
        Assert.assertEquals(property.dataType, "kotlin.Array<kotlin.String>");
        Assert.assertEquals(property.name, "examples");
        Assert.assertEquals(property.defaultValue, null);
        Assert.assertEquals(property.baseType, "kotlin.Array");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to a kotlin.collections.List")
    public void listPropertyTest() {
        final Schema<?> model = getArrayTestSchema();

        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setCollectionType(KotlinClientCodegen.CollectionType.LIST.value);
        codegen.processOpts();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.classname, "Sample");
        Assert.assertEquals(generated.description, "a sample model");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.getter, "getExamples");
        Assert.assertEquals(property.setter, "setExamples");
        Assert.assertEquals(property.dataType, "kotlin.collections.List<kotlin.String>");
        Assert.assertEquals(property.name, "examples");
        Assert.assertEquals(property.defaultValue, null);
        Assert.assertEquals(property.baseType, "kotlin.collections.List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema<?> schema = getMapSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "mapping");
        Assert.assertEquals(property1.dataType, "kotlin.collections.Map<kotlin.String, kotlin.String>");
        Assert.assertEquals(property1.name, "mapping");
        Assert.assertEquals(property1.baseType, "kotlin.collections.Map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema<?> schema = getComplexSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "child");
        Assert.assertEquals(property1.dataType, "Child");
        Assert.assertEquals(property1.name, "child");
        Assert.assertEquals(property1.baseType, "Child");
        Assert.assertFalse(property1.required);
        Assert.assertFalse(property1.isContainer);
    }

    @DataProvider(name = "modelNames")
    public static Object[][] modelNames() {
        return new Object[][]{
                {"TestNs.TestClass", new ModelNameTest("TestNs.TestClass", "TestNsTestClass")},
                {"$", new ModelNameTest("$", "Dollar")},
                {"for", new ModelNameTest("`for`", "For")},
                {"One<Two", new ModelNameTest("One<Two", "OneLessThanTwo")},
                {"One-Two", new ModelNameTest("One-Two", "OneTwo")},
                {"this is a test", new ModelNameTest("this is a test", "ThisIsATest")}
        };
    }

    @Test(dataProvider = "modelNames", description = "sanitize model names")
    public void sanitizeModelNames(final String name, final ModelNameTest testCase) {
        final Schema<?> schema = getComplexSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, schema);

        Assert.assertEquals(cm.name, testCase.expectedName);
        Assert.assertEquals(cm.classname, testCase.expectedClassName);
    }

    @Test
    public void testNativeClientExplodedQueryParamObject() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary("jvm-retrofit2")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/issue4808.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 31);
        TestUtils.assertFileContains(Paths.get(output + "/src/main/kotlin/xyz/abcdef/api/DefaultApi.kt"),
                "fun getSomeValue(@Query(\"since\") since: kotlin.String? = null, @Query(\"sinceBuild\") sinceBuild: kotlin.String? = null, @Query(\"maxBuilds\") maxBuilds: kotlin.Int? = null, @Query(\"maxWaitSecs\") maxWaitSecs: kotlin.Int? = null)"
        );
    }

    @Test
    public void testOmitGradleWrapperDoesNotGenerateWrapper() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        String path = output.getAbsolutePath();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .addAdditionalProperty("omitGradleWrapper", true)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        DefaultGenerator generator = new DefaultGenerator();

        generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileNotExists(Paths.get(path, "gradlew"));
        TestUtils.assertFileNotExists(Paths.get(path, "gradlew.bat"));
        TestUtils.assertFileNotExists(Paths.get(path, "gradle", "wrapper", "gradle-wrapper.properties"));
        TestUtils.assertFileNotExists(Paths.get(path, "gradle", "wrapper", "gradle-wrapper.jar"));
    }

    @Test
    public void testFailOnUnknownPropertiesAdditionalProperty() {
        final KotlinClientCodegen codegen = new KotlinClientCodegen();

        // Default case, nothing provided
        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        // Default to false
        configAssert.assertValue(KotlinClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, codegen::isFailOnUnknownProperties, Boolean.FALSE);

        // Provide true
        codegen.additionalProperties().put(KotlinClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, true);
        codegen.processOpts();

        // Should be true
        configAssert.assertValue(KotlinClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, codegen::isFailOnUnknownProperties, Boolean.TRUE);

        // Provide false
        codegen.additionalProperties().put(KotlinClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, false);
        codegen.processOpts();

        // Should be false
        configAssert.assertValue(KotlinClientCodegen.FAIL_ON_UNKNOWN_PROPERTIES, codegen::isFailOnUnknownProperties, Boolean.FALSE);
    }

    @DataProvider(name = "gsonClientLibraries")
    public Object[][] pathResponses() {
        return new Object[][]{
                {ClientLibrary.JVM_KTOR},
                {ClientLibrary.JVM_OKHTTP4},
                {ClientLibrary.JVM_RETROFIT2},
                {ClientLibrary.MULTIPLATFORM},
                {ClientLibrary.JVM_VOLLEY},
                {ClientLibrary.JVM_VERTX}
        };
    }

    @Test(dataProvider = "gsonClientLibraries")
    public void testLocalVariablesUseSanitizedDataTypeNamesForOneOfProperty_19942(ClientLibrary clientLibrary) throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        String path = output.getAbsolutePath();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary(clientLibrary.getLibraryName())
                .setInputSpec("src/test/resources/3_0/issue_19942.json")
                .addAdditionalProperty("omitGradleWrapper", true)
                .addAdditionalProperty("serializationLibrary", "gson")
                .addAdditionalProperty("dateLibrary", "kotlinx-datetime")
                .addAdditionalProperty("useSpringBoot3", "true")
                .addAdditionalProperty("generateOneOfAnyOfWrappers", true)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        DefaultGenerator generator = new DefaultGenerator();

        generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileNotContains(Paths.get(path + "/src/" + clientLibrary.getSourceRoot() + "/org/openapitools/client/models/ObjectWithComplexOneOfId.kt"),
                "val adapterkotlin.String", "val adapterjava.math.BigDecimal");
    }

    @Test(dataProvider = "gsonClientLibraries")
    public void testLocalVariablesUseSanitizedDataTypeNamesForAnyOfProperty_19942(ClientLibrary clientLibrary) throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        String path = output.getAbsolutePath();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary(clientLibrary.getLibraryName())
                .setInputSpec("src/test/resources/3_0/issue_19942.json")
                .addAdditionalProperty("omitGradleWrapper", true)
                .addAdditionalProperty("serializationLibrary", "gson")
                .addAdditionalProperty("dateLibrary", "kotlinx-datetime")
                .addAdditionalProperty("useSpringBoot3", "true")
                .addAdditionalProperty("generateOneOfAnyOfWrappers", true)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        DefaultGenerator generator = new DefaultGenerator();

        generator.opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileNotContains(Paths.get(path + "/src/" + clientLibrary.getSourceRoot() + "/org/openapitools/client/models/ObjectWithComplexAnyOfId.kt"),
                "val adapterkotlin.String", "val adapterjava.math.BigDecimal");
    }

    @Test(description = "Issue #20960")
    private void givenSchemaObjectPropertyNameContainsDollarSignWhenGenerateThenDollarSignIsProperlyEscapedInAnnotation() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        Map<String, Object> properties = new HashMap<>();
//        properties.put(CodegenConstants.LIBRARY, ClientLibrary.JVM_KTOR);
        properties.put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.toString());
        properties.put(SERIALIZATION_LIBRARY, KotlinClientCodegen.SERIALIZATION_LIBRARY_TYPE.gson.toString());
        properties.put(KotlinClientCodegen.GENERATE_ONEOF_ANYOF_WRAPPERS, true);
        properties.put(API_PACKAGE, "com.toasttab.service.scim.api");
        properties.put(MODEL_PACKAGE, "com.toasttab.service.scim.models");
        properties.put(PACKAGE_NAME, "com.toasttab.service.scim");
        codegen.additionalProperties().putAll(properties);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/issue_20960.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/com/toasttab/service/scim";
        Path baseGroupModel = Paths.get(outputPath + "/models/BaseGroupMembersInner.kt");
        String baseGroupModelContent = Files.readString(baseGroupModel);
        KotlinLexer kotlinLexer = new KotlinLexer(CharStreams.fromString(baseGroupModelContent));
        KotlinTestUtils.SyntaxErrorListener syntaxErrorListener = new KotlinTestUtils.SyntaxErrorListener();
        kotlinLexer.addErrorListener(syntaxErrorListener);
        CommonTokenStream commonTokenStream = new CommonTokenStream(kotlinLexer);
        KotlinParser kotlinParser = new KotlinParser(commonTokenStream);
        kotlinParser.addErrorListener(syntaxErrorListener);
        ParseTree parseTree = kotlinParser.kotlinFile();
        ParseTreeWalker parseTreeWalker = new ParseTreeWalker();
        KotlinTestUtils.CustomKotlinParseListener customKotlinParseListener = new KotlinTestUtils.CustomKotlinParseListener();
        parseTreeWalker.walk(customKotlinParseListener, parseTree);
        Assert.assertEquals(syntaxErrorListener.getSyntaxErrorCount(), 0);
        Assert.assertEquals(customKotlinParseListener.getStringReferenceCount(), 0);
    }

    @Test(description = "generate polymorphic kotlinx_serialization model")
    public void polymorphicKotlinxSerialization() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary("jvm-retrofit2")
                .setAdditionalProperties(new HashMap<>() {{
                    put(CodegenConstants.SERIALIZATION_LIBRARY, "kotlinx_serialization");
                    put(CodegenConstants.MODEL_PACKAGE, "xyz.abcdef.model");
                }})
                .setInputSpec("src/test/resources/3_0/kotlin/polymorphism.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 36);

        final Path animalKt = Paths.get(output + "/src/main/kotlin/xyz/abcdef/model/Animal.kt");
        // base doesn't contain discriminator
        TestUtils.assertFileNotContains(animalKt, "val discriminator");
        // base is sealed class
        TestUtils.assertFileContains(animalKt, "sealed class Animal");
        // base properties are abstract
        TestUtils.assertFileContains(animalKt, "abstract val id");
        TestUtils.assertFileContains(animalKt, "abstract val optionalProperty");
        // base has extra imports
        TestUtils.assertFileContains(animalKt, "import kotlinx.serialization.ExperimentalSerializationApi");
        TestUtils.assertFileContains(animalKt, "import kotlinx.serialization.json.JsonClassDiscriminator");

        final Path birdKt = Paths.get(output + "/src/main/kotlin/xyz/abcdef/model/Bird.kt");
        // derived doesn't contain disciminator
        TestUtils.assertFileNotContains(birdKt, "val discriminator");
        // derived has serial name set to mapping key
        TestUtils.assertFileContains(birdKt, "@SerialName(value = \"BIRD\")");
    }

    @Test(description = "generate polymorphic jackson model")
    public void polymorphicJacksonSerialization() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
//        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary("jvm-okhttp4")
                .setAdditionalProperties(new HashMap<>() {{
                    put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");
                    put(CodegenConstants.MODEL_PACKAGE, "xyz.abcdef.model");
                }})
                .setInputSpec("src/test/resources/3_0/kotlin/polymorphism.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 28);

        final Path animalKt = Paths.get(output + "/src/main/kotlin/xyz/abcdef/model/Animal.kt");
        // base has extra jackson imports
        TestUtils.assertFileContains(animalKt, "import com.fasterxml.jackson.annotation.JsonIgnoreProperties");
        TestUtils.assertFileContains(animalKt, "import com.fasterxml.jackson.annotation.JsonSubTypes");
        TestUtils.assertFileContains(animalKt, "import com.fasterxml.jackson.annotation.JsonTypeInfo");
        // and these are being used
        TestUtils.assertFileContains(animalKt, "@JsonIgnoreProperties");
        TestUtils.assertFileContains(animalKt, "@JsonSubTypes");
        TestUtils.assertFileContains(animalKt, "@JsonTypeInfo");
        // base is interface
        TestUtils.assertFileContains(animalKt, "interface Animal");
        // base properties are present
        TestUtils.assertFileContains(animalKt, "val id");
        TestUtils.assertFileContains(animalKt, "val optionalProperty");
        // base doesn't contain discriminator
        TestUtils.assertFileNotContains(animalKt, "val discriminator");

        final Path birdKt = Paths.get(output + "/src/main/kotlin/xyz/abcdef/model/Bird.kt");
        // derived has serial name set to mapping key
        TestUtils.assertFileContains(birdKt, "data class Bird");
        // derived properties are overridden
        TestUtils.assertFileContains(birdKt, "override val id");
        TestUtils.assertFileContains(birdKt, "override val optionalProperty");
        // derived doesn't contain disciminator
        TestUtils.assertFileNotContains(birdKt, "val discriminator");
    }

    private static class ModelNameTest {
        private final String expectedName;
        private final String expectedClassName;

        private ModelNameTest(String nameAndClass) {
            this.expectedName = nameAndClass;
            this.expectedClassName = nameAndClass;
        }

        private ModelNameTest(String expectedName, String expectedClassName) {
            this.expectedName = expectedName;
            this.expectedClassName = expectedClassName;
        }
    }
}
