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
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("static-method")
public class KotlinClientCodegenModelTest {

    private Schema getArrayTestSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format("int64"))
                .addProperties("examples", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
    }

    private Schema getSimpleSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format("int64"))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
    }

    private Schema getMapSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("mapping", new MapSchema()
                        .additionalProperties(new StringSchema()));
    }

    private Schema getComplexSchema() {
        return new ObjectSchema()
                .description("a sample model")
                .addProperties("child", new ObjectSchema().$ref("#/components/schemas/Child"));
    }

    @Test(description = "convert a simple model")
    public void simpleModelTest() {
        final Schema schema = getSimpleSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "kotlin.Long");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "kotlin.Long");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "kotlin.String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertEquals(property2.defaultValue, null);
        Assertions.assertEquals(property2.baseType, "kotlin.String");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: threetenbp")
    public void selectDateLibraryAsThreetenbp() {
        final Schema schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.THREETENBP.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "org.threeten.bp.OffsetDateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "org.threeten.bp.OffsetDateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: threetenbp-localdatetime")
    public void selectDateLibraryAsThreetenbpLocalDateTime() {
        final Schema schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        String value = KotlinClientCodegen.DateLibrary.THREETENBP_LOCALDATETIME.value;
        Assertions.assertEquals(value, "threetenbp-localdatetime");
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.THREETENBP_LOCALDATETIME.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "org.threeten.bp.LocalDateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "org.threeten.bp.LocalDateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date string")
    public void selectDateLibraryAsString() {
        final Schema schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.STRING.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "kotlin.String");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "kotlin.String");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date java8")
    public void selectDateLibraryAsJava8() {
        final Schema schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.JAVA8.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date java8-localdatetime")
    public void selectDateLibraryAsJava8LocalDateTime() {
        final Schema schema = getSimpleSchema();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        String value = KotlinClientCodegen.DateLibrary.JAVA8_LOCALDATETIME.value;
        Assertions.assertEquals(value, "java8-localdatetime");
        codegen.setDateLibrary(KotlinClientCodegen.DateLibrary.JAVA8_LOCALDATETIME.value);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "java.time.LocalDateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "java.time.LocalDateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a model with array property to default kotlin.Array")
    public void arrayPropertyTest() {
        final Schema model = getArrayTestSchema();

        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assertions.assertEquals(generated.name, "sample");
        Assertions.assertEquals(generated.classname, "Sample");
        Assertions.assertEquals(generated.description, "a sample model");
        Assertions.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assertions.assertEquals(property.baseName, "examples");
        Assertions.assertEquals(property.getter, "getExamples");
        Assertions.assertEquals(property.setter, "setExamples");
        Assertions.assertEquals(property.dataType, "kotlin.Array<kotlin.String>");
        Assertions.assertEquals(property.name, "examples");
        Assertions.assertEquals(property.defaultValue, null);
        Assertions.assertEquals(property.baseType, "kotlin.Array");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to a kotlin.collections.List")
    public void listPropertyTest() {
        final Schema model = getArrayTestSchema();

        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setCollectionType(KotlinClientCodegen.CollectionType.LIST.value);
        codegen.processOpts();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assertions.assertEquals(generated.name, "sample");
        Assertions.assertEquals(generated.classname, "Sample");
        Assertions.assertEquals(generated.description, "a sample model");
        Assertions.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assertions.assertEquals(property.baseName, "examples");
        Assertions.assertEquals(property.getter, "getExamples");
        Assertions.assertEquals(property.setter, "setExamples");
        Assertions.assertEquals(property.dataType, "kotlin.collections.List<kotlin.String>");
        Assertions.assertEquals(property.name, "examples");
        Assertions.assertEquals(property.defaultValue, null);
        Assertions.assertEquals(property.baseType, "kotlin.collections.List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = getMapSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "mapping");
        Assertions.assertEquals(property1.dataType, "kotlin.collections.Map<kotlin.String, kotlin.String>");
        Assertions.assertEquals(property1.name, "mapping");
        Assertions.assertEquals(property1.baseType, "kotlin.collections.Map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
        Assertions.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = getComplexSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "child");
        Assertions.assertEquals(property1.dataType, "Child");
        Assertions.assertEquals(property1.name, "child");
        Assertions.assertEquals(property1.baseType, "Child");
        Assertions.assertFalse(property1.required);
        Assertions.assertFalse(property1.isContainer);
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
        final Schema schema = getComplexSchema();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, schema);

        Assertions.assertEquals(cm.name, testCase.expectedName);
        Assertions.assertEquals(cm.classname, testCase.expectedClassName);
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

        Assertions.assertEquals(files.size(), 31);
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

