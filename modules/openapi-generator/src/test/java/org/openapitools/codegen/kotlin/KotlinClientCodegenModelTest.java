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
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

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
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "kotlin.String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, null);
        Assert.assertEquals(property2.baseType, "kotlin.String");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
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
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "org.threeten.bp.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "org.threeten.bp.OffsetDateTime");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: threetenbp-localdatetime")
    public void selectDateLibraryAsThreetenbpLocalDateTime() {
        final Schema schema = getSimpleSchema();
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
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
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
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "kotlin.String");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "kotlin.String");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
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
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "java.time.OffsetDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, null);
        Assert.assertEquals(property3.baseType, "java.time.OffsetDateTime");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a simple model: date java8-localdatetime")
    public void selectDateLibraryAsJava8LocalDateTime() {
        final Schema schema = getSimpleSchema();
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
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a model with array property to default kotlin.Array")
    public void arrayPropertyTest() {
        final Schema model = getArrayTestSchema();

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
        final Schema model = getArrayTestSchema();

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
        final Schema schema = getMapSchema();
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
        final Schema schema = getComplexSchema();
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

        Assert.assertEquals(cm.name, testCase.expectedName);
        Assert.assertEquals(cm.classname, testCase.expectedClassName);
    }

    private static class ModelNameTest {
        private String expectedName;
        private String expectedClassName;

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

