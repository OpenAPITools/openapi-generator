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

package org.openapitools.codegen.dart;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.DartClientCodegen;
import org.openapitools.codegen.languages.DartDioClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.*;

@SuppressWarnings("static-method")
public class DartModelTest {

    @Test(description = "convert a simple php model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema())
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("defaultItem", new IntegerSchema()._default(1))
                .addProperties("number", new NumberSchema())
                .addProperties("decimal", new StringSchema().format("number"))
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 6);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "DateTime");
        Assert.assertEquals(property3.dataType, "DateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "DateTime");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "defaultItem");
        Assert.assertEquals(property4.dataType, "int");
        Assert.assertEquals(property4.defaultValue, "1");
        Assert.assertEquals(property4.baseType, "int");
        Assert.assertFalse(property4.required);
        Assert.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "number");
        Assert.assertEquals(property5.dataType, "num");
        Assert.assertEquals(property5.baseType, "num");

        final CodegenProperty property6 = cm.vars.get(5);
        Assert.assertEquals(property6.baseName, "decimal");
        Assert.assertEquals(property6.dataType, "double");
        Assert.assertEquals(property6.baseType, "double");
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema())
                .addProperties("urls", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");

        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "List<String>");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "List");
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isContainer);
    }

    @Test(description = "convert a model with set property")
    public void setPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema())
                .addProperties("urls", new ArraySchema().items(new StringSchema()).uniqueItems(true))
                .addRequiredItem("id");

        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "Set<String>");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "Set");
        Assert.assertEquals(property2.containerType, "set");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.dataType, "Map<String, String>");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "Map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        Assert.assertFalse(property1.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "List<Children>");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "List");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapSchema() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        // {{imports}} is not used in template
        //Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "Map<String, Children>");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(model.getDescription(), "an array model");

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertTrue(cm.isArray);
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        // skip import test as import is not used by PHP codegen
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
    }

    @DataProvider(name = "modelNames")
    public static Object[][] modelNames() {
        return new Object[][] {
            {"sample", "Sample"},
            {"sample_name", "SampleName"},
            {"sample__name", "SampleName"},
            {"/sample", "Sample"},
            {"\\sample", "\\Sample"},
            {"sample.name", "SampleName"},
            {"_sample", "Sample"},
            {"sample name", "SampleName"},
            {"List", "ModelList"},
            {"list", "ModelList"},
            {"File", "TestModelFile"},
            {"Client", "TestModelClient"},
            {"String", "ModelString"},
        };
    }

    @Test(dataProvider = "modelNames", description = "correctly generate model names")
    public void modelNameTest(String name, String expectedName) {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema model = new Schema();
        final DefaultCodegen codegen = new DartClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.typeMapping().put("File", "TestModelFile");
        codegen.typeMapping().put("Client", "TestModelClient");
        final CodegenModel cm = codegen.fromModel(name, model);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, codegen.toModelName(expectedName));
    }

    @DataProvider(name = "varNames")
    public static Object[][] varNames() {
        return new Object[][] {
                {"Double", "double_"},
                {"double", "double_"},
                {"dynamic", "dynamic_"},
                {"String", "string"},
                {"string", "string"},
                {"hello", "hello"},
                {"FOO", "FOO"},
                {"FOO_BAR", "FOO_BAR"},
                {"FOO_BAR_BAZ_", "FOO_BAR_BAZ_"},
                {"123hello", "n123hello"},
                {"_hello", "hello"},
                {"_double", "double_"},
                {"_123hello", "n123hello"},
                {"_5FOO", "n5fOO"},
                 {"_FOO", "FOO"},
                {"_$foo", "dollarFoo"},
                {"_$_foo_", "dollarFoo"},
                {"$special[property.name]", "dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket"},
                {"foo bar", "fooBar"},
        };
    }

    @Test(dataProvider = "varNames", description = "test variable names are correctly escaped")
    public void convertVarName(String name, String expectedName) {
        final DefaultCodegen codegen = new DartClientCodegen();
        Assert.assertEquals(codegen.toVarName(name), expectedName);
    }

    private static class EnumVarName {
        final String name;
        final String expected;
        final String dataType;

        EnumVarName(String name, String expected, String dataType) {
            this.name = name;
            this.expected = expected;
            this.dataType = dataType;
        }
    }

    @DataProvider(name = "enumVarNames")
    public static Object[] enumVarNames() {
        return new Object[] {
                new EnumVarName("", "empty", "String"),
                new EnumVarName("Double", "double_", "String"),
                new EnumVarName("double", "double_", "String"),
                new EnumVarName("dynamic", "dynamic_", "String"),
                new EnumVarName("String", "string", "String"),
                new EnumVarName("string", "string", "String"),
                new EnumVarName("hello", "hello", "String"),
                new EnumVarName("FOO", "FOO", "String"),
                new EnumVarName("FOO_BAR", "FOO_BAR", "String"),
                new EnumVarName("FOO_BAR_BAZ_", "FOO_BAR_BAZ_", "String"),
                new EnumVarName("123hello", "n123hello", "String"),
                new EnumVarName("_hello", "hello", "String"),
                new EnumVarName("_double", "double_", "String"),
                new EnumVarName("_123hello", "n123hello", "String"),
                new EnumVarName("_5FOO", "n5fOO", "String"),
                new EnumVarName("_FOO", "FOO", "String"),
                new EnumVarName("_$foo", "dollarFoo", "String"),
                new EnumVarName("_$_foo_", "dollarFoo", "String"),
                new EnumVarName("$special[property.name]", "dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket", "String"),
                new EnumVarName("$", "dollar", "String"),
                new EnumVarName(">=", "greaterThanEqual", "String"),
                new EnumVarName("foo bar", "fooBar", "String"),
                new EnumVarName("1", "number1", "int"),
                new EnumVarName("2", "number2", "int"),
                new EnumVarName("-1", "numberNegative1", "int"),
                new EnumVarName("-99", "numberNegative99", "int"),
                new EnumVarName("1", "number1", "double"),
                new EnumVarName("1.1", "number1Period1", "double"),
                new EnumVarName("-1.2", "numberNegative1Period2", "double"),
        };
    }

    @Test(dataProvider = "enumVarNames", description = "test enum names are correctly escaped")
    public void convertEnumVarNames(EnumVarName enumVar) {
        final DefaultCodegen codegen = new DartClientCodegen();
        Assert.assertEquals(codegen.toEnumVarName(enumVar.name, enumVar.dataType), enumVar.expected);
    }

    @Test(description = "model names support `--model-name-prefix` and `--model-name-suffix`")
    public void modelPrefixSuffixTest() {
        final DefaultCodegen codegen = new DartClientCodegen();
        codegen.setModelNamePrefix("model");
        codegen.setModelNameSuffix("type");

        Assert.assertEquals(codegen.toModelName("hello_test"), "ModelHelloTestType");
    }

    @Test(description = "support normal enum values")
    public void testEnumValues() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("testStringEnum", new StringSchema()._enum(Arrays.asList("foo", "bar")))
                .addProperties("testIntEnum", new IntegerSchema().addEnumItem(1).addEnumItem(2));
        final DefaultCodegen codegen = new DartClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.postProcessModels(Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", cm))));

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "testStringEnum");
        Assert.assertEquals(property1.dataType, "String");
        Assert.assertEquals(property1.baseType, "String");
        Assert.assertEquals(property1.datatypeWithEnum, "SampleTestStringEnumEnum");
        Assert.assertEquals(property1.name, "testStringEnum");
        Assert.assertTrue(property1.isEnum);
        Assert.assertEquals(property1.allowableValues.size(), 2);
        Assert.assertEquals(((List<String>) property1.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars1 = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars1.size(), 2);

        Assert.assertEquals(enumVars1.get(0).get("name"), "foo");
        Assert.assertEquals(enumVars1.get(0).get("value"), "'foo'");
        Assert.assertEquals(enumVars1.get(0).get("isString"), true);

        Assert.assertEquals(enumVars1.get(1).get("name"), "bar");
        Assert.assertEquals(enumVars1.get(1).get("value"), "'bar'");
        Assert.assertEquals(enumVars1.get(1).get("isString"), true);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "testIntEnum");
        Assert.assertEquals(property2.dataType, "int");
        Assert.assertEquals(property2.baseType, "int");
        Assert.assertEquals(property2.datatypeWithEnum, "SampleTestIntEnumEnum");
        Assert.assertEquals(property2.name, "testIntEnum");
        Assert.assertTrue(property2.isEnum);
        Assert.assertEquals(property2.allowableValues.size(), 2);
        Assert.assertEquals(((List<String>) property2.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars2 = (List<Map<String, Object>>) property2.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars2.size(), 2);

        Assert.assertEquals(enumVars2.get(0).get("name"), "number1");
        Assert.assertEquals(enumVars2.get(0).get("value"), "1");
        Assert.assertEquals(enumVars2.get(0).get("isString"), false);

        Assert.assertEquals(enumVars2.get(1).get("name"), "number2");
        Assert.assertEquals(enumVars2.get(1).get("value"), "2");
        Assert.assertEquals(enumVars2.get(1).get("isString"), false);
    }

    @Test(description = "support for x-enum-values extension")
    public void testXEnumValuesExtension() {
        final Map<String, Object> enumValue1 = new HashMap<>();
        enumValue1.put("identifier", "foo");
        enumValue1.put("numericValue", 1);
        enumValue1.put("description", "the foo");
        final Map<String, Object> enumValue2 = new HashMap<>();
        enumValue2.put("identifier", "bar");
        enumValue2.put("numericValue", 2);
        enumValue2.put("description", "the bar");

        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("testIntEnum", new IntegerSchema().addEnumItem(1).addEnumItem(2)
                        .extensions(Collections.singletonMap("x-enum-values", Arrays.asList(enumValue1, enumValue2))));
        final DartClientCodegen codegen = new DartClientCodegen();
        codegen.setUseEnumExtension(true);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.postProcessModels(Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", cm))));

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "testIntEnum");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertEquals(property1.datatypeWithEnum, "SampleTestIntEnumEnum");
        Assert.assertEquals(property1.name, "testIntEnum");
        Assert.assertTrue(property1.isEnum);
        Assert.assertEquals(property1.allowableValues.size(), 2);
        Assert.assertEquals(((List<String>) property1.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars.size(), 2);

        Assert.assertEquals(enumVars.get(0).get("name"), "foo");
        Assert.assertEquals(enumVars.get(0).get("value"), "1");
        Assert.assertEquals(enumVars.get(0).get("isString"), false);
        Assert.assertEquals(enumVars.get(0).get("description"), "the foo");

        Assert.assertEquals(enumVars.get(1).get("name"), "bar");
        Assert.assertEquals(enumVars.get(1).get("value"), "2");
        Assert.assertEquals(enumVars.get(1).get("isString"), false);
        Assert.assertEquals(enumVars.get(1).get("description"), "the bar");
    }

    // datetime (or primitive type) not yet supported in HTTP request body
    @Test(description = "returns DateTime when using `--model-name-prefix`")
    public void dateTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new DartClientCodegen();
        codegen.setModelNamePrefix("foo");
        codegen.setOpenAPI(openAPI);

        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assert.assertEquals(op.returnType, "DateTime");
        Assert.assertEquals(op.bodyParam.dataType, "DateTime");
    }

    @Test(description = "correctly generate date/datetime default values, currently null")
    public void dateDefaultValues() {
        final DateSchema date = new DateSchema();
        date.setDefault("2021-01-01");
        final DateTimeSchema dateTime = new DateTimeSchema();
        dateTime.setDefault("2021-01-01T14:00:00Z");
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("date", date)
                .addProperties("dateTime", dateTime)
                .addProperties("mapNoDefault", new MapSchema());
        final DefaultCodegen codegen = new DartDioClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty dateDefault = cm.vars.get(0);
        Assert.assertEquals(dateDefault.name, "date");
        Assert.assertNull(dateDefault.defaultValue);

        final CodegenProperty dateTimeDefault = cm.vars.get(1);
        Assert.assertEquals(dateTimeDefault.name, "dateTime");
        Assert.assertNull(dateTimeDefault.defaultValue);
    }
}
