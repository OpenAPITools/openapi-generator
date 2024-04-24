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

import static org.openapitools.codegen.TestUtils.createCodegenModelWrapper;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.DartClientCodegen;
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 6);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "String");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.complexType, "DateTime");
        Assertions.assertEquals(property3.dataType, "DateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "DateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "defaultItem");
        Assertions.assertEquals(property4.dataType, "int");
        Assertions.assertEquals(property4.defaultValue, "1");
        Assertions.assertEquals(property4.baseType, "int");
        Assertions.assertFalse(property4.required);
        Assertions.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assertions.assertEquals(property5.baseName, "number");
        Assertions.assertEquals(property5.dataType, "num");
        Assertions.assertEquals(property5.baseType, "num");

        final CodegenProperty property6 = cm.vars.get(5);
        Assertions.assertEquals(property6.baseName, "decimal");
        Assertions.assertEquals(property6.dataType, "double");
        Assertions.assertEquals(property6.baseType, "double");
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<String>");
        Assertions.assertEquals(property2.name, "urls");
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "Set<String>");
        Assertions.assertEquals(property2.name, "urls");
        Assertions.assertEquals(property2.baseType, "Set");
        Assertions.assertEquals(property2.containerType, "set");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "Map<String, String>");
        Assertions.assertEquals(property1.name, "translations");
        Assertions.assertEquals(property1.baseType, "Map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
        Assertions.assertTrue(property1.isPrimitiveType);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "Children");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "Children");
        Assertions.assertFalse(property1.required);
        Assertions.assertFalse(property1.isContainer);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "List<Children>");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "List");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        // {{imports}} is not used in template
        //Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.dataType, "Map<String, Children>");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "Map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
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

        Assertions.assertEquals(model.getDescription(), "an array model");

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertTrue(cm.isArray);
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
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

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
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

        Assertions.assertEquals(cm.name, name);
        Assertions.assertEquals(cm.classname, codegen.toModelName(expectedName));
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
        Assertions.assertEquals(codegen.toVarName(name), expectedName);
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
        Assertions.assertEquals(codegen.toEnumVarName(enumVar.name, enumVar.dataType), enumVar.expected);
    }

    @Test(description = "model names support `--model-name-prefix` and `--model-name-suffix`")
    public void modelPrefixSuffixTest() {
        final DefaultCodegen codegen = new DartClientCodegen();
        codegen.setModelNamePrefix("model");
        codegen.setModelNameSuffix("type");

        Assertions.assertEquals(codegen.toModelName("hello_test"), "ModelHelloTestType");
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
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "testStringEnum");
        Assertions.assertEquals(property1.dataType, "String");
        Assertions.assertEquals(property1.baseType, "String");
        Assertions.assertEquals(property1.datatypeWithEnum, "SampleTestStringEnumEnum");
        Assertions.assertEquals(property1.name, "testStringEnum");
        Assertions.assertTrue(property1.isEnum);
        Assertions.assertEquals(property1.allowableValues.size(), 2);
        Assertions.assertEquals(((List<String>) property1.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars1 = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assertions.assertEquals(enumVars1.size(), 2);

        Assertions.assertEquals(enumVars1.get(0).get("name"), "foo");
        Assertions.assertEquals(enumVars1.get(0).get("value"), "'foo'");
        Assertions.assertEquals(enumVars1.get(0).get("isString"), true);

        Assertions.assertEquals(enumVars1.get(1).get("name"), "bar");
        Assertions.assertEquals(enumVars1.get(1).get("value"), "'bar'");
        Assertions.assertEquals(enumVars1.get(1).get("isString"), true);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "testIntEnum");
        Assertions.assertEquals(property2.dataType, "int");
        Assertions.assertEquals(property2.baseType, "int");
        Assertions.assertEquals(property2.datatypeWithEnum, "SampleTestIntEnumEnum");
        Assertions.assertEquals(property2.name, "testIntEnum");
        Assertions.assertTrue(property2.isEnum);
        Assertions.assertEquals(property2.allowableValues.size(), 2);
        Assertions.assertEquals(((List<String>) property2.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars2 = (List<Map<String, Object>>) property2.allowableValues.get("enumVars");
        Assertions.assertEquals(enumVars2.size(), 2);

        Assertions.assertEquals(enumVars2.get(0).get("name"), "number1");
        Assertions.assertEquals(enumVars2.get(0).get("value"), "1");
        Assertions.assertEquals(enumVars2.get(0).get("isString"), false);

        Assertions.assertEquals(enumVars2.get(1).get("name"), "number2");
        Assertions.assertEquals(enumVars2.get(1).get("value"), "2");
        Assertions.assertEquals(enumVars2.get(1).get("isString"), false);
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
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "testIntEnum");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertEquals(property1.datatypeWithEnum, "SampleTestIntEnumEnum");
        Assertions.assertEquals(property1.name, "testIntEnum");
        Assertions.assertTrue(property1.isEnum);
        Assertions.assertEquals(property1.allowableValues.size(), 2);
        Assertions.assertEquals(((List<String>) property1.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assertions.assertEquals(enumVars.size(), 2);

        Assertions.assertEquals(enumVars.get(0).get("name"), "foo");
        Assertions.assertEquals(enumVars.get(0).get("value"), "1");
        Assertions.assertEquals(enumVars.get(0).get("isString"), false);
        Assertions.assertEquals(enumVars.get(0).get("description"), "the foo");

        Assertions.assertEquals(enumVars.get(1).get("name"), "bar");
        Assertions.assertEquals(enumVars.get(1).get("value"), "2");
        Assertions.assertEquals(enumVars.get(1).get("isString"), false);
        Assertions.assertEquals(enumVars.get(1).get("description"), "the bar");
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

        Assertions.assertEquals(op.returnType, "DateTime");
        Assertions.assertEquals(op.bodyParam.dataType, "DateTime");
    }
}
