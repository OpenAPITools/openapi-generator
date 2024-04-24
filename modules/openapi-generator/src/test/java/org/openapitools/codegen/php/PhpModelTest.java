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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("static-method")
public class PhpModelTest {

    @Test(description = "convert a simple php model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema())
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);
        // {{imports}} is not used in template
        //Assertions.assertEquals(cm.imports.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "string");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertEquals(property2.defaultValue, null);
        Assertions.assertEquals(property2.baseType, "string");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.complexType, null);
        Assertions.assertEquals(property3.dataType, "\\DateTime");
        Assertions.assertEquals(property3.name, "created_at");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "\\DateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema())
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PhpClientCodegen();
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
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "string[]");
        Assertions.assertEquals(property2.name, "urls");
        Assertions.assertEquals(property2.baseType, "array");
        Assertions.assertEquals(property2.containerType, "array");
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
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "array<string,string>");
        Assertions.assertEquals(property1.name, "translations");
        Assertions.assertEquals(property1.baseType, "array");
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
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "\\OpenAPI\\Client\\Model\\Children");
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
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "\\OpenAPI\\Client\\Model\\Children[]");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "array");
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
        final DefaultCodegen codegen = new PhpClientCodegen();
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
        Assertions.assertEquals(property1.dataType, "array<string,\\OpenAPI\\Client\\Model\\Children>");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "array");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new PhpClientCodegen();
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
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        // {{imports}} is not used in template
        //Assertions.assertEquals(cm.imports.size(), 2);
        //Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @DataProvider(name = "modelNames")
    public static Object[][] primeNumbers() {
        return new Object[][] {
            {"sample", "Sample"},
            {"sample_name", "SampleName"},
            {"sample__name", "SampleName"},
            {"/sample", "Sample"},
            {"\\sample", "\\Sample"},
            {"sample.name", "SampleName"},
            {"_sample", "Sample"},
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Schema model = new Schema();
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        Assertions.assertEquals(cm.name, name);
        Assertions.assertEquals(cm.classname, expectedName);
    }

    @Test(description = "test enum array model")
    public void enumArrayModelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new PhpClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        final Schema definition = schemas.get("EnumArrays");

        Schema property =  (Schema) definition.getProperties().get("array_enum");
        CodegenProperty prope = codegen.fromProperty("array_enum", property);
        codegen.updateCodegenPropertyEnum(prope);
        Assertions.assertEquals(prope.datatypeWithEnum, "ARRAY_ENUM[]");
        Assertions.assertEquals(prope.enumName, "ARRAY_ENUM");
        Assertions.assertTrue(prope.isEnum);
        Assertions.assertEquals(prope.allowableValues.get("values"), Arrays.asList("fish", "crab"));

        HashMap<String, Object> fish= new HashMap<String, Object>();
        fish.put("name", "FISH");
        fish.put("value", "\'fish\'");
        fish.put("isString", true);
        HashMap<String, Object> crab= new HashMap<String, Object>();
        crab.put("name", "CRAB");
        crab.put("value", "\'crab\'");
        crab.put("isString", true);
        Assertions.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        // assert inner items
        Assertions.assertEquals(prope.datatypeWithEnum, "ARRAY_ENUM[]");
        Assertions.assertEquals(prope.enumName, "ARRAY_ENUM");
        Assertions.assertTrue(prope.items.isEnum);
        Assertions.assertEquals(prope.items.allowableValues.get("values"), Arrays.asList("fish", "crab"));
        Assertions.assertEquals(prope.items.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

    }

    @Test(description = "test enum model for values (numeric, string, etc)")
    public void enumModelValueTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new PhpClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Schema definition = openAPI.getComponents().getSchemas().get("Enum_Test");

        Schema property =  (Schema) definition.getProperties().get("enum_integer");
        CodegenProperty prope = codegen.fromProperty("enum_integer", property);
        codegen.updateCodegenPropertyEnum(prope);
        Assertions.assertEquals(prope.datatypeWithEnum, "ENUM_INTEGER");
        Assertions.assertEquals(prope.enumName, "ENUM_INTEGER");
        Assertions.assertTrue(prope.isEnum);
        Assertions.assertFalse(prope.isContainer);
        Assertions.assertNull(prope.items);
        Assertions.assertEquals(prope.allowableValues.get("values"), Arrays.asList(1, -1));

        HashMap<String, Object> one = new HashMap<String, Object>();
        one.put("name", "1");
        one.put("value", "1");
        one.put("isString", false);
        HashMap<String, Object> minusOne = new HashMap<String, Object>();
        minusOne.put("name", "MINUS_1");
        minusOne.put("value", "-1");
        minusOne.put("isString", false);
        Assertions.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(one, minusOne));
    }

    @Test(description = "test enum variable names for reserved words")
    public void testReservedWord() throws Exception {
        final DefaultCodegen codegen = new PhpClientCodegen();
        Assertions.assertEquals(codegen.toEnumVarName("public", null), "_PUBLIC");
        Assertions.assertEquals(codegen.toEnumVarName("Private", null), "_PRIVATE");
        Assertions.assertEquals(codegen.toEnumVarName("IF", null), "_IF");
        // should not escape non-reserved
        Assertions.assertEquals(codegen.toEnumVarName("hello", null), "HELLO");
    }

    // datetime (or primitive type) not yet supported in HTTP request body
    @Test(description = "returns DateTime when using `--model-name-prefix`")
    public void dateTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new PhpClientCodegen();
        codegen.setModelNamePrefix("foo");
        codegen.setOpenAPI(openAPI);

        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assertions.assertEquals(op.returnType, "\\DateTime");
        Assertions.assertEquals(op.bodyParam.dataType, "\\DateTime");
    }

    @Test(description = "test @ character replacements used in Hydra")
    public void hydraModelTest() {
        final Schema model = new Schema()
                .description("a hydra model")
                .addProperties("id", new IntegerSchema())
                .addProperties("@id", new StringSchema())
                .addProperties("type", new StringSchema())
                .addProperties("@type", new StringSchema())
                .addProperties("context", new StringSchema())
                .addProperties("@context", new StringSchema())
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PhpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.vars.size(), 6);


        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "int");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "@id");
        Assertions.assertEquals(property2.dataType, "string");
        Assertions.assertEquals(property2.name, "at_id");
        Assertions.assertEquals(property2.defaultValue, null);
        Assertions.assertEquals(property2.baseType, "string");
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "type");
        Assertions.assertEquals(property3.dataType, "string");
        Assertions.assertEquals(property3.name, "type");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertEquals(property3.baseType, "string");
        Assertions.assertTrue(property3.isPrimitiveType);
        Assertions.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "@type");
        Assertions.assertEquals(property4.dataType, "string");
        Assertions.assertEquals(property4.name, "at_type");
        Assertions.assertEquals(property4.defaultValue, null);
        Assertions.assertEquals(property4.baseType, "string");
        Assertions.assertTrue(property4.isPrimitiveType);
        Assertions.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assertions.assertEquals(property5.baseName, "context");
        Assertions.assertEquals(property5.dataType, "string");
        Assertions.assertEquals(property5.name, "context");
        Assertions.assertEquals(property5.defaultValue, null);
        Assertions.assertEquals(property5.baseType, "string");
        Assertions.assertTrue(property5.isPrimitiveType);
        Assertions.assertFalse(property5.isContainer);

        final CodegenProperty property6 = cm.vars.get(5);
        Assertions.assertEquals(property6.baseName, "@context");
        Assertions.assertEquals(property6.dataType, "string");
        Assertions.assertEquals(property6.name, "at_context");
        Assertions.assertEquals(property6.defaultValue, null);
        Assertions.assertEquals(property6.baseType, "string");
        Assertions.assertTrue(property6.isPrimitiveType);
        Assertions.assertFalse(property6.isContainer);
    }
}
