/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java;

import com.google.common.collect.Sets;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.ByteArraySchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.media.XML;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.junit.rules.TemporaryFolder;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.ClientOpts;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JavaModelTest {
    private TemporaryFolder folder = new TemporaryFolder();

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema()
                        .example("Tony"))
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property1 = vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.nameInCamelCase, "Id");
        Assert.assertEquals(property1.nameInSnakeCase, "ID");
        Assert.assertEquals(property1.getter, "getId");
        Assert.assertEquals(property1.setter, "setId");
        Assert.assertEquals(property1.dataType, "Long");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "Long");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.nameInCamelCase, "Name");
        Assert.assertEquals(property2.nameInSnakeCase, "NAME");
        Assert.assertEquals(property2.getter, "getName");
        Assert.assertEquals(property2.setter, "setName");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertEquals(property2.example, "Tony");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.nameInCamelCase, "CreatedAt");
        Assert.assertEquals(property3.nameInSnakeCase, "CREATED_AT");
        Assert.assertEquals(property3.getter, "getCreatedAt");
        Assert.assertEquals(property3.setter, "setCreatedAt");
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property = cm.vars.get(1);
        Assert.assertEquals(property.baseName, "urls");
        Assert.assertEquals(property.getter, "getUrls");
        Assert.assertEquals(property.setter, "setUrls");
        Assert.assertEquals(property.dataType, "List<String>");
        Assert.assertEquals(property.name, "urls");
        Assert.assertEquals(property.defaultValue, "new ArrayList<String>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "translations");
        Assert.assertEquals(property.getter, "getTranslations");
        Assert.assertEquals(property.setter, "setTranslations");
        Assert.assertEquals(property.dataType, "Map<String, String>");
        Assert.assertEquals(property.name, "translations");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, String>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map with complex list property")
    public void mapWithListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new ArraySchema().items(new Schema().$ref("Pet"))))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "translations");
        Assert.assertEquals(property.getter, "getTranslations");
        Assert.assertEquals(property.setter, "setTranslations");
        Assert.assertEquals(property.dataType, "Map<String, List<Pet>>");
        Assert.assertEquals(property.name, "translations");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, List<Pet>>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a 2D list property")
    public void list2DPropertyTest() {
        final Schema model = new Schema()
                .name("sample")
                .addProperties("list2D", new ArraySchema().items(
                        new ArraySchema().items(new Schema().$ref("Pet"))));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "list2D");
        Assert.assertEquals(property.getter, "getList2D");
        Assert.assertEquals(property.setter, "setList2D");
        Assert.assertEquals(property.dataType, "List<List<Pet>>");
        Assert.assertEquals(property.name, "list2D");
        Assert.assertEquals(property.defaultValue, "new ArrayList<List<Pet>>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex properties")
    public void complexPropertiesTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/components/schemas/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.dataType, "Children");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "Children");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.complexType, "Children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.dataType, "List<Children>");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "new ArrayList<Children>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "List", "Children")).size(), 3);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.complexType, "Children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.dataType, "Map<String, Children>");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, Children>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
        Assert.assertFalse(property.isNotContainer);

    }

    @Test(description = "convert a model with an array property with item name")
    public void arrayModelWithItemNameTest() {
        final Schema propertySchema = new ArraySchema()
                .items(new Schema().$ref("#/components/schemas/Child"))
                .description("an array property");
        propertySchema.addExtension("x-item-name", "child");
        final Schema schema = new Schema()
                .type("object")
                .description("a sample model")
                .addProperties("children", propertySchema);


        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("List", "Child")).size(), 2);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.complexType, "Child");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.dataType, "List<Child>");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "new ArrayList<Child>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
        Assert.assertFalse(property.isNotContainer);

        final CodegenProperty itemsProperty = property.items;
        Assert.assertEquals(itemsProperty.baseName,"child");
        Assert.assertEquals(itemsProperty.name,"child");
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().name("elobjeto").$ref("#/components/schemas/Children"))
                .name("arraySchema")
                .description("an array model");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "ArrayList<Children>");
        Assert.assertEquals(cm.imports.size(), 4);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "List", "ArrayList", "Children")).size(), 4);
    }

    @Test(description = "convert an map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("an map model")
                .additionalProperties(new Schema().$ref("#/components/schemas/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "HashMap<String, Children>");
        Assert.assertEquals(cm.imports.size(), 4);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "Map", "HashMap", "Children")).size(), 4);
    }

    @Test(description = "convert a model with upper-case property names")
    public void upperCaseNamesTest() {
        final Schema schema = new Schema()
                .description("a model with upper-case property names")
                .addProperties("NAME", new StringSchema())
                .addRequiredItem("NAME");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "NAME");
        Assert.assertEquals(property.getter, "getNAME");
        Assert.assertEquals(property.setter, "setNAME");
        Assert.assertEquals(property.dataType, "String");
        Assert.assertEquals(property.name, "NAME");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertFalse(property.hasMore);
        Assert.assertTrue(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with a 2nd char upper-case property names")
    public void secondCharUpperCaseNamesTest() {
        final Schema schema = new Schema()
                .description("a model with a 2nd char upper-case property names")
                .addProperties("pId", new StringSchema())
                .addRequiredItem("pId");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "pId");
        Assert.assertEquals(property.getter, "getPId");
        Assert.assertEquals(property.setter, "setPId");
        Assert.assertEquals(property.dataType, "String");
        Assert.assertEquals(property.name, "pId");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertFalse(property.hasMore);
        Assert.assertTrue(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model starting with two upper-case letter property names")
    public void firstTwoUpperCaseLetterNamesTest() {
        final Schema schema = new Schema()
                .description("a model with a property name starting with two upper-case letters")
                .addProperties("ATTName", new StringSchema())
                .addRequiredItem("ATTName");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "ATTName");
        Assert.assertEquals(property.getter, "getAtTName");
        Assert.assertEquals(property.setter, "setAtTName");
        Assert.assertEquals(property.dataType, "String");
        Assert.assertEquals(property.name, "atTName");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertFalse(property.hasMore);
        Assert.assertTrue(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert hyphens per issue 503")
    public void hyphensTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("created-at", new DateTimeSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "created-at");
        Assert.assertEquals(property.getter, "getCreatedAt");
        Assert.assertEquals(property.setter, "setCreatedAt");
        Assert.assertEquals(property.name, "createdAt");
    }

    @Test(description = "convert query[password] to queryPassword")
    public void squareBracketsTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("query[password]", new StringSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "query[password]");
        Assert.assertEquals(property.getter, "getQueryPassword");
        Assert.assertEquals(property.setter, "setQueryPassword");
        Assert.assertEquals(property.name, "queryPassword");
    }

    @Test(description = "properly escape names per 567")
    public void escapeNamesTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("created-at", new DateTimeSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("with.dots", schema, Collections.singletonMap("with.dots", schema));

        Assert.assertEquals(cm.classname, "WithDots");
    }

    @Test(description = "convert a model with binary data")
    public void binaryDataTest() {
        final Schema schema = new Schema()
                .description("model with binary")
                .addProperties("inputBinaryData", new ByteArraySchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "inputBinaryData");
        Assert.assertEquals(property.getter, "getInputBinaryData");
        Assert.assertEquals(property.setter, "setInputBinaryData");
        Assert.assertEquals(property.dataType, "byte[]");
        Assert.assertEquals(property.name, "inputBinaryData");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "byte[]");
        Assert.assertFalse(property.hasMore);
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "translate an invalid param name")
    public void invalidParamNameTest() {
        final Schema schema = new Schema()
                .description("a model with a 2nd char upper-case property names")
                .addProperties("_", new StringSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "_");
        Assert.assertEquals(property.getter, "getU");
        Assert.assertEquals(property.setter, "setU");
        Assert.assertEquals(property.dataType, "String");
        Assert.assertEquals(property.name, "u");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertFalse(property.hasMore);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a parameter")
    public void convertParameterTest() {
        final Parameter parameter = new QueryParameter()
                .description("this is a description")
                .name("limit")
                .required(true);
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenParameter cm = codegen.fromParameter(parameter, null);

        Assert.assertNull(cm.allowableValues);
        Assert.assertEquals(cm.description, "this is a description");
    }

    @Test(description = "types used by inner properties should be imported")
    public void mapWithAnListOfBigDecimalTest() {
        Schema schema1 = new Schema()
                .description("model with Map<String, List<BigDecimal>>")
                .addProperties("map", new MapSchema()
                        .additionalProperties(new ArraySchema().items(new NumberSchema())));
        final CodegenModel cm1 = new JavaClientCodegen().fromModel("sample", schema1, Collections.singletonMap("sample", schema1));
        Assert.assertEquals(cm1.vars.get(0).dataType, "Map<String, List<BigDecimal>>");
        Assert.assertTrue(cm1.imports.contains("BigDecimal"));

        Schema schema2 = new Schema()
                .description("model with Map<String, Map<String, List<BigDecimal>>>")
                .addProperties("map", new MapSchema()
                        .additionalProperties(new MapSchema()
                                .additionalProperties(new ArraySchema().items(new NumberSchema()))));
        final CodegenModel cm2 = new JavaClientCodegen().fromModel("sample", schema2, Collections.singletonMap("sample", schema2));
        Assert.assertEquals(cm2.vars.get(0).dataType, "Map<String, Map<String, List<BigDecimal>>>");
        Assert.assertTrue(cm2.imports.contains("BigDecimal"));
    }

    @DataProvider(name = "modelNames")
    public static Object[][] primeNumbers() {
        return new Object[][] {
                {"sample", "Sample"},
                {"sample_name", "SampleName"},
                {"sample__name", "SampleName"},
                {"/sample", "Sample"},
                {"\\sample", "Sample"},
                {"sample.name", "SampleName"},
                {"_sample", "Sample"},
                {"Sample", "Sample"},
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Schema schema = new Schema();
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel(name, schema, Collections.singletonMap(name, schema));

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }

    @DataProvider(name = "classProperties")
    public static Object[][] classProperties() {
        return new Object[][] {
                {"class", "getPropertyClass", "setPropertyClass", "propertyClass"},
                {"_class", "getPropertyClass", "setPropertyClass", "propertyClass"},
                {"__class", "getPropertyClass", "setPropertyClass", "propertyClass"}
        };
    }

    @Test(dataProvider = "classProperties", description = "handle 'class' properties")
    public void classPropertyTest(String baseName, String getter, String setter, String name) {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties(baseName, new StringSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, baseName);
        Assert.assertEquals(property.getter, getter);
        Assert.assertEquals(property.setter, setter);
        Assert.assertEquals(property.name, name);
    }


    @Test(description = "test models with xml")
    public void modelWithXmlTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .xml(new XML()
                        .prefix("my")
                        .namespace("xmlNamespace")
                        .name("customXmlName"))
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema()
                        .example("Tony")
                        .xml(new XML()
                                .attribute(true)
                                .prefix("my")
                                .name("myName")))
                .addProperties("createdAt", new DateTimeSchema()
                        .xml(new XML()
                                .prefix("my")
                                .namespace("myNamespace")
                                .name("myCreatedAt")))
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.xmlPrefix, "my");
        Assert.assertEquals(cm.xmlName, "customXmlName");
        Assert.assertEquals(cm.xmlNamespace, "xmlNamespace");
        Assert.assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property2 = vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.getter, "getName");
        Assert.assertEquals(property2.setter, "setName");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertEquals(property2.example, "Tony");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isNotContainer);
        Assert.assertTrue(property2.isXmlAttribute);
        Assert.assertEquals(property2.xmlName, "myName");
        Assert.assertNull(property2.xmlNamespace);

        final CodegenProperty property3 = vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.getter, "getCreatedAt");
        Assert.assertEquals(property3.setter, "setCreatedAt");
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);
        Assert.assertFalse(property3.isXmlAttribute);
        Assert.assertEquals(property3.xmlName, "myCreatedAt");
        Assert.assertEquals(property3.xmlNamespace, "myNamespace");
        Assert.assertEquals(property3.xmlPrefix, "my");
    }

    @Test(description = "test models with wrapped xml")
    public void modelWithWrappedXmlTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .xml(new XML()
                        .prefix("my")
                        .namespace("xmlNamespace")
                        .name("customXmlName"))
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("array", new ArraySchema()
                        .items(new StringSchema()
                                .xml(new XML()
                                        .name("i")))
                        .xml(new XML()
                                .prefix("my")
                                .wrapped(true)
                                .namespace("myNamespace")
                                .name("xmlArray")))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, Collections.singletonMap("sample", schema));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.xmlPrefix, "my");
        Assert.assertEquals(cm.xmlName, "customXmlName");
        Assert.assertEquals(cm.xmlNamespace, "xmlNamespace");
        Assert.assertEquals(cm.vars.size(), 2);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property2 = vars.get(1);
        Assert.assertEquals(property2.baseName, "array");
        Assert.assertEquals(property2.getter, "getArray");
        Assert.assertEquals(property2.setter, "setArray");
        Assert.assertEquals(property2.dataType, "List<String>");
        Assert.assertEquals(property2.name, "array");
        Assert.assertEquals(property2.defaultValue, "new ArrayList<String>()");
        Assert.assertEquals(property2.baseType, "List");
        Assert.assertTrue(property2.isContainer);
        Assert.assertTrue(property2.isXmlWrapped);
        Assert.assertEquals(property2.xmlName, "xmlArray");
        Assert.assertNotNull(property2.xmlNamespace);
        Assert.assertNotNull(property2.items);
        CodegenProperty items = property2.items;
        Assert.assertEquals(items.xmlName, "i");
        Assert.assertEquals(items.baseName, "array");
    }

    @Test(description = "convert a boolean parameter")
    public void booleanPropertyTest() {
        final BooleanSchema property = new BooleanSchema();
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.dataType, "Boolean");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "Boolean");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertTrue(cp.isBoolean);
        Assert.assertEquals(cp.getter, "isProperty");
    }

    @Test(description = "convert an integer property")
    public void integerPropertyTest() {
        final IntegerSchema property = new IntegerSchema();
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.dataType, "Integer");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "Integer");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertTrue(cp.isInteger);
        Assert.assertFalse(cp.isLong);
        Assert.assertEquals(cp.getter, "getProperty");
    }

    @Test(description = "convert a long property")
    public void longPropertyTest() {
        final IntegerSchema property = new IntegerSchema().format("int64");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.nameInCamelCase, "Property");
        Assert.assertEquals(cp.nameInSnakeCase, "PROPERTY");
        Assert.assertEquals(cp.dataType, "Long");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "Long");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertTrue(cp.isLong);
        Assert.assertFalse(cp.isInteger);
        Assert.assertEquals(cp.getter, "getProperty");
    }

    @Test(description = "convert an integer property in a referenced schema")
    public void integerPropertyInReferencedSchemaTest() {
        final IntegerSchema longProperty = new IntegerSchema().format("int32");
        final Schema testSchema = new ObjectSchema()
                .addProperties("Integer1", new Schema<>().$ref("#/components/schemas/IntegerProperty"))
                .addProperties("Integer2", new IntegerSchema().format("int32"));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("IntegerProperty", longProperty);
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("test", testSchema, allDefinitions);

        Assert.assertEquals(cm.vars.size(), 2);

        CodegenProperty cp1 = cm.vars.get(0);
        Assert.assertEquals(cp1.baseName, "Integer1");
        Assert.assertEquals(cp1.nameInCamelCase, "Integer1");
        Assert.assertEquals(cp1.nameInSnakeCase, "INTEGER1");
        Assert.assertEquals(cp1.dataType, "Integer");
        Assert.assertEquals(cp1.name, "integer1");
        Assert.assertEquals(cp1.baseType, "Integer");
        Assert.assertEquals(cp1.getter, "getInteger1");

        CodegenProperty cp2 = cm.vars.get(1);
        Assert.assertEquals(cp2.baseName, "Integer2");
        Assert.assertEquals(cp2.nameInCamelCase, "Integer2");
        Assert.assertEquals(cp2.nameInSnakeCase, "INTEGER2");
        Assert.assertEquals(cp2.dataType, "Integer");
        Assert.assertEquals(cp2.name, "integer2");
        Assert.assertEquals(cp2.baseType, "Integer");
        Assert.assertEquals(cp2.getter, "getInteger2");
    }

    @Test(description = "convert a long property in a referenced schema")
    public void longPropertyInReferencedSchemaTest() {
        final IntegerSchema longProperty = new IntegerSchema().format("int64");
        final Schema TestSchema = new ObjectSchema()
                .addProperties("Long1", new Schema<>().$ref("#/components/schemas/LongProperty"))
                .addProperties("Long2", new IntegerSchema().format("int64"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final Map<String, Schema> allDefinitions = Collections.<String, Schema>singletonMap("LongProperty",
                longProperty);
        final CodegenModel cm = codegen.fromModel("test", TestSchema, allDefinitions);

        Assert.assertEquals(cm.vars.size(), 2);

        CodegenProperty cp1 = cm.vars.get(0);
        Assert.assertEquals(cp1.baseName, "Long1");
        Assert.assertEquals(cp1.dataType, "Long");
        Assert.assertEquals(cp1.name, "long1");
        Assert.assertEquals(cp1.baseType, "Long");
        Assert.assertEquals(cp1.getter, "getLong1");

        CodegenProperty cp2 = cm.vars.get(1);
        Assert.assertEquals(cp2.baseName, "Long2");
        Assert.assertEquals(cp2.dataType, "Long");
        Assert.assertEquals(cp2.name, "long2");
        Assert.assertEquals(cp2.baseType, "Long");
        Assert.assertEquals(cp2.getter, "getLong2");
    }

    @Test(description = "convert string property")
    public void stringPropertyTest() {
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenProperty cp = codegen.fromProperty("somePropertyWithMinMaxAndPattern", property);

        Assert.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInCamelCase, "SomePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assert.assertEquals(cp.dataType, "String");
        Assert.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.baseType, "String");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isInteger);
        Assert.assertTrue(cp.isString);
        Assert.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.minLength, Integer.valueOf(3));
        Assert.assertEquals(cp.maxLength, Integer.valueOf(10));
        Assert.assertEquals(cp.pattern, "^[A-Z]+$");
    }

    @Test(description = "convert string property in an object")
    public void stringPropertyInObjectTest() {
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final Schema myObject = new ObjectSchema().addProperties("somePropertyWithMinMaxAndPattern", property);

        final DefaultCodegen codegen = new JavaClientCodegen();
        CodegenModel cm = codegen.fromModel("myObject", myObject, Collections.singletonMap("myObject", myObject));

        Assert.assertEquals(cm.getVars().size(), 1);
        CodegenProperty cp = cm.getVars().get(0);
        Assert.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInCamelCase, "SomePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assert.assertEquals(cp.dataType, "String");
        Assert.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.baseType, "String");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isInteger);
        Assert.assertTrue(cp.isString);
        Assert.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.minLength, Integer.valueOf(3));
        Assert.assertEquals(cp.maxLength, Integer.valueOf(10));
        Assert.assertEquals(cp.pattern, "^[A-Z]+$");
    }

    @Test(description = "convert referenced string property in an object")
    public void stringPropertyReferencedInObjectTest() {
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final Schema myObject = new ObjectSchema().addProperties("somePropertyWithMinMaxAndPattern", new ObjectSchema().$ref("refObj"));

        final DefaultCodegen codegen = new JavaClientCodegen();
        Map<String, Schema> schemaMap = new HashMap<>();
        schemaMap.put("myObject", myObject);
        schemaMap.put("refObj", property);
        CodegenModel cm = codegen.fromModel("myObject", myObject, schemaMap);

        Assert.assertEquals(cm.getVars().size(), 1);
        CodegenProperty cp = cm.getVars().get(0);
        Assert.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInCamelCase, "SomePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assert.assertEquals(cp.dataType, "String");
        Assert.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assert.assertEquals(cp.baseType, "String");
        Assert.assertTrue(cp.isNotContainer);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isInteger);
        // Assert.assertTrue(cp.isString); //TODO: issue swagger-api/swagger-codegen#8001
        Assert.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        // Assert.assertEquals(cp.minLength, Integer.valueOf(3)); //TODO: issue swagger-api/swagger-codegen#8001
        // Assert.assertEquals(cp.maxLength, Integer.valueOf(10)); //TODO: issue swagger-api/swagger-codegen#8001
        // Assert.assertEquals(cp.pattern, "^[A-Z]+$"); //TODO: issue swagger-api/swagger-codegen#8001
    }

    @Test(description = "convert an array schema")
    public void arraySchemaTest() {
        final Schema testSchema = new ObjectSchema()
                .addProperties("pets", new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet")));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("test", testSchema, allDefinitions);

        Assert.assertEquals(cm.vars.size(), 1);
        CodegenProperty cp1 = cm.vars.get(0);
        Assert.assertEquals(cp1.baseName, "pets");
        Assert.assertEquals(cp1.dataType, "List<Pet>");
        Assert.assertEquals(cp1.name, "pets");
        Assert.assertEquals(cp1.baseType, "List");
        Assert.assertTrue(cp1.isContainer);
        Assert.assertTrue(cp1.isListContainer);
        Assert.assertFalse(cp1.isMapContainer);
        Assert.assertEquals(cp1.getter, "getPets");
        Assert.assertEquals(cp1.items.baseType, "Pet");

        Assert.assertTrue(cm.imports.contains("List"));
        Assert.assertTrue(cm.imports.contains("Pet"));
    }

    @Test(description = "convert an array schema in a RequestBody")
    public void arraySchemaTestInRequestBody() {
        final Schema testSchema = new ArraySchema()
                .items(new Schema<>().$ref("#/components/schemas/Pet"));
        Operation operation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content().addMediaType("application/json", 
                                new MediaType().schema(testSchema))))
                .responses(
                        new ApiResponses().addApiResponse("204", new ApiResponse()
                                .description("Ok response")));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, allDefinitions);

        Assert.assertEquals(co.bodyParams.size(), 1);
        CodegenParameter cp1 = co.bodyParams.get(0);
        Assert.assertEquals(cp1.baseType, "List");
        Assert.assertEquals(cp1.dataType, "List<Pet>");
        Assert.assertTrue(cp1.isContainer);
        Assert.assertTrue(cp1.isListContainer);
        Assert.assertFalse(cp1.isMapContainer);
        Assert.assertEquals(cp1.items.baseType, "Pet");
        Assert.assertEquals(cp1.items.complexType, "Pet");
        Assert.assertEquals(cp1.items.dataType, "Pet");

        Assert.assertEquals(co.responses.size(), 1);

        Assert.assertTrue(co.imports.contains("List"));
        Assert.assertTrue(co.imports.contains("Pet"));
    }

    @Test(description = "convert an array schema in a ApiResponse")
    public void arraySchemaTestInOperationResponse() {
        final Schema testSchema = new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet"));
        Operation operation = new Operation().responses(
                new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("Ok response")
                        .content(new Content().addMediaType("application/json", 
                                new MediaType().schema(testSchema)))));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, allDefinitions);

        Assert.assertEquals(co.responses.size(), 1);
        CodegenResponse cr = co.responses.get(0);
        Assert.assertEquals(cr.baseType, "Pet");
        Assert.assertEquals(cr.dataType, "List<Pet>");
        Assert.assertEquals(cr.containerType, "array");

        Assert.assertTrue(co.imports.contains("Pet"));
    }

    @Test(description = "convert a array of array schema")
    public void arrayOfArraySchemaTest() {
        final Schema testSchema = new ObjectSchema()
                .addProperties("pets", new ArraySchema()
                        .items(new ArraySchema()
                                .items(new Schema<>().$ref("#/components/schemas/Pet"))));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("test", testSchema, allDefinitions);

        Assert.assertEquals(cm.vars.size(), 1);
        CodegenProperty cp1 = cm.vars.get(0);
        Assert.assertEquals(cp1.baseName, "pets");
        Assert.assertEquals(cp1.dataType, "List<List<Pet>>");
        Assert.assertEquals(cp1.name, "pets");
        Assert.assertEquals(cp1.baseType, "List");
        Assert.assertEquals(cp1.getter, "getPets");

        Assert.assertTrue(cm.imports.contains("List"));
        Assert.assertTrue(cm.imports.contains("Pet"));
    }

    @Test(description = "convert an array of array schema in a RequestBody")
    public void arrayOfArraySchemaTestInRequestBody() {
        final Schema testSchema = new ArraySchema()
                .items(new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet")));
        Operation operation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content().addMediaType("application/json", 
                                new MediaType().schema(testSchema))))
                .responses(
                        new ApiResponses().addApiResponse("204", new ApiResponse()
                                .description("Ok response")));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, allDefinitions);

        Assert.assertEquals(co.bodyParams.size(), 1);
        CodegenParameter cp1 = co.bodyParams.get(0);
        Assert.assertEquals(cp1.baseType, "List");
        Assert.assertEquals(cp1.dataType, "List<List<Pet>>");
        Assert.assertTrue(cp1.isContainer);
        Assert.assertTrue(cp1.isListContainer);
        Assert.assertFalse(cp1.isMapContainer);
        Assert.assertEquals(cp1.items.baseType, "List");
        Assert.assertEquals(cp1.items.complexType, "Pet");
        Assert.assertEquals(cp1.items.dataType, "List<Pet>");
        Assert.assertEquals(cp1.items.items.baseType, "Pet");
        Assert.assertEquals(cp1.items.items.complexType, "Pet");
        Assert.assertEquals(cp1.items.items.dataType, "Pet");

        Assert.assertEquals(co.responses.size(), 1);

        Assert.assertTrue(co.imports.contains("Pet"));
        Assert.assertTrue(co.imports.contains("List"));
    }

    @Test(description = "convert a array schema in a ApiResponse")
    public void arrayOfArraySchemaTestInOperationResponse() {
        final Schema testSchema = new ArraySchema()
                .items(new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet")));
        Operation operation = new Operation().responses(
                new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("Ok response")
                        .content(new Content().addMediaType("application/json", 
                                new MediaType().schema(testSchema)))));
        final Map<String, Schema> allDefinitions = Collections.<String, Schema> singletonMap("Pet", new ObjectSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, allDefinitions);

        Assert.assertEquals(co.responses.size(), 1);
        CodegenResponse cr = co.responses.get(0);
        Assert.assertEquals(cr.baseType, "Pet");
        Assert.assertEquals(cr.dataType, "List<List<Pet>>");
        Assert.assertEquals(cr.containerType, "array");

        Assert.assertTrue(co.imports.contains("Pet"));
    }

    @Test
    public void generateModel() throws Exception {
        String inputSpec = "src/test/resources/3_0/petstore.json";

        folder.create();
        final File output = folder.getRoot();
        Assert.assertTrue(new File(inputSpec).exists());

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary("jersey2")
                //.addAdditionalProperty("withXml", true)
                .addAdditionalProperty(CodegenConstants.SERIALIZABLE_MODEL, true)
                .setInputSpec(inputSpec)
                .setOutputDir(output.getAbsolutePath());

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        new DefaultGenerator().opts(clientOptInput).generate();

        File orderFile = new File(output, "src/main/java/org/openapitools/client/model/Order.java");
        Assert.assertTrue(orderFile.exists());
        folder.delete();
    }

    @Test
    public void generateEmpty() throws Exception {
        String inputSpec = "src/test/resources/3_0/ping.yaml";

        folder.create();
        final File output = folder.getRoot();
        Assert.assertTrue(new File(inputSpec).exists());

        JavaClientCodegen config = new org.openapitools.codegen.languages.JavaClientCodegen();
        config.setJava8Mode(true);
        config.setHideGenerationTimestamp(true);
        config.setOutputDir(output.getAbsolutePath());

        final OpenAPIParser openApiParser = new OpenAPIParser();
        final ParseOptions options = new ParseOptions();
        final OpenAPI openAPI = openApiParser.readLocation(inputSpec, null, options).getOpenAPI();

        final ClientOptInput opts = new ClientOptInput();
        opts.setConfig(config);
        opts.setOpenAPI(openAPI);
        opts.setOpts(new ClientOpts());
        new DefaultGenerator().opts(opts).generate();

        File orderFile = new File(output, "src/main/java/org/openapitools/client/api/DefaultApi.java");
        Assert.assertTrue(orderFile.exists());
        folder.delete();
    }
}
