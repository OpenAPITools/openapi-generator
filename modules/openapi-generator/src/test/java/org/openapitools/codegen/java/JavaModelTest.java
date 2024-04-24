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

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.List;

public class JavaModelTest {

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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property1 = vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.nameInPascalCase, "Id");
        Assertions.assertEquals(property1.nameInCamelCase, "id");
        Assertions.assertEquals(property1.nameInSnakeCase, "ID");
        Assertions.assertEquals(property1.getter, "getId");
        Assertions.assertEquals(property1.setter, "setId");
        Assertions.assertEquals(property1.dataType, "Long");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "Long");
        Assertions.assertTrue(property1.required);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.nameInPascalCase, "Name");
        Assertions.assertEquals(property2.nameInCamelCase, "name");
        Assertions.assertEquals(property2.nameInSnakeCase, "NAME");
        Assertions.assertEquals(property2.getter, "getName");
        Assertions.assertEquals(property2.setter, "setName");
        Assertions.assertEquals(property2.dataType, "String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "String");
        Assertions.assertEquals(property2.example, "Tony");
        Assertions.assertTrue(property2.required);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.nameInPascalCase, "CreatedAt");
        Assertions.assertEquals(property3.nameInCamelCase, "createdAt");
        Assertions.assertEquals(property3.nameInSnakeCase, "CREATED_AT");
        Assertions.assertEquals(property3.getter, "getCreatedAt");
        Assertions.assertEquals(property3.setter, "setCreatedAt");
        Assertions.assertEquals(property3.dataType, "Date");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "Date");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property = cm.vars.get(1);
        Assertions.assertEquals(property.baseName, "urls");
        Assertions.assertEquals(property.getter, "getUrls");
        Assertions.assertEquals(property.setter, "setUrls");
        Assertions.assertEquals(property.dataType, "List<String>");
        Assertions.assertEquals(property.name, "urls");
        Assertions.assertEquals(property.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with set property")
    public void setPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema())
                        .uniqueItems(true))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property = cm.vars.get(1);
        Assertions.assertEquals(property.baseName, "urls");
        Assertions.assertEquals(property.getter, "getUrls");
        Assertions.assertEquals(property.setter, "setUrls");
        Assertions.assertEquals(property.dataType, "Set<String>");
        Assertions.assertEquals(property.name, "urls");
        Assertions.assertEquals(property.defaultValue, "new LinkedHashSet<>()");
        Assertions.assertEquals(property.baseType, "Set");
        Assertions.assertEquals(property.containerType, "set");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "translations");
        Assertions.assertEquals(property.getter, "getTranslations");
        Assertions.assertEquals(property.setter, "setTranslations");
        Assertions.assertEquals(property.dataType, "Map<String, String>");
        Assertions.assertEquals(property.name, "translations");
        Assertions.assertEquals(property.defaultValue, "new HashMap<>()");
        Assertions.assertEquals(property.baseType, "Map");
        Assertions.assertEquals(property.containerType, "map");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map with complex list property")
    public void mapWithListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new ArraySchema().items(new Schema().$ref("Pet"))))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "translations");
        Assertions.assertEquals(property.getter, "getTranslations");
        Assertions.assertEquals(property.setter, "setTranslations");
        Assertions.assertEquals(property.dataType, "Map<String, List<Pet>>");
        Assertions.assertEquals(property.name, "translations");
        Assertions.assertEquals(property.defaultValue, "new HashMap<>()");
        Assertions.assertEquals(property.baseType, "Map");
        Assertions.assertEquals(property.containerType, "map");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a 2D list property")
    public void list2DPropertyTest() {
        final Schema model = new Schema()
                .name("sample")
                .addProperties("list2D", new ArraySchema().items(
                        new ArraySchema().items(new Schema().$ref("Pet"))));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "list2D");
        Assertions.assertEquals(property.getter, "getList2D");
        Assertions.assertEquals(property.setter, "setList2D");
        Assertions.assertEquals(property.dataType, "List<List<Pet>>");
        Assertions.assertEquals(property.name, "list2D");
        Assertions.assertEquals(property.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with restricted characters")
    public void restrictedCharactersPropertiesTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("@Some:restricted%characters#to!handle+", new BooleanSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "@Some:restricted%characters#to!handle+");
        Assertions.assertEquals(property.getter, "getAtSomeColonRestrictedPercentCharactersHashToExclamationHandlePlus");
        Assertions.assertEquals(property.setter, "setAtSomeColonRestrictedPercentCharactersHashToExclamationHandlePlus");
        Assertions.assertEquals(property.dataType, "Boolean");
        Assertions.assertEquals(property.name, "atSomeColonRestrictedPercentCharactersHashToExclamationHandlePlus");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "Boolean");
        Assertions.assertFalse(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model with complex properties")
    public void complexPropertiesTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/components/schemas/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "Children");
        Assertions.assertEquals(property.name, "children");
        // "null" as default value for model
        Assertions.assertEquals(property.defaultValue, "null");
        Assertions.assertEquals(property.baseType, "Children");
        Assertions.assertFalse(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.complexType, "Children");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "List<Children>");
        Assertions.assertEquals(property.name, "children");
        Assertions.assertEquals(property.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "Children")).size(), 2);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.complexType, "Children");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "Map<String, Children>");
        Assertions.assertEquals(property.name, "children");
        Assertions.assertEquals(property.defaultValue, "new HashMap<>()");
        Assertions.assertEquals(property.baseType, "Map");
        Assertions.assertEquals(property.containerType, "map");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
        Assertions.assertTrue(property.isMap);
    }

    @Test(description = "convert a model with complex array property")
    public void complexArrayPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("List", "Children")).size(), 2);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.complexType, "Children");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "List<Children>");
        Assertions.assertEquals(property.name, "children");
        Assertions.assertEquals(property.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
        Assertions.assertTrue(property.isArray);
    }

    @Test(description = "convert a model with complex set property")
    public void complexSetPropertyTest() {
        Schema set = new ArraySchema().items(new Schema().$ref("#/components/schemas/Children"));
        set.setUniqueItems(true); // set
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", set);
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertTrue(cm.imports.contains("Set"));

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.complexType, "Children");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "Set<Children>");
        Assertions.assertEquals(property.name, "children");
        Assertions.assertEquals(property.defaultValue, "new LinkedHashSet<>()");
        Assertions.assertEquals(property.baseType, "Set");
        Assertions.assertEquals(property.containerType, "set");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
        Assertions.assertTrue(property.getUniqueItemsBoolean());
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("List", "Child")).size(), 2);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "children");
        Assertions.assertEquals(property.complexType, "Child");
        Assertions.assertEquals(property.getter, "getChildren");
        Assertions.assertEquals(property.setter, "setChildren");
        Assertions.assertEquals(property.dataType, "List<Child>");
        Assertions.assertEquals(property.name, "children");
        Assertions.assertEquals(property.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);

        final CodegenProperty itemsProperty = property.items;
        Assertions.assertEquals(itemsProperty.baseName, "child");
        Assertions.assertEquals(itemsProperty.name, "child");
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().name("elobjeto").$ref("#/components/schemas/Children"))
                .name("arraySchema")
                .description("an array model");
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setAnnotationLibrary(AnnotationLibrary.SWAGGER1);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "ArrayList<Children>");
        Assertions.assertEquals(cm.imports.size(), 4);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "List", "ArrayList", "Children")).size(), 4);
    }

    @Test(description = "convert a set model")
    public void setModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().name("elobjeto").$ref("#/components/schemas/Children"))
                .uniqueItems(true)
                .name("arraySchema")
                .description("an array model");
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setAnnotationLibrary(AnnotationLibrary.SWAGGER1);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "LinkedHashSet<Children>");
        Assertions.assertEquals(cm.imports.size(), 4);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "Set", "LinkedHashSet", "Children")).size(), 4);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/components/schemas/Children"));
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setAnnotationLibrary(AnnotationLibrary.SWAGGER1);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "HashMap<String, Children>");
        Assertions.assertEquals(cm.imports.size(), 4);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "Map", "HashMap", "Children")).size(), 4);
    }

    @Test(description = "convert a model with upper-case property names")
    public void upperCaseNamesTest() {
        final Schema schema = new Schema()
                .description("a model with upper-case property names")
                .addProperties("NAME", new StringSchema())
                .addRequiredItem("NAME");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "NAME");
        Assertions.assertEquals(property.getter, "getNAME");
        Assertions.assertEquals(property.setter, "setNAME");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "NAME");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertTrue(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model with upper-case property names and Numbers")
    public void upperCaseNamesNumbersTest() {
        final Schema schema = new Schema()
                .description("a model with upper-case property names and numbers")
                .addProperties("NAME1", new StringSchema())
                .addRequiredItem("NAME1");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "NAME1");
        Assertions.assertEquals(property.getter, "getNAME1");
        Assertions.assertEquals(property.setter, "setNAME1");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "NAME1");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertTrue(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model with a 2nd char upper-case property names")
    public void secondCharUpperCaseNamesTest() {
        final Schema schema = new Schema()
                .description("a model with a 2nd char upper-case property names")
                .addProperties("pId", new StringSchema())
                .addRequiredItem("pId");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "pId");
        Assertions.assertEquals(property.getter, "getpId");
        Assertions.assertEquals(property.setter, "setpId");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "pId");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertTrue(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model starting with two upper-case letter property names")
    public void firstTwoUpperCaseLetterNamesTest() {
        final Schema schema = new Schema()
                .description("a model with a property name starting with two upper-case letters")
                .addProperties("ATTName", new StringSchema())
                .addRequiredItem("ATTName");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "ATTName");
        Assertions.assertEquals(property.getter, "getAtTName");
        Assertions.assertEquals(property.setter, "setAtTName");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "atTName");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertTrue(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a model with an all upper-case letter and one non letter property names")
    public void allUpperCaseOneNonLetterNamesTest() {
        final Schema schema = new Schema()
                .description("a model with a property name starting with two upper-case letters")
                .addProperties("ATT_NAME", new StringSchema())
                .addRequiredItem("ATT_NAME");
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "ATT_NAME");
        Assertions.assertEquals(property.getter, "getATTNAME");
        Assertions.assertEquals(property.setter, "setATTNAME");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "ATT_NAME");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertTrue(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert hyphens per issue 503")
    public void hyphensTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("created-at", new DateTimeSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "created-at");
        Assertions.assertEquals(property.getter, "getCreatedAt");
        Assertions.assertEquals(property.setter, "setCreatedAt");
        Assertions.assertEquals(property.name, "createdAt");
    }

    @Test(description = "convert query[password] to queryPassword")
    public void squareBracketsTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("query[password]", new StringSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "query[password]");
        Assertions.assertEquals(property.getter, "getQueryPassword");
        Assertions.assertEquals(property.setter, "setQueryPassword");
        Assertions.assertEquals(property.name, "queryPassword");
    }

    @Test(description = "properly escape names per 567")
    public void escapeNamesTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("created-at", new DateTimeSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("with.dots", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("with.dots", schema);

        Assertions.assertEquals(cm.classname, "WithDots");
    }

    @Test(description = "convert a model with binary data")
    public void binaryDataTest() {
        final Schema schema = new Schema()
                .description("model with binary")
                .addProperties("inputBinaryData", new ByteArraySchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "inputBinaryData");
        Assertions.assertEquals(property.getter, "getInputBinaryData");
        Assertions.assertEquals(property.setter, "setInputBinaryData");
        Assertions.assertEquals(property.dataType, "byte[]");
        Assertions.assertEquals(property.name, "inputBinaryData");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "byte[]");
        Assertions.assertFalse(property.required);
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "translate an invalid param name")
    public void invalidParamNameTest() {
        final Schema schema = new Schema()
                .description("a model with a 2nd char upper-case property names")
                .addProperties("_", new StringSchema());
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, "_");
        Assertions.assertEquals(property.getter, "getU");
        Assertions.assertEquals(property.setter, "setU");
        Assertions.assertEquals(property.dataType, "String");
        Assertions.assertEquals(property.name, "u");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "String");
        Assertions.assertFalse(property.isContainer);
    }

    @Test(description = "convert a parameter")
    public void convertParameterTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Parameter parameter = new QueryParameter()
                .description("this is a description")
                .name("limit")
                .required(true);
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenParameter cm = codegen.fromParameter(parameter, null);

        Assertions.assertNull(cm.allowableValues);
        Assertions.assertEquals(cm.description, "this is a description");
    }

    @Test(description = "types used by inner properties should be imported")
    public void mapWithAnListOfBigDecimalTest() {
        Schema decimal = new StringSchema();
        decimal.setFormat("number");

        Schema schema1 = new Schema()
                .description("model with Map<String, List<BigDecimal>>")
                .addProperties("map", new MapSchema()
                        .additionalProperties(new ArraySchema().items(decimal)));
        OpenAPI openAPI1 = TestUtils.createOpenAPIWithOneSchema("sample", schema1);
        JavaClientCodegen codegen1 = new JavaClientCodegen();
        codegen1.setOpenAPI(openAPI1);
        final CodegenModel cm1 = codegen1.fromModel("sample", schema1);
        Assertions.assertEquals(cm1.vars.get(0).dataType, "Map<String, List<BigDecimal>>");
        Assertions.assertTrue(cm1.imports.contains("BigDecimal"));

        Schema schema2 = new Schema()
                .description("model with Map<String, Map<String, List<BigDecimal>>>")
                .addProperties("map", new MapSchema()
                        .additionalProperties(new MapSchema()
                                .additionalProperties(new ArraySchema().items(decimal))));
        OpenAPI openAPI2 = TestUtils.createOpenAPIWithOneSchema("sample", schema2);
        JavaClientCodegen codegen2 = new JavaClientCodegen();
        codegen2.setOpenAPI(openAPI2);
        final CodegenModel cm2 = codegen2.fromModel("sample", schema2);
        Assertions.assertEquals(cm2.vars.get(0).dataType, "Map<String, Map<String, List<BigDecimal>>>");
        Assertions.assertTrue(cm2.imports.contains("BigDecimal"));
    }

    @DataProvider(name = "modelNames")
    public static Object[][] primeNumbers() {
        return new Object[][]{
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, schema);

        Assertions.assertEquals(cm.name, name);
        Assertions.assertEquals(cm.classname, expectedName);
    }

    @DataProvider(name = "classProperties")
    public static Object[][] classProperties() {
        return new Object[][]{
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property = cm.vars.get(0);
        Assertions.assertEquals(property.baseName, baseName);
        Assertions.assertEquals(property.getter, getter);
        Assertions.assertEquals(property.setter, setter);
        Assertions.assertEquals(property.name, name);
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.xmlPrefix, "my");
        Assertions.assertEquals(cm.xmlName, "customXmlName");
        Assertions.assertEquals(cm.xmlNamespace, "xmlNamespace");
        Assertions.assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property2 = vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.getter, "getName");
        Assertions.assertEquals(property2.setter, "setName");
        Assertions.assertEquals(property2.dataType, "String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "String");
        Assertions.assertEquals(property2.example, "Tony");
        Assertions.assertTrue(property2.required);
        Assertions.assertFalse(property2.isContainer);
        Assertions.assertTrue(property2.isXmlAttribute);
        Assertions.assertEquals(property2.xmlName, "myName");
        Assertions.assertNull(property2.xmlNamespace);

        final CodegenProperty property3 = vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.getter, "getCreatedAt");
        Assertions.assertEquals(property3.setter, "setCreatedAt");
        Assertions.assertEquals(property3.dataType, "Date");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "Date");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
        Assertions.assertFalse(property3.isXmlAttribute);
        Assertions.assertEquals(property3.xmlName, "myCreatedAt");
        Assertions.assertEquals(property3.xmlNamespace, "myNamespace");
        Assertions.assertEquals(property3.xmlPrefix, "my");
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.xmlPrefix, "my");
        Assertions.assertEquals(cm.xmlName, "customXmlName");
        Assertions.assertEquals(cm.xmlNamespace, "xmlNamespace");
        Assertions.assertEquals(cm.vars.size(), 2);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property2 = vars.get(1);
        Assertions.assertEquals(property2.baseName, "array");
        Assertions.assertEquals(property2.getter, "getArray");
        Assertions.assertEquals(property2.setter, "setArray");
        Assertions.assertEquals(property2.dataType, "List<String>");
        Assertions.assertEquals(property2.name, "array");
        Assertions.assertEquals(property2.defaultValue, "new ArrayList<>()");
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertTrue(property2.isContainer);
        Assertions.assertTrue(property2.isXmlWrapped);
        Assertions.assertEquals(property2.xmlName, "xmlArray");
        Assertions.assertNotNull(property2.xmlNamespace);
        Assertions.assertNotNull(property2.items);
        CodegenProperty items = property2.items;
        Assertions.assertEquals(items.xmlName, "i");
        Assertions.assertEquals(items.baseName, "array");
    }

    @Test(description = "convert a boolean parameter")
    public void booleanPropertyTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final BooleanSchema property = new BooleanSchema();
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setBooleanGetterPrefix("is");
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assertions.assertEquals(cp.baseName, "property");
        Assertions.assertEquals(cp.dataType, "Boolean");
        Assertions.assertEquals(cp.name, "property");
        Assertions.assertEquals(cp.baseType, "Boolean");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertTrue(cp.isBoolean);
        Assertions.assertEquals(cp.getter, "isProperty");
    }

    @Test(description = "convert an integer property")
    public void integerPropertyTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final IntegerSchema property = new IntegerSchema();
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assertions.assertEquals(cp.baseName, "property");
        Assertions.assertEquals(cp.dataType, "Integer");
        Assertions.assertEquals(cp.name, "property");
        Assertions.assertEquals(cp.baseType, "Integer");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertTrue(cp.isInteger);
        Assertions.assertFalse(cp.isLong);
        Assertions.assertEquals(cp.getter, "getProperty");
    }

    @Test(description = "convert a long property")
    public void longPropertyTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final IntegerSchema property = new IntegerSchema().format("int64");
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assertions.assertEquals(cp.baseName, "property");
        Assertions.assertEquals(cp.nameInPascalCase, "Property");
        Assertions.assertEquals(cp.nameInCamelCase, "property");
        Assertions.assertEquals(cp.nameInSnakeCase, "PROPERTY");
        Assertions.assertEquals(cp.dataType, "Long");
        Assertions.assertEquals(cp.name, "property");
        Assertions.assertEquals(cp.baseType, "Long");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertTrue(cp.isLong);
        Assertions.assertFalse(cp.isInteger);
        Assertions.assertEquals(cp.getter, "getProperty");
    }

    @Test(description = "convert an integer property in a referenced schema")
    public void integerPropertyInReferencedSchemaTest() {
        final IntegerSchema longProperty = new IntegerSchema().format("int32");
        final Schema testSchema = new ObjectSchema()
                .addProperties("Integer1", new Schema<>().$ref("#/components/schemas/IntegerProperty"))
                .addProperties("Integer2", new IntegerSchema().format("int32"));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("IntegerProperty", longProperty);
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("test", testSchema);

        Assertions.assertEquals(cm.vars.size(), 2);

        CodegenProperty cp1 = cm.vars.get(0);
        Assertions.assertEquals(cp1.baseName, "Integer1");
        Assertions.assertEquals(cp1.nameInPascalCase, "Integer1");
        Assertions.assertEquals(cp1.nameInCamelCase, "integer1");
        Assertions.assertEquals(cp1.nameInSnakeCase, "INTEGER1");
        Assertions.assertEquals(cp1.dataType, "Integer");
        Assertions.assertEquals(cp1.name, "integer1");
        Assertions.assertEquals(cp1.baseType, "Integer");
        Assertions.assertEquals(cp1.getter, "getInteger1");

        CodegenProperty cp2 = cm.vars.get(1);
        Assertions.assertEquals(cp2.baseName, "Integer2");
        Assertions.assertEquals(cp2.nameInPascalCase, "Integer2");
        Assertions.assertEquals(cp2.nameInCamelCase, "integer2");
        Assertions.assertEquals(cp2.nameInSnakeCase, "INTEGER2");
        Assertions.assertEquals(cp2.dataType, "Integer");
        Assertions.assertEquals(cp2.name, "integer2");
        Assertions.assertEquals(cp2.baseType, "Integer");
        Assertions.assertEquals(cp2.getter, "getInteger2");
    }

    @Test(description = "convert a long property in a referenced schema")
    public void longPropertyInReferencedSchemaTest() {
        final IntegerSchema longProperty = new IntegerSchema().format("int64");
        final Schema TestSchema = new ObjectSchema()
                .addProperties("Long1", new Schema<>().$ref("#/components/schemas/LongProperty"))
                .addProperties("Long2", new IntegerSchema().format("int64"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("LongProperty", longProperty);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("test", TestSchema);

        Assertions.assertEquals(cm.vars.size(), 2);

        CodegenProperty cp1 = cm.vars.get(0);
        Assertions.assertEquals(cp1.baseName, "Long1");
        Assertions.assertEquals(cp1.dataType, "Long");
        Assertions.assertEquals(cp1.name, "long1");
        Assertions.assertEquals(cp1.baseType, "Long");
        Assertions.assertEquals(cp1.getter, "getLong1");

        CodegenProperty cp2 = cm.vars.get(1);
        Assertions.assertEquals(cp2.baseName, "Long2");
        Assertions.assertEquals(cp2.dataType, "Long");
        Assertions.assertEquals(cp2.name, "long2");
        Assertions.assertEquals(cp2.baseType, "Long");
        Assertions.assertEquals(cp2.getter, "getLong2");
    }

    @Test(description = "convert string property")
    public void stringPropertyTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenProperty cp = codegen.fromProperty("somePropertyWithMinMaxAndPattern", property);

        Assertions.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInPascalCase, "SomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInCamelCase, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assertions.assertEquals(cp.dataType, "String");
        Assertions.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.baseType, "String");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertFalse(cp.isLong);
        Assertions.assertFalse(cp.isInteger);
        Assertions.assertTrue(cp.isString);
        Assertions.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.minLength, Integer.valueOf(3));
        Assertions.assertEquals(cp.maxLength, Integer.valueOf(10));
        Assertions.assertEquals(cp.pattern, "^[A-Z]+$");
    }
    
    @Test(description = "convert string property with password format")
    public void stringPropertyPasswordFormatTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema property = new StringSchema().format("password");
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        
        final CodegenProperty cp = codegen.fromProperty("somePropertyWithPasswordFormat", property);
        Assertions.assertEquals(cp.isPassword, true);
    }

    @Test(description = "convert string property in an object")
    public void stringPropertyInObjectTest() {
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final Schema myObject = new ObjectSchema().addProperties("somePropertyWithMinMaxAndPattern", property);

        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("myObject", myObject);
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("myObject", myObject);

        Assertions.assertEquals(cm.getVars().size(), 1);
        CodegenProperty cp = cm.getVars().get(0);
        Assertions.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInPascalCase, "SomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInCamelCase, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assertions.assertEquals(cp.dataType, "String");
        Assertions.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.baseType, "String");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertFalse(cp.isLong);
        Assertions.assertFalse(cp.isInteger);
        Assertions.assertTrue(cp.isString);
        Assertions.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.minLength, Integer.valueOf(3));
        Assertions.assertEquals(cp.maxLength, Integer.valueOf(10));
        Assertions.assertEquals(cp.pattern, "^[A-Z]+$");
    }

    @Test(description = "convert referenced string property in an object")
    public void stringPropertyReferencedInObjectTest() {
        final Schema property = new StringSchema().maxLength(10).minLength(3).pattern("^[A-Z]+$");
        final Schema myObject = new ObjectSchema().addProperties("somePropertyWithMinMaxAndPattern", new ObjectSchema().$ref("refObj"));

        final DefaultCodegen codegen = new JavaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.setComponents(new Components()
                .addSchemas("myObject", myObject)
                .addSchemas("refObj", property)
        );
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("myObject", myObject);

        Assertions.assertEquals(cm.getVars().size(), 1);
        CodegenProperty cp = cm.getVars().get(0);
        Assertions.assertEquals(cp.baseName, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInPascalCase, "SomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInCamelCase, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.nameInSnakeCase, "SOME_PROPERTY_WITH_MIN_MAX_AND_PATTERN");
        Assertions.assertEquals(cp.dataType, "String");
        Assertions.assertEquals(cp.name, "somePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.baseType, "String");
        Assertions.assertFalse(cp.isContainer);
        Assertions.assertFalse(cp.isLong);
        Assertions.assertFalse(cp.isInteger);
        Assertions.assertTrue(cp.isString);
        Assertions.assertEquals(cp.getter, "getSomePropertyWithMinMaxAndPattern");
        Assertions.assertEquals(cp.minLength, Integer.valueOf(3));
        Assertions.assertEquals(cp.maxLength, Integer.valueOf(10));
        Assertions.assertEquals(cp.pattern, "^[A-Z]+$");
    }

    @Test(description = "convert an array schema")
    public void arraySchemaTest() {
        final Schema testSchema = new ObjectSchema()
                .addProperties("pets", new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet")));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("test", testSchema);

        Assertions.assertEquals(cm.vars.size(), 1);
        CodegenProperty cp1 = cm.vars.get(0);
        Assertions.assertEquals(cp1.baseName, "pets");
        Assertions.assertEquals(cp1.dataType, "List<Pet>");
        Assertions.assertEquals(cp1.name, "pets");
        Assertions.assertEquals(cp1.baseType, "List");
        Assertions.assertTrue(cp1.isContainer);
        Assertions.assertTrue(cp1.isArray);
        Assertions.assertFalse(cp1.isMap);
        Assertions.assertEquals(cp1.getter, "getPets");
        Assertions.assertEquals(cp1.items.baseType, "Pet");

        Assertions.assertTrue(cm.imports.contains("List"));
        Assertions.assertTrue(cm.imports.contains("Pet"));
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, null);

        Assertions.assertEquals(co.bodyParams.size(), 1);
        CodegenParameter cp1 = co.bodyParams.get(0);
        Assertions.assertEquals(cp1.baseType, "Pet");
        Assertions.assertEquals(cp1.dataType, "List<Pet>");
        Assertions.assertTrue(cp1.isContainer);
        Assertions.assertTrue(cp1.isArray);
        Assertions.assertFalse(cp1.isMap);
        Assertions.assertEquals(cp1.items.baseType, "Pet");
        Assertions.assertEquals(cp1.items.complexType, "Pet");
        Assertions.assertEquals(cp1.items.dataType, "Pet");

        Assertions.assertEquals(co.responses.size(), 1);

        Assertions.assertTrue(co.imports.contains("List"));
        Assertions.assertTrue(co.imports.contains("Pet"));
    }

    @Test(description = "convert an array schema in an ApiResponse")
    public void arraySchemaTestInOperationResponse() {
        final Schema testSchema = new ArraySchema()
                .items(new Schema<>().$ref("#/components/schemas/Pet"));
        Operation operation = new Operation().responses(
                new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("Ok response")
                        .content(new Content().addMediaType("application/json",
                                new MediaType().schema(testSchema)))));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, null);

        Assertions.assertEquals(co.responses.size(), 1);
        CodegenResponse cr = co.responses.get(0);
        Assertions.assertEquals(cr.baseType, "Pet");
        Assertions.assertEquals(cr.dataType, "List<Pet>");
        Assertions.assertEquals(cr.containerType, "array");

        Assertions.assertTrue(co.imports.contains("Pet"));
    }

    @Test(description = "convert an array of array schema")
    public void arrayOfArraySchemaTest() {
        final Schema testSchema = new ObjectSchema()
                .addProperties("pets", new ArraySchema()
                        .items(new ArraySchema()
                                .items(new Schema<>().$ref("#/components/schemas/Pet"))));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("test", testSchema);

        Assertions.assertEquals(cm.vars.size(), 1);
        CodegenProperty cp1 = cm.vars.get(0);
        Assertions.assertEquals(cp1.baseName, "pets");
        Assertions.assertEquals(cp1.dataType, "List<List<Pet>>");
        Assertions.assertEquals(cp1.name, "pets");
        Assertions.assertEquals(cp1.baseType, "List");
        Assertions.assertEquals(cp1.getter, "getPets");

        Assertions.assertTrue(cm.imports.contains("List"));
        Assertions.assertTrue(cm.imports.contains("Pet"));
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
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, null);

        Assertions.assertEquals(co.bodyParams.size(), 1);
        CodegenParameter cp1 = co.bodyParams.get(0);
        Assertions.assertEquals(cp1.baseType, "List");
        Assertions.assertEquals(cp1.dataType, "List<List<Pet>>");
        Assertions.assertTrue(cp1.isContainer);
        Assertions.assertTrue(cp1.isArray);
        Assertions.assertFalse(cp1.isMap);
        Assertions.assertEquals(cp1.items.baseType, "List");
        Assertions.assertEquals(cp1.items.complexType, "Pet");
        Assertions.assertEquals(cp1.items.dataType, "List<Pet>");
        Assertions.assertEquals(cp1.items.items.baseType, "Pet");
        Assertions.assertEquals(cp1.items.items.complexType, "Pet");
        Assertions.assertEquals(cp1.items.items.dataType, "Pet");

        Assertions.assertEquals(co.responses.size(), 1);

        Assertions.assertTrue(co.imports.contains("Pet"));
        Assertions.assertTrue(co.imports.contains("List"));
    }

    @Test(description = "convert an array schema in an ApiResponse")
    public void arrayOfArraySchemaTestInOperationResponse() {
        final Schema testSchema = new ArraySchema()
                .items(new ArraySchema()
                        .items(new Schema<>().$ref("#/components/schemas/Pet")));
        Operation operation = new Operation().responses(
                new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("Ok response")
                        .content(new Content().addMediaType("application/json",
                                new MediaType().schema(testSchema)))));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema().addProperties("name", new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenOperation co = codegen.fromOperation("testSchema", "GET", operation, null);

        Assertions.assertEquals(co.responses.size(), 1);
        CodegenResponse cr = co.responses.get(0);
        Assertions.assertEquals(cr.baseType, "Pet");
        Assertions.assertEquals(cr.dataType, "List<List<Pet>>");
        Assertions.assertEquals(cr.containerType, "array");

        Assertions.assertTrue(co.imports.contains("Pet"));
    }

    @Test
    public void generateModel() throws Exception {
        String inputSpec = "src/test/resources/3_0/petstore.json";

        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        Assertions.assertTrue(new File(inputSpec).exists());

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
        Assertions.assertTrue(orderFile.exists());
    }

    @Test
    public void generateEmpty() throws Exception {
        String inputSpec = "src/test/resources/3_0/ping.yaml";

        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        Assertions.assertTrue(new File(inputSpec).exists());

        JavaClientCodegen config = new org.openapitools.codegen.languages.JavaClientCodegen();
        config.setHideGenerationTimestamp(true);
        config.setOutputDir(output.getAbsolutePath());

        final OpenAPI openAPI = TestUtils.parseFlattenSpec(inputSpec);

        final ClientOptInput opts = new ClientOptInput();
        opts.config(config);
        opts.openAPI(openAPI);
        new DefaultGenerator().opts(opts).generate();

        File orderFile = new File(output, "src/main/java/org/openapitools/client/api/DefaultApi.java");
        Assertions.assertTrue(orderFile.exists());
    }
}
