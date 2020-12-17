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

package org.openapitools.codegen.go;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.GoDeprecatedClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class GoModelTest {

    @Test(description = "convert a simple Go model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);
        Assert.assertEquals(cm.imports.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "string");
        Assert.assertEquals(property2.name, "Name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "time.Time");
        Assert.assertEquals(property3.dataType, "time.Time");
        Assert.assertEquals(property3.name, "CreatedAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "time.Time");
        Assert.assertFalse(property3.required);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "[]string");
        Assert.assertEquals(property2.name, "Urls");
        Assert.assertEquals(property2.baseType, "array");
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.dataType, "map[string]string");
        Assert.assertEquals(property1.name, "Translations");
        Assert.assertEquals(property1.baseType, "map");
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
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
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
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "[]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "array");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "map[string]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .additionalProperties(new Schema().$ref("#/definitions/Children"))
                .description("a map model");
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert file type and file schema models")
    public void filePropertyTest() {
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        final Schema model1 = new Schema().type("file");
        Assert.assertEquals(codegen.getSchemaType(model1), "*os.File");
        Assert.assertEquals(codegen.getTypeDeclaration(model1), "*os.File");

        final Schema model2 = new Schema().$ref("#/definitions/File");
        Assert.assertEquals(codegen.getSchemaType(model2), "File");
        Assert.assertEquals(codegen.getTypeDeclaration(model2), "File");

        final Schema model3 = new Schema().$ref("#/components/schemas/File");
        Assert.assertEquals(codegen.getSchemaType(model3), "File");
        Assert.assertEquals(codegen.getTypeDeclaration(model3), "File");
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
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Schema model = new Schema();
        final DefaultCodegen codegen = new GoDeprecatedClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }
}
