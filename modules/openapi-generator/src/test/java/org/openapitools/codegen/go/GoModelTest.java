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
import org.openapitools.codegen.languages.GoClientCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;

@SuppressWarnings("static-method")
public class GoModelTest {

    @Test(description = "convert a simple Go model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("name", new StringSchema())
                .addProperty("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);
        Assertions.assertEquals(cm.imports.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int64");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "int64");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "string");
        Assertions.assertEquals(property2.name, "Name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "string");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.complexType, "time.Time");
        Assertions.assertEquals(property3.dataType, "time.Time");
        Assertions.assertEquals(property3.name, "CreatedAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "time.Time");
        Assertions.assertFalse(property3.required);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "int64");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "int64");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "[]string");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertEquals(property2.baseType, "array");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "map[string]string");
        Assertions.assertEquals(property1.name, "Translations");
        Assertions.assertEquals(property1.baseType, "map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
        Assertions.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new GoClientCodegen();
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
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "Children");
        Assertions.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "[]Children");
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "array");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.dataType, "map[string]Children");
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.imports.size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .additionalProperties(new Schema().$ref("#/definitions/Children"))
                .description("a map model");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert file type and file schema models")
    public void filePropertyTest() {
        final DefaultCodegen codegen = new GoClientCodegen();
        final Schema model1 = new Schema().type("file");
        Assertions.assertEquals(codegen.getSchemaType(model1), "*os.File");
        Assertions.assertEquals(codegen.getTypeDeclaration(model1), "*os.File");

        final Schema model2 = new Schema().$ref("#/definitions/File");
        Assertions.assertEquals(codegen.getSchemaType(model2), "File");
        Assertions.assertEquals(codegen.getTypeDeclaration(model2), "File");

        final Schema model3 = new Schema().$ref("#/components/schemas/File");
        Assertions.assertEquals(codegen.getSchemaType(model3), "File");
        Assertions.assertEquals(codegen.getTypeDeclaration(model3), "File");
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
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        Assertions.assertEquals(cm.name, name);
        Assertions.assertEquals(cm.classname, expectedName);
    }

    @DataProvider(name = "modelMappedNames")
    public static Object[][] mappedNames() {
        return new Object[][] {
            {"mapped", "Remapped", "model_remapped.go"},
            {"mapped_underscore", "RemappedUnderscore", "model_remapped_underscore.go"},
        };
    }

    @Test(dataProvider = "modelMappedNames", description = "map model names")
    public void modelNameMappingsTest(String name, String expectedName, String expectedFilename) {
        final Schema model = new Schema();
        final DefaultCodegen codegen = new GoClientCodegen();
        codegen.modelNameMapping().put(name, expectedName);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        final String fn = codegen.modelFilename("model.mustache", name, "");
        Assertions.assertEquals(fn, File.separator + expectedFilename);

        Assertions.assertEquals(cm.name, name);
        Assertions.assertEquals(cm.classname, expectedName);
    }
}
