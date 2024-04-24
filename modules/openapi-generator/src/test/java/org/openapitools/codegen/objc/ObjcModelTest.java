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

package org.openapitools.codegen.objc;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.ObjcClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

@SuppressWarnings("static-method")
public class ObjcModelTest {

    @Test(description = "convert a model with an advanced map property")
    public void advancedMapPropertyTest() {
        final Schema model = new Schema()
        .description("a sample model")
        .addProperties("translations", new MapSchema()
                  .additionalProperties(new MapSchema().additionalProperties(new StringSchema())))
        .addRequiredItem("id");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "NSDictionary<NSString*, NSDictionary<NSString*, NSString*>*>*");
        Assertions.assertEquals(property1.name, "translations");
        Assertions.assertEquals(property1.baseType, "NSDictionary");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "NSNumber*");
        Assertions.assertEquals(property1.name, "_id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "NSNumber");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "NSString*");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "NSString");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "NSDate*");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "NSDate");
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
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "NSNumber*");
        Assertions.assertEquals(property1.name, "_id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "NSNumber");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "NSArray<NSString*>*");
        Assertions.assertEquals(property2.name, "urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "NSArray");
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
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "NSDictionary<NSString*, NSString*>*");
        Assertions.assertEquals(property1.name, "translations");
        Assertions.assertEquals(property1.baseType, "NSDictionary");
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
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "OAIChildren*");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "OAIChildren");
        Assertions.assertFalse(property1.required);
        Assertions.assertFalse(property1.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "OAIChildren");
        Assertions.assertEquals(property1.dataType, "NSArray<OAIChildren>*");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "NSArray");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("OAIChildren")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "OAIChildren");
        Assertions.assertEquals(property1.dataType, "NSDictionary<OAIChildren>*");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "NSDictionary");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "OAISample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "NSMutableArray");
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("OAIChildren")).size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model for testing ObjC generator")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("map_model", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("map_model", model);

        Assertions.assertEquals(cm.name, "map_model");
        Assertions.assertEquals(cm.classname, "OAIMapModel");
        Assertions.assertEquals(cm.description, "a map model for testing ObjC generator");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "NSMutableDictionary");
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("OAIChildren")).size(), 1);
    }

    @Test(description = "test uuid")
    public void uuidAndPasswordDataModelTest() {
        final OpenAPI openAPI =  TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Schema definition = openAPI.getComponents().getSchemas().get("format_test");

        Schema property = ((Map<String, Schema>) definition.getProperties()).get("uuid");
        CodegenProperty prope = codegen.fromProperty("uuid", property);
        Assertions.assertEquals(prope.baseType, "NSString");

        prope = codegen.fromProperty("password", property);
        Assertions.assertEquals(prope.baseType, "NSString");
    }

    @Test(description = "test mixedProperties")
    public void mixedPropertiesDataModelTest() {
        final OpenAPI openAPI =  TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Schema definition = openAPI.getComponents().getSchemas().get("MixedPropertiesAndAdditionalPropertiesClass");

        Schema property = ((Map<String, Schema>)definition.getProperties()).get("map");
        CodegenProperty prope = codegen.fromProperty("map", property);
        Assertions.assertEquals(prope.baseType, "NSDictionary");
    }

    @Test(description = "test isArray")
    public void isArrayModelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final Schema definition = openAPI.getComponents().getSchemas().get("AnimalFarm");
        codegen.setOpenAPI(openAPI);
        final CodegenModel codegenModel = codegen.fromModel("AnimalFarm", definition);

        Assertions.assertEquals(codegenModel.isArray, true);
        Assertions.assertEquals(codegenModel.arrayModelType,"OAIAnimal");
    }


    @Test(description = "test binary data")
    public void binaryDataModelTest() {
        final OpenAPI openAPI =  TestUtils.parseFlattenSpec("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final String path = "/tests/binaryResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        codegen.setOpenAPI(openAPI);
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assertions.assertTrue(op.bodyParam.isBinary);
        Assertions.assertTrue(op.responses.get(0).isBinary);
        Assertions.assertEquals(op.returnType, "NSURL*");
        Assertions.assertEquals(op.bodyParam.dataType, "NSURL*");
    }

    @Test(description = "create proper imports per #316")
    public void issue316Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/postBodyTest.json");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        codegen.setOpenAPI(openAPI);

        final Map<String, PathItem> animalPaths = openAPI.getPaths();

        final PathItem animalOps = animalPaths.get("/animals");
        Assertions.assertNotNull(animalOps.getPost());

        final CodegenOperation animalCo = codegen.fromOperation("/animals", "POST", animalOps.getPost(), null);
        Assertions.assertEquals(animalCo.imports.size(), 1);
        Assertions.assertTrue(animalCo.imports.contains("OAIAnimal"));

        final Map<String, PathItem> insectPaths = openAPI.getPaths();
        final PathItem insectOps = insectPaths.get("/insects");
        Assertions.assertNotNull(insectOps.getPost());

        final CodegenOperation insectCo = codegen.fromOperation("/insects", "POST", insectOps.getPost(), null);
        Assertions.assertEquals(insectCo.imports.size(), 1);
        Assertions.assertTrue(insectCo.imports.contains("OAIInsect"));
    }
}
