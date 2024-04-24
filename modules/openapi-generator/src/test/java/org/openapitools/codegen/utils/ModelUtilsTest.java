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

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.*;

public class ModelUtilsTest {

    @Test
    public void testGetAllUsedSchemas() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> allUsedSchemas = ModelUtils.getAllUsedSchemas(openAPI);
        List<String> expectedAllUsedSchemas = Arrays.asList(
                "SomeObj1",
                "SomeObj2",
                "SomeObj3",
                "SomeObjShared",
                "SomeObj6",
                "SomeObj7",
                "SomeObj8",
                "SomeObj9A",
                "SomeObj9B",
                "SomeObj10A",
                "SomeObj10B",
                "SomeObj11",
                "SomeArrayObj12",
                "ArrayItem12",
                "SomeArrayObj13",
                "ArrayItem13",
                "SomeObj14",
                "PropertyObj14",
                "SomeObj15",
                "SomeMapObj16",
                "MapItem16",
                "p17_200_response",
                "SomeObj17",
                "SomeObj18",
                "Common18",
                "_some_p19_patch_request",
                "Obj19ByAge",
                "Obj19ByType",
                "SomeObj20",
                "OtherObj20",
                "PingDataInput21",
                "PingDataOutput21",
                "SInput22",
                "SOutput22",
                "SomeHeader23",
                "SomeHeader24",
                "SomeObj25",
                "SomeObj26",
                "Param27",
                "Param28",
                "Parent30",
                "AChild30",
                "BChild30"
        );
        Assertions.assertEquals(allUsedSchemas, expectedAllUsedSchemas);
        Assertions.assertTrue(allUsedSchemas.containsAll(expectedAllUsedSchemas));
    }

    @Test
    public void testGetUnusedSchemas() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getUnusedSchemas(openAPI);
        List<String> expectedUnusedSchemas = Arrays.asList(
                "UnusedObj1",
                "UnusedObj2",
                "UnusedObj3",
                "UnusedObj4",
                "Parent29",
                "AChild29",
                "BChild29"
        );
        Assertions.assertEquals(unusedSchemas.size(), expectedUnusedSchemas.size());
        Assertions.assertTrue(unusedSchemas.containsAll(expectedUnusedSchemas));
    }

    @Test
    public void testSchemasUsedOnlyInFormParam() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assertions.assertEquals(unusedSchemas.size(), 3);
        //SomeObj2 is only used in an 'application/x-www-form-urlencoded' request
        Assertions.assertTrue(unusedSchemas.contains("SomeObj2"), "contains 'SomeObj2'");
        //SomeObj3 is only used in a 'multipart/form-data' request
        Assertions.assertTrue(unusedSchemas.contains("SomeObj3"), "contains 'SomeObj3'");
        //SomeObj7 is only used in an 'application/x-www-form-urlencoded' request (with referenced request body)
        Assertions.assertTrue(unusedSchemas.contains("SomeObj7"), "contains 'SomeObj7'");
    }

    @Test
    public void testNestedFormParameter() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/nestedFormParameter.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assertions.assertEquals(unusedSchemas.size(), 1);
        Assertions.assertTrue(unusedSchemas.contains("OuterObject"), "contains 'OuterObject'");
    }

    @Test
    public void testNoComponentsSection() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assertions.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testGlobalProducesConsumes() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/globalProducesConsumesTest.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assertions.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testIsModelAllowsEmptyBaseModel() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/emptyBaseModel.yaml");
        Schema commandSchema = ModelUtils.getSchema(openAPI, "Command");

        Assertions.assertTrue(ModelUtils.isModel(commandSchema));
        Assertions.assertFalse(ModelUtils.isFreeFormObject(commandSchema));
    }

    @Test
    public void testReferencedSchema() {
        Schema otherObj = new ObjectSchema().addProperties("sprop", new StringSchema()).addProperties("iprop", new IntegerSchema());

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas("OtherObj", otherObj);

        Schema notExistingReferencedSchema = new Schema().$ref("NotExisting");
        Schema result1 = ModelUtils.getReferencedSchema(openAPI, notExistingReferencedSchema);
        Assertions.assertEquals(result1, notExistingReferencedSchema);

        Schema result2 = ModelUtils.getReferencedSchema(openAPI, new Schema().$ref("#/components/schemas/OtherObj"));
        Assertions.assertEquals(result2, otherObj);
    }

    @Test
    public void testReferencedRequestBody() {
        RequestBody otherRequestBody = new RequestBody().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addRequestBodies("OtherRequestBody", otherRequestBody);

        RequestBody notExistingRequestBody = new RequestBody().$ref("NotExisting");
        RequestBody result1 = ModelUtils.getReferencedRequestBody(openAPI, notExistingRequestBody);
        Assertions.assertEquals(result1, notExistingRequestBody);

        RequestBody result2 = ModelUtils.getReferencedRequestBody(openAPI, new RequestBody().$ref("#/components/requestBodies/OtherRequestBody"));
        Assertions.assertEquals(result2, otherRequestBody);
    }

    @Test
    public void testReferencedApiResponse() {
        ApiResponse otherApiResponse = new ApiResponse().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addResponses("OtherApiResponse", otherApiResponse);

        ApiResponse notExistingApiResponse = new ApiResponse().$ref("NotExisting");
        ApiResponse result1 = ModelUtils.getReferencedApiResponse(openAPI, notExistingApiResponse);
        Assertions.assertEquals(result1, notExistingApiResponse);

        ApiResponse result2 = ModelUtils.getReferencedApiResponse(openAPI, new ApiResponse().$ref("#/components/responses/OtherApiResponse"));
        Assertions.assertEquals(result2, otherApiResponse);
    }

    @Test
    public void testReferencedParameter() {
        Parameter otherParameter = new Parameter().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addParameters("OtherParameter", otherParameter);

        Parameter notExistingParameter = new Parameter().$ref("NotExisting");
        Parameter result1 = ModelUtils.getReferencedParameter(openAPI, notExistingParameter);
        Assertions.assertEquals(result1, notExistingParameter);

        Parameter result2 = ModelUtils.getReferencedParameter(openAPI, new Parameter().$ref("#/components/parameters/OtherParameter"));
        Assertions.assertEquals(result2, otherParameter);
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/582.
     * Composed schemas should not get unaliased when generating model properties, in order to properly
     * generate the property data type name.
     */
    @Test
    public void testComposedSchemasAreNotUnaliased() {
        ComposedSchema composedSchema = new ComposedSchema();
        composedSchema.allOf(Arrays.asList(
                new Schema<>().$ref("#/components/schemas/SomeSchema"),
                new ObjectSchema()
        ));
        Schema refToComposedSchema = new Schema().$ref("#/components/schemas/SomeComposedSchema");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("SomeComposedSchema", composedSchema);

        Assertions.assertEquals(refToComposedSchema, ModelUtils.unaliasSchema(openAPI, refToComposedSchema, new HashMap<>()));
    }

    @Test
    public void testAliasedTypeIsNotUnaliasedIfUsedForImportMapping() {
        Schema emailSchema = new Schema().$ref("#/components/schemas/Email").type("string");
        StringSchema stringSchema = new StringSchema();
        HashMap<String, String> schemaMappings = new HashMap<>();
        schemaMappings.put("Email", "foo.bar.Email");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Email", stringSchema);

        Assertions.assertEquals(emailSchema, ModelUtils.unaliasSchema(openAPI, emailSchema, schemaMappings));
        Assertions.assertEquals(stringSchema, ModelUtils.unaliasSchema(openAPI, emailSchema, new HashMap<>()));
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/1624.
     * ModelUtils.isFreeFormObject() should not throw an NPE when passed an empty
     * object schema that has additionalProperties defined as an empty object schema.
     */
    @Test
    public void testIsFreeFormObject() {
        OpenAPI openAPI = new OpenAPI().openapi("3.0.0");
        // Create initial "empty" object schema.
        ObjectSchema objSchema = new ObjectSchema();
        Assertions.assertTrue(ModelUtils.isFreeFormObject(objSchema));

        // Set additionalProperties to an empty ObjectSchema.
        objSchema.setAdditionalProperties(new ObjectSchema());
        Assertions.assertTrue(ModelUtils.isFreeFormObject(objSchema));

        // Add a single property to the schema (no longer a free-form object).
        Map<String, Schema> props = new HashMap<>();
        props.put("prop1", new StringSchema());
        objSchema.setProperties(props);
        Assertions.assertFalse(ModelUtils.isFreeFormObject(objSchema));

        // Test a non-object schema
        Assertions.assertFalse(ModelUtils.isFreeFormObject(new StringSchema()));

        // Test a null schema
        Assertions.assertFalse(ModelUtils.isFreeFormObject(null));
    }

    @Test
    public void testIsSetForValidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(true);

        Assertions.assertTrue(ModelUtils.isSet(as));
    }

    @Test
    public void testIsSetFalseForInvalidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(false);

        Assertions.assertFalse(ModelUtils.isSet(as));
    }

    @Test
    public void testIsSetFailsForNullSchema() {
        ArraySchema as = null;
        Assertions.assertFalse(ModelUtils.isSet(as));
    }

    @Test
    public void testSimpleRefDecoding() {
        String decoded = ModelUtils.getSimpleRef("#/components/~01%20Hallo~1Welt");
        Assertions.assertEquals(decoded, "~1 Hallo/Welt");
    }

    @Test
    public void testRefToSchemaProperties() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");

        Schema category = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/category");
        Assertions.assertEquals(category.get$ref(), "#/components/schemas/Category");

        Schema name = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/name");
        Assertions.assertEquals(name.getType(), "string");

        Schema id = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/id");
        Assertions.assertEquals(id.getType(), "integer");
        Assertions.assertEquals(id.getFormat(), "int64");

        Assertions.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/prop/category"));
        Assertions.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/categoryyyy"));
        Assertions.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet"));
    }


    // 3.0 spec tests
    @Test
    public void test30Schemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/schema.yaml");
        Schema misc = ModelUtils.getSchema(openAPI, "Misc");

        // test map
        Assertions.assertTrue(ModelUtils.isMapSchema((Schema) misc.getProperties().get("map1")));

        // test free form object
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_1")));
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_2")));
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_3")));

        // test oneOf
        Assertions.assertTrue(ModelUtils.isOneOf((Schema) misc.getProperties().get("oneof1")));

        // test anyOf model
        Schema anyof1 = ModelUtils.getSchema(openAPI, "anyof1");
        Assertions.assertNotNull(anyof1);
        Assertions.assertNull(anyof1.getTypes());
        Assertions.assertNull(anyof1.getType());
        Assertions.assertTrue(ModelUtils.hasAnyOf(anyof1));
        Assertions.assertTrue(ModelUtils.isAnyOf(anyof1));

        // test anyOf in properties
        Schema anyof1Property = (Schema) misc.getProperties().get("anyof1");
        Assertions.assertNotNull(anyof1Property);
        Assertions.assertNull(anyof1Property.getTypes());
        Assertions.assertNull(anyof1Property.getType());
        Assertions.assertTrue(ModelUtils.hasAnyOf(anyof1Property));
        Assertions.assertTrue(ModelUtils.isAnyOf(anyof1Property));
    }

    // 3.1 spec tests
    @Test
    public void test31Schemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/schema.yaml");
        Schema misc = ModelUtils.getSchema(openAPI, "Misc");

        // test map
        Assertions.assertTrue(ModelUtils.isMapSchema((Schema) misc.getProperties().get("map1")));

        // test free form object
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_1")));
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_2")));
        Assertions.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_3")));

        // test oneOf property
        Assertions.assertTrue(ModelUtils.isOneOf((Schema) misc.getProperties().get("oneof1")));

        // test anyOf property
        Schema anyof1 = (Schema) misc.getProperties().get("anyof1");
        Assertions.assertNotNull(anyof1);
        Assertions.assertNull(anyof1.getTypes());
        Assertions.assertNull(anyof1.getType());
        Assertions.assertNotNull(anyof1.getAnyOf());
        Assertions.assertFalse(anyof1.getAnyOf().isEmpty());
        Assertions.assertTrue(ModelUtils.hasAnyOf(anyof1));
        Assertions.assertTrue(ModelUtils.isAnyOf(anyof1));

        Schema anyof2 = (Schema) misc.getProperties().get("anyof2");
        Assertions.assertNotNull(anyof2);
        Assertions.assertNull(anyof2.getTypes());
        Assertions.assertNull(anyof2.getType());
        Assertions.assertNotNull(anyof2.getAnyOf());
        Assertions.assertFalse(anyof2.getAnyOf().isEmpty());
        Assertions.assertTrue(ModelUtils.hasAnyOf(anyof2));
        Assertions.assertTrue(ModelUtils.isAnyOf(anyof2));

        Schema objectSchema = ModelUtils.getSchema(openAPI, "ObjectSchema");
        Assertions.assertTrue(ModelUtils.isMapSchema(objectSchema));

        Schema complexComposedSchema = ModelUtils.getSchema(openAPI, "ComplexComposedSchema");
        Assertions.assertTrue(ModelUtils.isComplexComposedSchema(complexComposedSchema));
    }
}
