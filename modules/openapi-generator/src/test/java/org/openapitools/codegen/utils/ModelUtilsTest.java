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
        ModelUtils modelUtils = new ModelUtils(openAPI);
        List<String> allUsedSchemas = modelUtils.getAllUsedSchemas();
        List<String> expectedallUsedSchemas = Arrays.asList(
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
                "SomeObj17",
                "SomeObj18",
                "Common18",
                "SomeObj18_allOf",
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
        Assert.assertEquals(allUsedSchemas.size(), expectedallUsedSchemas.size());
        Assert.assertTrue(allUsedSchemas.containsAll(expectedallUsedSchemas));
    }

    @Test
    public void testGetUnusedSchemas() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        ModelUtils modelUtils = new ModelUtils(openAPI);
        List<String> unusedSchemas = modelUtils.getUnusedSchemas();
        List<String> expectedUnusedSchemas = Arrays.asList(
                "UnusedObj1",
                "UnusedObj2",
                "UnusedObj3",
                "UnusedObj4",
                "Parent29",
                "AChild29",
                "BChild29",
                "AChild29_allOf",
                "BChild29_allOf"
        );
        Assert.assertEquals(unusedSchemas.size(), expectedUnusedSchemas.size());
        Assert.assertTrue(unusedSchemas.containsAll(expectedUnusedSchemas));
    }

    @Test
    public void testSchemasUsedOnlyInFormParam() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        ModelUtils modelUtils = new ModelUtils(openAPI);
        List<String> unusedSchemas = modelUtils.getSchemasUsedOnlyInFormParam();
        Assert.assertEquals(unusedSchemas.size(), 3);
        //SomeObj2 is only used in an 'application/x-www-form-urlencoded' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj2"), "contains 'SomeObj2'");
        //SomeObj3 is only used in a 'multipart/form-data' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj3"), "contains 'SomeObj3'");
        //SomeObj7 is only used in an 'application/x-www-form-urlencoded' request (with referenced request body)
        Assert.assertTrue(unusedSchemas.contains("SomeObj7"), "contains 'SomeObj7'");
    }

    @Test
    public void testNoComponentsSection() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");
        ModelUtils modelUtils = new ModelUtils(openAPI);
        List<String> unusedSchemas = modelUtils.getSchemasUsedOnlyInFormParam();
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testGlobalProducesConsumes() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/globalProducesConsumesTest.yaml");
        ModelUtils modelUtils = new ModelUtils(openAPI);
        List<String> unusedSchemas = modelUtils.getSchemasUsedOnlyInFormParam();
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testIsModelAllowsEmptyBaseModel() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/emptyBaseModel.yaml");
        ModelUtils modelUtils = new ModelUtils(openAPI);
        Schema commandSchema = modelUtils.getSchema("Command");

        Assert.assertTrue(modelUtils.isModel(commandSchema));
        Assert.assertFalse(modelUtils.isFreeFormObject(commandSchema));
    }

    @Test
    public void testReferencedSchema() {
        Schema otherObj = new ObjectSchema().addProperties("sprop", new StringSchema()).addProperties("iprop", new IntegerSchema());

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas("OtherObj", otherObj);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        Schema notExistingReferencedSchema = new Schema().$ref("NotExisting");
        Schema result1 = modelUtils.getReferencedSchema(notExistingReferencedSchema);
        Assert.assertEquals(result1, notExistingReferencedSchema);

        Schema result2 = modelUtils.getReferencedSchema(new Schema().$ref("#/components/schemas/OtherObj"));
        Assert.assertEquals(result2, otherObj);
    }

    @Test
    public void testReferencedRequestBody() {
        RequestBody otherRequestBody = new RequestBody().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addRequestBodies("OtherRequestBody", otherRequestBody);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        RequestBody notExistingRequestBody = new RequestBody().$ref("NotExisting");
        RequestBody result1 = modelUtils.getReferencedRequestBody(notExistingRequestBody);
        Assert.assertEquals(result1, notExistingRequestBody);

        RequestBody result2 = modelUtils.getReferencedRequestBody(new RequestBody().$ref("#/components/requestBodies/OtherRequestBody"));
        Assert.assertEquals(result2, otherRequestBody);
    }

    @Test
    public void testReferencedApiResponse() {
        ApiResponse otherApiResponse = new ApiResponse().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addResponses("OtherApiResponse", otherApiResponse);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        ApiResponse notExistingApiResponse = new ApiResponse().$ref("NotExisting");
        ApiResponse result1 = modelUtils.getReferencedApiResponse(notExistingApiResponse);
        Assert.assertEquals(result1, notExistingApiResponse);

        ApiResponse result2 = modelUtils.getReferencedApiResponse(new ApiResponse().$ref("#/components/responses/OtherApiResponse"));
        Assert.assertEquals(result2, otherApiResponse);
    }

    @Test
    public void testReferencedParameter() {
        Parameter otherParameter = new Parameter().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addParameters("OtherParameter", otherParameter);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        Parameter notExistingParameter = new Parameter().$ref("NotExisting");
        Parameter result1 = modelUtils.getReferencedParameter(notExistingParameter);
        Assert.assertEquals(result1, notExistingParameter);

        Parameter result2 = modelUtils.getReferencedParameter(new Parameter().$ref("#/components/parameters/OtherParameter"));
        Assert.assertEquals(result2, otherParameter);
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/582.
     * Composed schemas should not get unaliased when generating model properties, in order to properly
     * generate the property data type name.
     */
    @Test
    public void testComposedSchemasAreNotUnaliased() {
        ComposedSchema composedSchema = new ComposedSchema().allOf(Arrays.asList(
                new Schema<>().$ref("#/components/schemas/SomeSchema"),
                new ObjectSchema()
        ));
        Schema refToComposedSchema = new Schema().$ref("#/components/schemas/SomeComposedSchema");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("SomeComposedSchema", composedSchema);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        Assert.assertEquals(refToComposedSchema, modelUtils.unaliasSchema(refToComposedSchema, new HashMap<>()));
    }

    @Test
    public void testAliasedTypeIsNotUnaliasedIfUsedForImportMapping() {
        Schema emailSchema = new Schema().$ref("#/components/schemas/Email").type("string");
        StringSchema stringSchema = new StringSchema();
        HashMap<String, String> importMappings = new HashMap<>();
        importMappings.put("Email", "foo.bar.Email");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Email", stringSchema);
        ModelUtils modelUtils = new ModelUtils(openAPI);

        Assert.assertEquals(emailSchema, modelUtils.unaliasSchema(emailSchema, importMappings));
        Assert.assertEquals(stringSchema, modelUtils.unaliasSchema(emailSchema, new HashMap<>()));
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/1624.
     * modelUtils.isFreeFormObject() should not throw an NPE when passed an empty
     * object schema that has additionalProperties defined as an empty object schema.
     */
    @Test
    public void testIsFreeFormObject() {
        OpenAPI openAPI = new OpenAPI().openapi("3.0.0");
        ModelUtils modelUtils = new ModelUtils(openAPI);

        // Create initial "empty" object schema.
        ObjectSchema objSchema = new ObjectSchema();
        Assert.assertTrue(modelUtils.isFreeFormObject(objSchema));

        // Set additionalProperties to an empty ObjectSchema.
        objSchema.setAdditionalProperties(new ObjectSchema());
        Assert.assertTrue(modelUtils.isFreeFormObject(objSchema));

        // Add a single property to the schema (no longer a free-form object).
        Map<String, Schema> props = new HashMap<>();
        props.put("prop1", new StringSchema());
        objSchema.setProperties(props);
        Assert.assertFalse(modelUtils.isFreeFormObject(objSchema));

        // Test a non-object schema
        Assert.assertFalse(modelUtils.isFreeFormObject(new StringSchema()));

        // Test a null schema
        Assert.assertFalse(modelUtils.isFreeFormObject(null));
    }

    @Test
    public void testIsSetForValidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(true);
        ModelUtils modelUtils = new ModelUtils(null);
        Assert.assertTrue(modelUtils.isSet(as));
    }

    @Test
    public void testIsSetFalseForInvalidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(false);
        ModelUtils modelUtils = new ModelUtils(null);
        Assert.assertFalse(modelUtils.isSet(as));
    }

    @Test
    public void testIsSetFailsForNullSchema() {
        ArraySchema as = null;
        ModelUtils modelUtils = new ModelUtils(null);
        Assert.assertFalse(modelUtils.isSet(as));
    }

    @Test
    public void testSimpleRefDecoding() {
        ModelUtils modelUtils = new ModelUtils(null);
        String decoded = modelUtils.getSimpleRef("#/components/~01%20Hallo~1Welt");
        Assert.assertEquals(decoded, "~1 Hallo/Welt");
    }
}
