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

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ModelUtilsTest {

    @Test
    public void testGetAllUsedSchemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> allUsedSchemas = ModelUtils.getAllUsedSchemas(openAPI);
        Assert.assertEquals(allUsedSchemas.size(), 34);

        Assert.assertTrue(allUsedSchemas.contains("SomeObjShared"), "contains 'SomeObjShared'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj1"), "contains 'UnusedObj1'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj2"), "contains 'SomeObj2'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj3"), "contains 'SomeObj3'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj6"), "contains 'SomeObj6'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj7"), "contains 'SomeObj7'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj8"), "contains 'SomeObj8'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj9A"), "contains 'SomeObj9A'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj9B"), "contains 'SomeObj9B'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj10A"), "contains 'SomeObj10A'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj10B"), "contains 'SomeObj10B'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj11"), "contains 'SomeObj11'");
        Assert.assertTrue(allUsedSchemas.contains("SomeArrayObj12"), "contains 'SomeArrayObj12'");
        Assert.assertTrue(allUsedSchemas.contains("ArrayItem12"), "contains 'ArrayItem12'");
        Assert.assertTrue(allUsedSchemas.contains("SomeArrayObj13"), "contains 'SomeArrayObj13'");
        Assert.assertTrue(allUsedSchemas.contains("ArrayItem13"), "contains 'ArrayItem13'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj14"), "contains 'SomeObj14'");
        Assert.assertTrue(allUsedSchemas.contains("PropertyObj14"), "contains 'PropertyObj14'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj15"), "contains 'SomeObj15'");
        Assert.assertTrue(allUsedSchemas.contains("SomeMapObj16"), "contains 'SomeMapObj16'");
        Assert.assertTrue(allUsedSchemas.contains("MapItem16"), "contains 'MapItem16'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj17"), "contains 'SomeObj17'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj18"), "contains 'SomeObj18'");
        Assert.assertTrue(allUsedSchemas.contains("Common18"), "contains 'Common18'");
        Assert.assertTrue(allUsedSchemas.contains("Obj19ByAge"), "contains 'Obj19ByAge'");
        Assert.assertTrue(allUsedSchemas.contains("Obj19ByType"), "contains 'Obj19ByType'");
        Assert.assertTrue(allUsedSchemas.contains("SomeObj20"), "contains 'SomeObj20'");
        Assert.assertTrue(allUsedSchemas.contains("OtherObj20"), "contains 'OtherObj20'");
        Assert.assertTrue(allUsedSchemas.contains("PingDataInput21"), "contains 'PingDataInput21'");
        Assert.assertTrue(allUsedSchemas.contains("PingDataOutput21"), "contains 'PingDataOutput21'");
        Assert.assertTrue(allUsedSchemas.contains("SInput22"), "contains 'SInput22'");
        Assert.assertTrue(allUsedSchemas.contains("SOutput22"), "contains 'SInput22'");
        Assert.assertTrue(allUsedSchemas.contains("SomeHeader23"), "contains 'SomeHeader23'");
        Assert.assertTrue(allUsedSchemas.contains("SomeHeader24"), "contains 'SomeHeader24'");
    }

    @Test
    public void testGetUnusedSchemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getUnusedSchemas(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 4);
        //UnusedObj is not used at all:
        Assert.assertTrue(unusedSchemas.contains("UnusedObj1"), "contains 'UnusedObj1'");
        //SomeObjUnused is used in a request body that is not used.
        Assert.assertTrue(unusedSchemas.contains("UnusedObj2"), "contains 'UnusedObj2'");
        //SomeObjUnused is used in a response that is not used.
        Assert.assertTrue(unusedSchemas.contains("UnusedObj3"), "contains 'UnusedObj3'");
        //SomeObjUnused is used in a parameter that is not used.
        Assert.assertTrue(unusedSchemas.contains("UnusedObj4"), "contains 'UnusedObj4'");
    }

    @Test
    public void testSchemasUsedOnlyInFormParam() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/ping.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testGlobalProducesConsumes() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/globalProducesConsumesTest.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testReferencedSchema() {
        Schema otherObj = new ObjectSchema().addProperties("sprop", new StringSchema()).addProperties("iprop", new IntegerSchema());

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas("OtherObj", otherObj);

        Schema notExistingReferencedSchema = new Schema().$ref("NotExisting");
        Schema result1 = ModelUtils.getReferencedSchema(openAPI, notExistingReferencedSchema);
        Assert.assertEquals(result1, notExistingReferencedSchema);

        Schema result2 = ModelUtils.getReferencedSchema(openAPI, new Schema().$ref("#/components/schemas/OtherObj"));
        Assert.assertEquals(result2, otherObj);
    }

    @Test
    public void testReferencedRequestBody() {
        RequestBody otherRequestBody = new RequestBody().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addRequestBodies("OtherRequestBody", otherRequestBody);

        RequestBody notExistingRequestBody = new RequestBody().$ref("NotExisting");
        RequestBody result1 = ModelUtils.getReferencedRequestBody(openAPI, notExistingRequestBody);
        Assert.assertEquals(result1, notExistingRequestBody);

        RequestBody result2 = ModelUtils.getReferencedRequestBody(openAPI, new RequestBody().$ref("#/components/requestBodies/OtherRequestBody"));
        Assert.assertEquals(result2, otherRequestBody);
    }

    @Test
    public void testReferencedApiResponse() {
        ApiResponse otherApiResponse = new ApiResponse().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addResponses("OtherApiResponse", otherApiResponse);

        ApiResponse notExistingApiResponse = new ApiResponse().$ref("NotExisting");
        ApiResponse result1 = ModelUtils.getReferencedApiResponse(openAPI, notExistingApiResponse);
        Assert.assertEquals(result1, notExistingApiResponse);

        ApiResponse result2 = ModelUtils.getReferencedApiResponse(openAPI, new ApiResponse().$ref("#/components/responses/OtherApiResponse"));
        Assert.assertEquals(result2, otherApiResponse);
    }

    @Test
    public void testReferencedParameter() {
        Parameter otherParameter = new Parameter().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addParameters("OtherParameter", otherParameter);

        Parameter notExistingParameter = new Parameter().$ref("NotExisting");
        Parameter result1 = ModelUtils.getReferencedParameter(openAPI, notExistingParameter);
        Assert.assertEquals(result1, notExistingParameter);

        Parameter result2 = ModelUtils.getReferencedParameter(openAPI, new Parameter().$ref("#/components/parameters/OtherParameter"));
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

        Assert.assertEquals(refToComposedSchema, ModelUtils.unaliasSchema(openAPI, refToComposedSchema));
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/1624.
     * ModelUtils.isFreeFormObject() should not throw an NPE when passed an empty
     * object schema that has additionalProperties defined as an empty object schema.
     */
    @Test
    public void testIsFreeFormObject() {
        // Create initial "empty" object schema.
        ObjectSchema objSchema = new ObjectSchema();
        Assert.assertTrue(ModelUtils.isFreeFormObject(objSchema));

        // Set additionalProperties to an empty ObjectSchema.
        objSchema.setAdditionalProperties(new ObjectSchema());
        Assert.assertTrue(ModelUtils.isFreeFormObject(objSchema));

        // Add a single property to the schema (no longer a free-form object).
        Map<String, Schema> props = new HashMap<>();
        props.put("prop1", new StringSchema());
        objSchema.setProperties(props);
        Assert.assertFalse(ModelUtils.isFreeFormObject(objSchema));

        // Test a non-object schema
        Assert.assertFalse(ModelUtils.isFreeFormObject(new StringSchema()));

        // Test a null schema
        Assert.assertFalse(ModelUtils.isFreeFormObject(null));
    }
}
