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

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class ModelUtilsTest {

    @Test
    public void testGetAllUsedSchemas() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/unusedSchemas.yaml", null, new ParseOptions()).getOpenAPI();
        List<String> allUsedSchemas = ModelUtils.getAllUsedSchemas(openAPI);
        Assert.assertEquals(allUsedSchemas.size(), 12);

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
    }

    @Test
    public void testGetUnusedSchemas() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/unusedSchemas.yaml", null, new ParseOptions()).getOpenAPI();
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
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/unusedSchemas.yaml", null, new ParseOptions()).getOpenAPI();
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 3);
        //SomeObj2 is only used in a 'application/x-www-form-urlencoded' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj2"), "contains 'SomeObj2'");
        //SomeObj3 is only used in a 'multipart/form-data' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj3"), "contains 'SomeObj3'");
        //SomeObj7 is only used in a 'application/x-www-form-urlencoded' request (with referenced request body)
        Assert.assertTrue(unusedSchemas.contains("SomeObj7"), "contains 'SomeObj7'");
    }

    @Test
    public void testNoComponentsSection() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testGlobalProducesConsumes() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/globalProducesConsumesTest.yaml", null, new ParseOptions()).getOpenAPI();
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
}
