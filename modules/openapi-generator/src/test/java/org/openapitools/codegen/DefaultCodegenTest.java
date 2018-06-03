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

package org.openapitools.codegen;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;

import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class DefaultCodegenTest {

    @Test
    public void testCamelize() throws Exception {
        Assert.assertEquals(DefaultCodegen.camelize("abcd"), "Abcd");
        Assert.assertEquals(DefaultCodegen.camelize("some-value"), "SomeValue");
        Assert.assertEquals(DefaultCodegen.camelize("some_value"), "SomeValue");
        Assert.assertEquals(DefaultCodegen.camelize("$type"), "$Type");

        Assert.assertEquals(DefaultCodegen.camelize("abcd", true), "abcd");
        Assert.assertEquals(DefaultCodegen.camelize("some-value", true), "someValue");
        Assert.assertEquals(DefaultCodegen.camelize("some_value", true), "someValue");
        Assert.assertEquals(DefaultCodegen.camelize("Abcd", true), "abcd");
        Assert.assertEquals(DefaultCodegen.camelize("$type", true), "$type");

        Assert.assertEquals(DefaultCodegen.camelize("123", true), "123");
        Assert.assertEquals(DefaultCodegen.camelize("$123", true), "$123");
    }

    @Test
    public void testHasBodyParameter() throws Exception {
        final Schema refSchema = new Schema<>().$ref("#/components/schemas/Pet");
        Operation pingOperation = new Operation()
                .responses(
                        new ApiResponses().addApiResponse("204", new ApiResponse()
                                .description("Ok response")));
        Operation createOperation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content().addMediaType("application/json", 
                                new MediaType().schema(refSchema))))
                .responses(
                        new ApiResponses().addApiResponse("201", new ApiResponse()
                                .description("Created response")));
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas("Pet", new ObjectSchema());

        final DefaultCodegen codegen = new DefaultCodegen();

        Assert.assertEquals(codegen.hasBodyParameter(openAPI, pingOperation), false);
        Assert.assertEquals(codegen.hasBodyParameter(openAPI, createOperation), true);
    }

    @Test
    public void testGetConsumesInfoAndGetProducesInfo() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        final Schema refSchema = new Schema<>().$ref("#/components/schemas/Pet");
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas("Pet", new ObjectSchema());
        openAPI.getComponents().addRequestBodies("MyRequestBody", new RequestBody()
                .content(new Content().addMediaType("application/json", 
                        new MediaType().schema(refSchema))));
        openAPI.getComponents().addResponses("MyResponse", new ApiResponse()
                        .description("Ok response")
                        .content(new Content().addMediaType("application/xml", 
                        new MediaType().schema(refSchema))));

        Operation createOperation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content()
                                .addMediaType("application/json", new MediaType().schema(refSchema))
                                .addMediaType("application/xml", new MediaType().schema(refSchema))
                        ))
                .responses(
                        new ApiResponses().addApiResponse("201", new ApiResponse()
                                .description("Created response")));
        Set<String> createConsumesInfo = DefaultCodegen.getConsumesInfo(openAPI, createOperation);
        Assert.assertEquals(createConsumesInfo.size(), 2);
        Assert.assertTrue(createConsumesInfo.contains("application/json"), "contains 'application/json'");
        Assert.assertTrue(createConsumesInfo.contains("application/xml"), "contains 'application/xml'");
        Set<String> createProducesInfo = DefaultCodegen.getProducesInfo(openAPI, createOperation);
        Assert.assertEquals(createProducesInfo.size(), 0);
        CodegenOperation coCreate = codegen.fromOperation("somepath", "post", createOperation, openAPI.getComponents().getSchemas(), openAPI);
        Assert.assertTrue(coCreate.hasConsumes);
        Assert.assertEquals(coCreate.consumes.size(), 2);
        Assert.assertFalse(coCreate.hasProduces);

        Operation updateOperationWithRef = new Operation()
                .requestBody(new RequestBody().$ref("#/components/requestBodies/MyRequestBody"))
                .responses(new ApiResponses().addApiResponse("201", new ApiResponse().$ref("#/components/responses/MyResponse")));
        Set<String> updateConsumesInfo = DefaultCodegen.getConsumesInfo(openAPI, updateOperationWithRef);
        Assert.assertEquals(updateConsumesInfo.size(), 1);
        Assert.assertTrue(updateConsumesInfo.contains("application/json"), "contains 'application/json'");
        Set<String> updateProducesInfo = DefaultCodegen.getProducesInfo(openAPI, updateOperationWithRef);
        Assert.assertEquals(updateProducesInfo.size(), 1);
        Assert.assertTrue(updateProducesInfo.contains("application/xml"), "contains 'application/xml'");

        CodegenOperation coUpdate = codegen.fromOperation("somepath", "post", updateOperationWithRef, openAPI.getComponents().getSchemas(), openAPI);
        Assert.assertTrue(coUpdate.hasConsumes);
        Assert.assertEquals(coUpdate.consumes.size(), 1);
        Assert.assertEquals(coUpdate.consumes.get(0).get("mediaType"), "application/json");
        Assert.assertTrue(coUpdate.hasProduces);
        Assert.assertEquals(coUpdate.produces.size(), 1);
        Assert.assertEquals(coUpdate.produces.get(0).get("mediaType"), "application/xml");
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false );
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testArraySchemaIsNotIncluedInAliases() throws Exception {
        Map<String, Schema> schemas = new HashMap<String, Schema>() {
            {
                put("ArraySchemaTest", new ArraySchema());
            }

        };

        Method method = DefaultCodegen.class.getDeclaredMethod("getAllAliases", Map.class);
        method.setAccessible(true);
        Map<String, String> aliases = (Map<String, String>)method.invoke(null, schemas);

        Assert.assertEquals(aliases.size(), 0);
    }

    @Test
    public void testFormParameterHasDefaultValue() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Schema requestBodySchema = ModelUtils.getSchemaFromRequestBody(openAPI.getPaths().get("/fake").getGet().getRequestBody());
        CodegenParameter codegenParameter = codegen.fromFormProperty("enum_form_string", (Schema) requestBodySchema.getProperties().get("enum_form_string"), new HashSet<String>());

        Assert.assertEquals(codegenParameter.defaultValue, "-efg");
    }

    @Test
    public void testEnsureNoDuplicateProduces() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/two-responses.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/test").getGet();
        CodegenOperation co = codegen.fromOperation("/test", "get", operation, ModelUtils.getSchemas(openAPI), openAPI);

        Assert.assertEquals(co.produces.size(), 1);
        Assert.assertEquals(co.produces.get(0).get("mediaType"), "application/json");
    }
    
    @Test
    public void testGetSchemaTypeWithComposedSchemaWithOneOf() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/composed-oneof.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/state").getPost();
        Schema schema = ModelUtils.getSchemaFromRequestBody(operation.getRequestBody());
        String type = codegen.getSchemaType(schema);

        Assert.assertNotNull(type);
    }

    @Test
    public void testExample1() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/examples.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example1").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example1 value");
    }

    @Test
    public void testExample2() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/examples.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example2").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example2 value");
    }

    @Test
    public void testExample3() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/examples.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example3").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example3: parameter value");
    }

    @Test
    public void testExample4() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/examples.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example4").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getRequestBody());

        Assert.assertEquals(codegenParameter.example, "example4 value");
    }
}
