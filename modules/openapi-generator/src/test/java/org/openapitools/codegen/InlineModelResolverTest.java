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

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.AssertJUnit.*;

@SuppressWarnings("static-method")
public class InlineModelResolverTest {
    @Test
    public void resolveInlineModelTestWithoutTitle() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .description("description")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        assertNotNull((openapi.getComponents().getSchemas().get("User")).getProperties().get("address"));

        new InlineModelResolver().flatten(openapi);

        Schema user = openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertNotNull(user.getProperties().get("address"));
        assertNotNull(((Schema) user.getProperties().get("address")).get$ref());
        assertEquals(((Schema) user.getProperties().get("address")).get$ref(), "#/components/schemas/User_address");

        Schema address = openapi.getComponents().getSchemas().get("User_address");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModelTestWithTitle() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema user = openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModel2EqualInnerModels() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        openapi.getComponents().addSchemas("AnotherUser", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("lastName", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        ObjectSchema user = (ObjectSchema) openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertNotNull(user.getProperties().get("address"));

        Schema address = openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        Schema duplicateAddress = openapi.getComponents().getSchemas().get("UserAddressTitle_0");
        assertNull(duplicateAddress);
    }

    @Test
    public void resolveInlineModel2DifferentInnerModelsWIthSameTitle() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        openapi.getComponents().addSchemas("AnotherUser", new ObjectSchema()
                .name("AnotherUser")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("lastName", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())
                        .addProperties("apartment", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema user = openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        Schema duplicateAddress = openapi.getComponents().getSchemas().get("UserAddressTitle_1");
        assertNotNull(duplicateAddress);
        assertNotNull(duplicateAddress.getProperties().get("city"));
        assertNotNull(duplicateAddress.getProperties().get("street"));
        assertNotNull(duplicateAddress.getProperties().get("apartment"));
    }

    @Test
    public void testInlineResponseModel() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());

        Map<String, String> propExt = new HashMap<>();
        propExt.put("x-ext", "ext-prop");

        Map<String, Object> responseExt = new HashMap<>();
        responseExt.put("x-foo", "bar");

        openapi.path("/foo/bar", new PathItem()
                .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("it works!")
                        .content(new Content().addMediaType("application/json",
                                new MediaType().schema(new ObjectSchema().title("inline_response_200")
                                        .addProperties("name", new StringSchema()).extensions(propExt))))))))
                .path("/foo/baz", new PathItem()
                        .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("it works!")
                                .extensions(responseExt)
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(new ObjectSchema()
                                                .addProperties("name", new StringSchema()).extensions(propExt))))))));

        new InlineModelResolver().flatten(openapi);

        Map<String, ApiResponse> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        ApiResponse response = responses.get("200");
        assertNotNull(response);
        Schema schema = response.getContent().get("application/json").getSchema();
        assertNotNull(schema);
        assertEquals(1, schema.getExtensions().size());
        assertEquals("ext-prop", schema.getExtensions().get("x-ext"));

        Schema model = openapi.getComponents().getSchemas().get("inline_response_200");
        assertEquals(1, model.getProperties().size());
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void testInlineResponseModelWithTitle() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());

        Map<String, String> propExt = new HashMap<>();
        propExt.put("x-ext", "ext-prop");

        Map<String, Object> responseExt = new HashMap<>();
        responseExt.put("x-foo", "bar");

        openapi.path("/foo/bar", new PathItem()
                .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                        .description("it works!")
                        .content(new Content().addMediaType("application/json",
                                new MediaType().schema(new ObjectSchema().title("GetBarResponse")
                                        .addProperties("name", new StringSchema()).extensions(propExt))))))))
                .path("/foo/baz", new PathItem()
                        .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("it works!")
                                .extensions(responseExt)
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(new ObjectSchema()
                                                .addProperties("name", new StringSchema()).extensions(propExt))))))));

        new InlineModelResolver().flatten(openapi);

        Map<String, ApiResponse> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        ApiResponse response = responses.get("200");
        assertNotNull(response);
        Schema schema = response.getContent().get("application/json").getSchema();
        assertNotNull(schema);
        assertEquals(1, schema.getExtensions().size());
        assertEquals("ext-prop", schema.getExtensions().get("x-ext"));

        Schema model = openapi.getComponents().getSchemas().get("GetBarResponse");
        assertEquals(1, model.getProperties().size());
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void resolveInlineRequestBodyWhenNoComponents() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_request_body_no_components.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertNotNull(openAPI.getComponents());
        assertNotNull(openAPI.getComponents().getRequestBodies());
    }
    
    @Test
    public void resolveInlineArraySchemaWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("Users") instanceof ArraySchema);

        ArraySchema users = (ArraySchema) openAPI.getComponents().getSchemas().get("Users");
        assertTrue(users.getItems() instanceof ObjectSchema);

        ObjectSchema user = (ObjectSchema) users.getItems();
        assertEquals("User", user.getTitle());
        assertTrue(user.getProperties().get("street") instanceof StringSchema);
        assertTrue(user.getProperties().get("city") instanceof StringSchema);
    }
    
    @Test
    public void resolveInlineRequestBody() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI
                .getPaths()
                .get("/resolve_inline_request_body")
                .getPost()
                .getRequestBody();
        assertNotNull(requestBodyReference.get$ref());

        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, requestBodyReference);
        MediaType mediaType = requestBody.getContent().get("application/json");
        assertTrue(ModelUtils.getReferencedSchema(openAPI, mediaType.getSchema()) instanceof ObjectSchema);

        ObjectSchema schema = (ObjectSchema) ModelUtils.getReferencedSchema(openAPI, mediaType.getSchema());
        assertTrue(schema.getProperties().get("name") instanceof StringSchema);
        assertNotNull(schema.getProperties().get("address").get$ref());

        Schema address = ModelUtils.getReferencedSchema(openAPI, schema.getProperties().get("address"));
        assertTrue(address.getProperties().get("street") instanceof StringSchema);
    }

    @Test
    public void resolveInlineRequestBodyWithRequired() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI.getPaths().get("/resolve_inline_request_body_with_required").getPost().getRequestBody();
        assertTrue(requestBodyReference.getRequired());

        RequestBody referencedRequestBody = ModelUtils.getReferencedRequestBody(openAPI, requestBodyReference);
        assertTrue(referencedRequestBody.getRequired());
    }

    @Test
    public void resolveInlineRequestBodyWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI.getPaths().get("/resolve_inline_request_body_with_title").getPost().getRequestBody();
        assertEquals("#/components/requestBodies/resolve_inline_request_body_with_title", requestBodyReference.get$ref());
    }

    @Test
    public void nonModelRequestBody() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/non_model_request_body")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("multipart/form-data");

        assertTrue(mediaType.getSchema() instanceof BinarySchema);
        assertEquals("string", mediaType.getSchema().getType());
        assertEquals("binary", mediaType.getSchema().getFormat());
    }

    @Test
    public void resolveInlineArrayRequestBody() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/resolve_inline_array_request_body")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);

        ArraySchema requestBody = (ArraySchema) mediaType.getSchema();
        assertNotNull(requestBody.getItems().get$ref());
        assertEquals("#/components/schemas/NULL_UNIQUE_NAME", requestBody.getItems().get$ref());

        Schema items = ModelUtils.getReferencedSchema(openAPI, ((ArraySchema) mediaType.getSchema()).getItems());
        assertTrue(items.getProperties().get("street") instanceof StringSchema);
        assertTrue(items.getProperties().get("city") instanceof StringSchema);
    }

    @Test
    public void resolveInlineArrayRequestBodyWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        ArraySchema requestBodySchema = (ArraySchema) openAPI
                .getPaths()
                .get("/resolve_inline_array_request_body_with_title")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json")
                .getSchema();

        assertNotNull(requestBodySchema.getItems().get$ref());
        assertEquals("#/components/schemas/resolveInlineArrayRequestBodyWithTitleItems", requestBodySchema.getItems().get$ref());
    }

    @Test
    public void resolveInlineArrayResponse() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/resolve_inline_array_response")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);

        ArraySchema responseSchema = (ArraySchema) mediaType.getSchema();
        assertEquals("#/components/schemas/inline_response_200", responseSchema.getItems().get$ref());

        Schema items = ModelUtils.getReferencedSchema(openAPI, responseSchema.getItems());
        assertTrue(items.getProperties().get("array_response_property") instanceof StringSchema);
    }

    @Test
    public void resolveInlineArrayResponseWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/resolve_inline_array_response_with_title")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        ArraySchema responseSchema = (ArraySchema) mediaType.getSchema();
        assertEquals("#/components/schemas/resolveInlineArrayResponseWithTitleItems", responseSchema.getItems().get$ref());
    }

    @Test
    public void resolveInlineObjectResponseWithAdditionalProperties() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/resolve_inline_object_response_with_additional_properties")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ObjectSchema);
        assertTrue(mediaType.getSchema().getAdditionalProperties() instanceof ObjectSchema);

        ObjectSchema additionalProperties = (ObjectSchema) mediaType.getSchema().getAdditionalProperties();
        assertTrue(additionalProperties.getProperties().get("resolve_inline_object_response_with_additional_properties") instanceof StringSchema);
    }

    @Test
    public void arbitraryObjectRequestBody() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_request_body")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ObjectSchema);
    }

    @Test
    public void arbitraryObjectRequestBodyProperty() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_request_body_property")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ObjectSchema);

        ObjectSchema requestBodySchema = (ObjectSchema) mediaType.getSchema();
        assertTrue(requestBodySchema.getProperties().get("arbitrary_object_request_body_property") instanceof ObjectSchema);
    }

    @Test
    public void arbitraryRequestBodyArray() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_request_body_array")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);

        ArraySchema requestBodySchema = (ArraySchema) mediaType.getSchema();
        assertTrue(requestBodySchema.getItems() instanceof ObjectSchema);
        assertNull(requestBodySchema.getItems().getProperties());
    }

    @Test
    public void arbitraryRequestBodyArrayProperty() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_request_body_array_property")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);

        ArraySchema requestBodySchema = (ArraySchema) mediaType.getSchema();
        assertNotNull(requestBodySchema.getItems().get$ref());

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, requestBodySchema.getItems());
        assertTrue(referencedSchema.getProperties().get("arbitrary_request_body_array_property") instanceof ObjectSchema);
    }

    @Test
    public void arbitraryObjectResponse() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_response")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ObjectSchema);
        assertNull(mediaType.getSchema().getProperties());
    }

    @Test
    public void arbitraryObjectResponseArray() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_response_array")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);
        ArraySchema schema = (ArraySchema) mediaType.getSchema();
        assertNull(schema.getItems().getProperties());
    }

    @Test
    public void arbitraryObjectResponseArrayInline() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_response_array_inline")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);
        ArraySchema schema = (ArraySchema) mediaType.getSchema();
        assertNotNull(schema.getItems().get$ref());

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, schema.getItems());
        assertTrue(referencedSchema.getProperties().get("arbitrary_object_response_array_inline") instanceof ObjectSchema);

        ObjectSchema arbitaryObject = (ObjectSchema) referencedSchema.getProperties().get("arbitrary_object_response_array_inline");
        assertNull(arbitaryObject.getProperties());
    }

    @Test
    public void arbitraryObjectResponseWithAdditionalProperty() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/arbitrary_object_response_with_additional_property")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ObjectSchema);
        assertTrue(mediaType.getSchema().getAdditionalProperties() instanceof ObjectSchema);

        ObjectSchema additionalProperty = (ObjectSchema) mediaType.getSchema().getAdditionalProperties();
        assertNull(additionalProperty.getProperties());
    }

    @Test
    public void arbitraryObjectModelInline() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        ObjectSchema model = (ObjectSchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelInline");
        assertTrue(model.getProperties().get("arbitrary_object_model_inline") instanceof ObjectSchema);

        ObjectSchema schema = (ObjectSchema) model.getProperties().get("arbitrary_object_model_inline");
        assertNull(schema.getProperties());
    }

    @Test
    public void arbitraryObjectModelWithArrayInlineWithoutTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithoutTitle") instanceof ArraySchema);

        ArraySchema schema = (ArraySchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithoutTitle");
        assertTrue(schema.getItems() instanceof ObjectSchema);

        ObjectSchema items = (ObjectSchema) schema.getItems();
        assertTrue(items.getProperties().get("arbitrary_object_model_with_array_inline_without_title") instanceof ObjectSchema);

        ObjectSchema itemsProperty = (ObjectSchema) items.getProperties().get("arbitrary_object_model_with_array_inline_without_title");
        assertNull(itemsProperty.getProperties());
    }

    @Test
    public void arbitraryObjectModelWithArrayInlineWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle") instanceof ArraySchema);

        ArraySchema schema = (ArraySchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle");
        assertTrue(schema.getItems() instanceof ObjectSchema);

        ObjectSchema items = (ObjectSchema) schema.getItems();
        // TODO: Fix the model as referenced schema which named with the title value
        assertEquals("ArbitraryObjectModelWithArrayInlineWithTitleInner", items.getTitle());
        assertTrue(items.getProperties().get("arbitrary_object_model_with_array_inline_with_title") instanceof ObjectSchema);

        ObjectSchema itemsProperty = (ObjectSchema) items.getProperties().get("arbitrary_object_model_with_array_inline_with_title");
        assertNull(itemsProperty.getProperties());
    }

    @Test
    public void emptyExampleOnStringTypeModels() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
                .getPaths()
                .get("/empty_example_on_string_models")
                .getGet()
                .getResponses()
                .get("200")
                .getContent()
                .get("application/json");

        assertTrue(mediaType.getSchema() instanceof ArraySchema);

        ArraySchema schema = (ArraySchema) mediaType.getSchema();
        assertEquals("#/components/schemas/EmptyExampleOnStringTypeModels", schema.getItems().get$ref());

        assertTrue(ModelUtils.getReferencedSchema(openAPI, schema.getItems()) instanceof StringSchema);
        assertNull(ModelUtils.getReferencedSchema(openAPI, schema.getItems()).getExample());
    }

    @Test
    public void nullable() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        Schema nullablePropertyReference = (Schema) openAPI.getComponents().getSchemas().get("InlinePropertyIsNullable").getProperties().get("nullable_property");
        Schema nullablePropertySchema = ModelUtils.getReferencedSchema(openAPI, nullablePropertyReference);
        assertTrue(nullablePropertySchema.getNullable());


        Schema nullableRequestBodyReference = (Schema) openAPI
                .getPaths()
                .get("/nullable_properties")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json")
                .getSchema()
                .getProperties()
                .get("nullable_request_body_property");
        Schema nullableRequestBodySchema = ModelUtils.getReferencedSchema(openAPI, nullableRequestBodyReference);
        assertTrue(nullableRequestBodySchema.getNullable());
    }
}