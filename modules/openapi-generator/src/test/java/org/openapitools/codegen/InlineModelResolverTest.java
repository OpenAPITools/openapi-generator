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
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;
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
        assertEquals(((Schema) user.getProperties().get("address")).get$ref(), "#/components/schemas/UserAddress");

        Schema address = openapi.getComponents().getSchemas().get("UserAddress");
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
        assertNotNull(openAPI.getComponents().getSchemas());
    }
    
    @Test
    public void resolveInlineArraySchemaWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("Users") instanceof ArraySchema);

        ArraySchema users = (ArraySchema) openAPI.getComponents().getSchemas().get("Users");
        Schema ref = users.getItems();
        assertNotNull(ref);
        assertEquals("#/components/schemas/User", ref.get$ref());

        Schema userRef = openAPI.getComponents().getSchemas().get("User");
        assertTrue(userRef instanceof ObjectSchema);

        ObjectSchema user = (ObjectSchema) userRef;
        assertEquals("User", user.getTitle());
        assertTrue(user.getProperties().get("street") instanceof StringSchema);
        assertTrue(user.getProperties().get("city") instanceof StringSchema);
    }
    
    @Test
    public void resolveInlineRequestBody() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        Schema ref = openAPI
                .getPaths()
                .get("/resolve_inline_request_body")
                .getPost()
                .getRequestBody()
                .getContent()
                .get("application/json")
                .getSchema();
        assertNotNull(ref);
        assertEquals("#/components/schemas/resolveInlineRequestBodyBody", ref.get$ref());

        assertTrue(ModelUtils.getReferencedSchema(openAPI, ref) instanceof ObjectSchema);

        ObjectSchema schema = (ObjectSchema) ModelUtils.getReferencedSchema(openAPI, ref);
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

        MediaType mediaType = openAPI.getPaths().get("/resolve_inline_request_body_with_title").getPost().getRequestBody().getContent().get("application/json");
        assertNotNull(mediaType.getSchema());
        assertEquals("#/components/schemas/resolve_inline_request_body_with_title", mediaType.getSchema().get$ref());
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
        assertEquals("#/components/schemas/resolveInlineArrayRequestBodyBodyItems", requestBody.getItems().get$ref());

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
        assertEquals("#/components/schemas/resolveInlineArrayResponseResponseItems", responseSchema.getItems().get$ref());

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
        Schema ref = (Schema) mediaType.getSchema().getAdditionalProperties();
        assertNotNull(ref);
        assertEquals("#/components/schemas/resolveInlineObjectResponseWithAdditionalPropertiesResponseAddlProps", ref.get$ref());

        Schema addlProps = openAPI.getComponents().getSchemas().get("resolveInlineObjectResponseWithAdditionalPropertiesResponseAddlProps");
        assertTrue(addlProps instanceof ObjectSchema);

        ObjectSchema additionalProperties = (ObjectSchema) addlProps;
        assertTrue(additionalProperties.getProperties().get("resolve_inline_object_response_with_additional_properties") instanceof StringSchema);
    }

    @Test
    public void resolveInlineMapSchemaInResponse() {
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
        ApiResponse apiResponse = openAPI
            .getPaths()
            .get("/resolve_inline_map_schema_in_response")
            .getGet()
            .getResponses()
            .get("200");

        // NOTE: Swagger parser doesn't use MapSchema currently,
        //       so we need to set a MapSchema instance as the schema manually for testing.
        // @see https://github.com/swagger-api/swagger-parser/blob/master/modules/swagger-parser-v3/src/main/java/io/swagger/v3/parser/util/SchemaTypeUtil.java
        apiResponse.content(
            new Content().addMediaType(
                "application/json",
                new MediaType().schema(
                    new MapSchema().additionalProperties(
                        new ObjectSchema().addProperties(
                            "resolve_inline_map_schema_in_response_property",
                            new ObjectSchema().addProperties(
                                "resolve_inline_map_schema_in_response_property_string",
                                new StringSchema().example("example")
                            )
                        )
                    )
                )
            )
        );

        new InlineModelResolver().flatten(openAPI);

        MediaType mediaType = openAPI
            .getPaths()
            .get("/resolve_inline_map_schema_in_response")
            .getGet()
            .getResponses()
            .get("200")
            .getContent()
            .get("application/json");
        assertTrue(mediaType.getSchema() instanceof MapSchema);

        Schema additionalProperties = (Schema) mediaType.getSchema().getAdditionalProperties();
        assertNotNull(additionalProperties.get$ref());
        assertEquals("#/components/schemas/resolveInlineMapSchemaInResponseResponseAddlProps", additionalProperties.get$ref());

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, additionalProperties);
        Schema referencedSchemaProperty = (Schema) referencedSchema.getProperties().get("resolve_inline_map_schema_in_response_property");

        assertEquals(
            "#/components/schemas/resolveInlineMapSchemaInResponseResponseAddlPropsResolveInlineMapSchemaInResponseProperty",
            referencedSchemaProperty.get$ref()
        );
        assertNotNull(ModelUtils.getReferencedSchema(openAPI, referencedSchemaProperty));
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

        assertNotNull(mediaType.getSchema());
        assertEquals("#/components/schemas/arbitraryObjectRequestBodyPropertyBody", mediaType.getSchema().get$ref());
        Schema ref = openAPI.getComponents().getSchemas().get("arbitraryObjectRequestBodyPropertyBody");
        assertNotNull(ref);

        assertTrue(ref instanceof ObjectSchema);
        ObjectSchema requestBodySchema = (ObjectSchema) ref;
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
        Schema itemsRef = schema.getItems();
        assertNotNull(itemsRef);
        assertEquals("#/components/schemas/ArbitraryObjectModelWithArrayInlineWithoutTitleItems", itemsRef.get$ref());
        Schema ref = openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithoutTitleItems");
        assertNotNull(ref);
        assertTrue(ref instanceof ObjectSchema);
        ObjectSchema itemsObj = (ObjectSchema) ref;
        assertTrue(itemsObj.getProperties().get("arbitrary_object_model_with_array_inline_without_title") instanceof ObjectSchema);
    }

    @Test
    public void arbitraryObjectModelWithArrayInlineWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle") instanceof ArraySchema);

        ArraySchema schema = (ArraySchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle");

        Schema itemsRef = schema.getItems();
        assertNotNull(itemsRef);
        assertEquals("#/components/schemas/ArbitraryObjectModelWithArrayInlineWithTitleInner", itemsRef.get$ref());
        Schema ref = openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitleInner");
        assertNotNull(ref);

        assertTrue(ref instanceof ObjectSchema);
        ObjectSchema items = (ObjectSchema) ref;
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
                .getSchema();

        assertNotNull(nullableRequestBodyReference);
        assertEquals("#/components/schemas/arbitraryObjectRequestBodyPropertyBody_1", nullableRequestBodyReference.get$ref());
        Schema ref = openAPI.getComponents().getSchemas().get("arbitraryObjectRequestBodyPropertyBody_1");
        assertTrue(ref instanceof ObjectSchema);

        ObjectSchema nullableRequestBody = (ObjectSchema) ref;
        Schema bodyProp = nullableRequestBody.getProperties().get("nullable_request_body_property");
        assertNotNull(bodyProp);
        assertEquals("#/components/schemas/arbitraryObjectRequestBodyPropertyBody_1NullableRequestBodyProperty", bodyProp.get$ref());
        Schema ref2 = openAPI.getComponents().getSchemas().get("arbitraryObjectRequestBodyPropertyBody_1NullableRequestBodyProperty");
        assertTrue(ref2 instanceof ObjectSchema);

        ObjectSchema nullableRequestBodyProperty = (ObjectSchema) ref2;
        assertTrue(nullableRequestBodyProperty.getNullable());
        Schema prop2 = nullableRequestBodyProperty.getProperties().get("nullable_request_body_property_name");
        assertNotNull(prop2);
        assertEquals("string", prop2.getType());
    }

    @Test
    public void complexModelWithInlineModels() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("Cat") instanceof ObjectSchema);
        ObjectSchema cat = (ObjectSchema) openAPI.getComponents().getSchemas().get("Cat");

        // assert properties should be pulled into their own schemas
        assertEquals("#/components/schemas/CatMyFood", cat.getProperties().get("myFood").get$ref());
        assertEquals("#/components/schemas/CatMyHabitat", cat.getProperties().get("myHabitat").get$ref());
        assertEquals("#/components/schemas/CatMyTaxonomy", cat.getProperties().get("myTaxonomy").get$ref());
        assertEquals("#/components/schemas/CatCuteness", cat.getProperties().get("cuteness").get$ref());
        assertEquals("#/components/schemas/CatPreferences", cat.getProperties().get("preferences").get$ref());

        // kittens should be an array, but its items should be their own schema
        assertTrue(cat.getProperties().get("kittens") instanceof ArraySchema);
        ArraySchema kittens = (ArraySchema) cat.getProperties().get("kittens");
        assertEquals("#/components/schemas/CatKittensItems", kittens.getItems().get$ref());

        // assert that schemas pulled out contain expected details
        // myFood is a oneOf
        Schema foodSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myFood"));
        assertTrue(foodSchema instanceof ComposedSchema);
        ComposedSchema food = (ComposedSchema) foodSchema;
        assertEquals(1, food.getOneOf().size());
        assertEquals("#/components/schemas/Food", food.getOneOf().get(0).get$ref());

        // myHabitat is an allOf
        Schema habSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myHabitat"));
        assertTrue(habSchema instanceof ComposedSchema);
        ComposedSchema hab = (ComposedSchema) habSchema;
        assertEquals(1, hab.getAllOf().size());
        assertEquals("#/components/schemas/Habitat", hab.getAllOf().get(0).get$ref());

        // myTaxonomy is an anyOf
        Schema taxSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myTaxonomy"));
        assertTrue(taxSchema instanceof ComposedSchema);
        ComposedSchema tax = (ComposedSchema) taxSchema;
        assertEquals(2, tax.getAnyOf().size());
        assertEquals("#/components/schemas/Species", tax.getAnyOf().get(0).get$ref());
        assertEquals("#/components/schemas/Order", tax.getAnyOf().get(1).get$ref());

        // cuteness is a string enum
        Schema cute = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("cuteness"));
        assertEquals("integer", cute.getType());
        assertNotNull(cute.getEnum());
        assertEquals(3, cute.getEnum().size());
        assertEquals(1, cute.getEnum().get(0));
        assertEquals(3, cute.getEnum().get(1));
        assertEquals(5, cute.getEnum().get(2));

        // preferences is an object with additional props that are its own enum model
        Schema prefsSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("preferences"));
        assertTrue(prefsSchema instanceof ObjectSchema);
        ObjectSchema prefs = (ObjectSchema) prefsSchema;
        // has a favoriteToy property
        assertEquals(1, prefs.getProperties().size());
        assertNotNull(prefs.getProperties().get("favoriteToy"));
        assertEquals("string", prefs.getProperties().get("favoriteToy").getType());
        // has additionalProperties with its own Metadata schema name from its title
        assertTrue(prefs.getAdditionalProperties() instanceof Schema);
        Schema addlProps = (Schema) prefs.getAdditionalProperties();
        assertEquals("#/components/schemas/Metadata", addlProps.get$ref());

        // Metadata should be string enum with hidden,createdOn,createdBy,modifiedOn,modifiedBy
        Schema meta = ModelUtils.getReferencedSchema(openAPI, addlProps);
        assertEquals("string", meta.getType());
        assertNotNull(meta.getEnum());
        assertEquals(5, meta.getEnum().size());
        assertEquals("hidden", meta.getEnum().get(0));
        assertEquals("createdOn", meta.getEnum().get(1));
        assertEquals("createdBy", meta.getEnum().get(2));
        assertEquals("modifiedOn", meta.getEnum().get(3));
        assertEquals("modifiedBy", meta.getEnum().get(4));
    }
}