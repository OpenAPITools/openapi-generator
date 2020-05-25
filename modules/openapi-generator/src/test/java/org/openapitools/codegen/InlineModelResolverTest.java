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
import java.util.List;

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
                        .title("AddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema user = openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = openapi.getComponents().getSchemas().get("AddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModelTestWithTitleWithSpaces() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .title("User Address Title")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema user = openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = openapi.getComponents().getSchemas().get("User_Address_Title");
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
                        .title("AddressTitle")
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
                        .title("AddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        ObjectSchema user = (ObjectSchema) openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertNotNull(user.getProperties().get("address"));

        Schema address = openapi.getComponents().getSchemas().get("AddressTitle");
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
                        .title("AddressTitle")
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
                        .title("AddressTitle")
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

        Schema address = openapi.getComponents().getSchemas().get("AddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        Schema anotherAddress = openapi.getComponents().getSchemas().get("AddressTitle1");
        assertNotNull(anotherAddress);
        assertNotNull(anotherAddress.getProperties().get("city"));
        assertNotNull(anotherAddress.getProperties().get("street"));
        assertNotNull(anotherAddress.getProperties().get("apartment"));
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
        assertEquals("#/components/schemas/InlineArrayItemsTitle", requestBodySchema.getItems().get$ref());
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
        assertEquals("#/components/schemas/InlineArrayItemsTitle1", responseSchema.getItems().get$ref());
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

        assertEquals("object", mediaType.getSchema().getType());
        Schema ref = (Schema) mediaType.getSchema().getAdditionalProperties();
        assertNotNull(ref);
        assertEquals("#/components/schemas/resolveInlineObjectResponseWithAdditionalPropertiesResponseAddlProps", ref.get$ref());

        Schema addlProps = openAPI.getComponents().getSchemas().get("resolveInlineObjectResponseWithAdditionalPropertiesResponseAddlProps");
        assertEquals("object", addlProps.getType());
        assertTrue(addlProps.getProperties().get("resolve_inline_object_response_with_additional_properties") instanceof StringSchema);
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

        assertEquals("object", mediaType.getSchema().getType());
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

        assertEquals("object", mediaType.getSchema().getType());
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


    private void checkComposedChildren(OpenAPI openAPI, List<Schema> children, String schemaName, String key) {
        assertNotNull(children);
        Schema inlined = children.get(0);
        assertEquals("#/components/schemas/ComposedObjectModelInline" + schemaName, inlined.get$ref());
        Schema child = ModelUtils.getReferencedSchema(openAPI, inlined);
        assertNotNull(child.getProperties());
        assertNotNull(child.getProperties().get("composed_object_model_inline_" + key));
    }

    @Test
    public void objectComposedWithInline() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("ComposedObjectModelInline") instanceof ComposedSchema);

        ComposedSchema schema = (ComposedSchema) openAPI.getComponents().getSchemas().get("ComposedObjectModelInline");

        checkComposedChildren(openAPI, schema.getAllOf(), "AllOf", "allOf");
        checkComposedChildren(openAPI, schema.getAnyOf(), "AnyOf", "anyOf");
        checkComposedChildren(openAPI, schema.getOneOf(), "OneOf", "oneOf");
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
        assertEquals("#/components/schemas/nullablePropertiesBody", nullableRequestBodyReference.get$ref());
        Schema ref = openAPI.getComponents().getSchemas().get("nullablePropertiesBody");
        assertTrue(ref instanceof ObjectSchema);

        ObjectSchema nullableRequestBody = (ObjectSchema) ref;
        Schema bodyProp = nullableRequestBody.getProperties().get("nullable_request_body_property");
        assertNotNull(bodyProp);
        assertEquals("#/components/schemas/nullablePropertiesBodyNullableRequestBodyProperty", bodyProp.get$ref());
        Schema ref2 = openAPI.getComponents().getSchemas().get("nullablePropertiesBodyNullableRequestBodyProperty");
        assertTrue(ref2 instanceof ObjectSchema);

        ObjectSchema nullableRequestBodyProperty = (ObjectSchema) ref2;
        assertTrue(nullableRequestBodyProperty.getNullable());
        Schema prop2 = nullableRequestBodyProperty.getProperties().get("nullable_request_body_property_name");
        assertNotNull(prop2);
        assertEquals("string", prop2.getType());
    }

    @Test
    public void complexModelWithInlineModelsDefault() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        // Test default behavior where inline enums are not promoted to their own models
        InlineModelResolver resolver = new InlineModelResolver();
        resolver.flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("Cat") instanceof ObjectSchema);
        ObjectSchema cat = (ObjectSchema) openAPI.getComponents().getSchemas().get("Cat");

        // assert properties should be pulled into their own schemas
        assertEquals("#/components/schemas/CatMyFood", cat.getProperties().get("myFood").get$ref());
        assertEquals("#/components/schemas/CatMyHabitat", cat.getProperties().get("myHabitat").get$ref());
        assertEquals("#/components/schemas/CatMyTaxonomy", cat.getProperties().get("myTaxonomy").get$ref());
        // Not CatCuteness since it is an inline enum and resolveInlineEnums is false by default
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
        assertEquals(2, food.getOneOf().size());
        assertEquals("#/components/schemas/CatMyFoodOneOf", food.getOneOf().get(0).get$ref());
        assertEquals("#/components/schemas/CatMyFoodOneOf1", food.getOneOf().get(1).get$ref());

        // myHabitat is an allOf
        Schema habSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myHabitat"));
        assertTrue(habSchema instanceof ComposedSchema);
        ComposedSchema hab = (ComposedSchema) habSchema;
        assertEquals(2, hab.getAllOf().size());
        assertEquals("#/components/schemas/Geography", hab.getAllOf().get(0).get$ref());
        assertEquals("#/components/schemas/CatMyHabitatAllOf", hab.getAllOf().get(1).get$ref());

        // geography properties
        Schema geoSchema = ModelUtils.getReferencedSchema(openAPI, hab.getAllOf().get(0));
        assertTrue(geoSchema instanceof ObjectSchema);
        ObjectSchema geo = (ObjectSchema) geoSchema;
        assertNotNull(geo.getProperties().get("lat"));
        assertEquals("number", geo.getProperties().get("lat").getType());
        assertEquals("float", geo.getProperties().get("lat").getFormat());
        assertNotNull(geo.getProperties().get("long"));
        assertEquals("number", geo.getProperties().get("long").getType());
        assertEquals("float", geo.getProperties().get("lat").getFormat());
        // Geography.continent is an enum
        Schema contSchema = geo.getProperties().get("continent");
        assertNotNull(contSchema);
        assertNotNull(contSchema.getEnum());
        assertEquals(7, contSchema.getEnum().size());
        assertEquals("Africa", contSchema.getEnum().get(0));
        assertEquals("Antarctica", contSchema.getEnum().get(1));
        assertEquals("Asia", contSchema.getEnum().get(2));
        assertEquals("Europe", contSchema.getEnum().get(3));
        assertEquals("North America", contSchema.getEnum().get(4));
        assertEquals("Oceania", contSchema.getEnum().get(5));
        assertEquals("South America", contSchema.getEnum().get(6));
        // weather properties
        Schema weatherSchema = ModelUtils.getReferencedSchema(openAPI, hab.getAllOf().get(1));
        assertTrue(weatherSchema instanceof ObjectSchema);
        ObjectSchema weather = (ObjectSchema) weatherSchema;
        assertNotNull(weather.getProperties().get("rainfallInches"));
        assertEquals("number", weather.getProperties().get("rainfallInches").getType());
        assertNotNull(weather.getProperties().get("averageTemperatureCelsius"));
        assertEquals("number", weather.getProperties().get("averageTemperatureCelsius").getType());

        // myTaxonomy is an anyOf
        Schema taxSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myTaxonomy"));
        assertTrue(taxSchema instanceof ComposedSchema);
        ComposedSchema tax = (ComposedSchema) taxSchema;
        assertEquals(2, tax.getAnyOf().size());
        assertEquals("#/components/schemas/Species", tax.getAnyOf().get(0).get$ref());
        assertEquals("#/components/schemas/Order", tax.getAnyOf().get(1).get$ref());
        // species properties
        Schema speciesSchema = ModelUtils.getReferencedSchema(openAPI, tax.getAnyOf().get(0));
        assertTrue(speciesSchema instanceof ObjectSchema);
        ObjectSchema species = (ObjectSchema) speciesSchema;
        assertNotNull(species.getProperties().get("name"));
        assertEquals("string", species.getProperties().get("name").getType());
        assertNotNull(species.getProperties().get("genus"));
        assertEquals("string", species.getProperties().get("genus").getType());
        assertNotNull(species.getProperties().get("karyotype"));
        assertEquals("string", species.getProperties().get("karyotype").getType());
        // order properties
        Schema orderSchema = ModelUtils.getReferencedSchema(openAPI, tax.getAnyOf().get(1));
        assertTrue(orderSchema instanceof ObjectSchema);
        ObjectSchema ordr = (ObjectSchema) orderSchema;
        assertNotNull(ordr.getProperties().get("name"));
        assertEquals("string", ordr.getProperties().get("name").getType());
        assertNotNull(ordr.getProperties().get("class"));
        assertEquals("string", ordr.getProperties().get("class").getType());

        // cuteness is a string enum
        Schema cute = cat.getProperties().get("cuteness");
        assertEquals("integer", cute.getType());
        assertNotNull(cute.getEnum());
        assertEquals(3, cute.getEnum().size());
        assertEquals(1, cute.getEnum().get(0));
        assertEquals(3, cute.getEnum().get(1));
        assertEquals(5, cute.getEnum().get(2));

        // preferences is an object with additional props that are its own enum model
        Schema prefs = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("preferences"));
        assertEquals("object", prefs.getType());
        // has a favoriteToy property
        assertEquals(1, prefs.getProperties().size());
        assertNotNull(prefs.getProperties().get("favoriteToy"));
        Schema toy = (Schema) prefs.getProperties().get("favoriteToy");
        assertEquals("string", toy.getType());
        // has additionalProperties with its own Metadata schema name from its title
        assertTrue(prefs.getAdditionalProperties() instanceof Schema);
        Schema meta = (Schema) prefs.getAdditionalProperties();
        // Metadata should be string enum with hidden,createdOn,createdBy,modifiedOn,modifiedBy
        assertEquals("string", meta.getType());
        assertNotNull(meta.getEnum());
        assertEquals(5, meta.getEnum().size());
        assertEquals("hidden", meta.getEnum().get(0));
        assertEquals("createdOn", meta.getEnum().get(1));
        assertEquals("createdBy", meta.getEnum().get(2));
        assertEquals("modifiedOn", meta.getEnum().get(3));
        assertEquals("modifiedBy", meta.getEnum().get(4));
    }

    @Test
    public void complexModelWithInlineModelsResolved() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        // Test resolveInlineEnums=true where inline enums are promoted to their own models
        resolver.resolveInlineEnums = true;
        resolver.flatten(openAPI);

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
        assertEquals(2, food.getOneOf().size());
        assertEquals("#/components/schemas/CatMyFoodOneOf", food.getOneOf().get(0).get$ref());
        assertEquals("#/components/schemas/CatMyFoodOneOf1", food.getOneOf().get(1).get$ref());

        // myHabitat is an allOf
        Schema habSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myHabitat"));
        assertTrue(habSchema instanceof ComposedSchema);
        ComposedSchema hab = (ComposedSchema) habSchema;
        assertEquals(2, hab.getAllOf().size());
        assertEquals("#/components/schemas/Geography", hab.getAllOf().get(0).get$ref());
        assertEquals("#/components/schemas/CatMyHabitatAllOf", hab.getAllOf().get(1).get$ref());

        // geography properties
        Schema geoSchema = ModelUtils.getReferencedSchema(openAPI, hab.getAllOf().get(0));
        assertTrue(geoSchema instanceof ObjectSchema);
        ObjectSchema geo = (ObjectSchema) geoSchema;
        assertNotNull(geo.getProperties().get("lat"));
        assertEquals("number", geo.getProperties().get("lat").getType());
        assertEquals("float", geo.getProperties().get("lat").getFormat());
        assertNotNull(geo.getProperties().get("long"));
        assertEquals("number", geo.getProperties().get("long").getType());
        assertEquals("float", geo.getProperties().get("lat").getFormat());
        // Geography.continent is an enum
        assertNotNull(geo.getProperties().get("continent"));
        assertEquals("#/components/schemas/CatMyHabitatAllOfContinent", geo.getProperties().get("continent").get$ref());
        Schema contSchema = ModelUtils.getReferencedSchema(openAPI, geo.getProperties().get("continent"));
        assertNotNull(contSchema.getEnum());
        assertEquals(7, contSchema.getEnum().size());
        assertEquals("Africa", contSchema.getEnum().get(0));
        assertEquals("Antarctica", contSchema.getEnum().get(1));
        assertEquals("Asia", contSchema.getEnum().get(2));
        assertEquals("Europe", contSchema.getEnum().get(3));
        assertEquals("North America", contSchema.getEnum().get(4));
        assertEquals("Oceania", contSchema.getEnum().get(5));
        assertEquals("South America", contSchema.getEnum().get(6));
        // weather properties
        Schema weatherSchema = ModelUtils.getReferencedSchema(openAPI, hab.getAllOf().get(1));
        assertTrue(weatherSchema instanceof ObjectSchema);
        ObjectSchema weather = (ObjectSchema) weatherSchema;
        assertNotNull(weather.getProperties().get("rainfallInches"));
        assertEquals("number", weather.getProperties().get("rainfallInches").getType());
        assertNotNull(weather.getProperties().get("averageTemperatureCelsius"));
        assertEquals("number", weather.getProperties().get("averageTemperatureCelsius").getType());

        // myTaxonomy is an anyOf
        Schema taxSchema = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("myTaxonomy"));
        assertTrue(taxSchema instanceof ComposedSchema);
        ComposedSchema tax = (ComposedSchema) taxSchema;
        assertEquals(2, tax.getAnyOf().size());
        assertEquals("#/components/schemas/Species", tax.getAnyOf().get(0).get$ref());
        assertEquals("#/components/schemas/Order", tax.getAnyOf().get(1).get$ref());
        // species properties
        Schema speciesSchema = ModelUtils.getReferencedSchema(openAPI, tax.getAnyOf().get(0));
        assertTrue(speciesSchema instanceof ObjectSchema);
        ObjectSchema species = (ObjectSchema) speciesSchema;
        assertNotNull(species.getProperties().get("name"));
        assertEquals("string", species.getProperties().get("name").getType());
        assertNotNull(species.getProperties().get("genus"));
        assertEquals("string", species.getProperties().get("genus").getType());
        assertNotNull(species.getProperties().get("karyotype"));
        assertEquals("string", species.getProperties().get("karyotype").getType());
        // order properties
        Schema orderSchema = ModelUtils.getReferencedSchema(openAPI, tax.getAnyOf().get(1));
        assertTrue(orderSchema instanceof ObjectSchema);
        ObjectSchema ordr = (ObjectSchema) orderSchema;
        assertNotNull(ordr.getProperties().get("name"));
        assertEquals("string", ordr.getProperties().get("name").getType());
        assertNotNull(ordr.getProperties().get("class"));
        assertEquals("string", ordr.getProperties().get("class").getType());

        // cuteness is a string enum
        Schema cute = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("cuteness"));
        assertEquals("integer", cute.getType());
        assertNotNull(cute.getEnum());
        assertEquals(3, cute.getEnum().size());
        assertEquals(1, cute.getEnum().get(0));
        assertEquals(3, cute.getEnum().get(1));
        assertEquals(5, cute.getEnum().get(2));

        // preferences is an object with additional props that are its own enum model
        Schema prefs = ModelUtils.getReferencedSchema(openAPI, cat.getProperties().get("preferences"));
        assertEquals("object", prefs.getType());
        // has a favoriteToy property
        assertEquals(1, prefs.getProperties().size());
        assertNotNull(prefs.getProperties().get("favoriteToy"));
        Schema toy = (Schema) prefs.getProperties().get("favoriteToy");
        assertEquals("string", toy.getType());
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

    @Test
    public void callbacks() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody callbackRequestBody = openAPI
                .getPaths()
                .get("/callback")
                .getPost()
                .getCallbacks()
                .get("webhook")
                .get("{$request.body#/callbackUri}")
                .getPost()
                .getRequestBody();
        assertNotNull(callbackRequestBody);

        Schema callbackRequestSchemaReference = callbackRequestBody
                .getContent()
                .get("application/json")
                .getSchema();
        assertNotNull(callbackRequestSchemaReference.get$ref());

        Schema resolvedCallbackSchema = openAPI
                .getComponents()
                .getSchemas()
                .get(ModelUtils.getSimpleRef(callbackRequestSchemaReference.get$ref()));

        Map properties = resolvedCallbackSchema.getProperties();
        assertTrue(properties.get("notificationId") instanceof StringSchema);
        assertTrue(properties.get("action") instanceof StringSchema);
        assertTrue(properties.get("data") instanceof StringSchema);
    }
}