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
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.AssertJUnit.*;

@SuppressWarnings("static-method")
public class InlineModelResolverTest {
    @Test
    public void resolveInlineModelTestWithoutTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("address", new ObjectSchema()
                        .description("description")
                        //._default("default")
                        //.access("access")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        assertNotNull(((Schema) openapi.getComponents().getSchemas().get("User")).getProperties().get("address"));

        new InlineModelResolver().flatten(openapi);

        Schema user = (Schema) openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertNotNull(user.getProperties().get("address"));
        assertNotNull(((Schema) user.getProperties().get("address")).get$ref());
        assertEquals(((Schema) user.getProperties().get("address")).get$ref(), "#/components/schemas/User_address");

        Schema address = (Schema) openapi.getComponents().getSchemas().get("User_address");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModelTestWithTitle() throws Exception {
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

        Schema user = (Schema) openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = (Schema) openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModel2EqualInnerModels() throws Exception {
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
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = (Schema) openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        Schema duplicateAddress = (Schema) openapi.getComponents().getSchemas().get("UserAddressTitle_0");
        assertNull(duplicateAddress);
    }

    @Test
    public void resolveInlineModel2DifferentInnerModelsWIthSameTitle() throws Exception {
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

        Schema user = (Schema) openapi.getComponents().getSchemas().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof Schema);

        Schema address = (Schema) openapi.getComponents().getSchemas().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        Schema duplicateAddress = (Schema) openapi.getComponents().getSchemas().get("UserAddressTitle_1");
        assertNotNull(duplicateAddress);
        assertNotNull(duplicateAddress.getProperties().get("city"));
        assertNotNull(duplicateAddress.getProperties().get("street"));
        assertNotNull(duplicateAddress.getProperties().get("apartment"));
    }

    @Test
    public void testInlineResponseModel() throws Exception {
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
        assertTrue(schema instanceof Schema);
        assertEquals(1, schema.getExtensions().size());
        assertEquals("ext-prop", schema.getExtensions().get("x-ext"));

        Schema model = (Schema) openapi.getComponents().getSchemas().get("inline_response_200");
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void testInlineResponseModelWithTitle() throws Exception {
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
        assertTrue(schema instanceof Schema);
        assertEquals(1, schema.getExtensions().size());
        assertEquals("ext-prop", schema.getExtensions().get("x-ext"));

        Schema model = (Schema) openapi.getComponents().getSchemas().get("GetBarResponse");
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void resolveInlineRequestBodyWhenNoComponents() {
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_request_body_no_components.yaml", null, new ParseOptions()).getOpenAPI();
        new InlineModelResolver().flatten(openAPI);

        assertNotNull(openAPI.getComponents());
        assertNotNull(openAPI.getComponents().getRequestBodies());
    }
    
    /*
    @Test
    public void resolveInlineArraySchemaWithTitle() {
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI.getPaths().get("/resolve_inline_request_body_with_required").getPost().getRequestBody();
        assertTrue(requestBodyReference.getRequired());
    }

/*
    @Test
    public void resolveInlineBodyParameterWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        ObjectSchema addressModelItem = new ObjectSchema();
        String addressModelName = "DetailedAddress";
    addressModelItem.setTitle(addressModelName);
    openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(addressModelItem
                                        .addProperties("address", new ObjectSchema()
                                            .addProperties("street", new StringSchema()))
                                        .addProperties("name", new StringSchema())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getComponents().getSchemas().get(addressModelName);
        assertTrue(body instanceof ObjectSchema);

        ObjectSchema impl = (ObjectSchema) body;
        assertNotNull(impl.getProperties().get("address"));
    }
    */

    @Test
    public void nonModelRequestBody() {
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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
        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/inline_model_resolver.yaml", null, new ParseOptions()).getOpenAPI();
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

/*
    @Test
    public void testArrayResponse() {
        OpenAPI openapi = new OpenAPI();

        ArrayProperty schema = new ArrayProperty();
        schema.setItems(new ObjectSchema()
                .addProperties("name", new StringSchema()));

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertTrue(response.getSchema() instanceof ArrayProperty);

        ArrayProperty am = (ArrayProperty) response.getSchema();
        Property items = am.getItems();
        assertTrue(items instanceof Schema);
        Schema rp = (Schema) items;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = openapi.getComponents().getSchemas().get("inline_response_200");
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void testBasicInput() {
        OpenAPI openapi = new OpenAPI();

        ObjectSchema user = new ObjectSchema()
                .addProperties("name", new StringSchema());

        openapi.path("/foo/baz", new Path()
                .post(new Operation()
                        .parameter(new BodyParameter()
                            .name("myBody")
                            .schema(new RefModel("User")))));

        openapi.getComponents().addSchemas("User", user);

        new InlineModelResolver().flatten(openapi);

        Json.prettyPrint(openapi);
    }

    @Test
    public void testArbitraryObjectBodyParam() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ObjectSchema()))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ObjectSchema);
        ObjectSchema m = (ObjectSchema) bp.getSchema();
        assertNull(m.getType());
    }

    @Test
    public void testArbitraryObjectBodyParamInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ObjectSchema()
                                        .addProperties("arbitrary", new ObjectSchema())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getComponents().getSchemas().get("body");
        assertTrue(body instanceof ObjectSchema);

        ObjectSchema impl = (ObjectSchema) body;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectSchema);
    }

    @Test
    public void testArbitraryObjectBodyParamWithArray() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArraySchema()
                                        .items(new ObjectSchema())))));

        new InlineModelResolver().flatten(openapi);

        Parameter param = openapi.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof ArraySchema);

        ArraySchema am = (ArraySchema) schema;
        Property inner = am.getItems();
        assertTrue(inner instanceof ObjectSchema);

        ObjectSchema op = (ObjectSchema) inner;
        assertNotNull(op);
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectBodyParamArrayInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArraySchema()
                                        .items(new ObjectSchema()
                                            .addProperties("arbitrary", new ObjectSchema()))))));

        new InlineModelResolver().flatten(openapi);

        Parameter param = openapi.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof ArraySchema);

        ArraySchema am = (ArraySchema) schema;
        Property inner = am.getItems();
        assertTrue(inner instanceof Schema);

        Schema rp = (Schema) inner;

        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/body");
        assertEquals(rp.getSimpleRef(), "body");

        Model inline = openapi.getComponents().getSchemas().get("body");
        assertNotNull(inline);
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectSchema);
    }

    @Test
    public void testArbitraryObjectResponse() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectSchema()))));
        new InlineModelResolver().flatten(openapi);

        Map<String, Response> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        assertTrue(response.getSchema() instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) response.getSchema();
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseArray() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .description("it works!")
                                .schema(new ArrayProperty()
                                        .items(new ObjectSchema())))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertTrue(response.getSchema() instanceof ArrayProperty);

        ArrayProperty am = (ArrayProperty) response.getSchema();
        Property items = am.getItems();
        assertTrue(items instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) items;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseArrayInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(new ArrayProperty()
                                        .items(new ObjectSchema()
                                            .addProperties("arbitrary", new ObjectSchema()))))));

        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertNotNull(response);

        assertNotNull(response.getSchema());
        Property responseProperty = response.getSchema();
        assertTrue(responseProperty instanceof ArrayProperty);

        ArrayProperty ap = (ArrayProperty) responseProperty;
        Property p = ap.getItems();
        assertNotNull(p);

        Schema rp = (Schema) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = openapi.getComponents().getSchemas().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        Property inlineProp = impl.getProperties().get("arbitrary");
        assertNotNull(inlineProp);
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseMapInline() {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectSchema());

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertTrue(openapi.getComponents().getSchemas().size() == 0);
        Property inlineProp = ((MapProperty) property).getAdditionalProperties();
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperties("name", new StringSchema())
                .addProperties("arbitrary", new ObjectSchema()
                        .title("title")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")));

        new InlineModelResolver().flatten(openapi);

        ObjectSchema user = (ObjectSchema)openapi.getComponents().getSchemas().get("User");
        assertNotNull(user);
        Property inlineProp = user.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelWithArrayInlineWithoutTitle() {
        OpenAPI openapi = new OpenAPI();

        openapi.getComponents().addSchemas("User", new ArraySchema()
                .items(new ObjectSchema()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .addProperties("arbitrary", new ObjectSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema model = openapi.getComponents().getSchemas().get("User");
        assertTrue(model instanceof ArraySchema);
        ArraySchema am = (ArraySchema) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof Schema);

        ObjectSchema userInner = (ObjectSchema)openapi.getComponents().getSchemas().get("User_inner");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) inlineProp;
        assertNull(op.getProperties());
    }
    
    @Test
    public void testArbitraryObjectModelWithArrayInlineWithTitle() {
        OpenAPI openapi = new OpenAPI();

        openapi.getComponents().addSchemas("User", new ArraySchema()
                .items(new ObjectSchema()
                        .title("InnerUserTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .addProperties("arbitrary", new ObjectSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema model = openapi.getComponents().getSchemas().get("User");
        assertTrue(model instanceof ArraySchema);
        ArraySchema am = (ArraySchema) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof Schema);

        ObjectSchema userInner = (ObjectSchema)openapi.getComponents().getSchemas().get("InnerUserTitle");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema op = (ObjectSchema) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testEmptyExampleOnStrinngTypeModels() {
        OpenAPI openapi = new OpenAPI();

        Schema Schema = new Schema();
        Schema.set$ref("#/definitions/Test");

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .schema(new ArrayProperty()
                                        .items(Schema)))));

        openapi.getComponents().addSchemas("Test", new ObjectSchema()
                .example(StringUtils.EMPTY)
                .type("string"));
        new InlineModelResolver().flatten(openapi);
    }
*/
}