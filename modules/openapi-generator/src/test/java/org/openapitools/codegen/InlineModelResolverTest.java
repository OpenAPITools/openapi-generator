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
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.List;
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
                .addProperty("name", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .description("description")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

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
                .addProperty("name", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

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
    public void resolveInlineModelTestWithTitleWithSpaces() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperty("name", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("User Address Title")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

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
                .addProperty("name", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

        openapi.getComponents().addSchemas("AnotherUser", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperty("name", new StringSchema())
                .addProperty("lastName", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

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
    public void resolveInlineModelDeduplicatesAgainstExistingComponentSchema() {
        // When a pre-existing components/schemas entry has the same title and content as an
        // inline schema encountered during flattening, the inline schema should reuse the
        // pre-existing name rather than being registered as a numbered variant (e.g. Foo_1).
        // Regression test for: external $ref chains producing ContainerMapping + ContainerMapping_1.
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());

        // Pre-existing named schema registered directly in components
        openapi.getComponents().addSchemas("ContainerMapping", new ObjectSchema()
                .title("ContainerMapping")
                .description("Describes the mapping of essence from a container")
                .addProperty("track_index", new IntegerSchema()));

        // Schema whose inline property content is identical to the pre-existing ContainerMapping
        openapi.getComponents().addSchemas("FlowCore", new ObjectSchema()
                .name("FlowCore")
                .addProperty("container_mapping", new ObjectSchema()
                        .title("ContainerMapping")
                        .description("Describes the mapping of essence from a container")
                        .addProperty("track_index", new IntegerSchema())));

        new InlineModelResolver().flatten(openapi);

        // The pre-existing entry must still exist
        assertNotNull(openapi.getComponents().getSchemas().get("ContainerMapping"));
        // No numbered duplicate should have been created
        assertNull(openapi.getComponents().getSchemas().get("ContainerMapping_1"));
    }

    @Test
    public void resolveInlineModelDeduplicatesAcrossExternalRefChains() {
        // Regression test: same external schema referenced from two schemas (one via plain $ref,
        // one via $ref + sibling description as allowed in OpenAPI 3.1) must not produce a
        // duplicate numbered model (Container_Mapping_1).
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        parseOptions.setResolveResponses(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(
                "src/test/resources/3_0/inline-model-resolver-dedup/root.yaml",
                null, parseOptions).getOpenAPI();
        new InlineModelResolver().flatten(openAPI);

        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        assertNotNull(schemas.get("Container_Mapping"));
        assertNull("Duplicate Container_Mapping_1 must not exist", schemas.get("Container_Mapping_1"));
    }

    @Test
    public void resolveInlineModelDeduplicatesWhenParserMutatesPropertyDescriptions() {
        // Regression test: when the Swagger Parser shares and mutates resolved Schema objects
        // (e.g. a shared uuid.json resolved Schema has its description overwritten by whichever
        // property referencing it was last processed), two Schema objects from the same external
        // file may end up with different serialised JSON despite being structurally identical.
        // The structural-hash fallback in matchGenerated() (serialised without any 'description'
        // fields at any level) must still deduplicate them rather than creating a numbered variant.
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.setPaths(new Paths());

        // Schema A: properties have "correct" descriptions (as the file author wrote them)
        Schema widgetA = new ObjectSchema()
                .title("Widget")
                .description("A reusable widget")
                .addProperty("id", new StringSchema().description("Widget identifier"))
                .addProperty("name", new StringSchema().description("Widget name"));

        // Schema B: same title/description/property-names, but 'id' has a DIFFERENT description
        // (simulating what the Swagger Parser does when it shares and mutates a resolved sub-schema)
        Schema widgetB = new ObjectSchema()
                .title("Widget")
                .description("A reusable widget")
                .addProperty("id", new StringSchema().description("MUTATED description from another schema"))
                .addProperty("name", new StringSchema().description("Widget name"));

        ApiResponse responseA = new ApiResponse()
                .description("OK")
                .content(new Content().addMediaType("application/json",
                        new MediaType().schema(widgetA)));
        ApiResponse responseB = new ApiResponse()
                .description("OK")
                .content(new Content().addMediaType("application/json",
                        new MediaType().schema(widgetB)));

        openapi.getPaths()
                .addPathItem("/a", new PathItem().get(
                        new Operation().operationId("getA").responses(new ApiResponses().addApiResponse("200", responseA))))
                .addPathItem("/b", new PathItem().get(
                        new Operation().operationId("getB").responses(new ApiResponses().addApiResponse("200", responseB))));

        new InlineModelResolver().flatten(openapi);

        Map<String, Schema> schemas = openapi.getComponents().getSchemas();
        assertNotNull("Widget schema must exist", schemas.get("Widget"));
        assertNull("Duplicate Widget_1 must not exist — shape-fingerprint dedup must fire", schemas.get("Widget_1"));
    }

    @Test
    public void resolveInlineModelDeduplicatesMultipleRefsToSameExternalFile() {
        // Regression test: when the same external schema file is referenced from three separate
        // paths (simulating DeletionRequest appearing multiple times in the TAMS spec), the parser
        // may share the same Java Schema object across all three references. After the first
        // processing mutates the object (inline sub-schemas replaced with $refs), subsequent
        // encounters hash differently. The identity-based fast path in matchGenerated() must
        // recognise the same object reference and avoid registering a numbered duplicate.
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        parseOptions.setResolveResponses(true);
        OpenAPI openAPI = new OpenAPIParser().readLocation(
                "src/test/resources/3_0/inline-model-resolver-dedup/root.yaml",
                null, parseOptions).getOpenAPI();
        new InlineModelResolver().flatten(openAPI);

        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        assertNotNull(schemas.get("Deletion_Request"));
        assertNull("Duplicate Deletion_Request_1 must not exist", schemas.get("Deletion_Request_1"));
    }

    @Test
    public void resolveInlineModelKeepsUntitledSchemasDifferingOnlyByDescriptionDistinct() {
        // Regression test for #24004: two distinct *untitled* inline object schemas that differ
        // only in their descriptions (here the nested 'result' property: "ABC Result" vs
        // "DEF Result") must NOT be merged. The volatile-stripped structural-signature fallback in
        // matchGenerated() collapses them once description/type are removed; that fallback is only
        // intended to unify titled named types across parser volatility, so it must not fire for
        // untitled inline schemas — otherwise 'def' silently gets the type generated for 'abc' and
        // breaks user code that expects two separate types (regression introduced in 7.23).
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.setPaths(new Paths());

        Schema abc = new ObjectSchema()
                .description("first container")
                .addProperty("result", new StringSchema().description("ABC Result"));
        Schema def = new ObjectSchema()
                .description("second container")
                .addProperty("result", new StringSchema().description("DEF Result"));

        Schema response = new ObjectSchema()
                .addProperty("abc", abc)
                .addProperty("def", def);

        ApiResponse apiResponse = new ApiResponse()
                .description("OK")
                .content(new Content().addMediaType("application/json",
                        new MediaType().schema(response)));

        openapi.getPaths().addPathItem("/default", new PathItem().get(
                new Operation().operationId("apiGetDefault")
                        .responses(new ApiResponses().addApiResponse("200", apiResponse))));

        new InlineModelResolver().flatten(openapi);

        // Locate the flattened response model (the only component schema carrying both properties).
        Schema responseModel = null;
        for (Schema candidate : openapi.getComponents().getSchemas().values()) {
            if (candidate.getProperties() != null
                    && candidate.getProperties().containsKey("abc")
                    && candidate.getProperties().containsKey("def")) {
                responseModel = candidate;
                break;
            }
        }
        assertNotNull("Flattened response model with abc/def properties must exist", responseModel);

        String abcRef = ((Schema) responseModel.getProperties().get("abc")).get$ref();
        String defRef = ((Schema) responseModel.getProperties().get("def")).get$ref();
        assertNotNull("abc property must be a $ref to a generated schema", abcRef);
        assertNotNull("def property must be a $ref to a generated schema", defRef);
        assertFalse("abc and def must resolve to DISTINCT schemas, not be merged: " + abcRef,
                abcRef.equals(defRef));
    }

    @Test
    public void resolveInlineModel2DifferentInnerModelsWithSameTitle() {
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.getComponents().addSchemas("User", new ObjectSchema()
                .name("user")
                .description("a common user")
                .addProperty("name", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())));

        openapi.getComponents().addSchemas("AnotherUser", new ObjectSchema()
                .name("AnotherUser")
                .description("a common user")
                .addProperty("name", new StringSchema())
                .addProperty("lastName", new StringSchema())
                .addProperty("address", new ObjectSchema()
                        .title("UserAddressTitle")
                        .readOnly(false)
                        .description("description")
                        .name("name")
                        .addProperty("street", new StringSchema())
                        .addProperty("city", new StringSchema())
                        .addProperty("apartment", new StringSchema())));

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
                                                .addProperty("name", new StringSchema()).extensions(propExt))))))))
                .path("/foo/baz", new PathItem()
                        .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("it works!")
                                .extensions(responseExt)
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(new ObjectSchema()
                                                .addProperty("name", new StringSchema()).extensions(propExt))))))));

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
    public void testInlineResponseModelType() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/6150_model_json_inline.yaml");
        new InlineModelResolver().flatten(openAPI);

        Schema InlineResponse200 = openAPI.getComponents().getSchemas().get("testOperation_200_response");
        assertEquals("object", InlineResponse200.getType());
        assertEquals("unknown", InlineResponse200.getFormat());
        Schema FooBarObject = openAPI.getComponents().getSchemas().get("FooBarObject");
        assertEquals("object", FooBarObject.getType());
        assertEquals("date-time", FooBarObject.getFormat());
        Schema Animal = openAPI.getComponents().getSchemas().get("Animal");
        assertEquals("object", Animal.getType());
        Schema Dog = openAPI.getComponents().getSchemas().get("Dog");
        assertNull(Dog.getType());
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
                                                .addProperty("name", new StringSchema()).extensions(propExt))))))))
                .path("/foo/baz", new PathItem()
                        .get(new Operation().responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("it works!")
                                .extensions(responseExt)
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(new ObjectSchema()
                                                .addProperty("name", new StringSchema()).extensions(propExt))))))));

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
        // no longer create inline requestBodies as references in the refactored inline model resolver (6.x)
        assertNull(openAPI.getComponents().getRequestBodies());
        assertNotNull(openAPI.getComponents().getSchemas().get("test1_request"));
    }

    @Test
    public void resolveInlineArraySchemaWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("Users") instanceof ArraySchema);

        ArraySchema users = (ArraySchema) openAPI.getComponents().getSchemas().get("Users");
        assertTrue(users.getItems() instanceof Schema);
        assertEquals("#/components/schemas/User", users.getItems().get$ref());

        ObjectSchema user = (ObjectSchema) openAPI.getComponents().getSchemas().get("User");
        assertEquals("User", user.getTitle());
        assertTrue(user.getProperties().get("street") instanceof StringSchema);
        assertTrue(user.getProperties().get("city") instanceof StringSchema);
    }

    @Test
    public void resolveComponentsResponses() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);
        ApiResponse apiResponse = openAPI.getComponents().getResponses().get("JustAnotherResponse");
        assertEquals(apiResponse.getContent().get("application/json").getSchema().get$ref(), "#/components/schemas/inline_object");
    }

    @Test
    public void resolveRequestBodyInvalidRef() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/invalid_ref_request_body.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI
                .getPaths()
                .get("/resolve_request_body_invalid_ref")
                .getPost()
                .getRequestBody();
        assertNull(requestBodyReference.getContent());
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
        assertEquals("#/components/schemas/resolveInlineRequestBody_request",
                requestBodyReference.getContent().get("application/json").getSchema().get$ref());

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
        assertEquals("#/components/schemas/resolve_inline_request_body_with_title",
                requestBodyReference.getContent().get("application/json").getSchema().get$ref());
    }

    @Test
    public void resolveInlineRequestBodyWithTitleInChinese() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI.getPaths().get("/resolve_inline_request_body_with_title_in_chinese").getPost().getRequestBody();
        assertEquals("#/components/schemas/resolveInlineRequestBodyWithRequired_request_1",
                requestBodyReference.getContent().get("application/json").getSchema().get$ref());
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
        assertEquals("#/components/schemas/resolveInlineArrayRequestBody_request_inner", requestBody.getItems().get$ref());

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
        assertEquals("#/components/schemas/resolveInlineArrayResponse_200_response_inner", responseSchema.getItems().get$ref());

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

        assertEquals("object", mediaType.getSchema().getType());
        Object additionalPropertiesObject = mediaType.getSchema().getAdditionalProperties();
        assertTrue(additionalPropertiesObject instanceof Schema);

        Schema additionalProperties = ModelUtils.getReferencedSchema(openAPI, (Schema) additionalPropertiesObject);
        assertNotNull(additionalProperties);
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
                                        new ObjectSchema().addProperty(
                                                "resolve_inline_map_schema_in_response_property",
                                                new ObjectSchema().addProperty(
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
        assertEquals("#/components/schemas/resolveInlineMapSchemaInResponse_200_response_value", additionalProperties.get$ref());

        Schema referencedSchema = ModelUtils.getReferencedSchema(openAPI, additionalProperties);
        Schema referencedSchemaProperty = (Schema) referencedSchema.getProperties().get("resolve_inline_map_schema_in_response_property");

        assertEquals(
                "#/components/schemas/resolveInlineMapSchemaInResponse_200_response_value_resolve_inline_map_schema_in_response_property",
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

        assertEquals("#/components/schemas/arbitraryObjectRequestBodyProperty_request", mediaType.getSchema().get$ref());
        Schema requestBodySchema = ModelUtils.getReferencedSchema(openAPI, mediaType.getSchema());
        assertNotNull(requestBodySchema);
        assertEquals(1, requestBodySchema.getProperties().size(), 1);
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

        ObjectSchema arbitraryObject = (ObjectSchema) referencedSchema.getProperties().get("arbitrary_object_response_array_inline");
        assertNull(arbitraryObject.getProperties());
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
        assertTrue(schema.getItems() instanceof Schema);
        assertEquals(schema.getItems().get$ref(), "#/components/schemas/ArbitraryObjectModelWithArrayInlineWithoutTitle_inner");

        ObjectSchema items = (ObjectSchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithoutTitle_inner");
        assertTrue(items.getProperties().get("arbitrary_object_model_with_array_inline_without_title") instanceof ObjectSchema);

        ObjectSchema itemsProperty = (ObjectSchema) items.getProperties().get("arbitrary_object_model_with_array_inline_without_title");
        assertNull(itemsProperty.getProperties());
    }

    private void checkComposedChildren(OpenAPI openAPI, List<Schema> children, String key) {
        assertNotNull(children);
        Schema inlined = children.get(0);
        assertEquals("#/components/schemas/ComposedObjectModelInline_" + key, inlined.get$ref());
        Schema child = ModelUtils.getReferencedSchema(openAPI, inlined);
        assertNotNull(child.getProperties());
        assertNotNull(child.getProperties().get("composed_object_model_inline_" + key));
    }

    @Test
    public void objectComposedWithInline() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("ComposedObjectModelInline")));

        ComposedSchema schema = (ComposedSchema) openAPI.getComponents().getSchemas().get("ComposedObjectModelInline");

        // since 7.0.0, allOf inline sub-schemas are not created as $ref schema
        assertEquals(1, schema.getAllOf().get(0).getProperties().size());
        assertNull(schema.getAllOf().get(0).get$ref());

        // anyOf, oneOf sub-schemas are created as $ref schema by inline model resolver
        checkComposedChildren(openAPI, schema.getAnyOf(), "anyOf");
        checkComposedChildren(openAPI, schema.getOneOf(), "oneOf");
    }


    @Test
    public void inheritanceWithInlineDiscriminator() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/regression_6905.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("PartyType") instanceof StringSchema);
        assertTrue(openAPI.getComponents().getSchemas().get("CustomerType") instanceof StringSchema);
        assertTrue(openAPI.getComponents().getSchemas().get("Entity") instanceof ObjectSchema);

        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Party")));
        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Contact")));
        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Customer")));
        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Person")));
        assertTrue(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Organization")));

        assertTrue(openAPI.getComponents().getSchemas().get("ApiError") instanceof ObjectSchema);

        assertFalse(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Party_allOf")));
        assertFalse(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Contact_allOf")));
        assertFalse(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Customer_allOf")));
        assertFalse(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Person_allOf")));
        assertFalse(ModelUtils.isComposedSchema(openAPI.getComponents().getSchemas().get("Organization_allOf")));

        // Party
        ComposedSchema party = (ComposedSchema) openAPI.getComponents().getSchemas().get("Party");
        List<Schema> partySchemas = party.getAllOf();
        Schema entity = ModelUtils.getReferencedSchema(openAPI, partySchemas.get(0));
        Schema partyAllOfChildSchema = partySchemas.get(1); // get the inline schema directly

        assertEquals(partySchemas.get(0).get$ref(), "#/components/schemas/Entity");
        assertEquals(partySchemas.get(1).get$ref(), null);

        assertNull(party.getDiscriminator());
        assertNull(entity.getDiscriminator());
        assertNotNull(partyAllOfChildSchema.getDiscriminator());
        assertEquals(partyAllOfChildSchema.getDiscriminator().getPropertyName(), "party_type");
        assertEquals(partyAllOfChildSchema.getRequired().get(0), "party_type");

        // Contact
        ComposedSchema contact = (ComposedSchema) openAPI.getComponents().getSchemas().get("Contact");
        Schema contactAllOf = contact.getAllOf().get(1); // use the inline child schema directly

        assertEquals(contact.getExtensions().get("x-discriminator-value"), "contact");
        assertEquals(contact.getAllOf().get(0).get$ref(), "#/components/schemas/Party");
        assertEquals(contact.getAllOf().get(1).get$ref(), null);
        assertEquals(contactAllOf.getProperties().size(), 4);

        // Customer
        ComposedSchema customer = (ComposedSchema) openAPI.getComponents().getSchemas().get("Customer");
        List<Schema> customerSchemas = customer.getAllOf();
        Schema customerAllOf = ModelUtils.getReferencedSchema(openAPI, customerSchemas.get(1));

        assertEquals(customerSchemas.get(0).get$ref(), "#/components/schemas/Party");
        assertNull(customer.getDiscriminator());
        assertEquals(customer.getExtensions().get("x-discriminator-value"), "customer");

        // Discriminators are not defined at this level in the schema doc
        assertNull(customerSchemas.get(0).getDiscriminator());
        assertNull(customerSchemas.get(1).get$ref());
        assertNotNull(customerSchemas.get(1).getDiscriminator());

        // Customer -> Party where Customer defines it's own discriminator
        assertNotNull(customerAllOf.getDiscriminator());
        assertEquals(customerAllOf.getDiscriminator().getPropertyName(), "customer_type");
        assertEquals(customerAllOf.getRequired().get(0), "customer_type");

        // Person
        ComposedSchema person = (ComposedSchema) openAPI.getComponents().getSchemas().get("Person");
        List<Schema> personSchemas = person.getAllOf();
        Schema personAllOf = ModelUtils.getReferencedSchema(openAPI, personSchemas.get(1));

        // Discriminators are not defined at this level in the schema doc
        assertEquals(personSchemas.get(0).get$ref(), "#/components/schemas/Customer");
        assertNull(personSchemas.get(0).getDiscriminator());
        assertNull(personSchemas.get(1).get$ref());
        assertNull(personSchemas.get(1).getDiscriminator());
        assertEquals(2, personSchemas.get(1).getProperties().size());

        // Person -> Customer -> Party, so discriminator is not at this level
        assertNull(person.getDiscriminator());
        assertEquals(person.getExtensions().get("x-discriminator-value"), "person");
        assertNull(personAllOf.getDiscriminator());

        // Organization
        ComposedSchema organization = (ComposedSchema) openAPI.getComponents().getSchemas().get("Organization");
        List<Schema> organizationSchemas = organization.getAllOf();
        Schema organizationAllOf = organizationSchemas.get(1); // get the inline child schema directly

        // Discriminators are not defined at this level in the schema doc
        assertEquals(organizationSchemas.get(0).get$ref(), "#/components/schemas/Customer");
        assertNull(organizationSchemas.get(0).getDiscriminator());
        assertNotNull(organizationAllOf);
        assertNull(organizationAllOf.getDiscriminator());
        assertEquals(1, organizationAllOf.getProperties().size());

        // Organization -> Customer -> Party, so discriminator is not at this level
        assertNull(organizationAllOf.getDiscriminator());
        assertEquals(organization.getExtensions().get("x-discriminator-value"), "organization");
    }

    @Test
    public void arbitraryObjectModelWithArrayInlineWithTitle() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        assertTrue(openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle") instanceof ArraySchema);

        ArraySchema schema = (ArraySchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitle");
        assertTrue(schema.getItems() instanceof Schema);
        assertEquals(schema.getItems().get$ref(), "#/components/schemas/ArbitraryObjectModelWithArrayInlineWithTitleInner");

        ObjectSchema items = (ObjectSchema) openAPI.getComponents().getSchemas().get("ArbitraryObjectModelWithArrayInlineWithTitleInner");
        assertEquals("ArbitraryObjectModelWithArrayInlineWithTitleInner", items.getTitle());
        assertTrue(items.getProperties().get("arbitrary_object_model_with_array_inline_with_title_property") instanceof ObjectSchema);

        ObjectSchema itemsProperty = (ObjectSchema) items.getProperties().get("arbitrary_object_model_with_array_inline_with_title_property");
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
        Assert.assertSame(ModelUtils.getReferencedSchema(openAPI, schema.getItems()).getExample(), "");
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

        Schema nullableRequestBodySchema = ModelUtils.getReferencedSchema(openAPI, nullableRequestBodyReference);
        Schema nullableSchema = ModelUtils.getReferencedSchema(openAPI,
                ((Schema) nullableRequestBodySchema.getProperties().get("nullable_request_body_property")));
        assertTrue(nullableSchema.getNullable());
    }

    @Test
    public void callbacks() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody callbackRequestBodyReference = openAPI
                .getPaths()
                .get("/callback")
                .getPost()
                .getCallbacks()
                .get("webhook")
                .get("{$request.body#/callbackUri}")
                .getPost()
                .getRequestBody();
        assertNotNull(callbackRequestBodyReference.getContent().get("application/json").getSchema().get$ref());
        assertEquals("#/components/schemas/webhookNotify_request", callbackRequestBodyReference.getContent().get("application/json").getSchema().get$ref());

        Schema callbackRequestSchemaReference = callbackRequestBodyReference
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

    @Test
    public void testInlineSchemaNameMapping() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaNames = new HashMap<>();
        inlineSchemaNames.put("resolveInlineArrayRequestBody_request_inner", "SomethingMapped");
        inlineSchemaNames.put("arbitraryRequestBodyArrayProperty_request_inner", "nothing_new");
        resolver.setInlineSchemaNameMapping(inlineSchemaNames);
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("SomethingMapped");
        assertTrue(schema.getProperties().get("street") instanceof StringSchema);
        assertTrue(schema.getProperties().get("city") instanceof StringSchema);

        Schema nothingNew = openAPI.getComponents().getSchemas().get("nothing_new");
        assertTrue(nothingNew.getProperties().get("arbitrary_request_body_array_property") instanceof ObjectSchema);
    }

    @Test
    public void testInlineSchemaOptions() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaOptions = new HashMap<>();
        inlineSchemaOptions.put("ARRAY_ITEM_SUFFIX", "_something");
        resolver.setInlineSchemaOptions(inlineSchemaOptions);
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("resolveInlineArrayRequestBody_request_something");
        assertTrue(schema.getProperties().get("street") instanceof StringSchema);
        assertTrue(schema.getProperties().get("city") instanceof StringSchema);

        Schema nothingNew = openAPI.getComponents().getSchemas().get("arbitraryRequestBodyArrayProperty_request_something");
        assertTrue(nothingNew.getProperties().get("arbitrary_request_body_array_property") instanceof ObjectSchema);
    }

    @Test
    public void testInlineSchemaSkipReuseSetToFalse() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaOptions = new HashMap<>();
        //inlineSchemaOptions.put("SKIP_SCHEMA_REUSE", "false"); // default is false
        resolver.setInlineSchemaOptions(inlineSchemaOptions);
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("meta_200_response");
        assertTrue(schema.getProperties().get("name") instanceof StringSchema);
        assertTrue(schema.getProperties().get("id") instanceof IntegerSchema);

        // mega_200_response is NOT created since meta_200_response is reused
        Schema schema2 = openAPI.getComponents().getSchemas().get("mega_200_response");
        assertNull(schema2);
    }

    @Test
    public void testInlineSchemaSkipReuseSetToTrue() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaOptions = new HashMap<>();
        inlineSchemaOptions.put("SKIP_SCHEMA_REUSE", "true");
        resolver.setInlineSchemaOptions(inlineSchemaOptions);
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("meta_200_response");
        assertTrue(schema.getProperties().get("name") instanceof StringSchema);
        assertTrue(schema.getProperties().get("id") instanceof IntegerSchema);

        Schema schema2 = openAPI.getComponents().getSchemas().get("mega_200_response");
        assertTrue(schema2.getProperties().get("name") instanceof StringSchema);
        assertTrue(schema2.getProperties().get("id") instanceof IntegerSchema);
    }

    @Test
    public void resolveInlineRequestBodyAllOf() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        new InlineModelResolver().flatten(openAPI);

        RequestBody requestBodyReference = openAPI.getPaths().get("/resolve_inline_request_body_allof").getPost().getRequestBody();
        assertEquals("#/components/schemas/resolveInlineRequestBodyAllOf_request",
                requestBodyReference.getContent().get("application/json").getSchema().get$ref());

        ComposedSchema allOfModel = (ComposedSchema) openAPI.getComponents().getSchemas().get("resolveInlineRequestBodyAllOf_request");
        assertEquals(null, allOfModel.getAllOf().get(0).get$ref());
        assertEquals(2, allOfModel.getAllOf().get(0).getProperties().size());
    }

    @Test
    public void testInlineSchemaAllOfPropertyOfOneOf() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_allof_propertyof_oneof.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("Order_allOf_inline_oneof");
        assertEquals(((Schema) schema.getOneOf().get(0)).get$ref(), "#/components/schemas/Tag");
        assertEquals(((Schema) schema.getOneOf().get(1)).get$ref(), "#/components/schemas/Filter");

        Schema schema2 = openAPI.getComponents().getSchemas().get("Order_allOf_inline_model");
        assertTrue(schema2.getProperties().get("something") instanceof StringSchema);
    }

    @Test
    public void testNestedAnyOf() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/nested_anyof.yaml");
        InlineModelResolver resolver = new InlineModelResolver();
        resolver.flatten(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("SomeData_anyOf");
        assertTrue((Schema) schema.getAnyOf().get(0) instanceof StringSchema);
        assertTrue((Schema) schema.getAnyOf().get(1) instanceof IntegerSchema);
    }

    @Test
    public void resolveOperationInlineEnum() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        Parameter parameter = openAPI.getPaths().get("/resolve_parameter_inline_enum").getGet().getParameters().get(0);
        assertNull(((ArraySchema) parameter.getSchema()).getItems().get$ref());

        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaOptions = new HashMap<>();
        inlineSchemaOptions.put("RESOLVE_INLINE_ENUMS", "true");
        resolver.setInlineSchemaOptions(inlineSchemaOptions);
        resolver.flatten(openAPI);

        Parameter parameter2 = openAPI.getPaths().get("/resolve_parameter_inline_enum").getGet().getParameters().get(0);
        assertEquals("#/components/schemas/resolveParameterInlineEnum_status_inline_enum_parameter_inner",
                ((ArraySchema) parameter2.getSchema()).getItems().get$ref());

    }

    @Test
    public void resolveOperationInlineEnumFormParameters() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_model_resolver.yaml");
        Schema requestBody = openAPI.getPaths().get("/resolve_parameter_inline_enum_form_parameters").getPost().getRequestBody().getContent().get("application/x-www-form-urlencoded").getSchema();
        assertNull(requestBody.get$ref());

        InlineModelResolver resolver = new InlineModelResolver();
        Map<String, String> inlineSchemaOptions = new HashMap<>();
        inlineSchemaOptions.put("RESOLVE_INLINE_ENUMS", "true");
        resolver.setInlineSchemaOptions(inlineSchemaOptions);
        resolver.flatten(openAPI);

        Schema requestBody2 = openAPI.getPaths().get("/resolve_parameter_inline_enum_form_parameters").getPost().getRequestBody().getContent().get("application/x-www-form-urlencoded").getSchema();
        assertEquals("#/components/schemas/resolve_parameter_inline_enum_form_parameters_request", requestBody2.get$ref());

        Schema inlineFormParaemter = (Schema) openAPI.getComponents().getSchemas().get("resolve_parameter_inline_enum_form_parameters_request");
        assertNotNull(inlineFormParaemter);
        assertEquals(2, inlineFormParaemter.getProperties().size());
        assertEquals("#/components/schemas/resolve_parameter_inline_enum_form_parameters_request_enum_form_string",
                ((Schema) inlineFormParaemter.getProperties().get("enum_form_string")).get$ref());

    }

    @Test
    public void doNotWrapSingleAllOfRefs() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_15077.yaml");
        new InlineModelResolver().flatten(openAPI);

        // None of these cases should be wrapped in an inline schema and should reference the original schema "NumberRange"
        Schema limitsModel = (Schema) openAPI.getComponents().getSchemas().get("Limits");
        final String numberRangeRef = "#/components/schemas/NumberRange";

        Schema allOfRef = (Schema) limitsModel.getProperties().get("allOfRef");
        assertNotNull(allOfRef.getAllOf());
        assertEquals(numberRangeRef, ((Schema) allOfRef.getAllOf().get(0)).get$ref());

        Schema allOfRefWithDescription = (Schema) limitsModel.getProperties().get("allOfRefWithDescription");
        assertNotNull(allOfRefWithDescription.getAllOf());
        assertEquals(numberRangeRef, ((Schema) allOfRefWithDescription.getAllOf().get(0)).get$ref());

        Schema allOfRefWithReadonly = (Schema) limitsModel.getProperties().get("allOfRefWithReadonly");
        assertNotNull(allOfRefWithReadonly.getAllOf());
        assertEquals(numberRangeRef, ((Schema) allOfRefWithReadonly.getAllOf().get(0)).get$ref());

        Schema allOfRefWithDescriptionAndReadonly = (Schema) limitsModel.getProperties().get("allOfRefWithDescriptionAndReadonly");
        assertNotNull(allOfRefWithDescriptionAndReadonly.getAllOf());
        assertEquals(numberRangeRef, ((Schema) allOfRefWithDescriptionAndReadonly.getAllOf().get(0)).get$ref());
    }

    @Test
    public void resolveInlineModelDeduplicatesWhenParserMutatesPropertyTypes() {
        // Regression test: the Swagger Parser shares a single resolved Schema object across all
        // usages of an external type (e.g. storage-backend.json) and strips the 'type' field
        // from its properties between processing passes.  The first usage sees properties with
        // type:"string"; the second usage sees the same object but with type stripped to null.
        // IgnoreVolatileFieldsMixIn now strips 'type' in addition to 'description', so the
        // structural-hash fallback in matchGenerated() must still unify them rather than creating
        // a numbered variant.
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.setPaths(new Paths());

        // First inline schema: properties carry explicit type annotations (as delivered by the
        // parser on first encounter of the shared external schema object)
        StringSchema prop1First = new StringSchema();
        prop1First.setDescription("The store type");
        StringSchema prop2First = new StringSchema();
        prop2First.setDescription("The provider");
        Schema schemaFirstPass = new ObjectSchema()
                .title("StorageBackend")
                .description("A storage backend")
                .addProperty("store_type", prop1First)
                .addProperty("provider",   prop2First);

        // Second inline schema: same structure but 'type' has been stripped from properties,
        // simulating the Swagger Parser mutating the shared resolved Schema object between passes.
        StringSchema prop1Second = new StringSchema();
        prop1Second.setType(null);   // simulate parser stripping the type field
        prop1Second.setDescription("The store type");
        StringSchema prop2Second = new StringSchema();
        prop2Second.setType(null);
        prop2Second.setDescription("The provider");
        Schema schemaSecondPass = new ObjectSchema()
                .title("StorageBackend")
                .description("A storage backend")
                .addProperty("store_type", prop1Second)
                .addProperty("provider",   prop2Second);

        openapi.getPaths()
                .addPathItem("/a", new PathItem().get(new Operation().operationId("getA")
                        .responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("OK")
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(schemaFirstPass)))))))
                .addPathItem("/b", new PathItem().get(new Operation().operationId("getB")
                        .responses(new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("OK")
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(schemaSecondPass)))))));

        new InlineModelResolver().flatten(openapi);

        Map<String, Schema> schemas = openapi.getComponents().getSchemas();
        assertNotNull("StorageBackend schema must exist", schemas.get("StorageBackend"));
        assertNull("Duplicate StorageBackend_1 must not exist — type-stripped structural match must fire",
                schemas.get("StorageBackend_1"));
    }

    @Test
    public void deduplicateComponentsRemovesNumberedDuplicateOfTitledSchemaAndRewritesRefs() {
        // Regression test: when flattening creates a numbered duplicate of a titled component
        // (e.g. FlowSegment_1 alongside FlowSegment) because matchGenerated() missed the match
        // due to T0-vs-T1 pre-populate timing, deduplicateComponents() must remove the duplicate
        // and rewrite all $refs to it throughout the spec so the generated code only contains one
        // class.
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());
        openapi.setPaths(new Paths());

        Schema canonical = new ObjectSchema()
                .title("Widget")
                .description("A widget")
                .addProperty("name", new StringSchema());

        // Duplicate: same title and structure — simulates what flatten() can produce when the
        // pre-populate T0 signature no longer matches the T1 form of the same inline schema.
        Schema duplicate = new ObjectSchema()
                .title("Widget")
                .description("A widget")
                .addProperty("name", new StringSchema());

        openapi.getComponents().addSchemas("Widget",   canonical);
        openapi.getComponents().addSchemas("Widget_1", duplicate);

        // Path whose response references the numbered duplicate
        openapi.getPaths().addPathItem("/widgets", new PathItem().get(
                new Operation().operationId("getWidget").responses(
                        new ApiResponses().addApiResponse("200", new ApiResponse()
                                .description("OK")
                                .content(new Content().addMediaType("application/json",
                                        new MediaType().schema(new Schema<>()
                                                .$ref("#/components/schemas/Widget_1"))))))));

        new InlineModelResolver().flatten(openapi);

        Map<String, Schema> schemas = openapi.getComponents().getSchemas();
        assertNotNull("Canonical Widget must survive deduplication", schemas.get("Widget"));
        assertNull("Duplicate Widget_1 must be removed by deduplicateComponents()", schemas.get("Widget_1"));

        // The $ref in the path response must have been rewritten to the canonical name
        Schema responseSchema = openapi.getPaths().get("/widgets").getGet()
                .getResponses().get("200").getContent().get("application/json").getSchema();
        assertEquals("$ref must be rewritten from Widget_1 to Widget",
                "#/components/schemas/Widget", responseSchema.get$ref());
    }

    @Test
    public void deduplicateComponentsKeepsDistinctlyNamedUserSchemas() {
        // Regression test for #24177: two component schemas the user authored with distinct,
        // non-numbered names (upper1 / upper2) that happen to be structurally identical AND share
        // the same title must NOT be merged. deduplicateComponents() only collapses numbered
        // duplicates generated by the parser/inline resolver (Foo_1 alongside Foo); user-authored
        // named types are the source of truth and must both survive.
        OpenAPI openapi = new OpenAPI();
        openapi.setComponents(new Components());

        Schema upper1 = new ObjectSchema()
                .title("foo title identical by purpose")
                .description("foo")
                .addProperty("foo", new StringSchema());
        Schema upper2 = new ObjectSchema()
                .title("foo title identical by purpose")
                .description("foo")
                .addProperty("foo", new StringSchema());

        openapi.getComponents().addSchemas("upper1", upper1);
        openapi.getComponents().addSchemas("upper2", upper2);

        new InlineModelResolver().flatten(openapi);

        Map<String, Schema> schemas = openapi.getComponents().getSchemas();
        assertNotNull("upper1 must survive", schemas.get("upper1"));
        assertNotNull("upper2 must survive — distinctly-named user schemas must not be deduplicated",
                schemas.get("upper2"));
    }
}
