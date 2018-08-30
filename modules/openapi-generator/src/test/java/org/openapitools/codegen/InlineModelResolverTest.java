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

import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.apache.commons.lang3.StringUtils;
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


    
    /*
    @Test
    public void resolveInlineArraySchemaWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.getComponents().addSchemas("User", new ArraySchema()
                .items(new ObjectSchema()
                        .title("InnerUserTitle")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema model = openapi.getComponents().getSchemas().get("User");
        assertTrue(model instanceof ArraySchema);

        Schema user = openapi.getComponents().getSchemas().get("InnerUserTitle");
        assertNotNull(user);
        assertEquals("description", user.getDescription());
    }
/*
    @Test
    public void resolveInlineArraySchemaWithoutTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.getComponents().addSchemas("User", new ArraySchema()
                .items(new ObjectSchema()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .addProperties("street", new StringSchema())
                        .addProperties("city", new StringSchema())));

        new InlineModelResolver().flatten(openapi);

        Schema model = openapi.getComponents().getSchemas().get("User");
        assertTrue(model instanceof ArraySchema);

        Model user = openapi.getComponents().getSchemas().get("User_inner");
        assertNotNull(user);
        assertEquals("description", user.getDescription());
    }    
    
    
    

    @Test
    public void resolveInlineBodyParameter() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ObjectSchema()
                                        .addProperties("address", new ObjectSchema()
                                            .addProperties("street", new StringSchema()))
                                        .addProperties("name", new StringSchema())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getComponents().getSchemas().get("body");
        assertTrue(body instanceof ObjectSchema);

        ObjectSchema impl = (ObjectSchema) body;
        assertNotNull(impl.getProperties().get("address"));
    }

    @Test
    public void resolveInlineBodyParameterWithRequired() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ObjectSchema()
                                        .addProperties("address", new ObjectSchema()
                                                .addProperties("street", new StringSchema()
                                                        .required(true))
                                                .required(true))
                                        .addProperties("name", new StringSchema())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getComponents().getSchemas().get("body");
        assertTrue(body instanceof ObjectSchema);

        ObjectSchema impl = (ObjectSchema) body;
        assertNotNull(impl.getProperties().get("address"));

        Property addressProperty = impl.getProperties().get("address");
        assertTrue(addressProperty instanceof Schema);
        assertTrue(addressProperty.getRequired());

        Model helloAddress = openapi.getComponents().getSchemas().get("hello_address");
        assertTrue(helloAddress instanceof ObjectSchema);

        ObjectSchema addressImpl = (ObjectSchema) helloAddress;
        assertNotNull(addressImpl);

        Property streetProperty = addressImpl.getProperties().get("street");
        assertTrue(streetProperty instanceof  StringSchema);
        assertTrue(streetProperty.getRequired());
    }
    
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

    @Test
    public void notResolveNonModelBodyParameter() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ObjectSchema()
                                        .type("string")
                                        .format("binary")))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ObjectSchema);
        ObjectSchema m = (ObjectSchema) bp.getSchema();
        assertEquals("string", m.getType());
        assertEquals("binary", m.getFormat());
    }

    @Test
    public void resolveInlineArrayBodyParameter() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArraySchema()
                                        .items(new ObjectSchema()
                                            .addProperties("address", new ObjectSchema()
                                                .addProperties("street", new StringSchema())))))));

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
        Schema rpAddress = (Schema) impl.getProperties().get("address");
        assertNotNull(rpAddress);
        assertEquals(rpAddress.getType(), "ref");
        assertEquals(rpAddress.get$ref(), "#/definitions/hello_address");
        assertEquals(rpAddress.getSimpleRef(), "hello_address");

        Model inlineProp = openapi.getComponents().getSchemas().get("hello_address");
        assertNotNull(inlineProp);
        assertTrue(inlineProp instanceof ObjectSchema);
        ObjectSchema implProp = (ObjectSchema) inlineProp;
        assertNotNull(implProp.getProperties().get("street"));
        assertTrue(implProp.getProperties().get("street") instanceof StringSchema);
    }

    @Test
    public void resolveInlineArrayResponse() throws Exception {
        OpenAPI openapi = new OpenAPI();

        ArrayProperty schema = new ArrayProperty()
                .items(new ObjectSchema()
                        .addProperties("name", new StringSchema())
                        .vendorExtension("x-ext", "ext-items"))
                .vendorExtension("x-ext", "ext-prop");
        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));

        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertNotNull(response);

        assertNotNull(response.getSchema());
        Property responseProperty = response.getSchema();

        // no need to flatten more
        assertTrue(responseProperty instanceof ArrayProperty);

        ArrayProperty ap = (ArrayProperty) responseProperty;
        assertEquals(1, ap.getVendorExtensions().size());
        assertEquals("ext-prop", ap.getVendorExtensions().get("x-ext"));
        
        Property p = ap.getItems();

        assertNotNull(p);

        Schema rp = (Schema) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");
        assertEquals(1, rp.getVendorExtensions().size());
        assertEquals("ext-items", rp.getVendorExtensions().get("x-ext"));

        Model inline = openapi.getComponents().getSchemas().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringSchema);
    }

    @Test
    public void resolveInlineArrayResponseWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/baz", new Path()
            .get(new Operation()
                .response(200, new Response()
                    .vendorExtension("x-foo", "bar")
                    .description("it works!")
                    .schema(new ArrayProperty()
                        .items(new ObjectSchema()
                            .title("FooBar")
                            .addProperties("name", new StringSchema()))))));

        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertNotNull(response);

        assertNotNull(response.getSchema());
        Property responseProperty = response.getSchema();

        // no need to flatten more
        assertTrue(responseProperty instanceof ArrayProperty);

        ArrayProperty ap = (ArrayProperty) responseProperty;
        Property p = ap.getItems();

        assertNotNull(p);

        Schema rp = (Schema) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/"+ "FooBar");
        assertEquals(rp.getSimpleRef(), "FooBar");

        Model inline = openapi.getComponents().getSchemas().get("FooBar");
        assertNotNull(inline);
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringSchema);
    }
    
    @Test
    public void testInlineMapResponse() throws Exception {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new StringSchema());
        schema.setVendorExtension("x-ext", "ext-prop");

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(openapi);
        Json.prettyPrint(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertTrue(openapi.getComponents().getSchemas().size() == 0);
        assertEquals(1, property.getVendorExtensions().size());
        assertEquals("ext-prop", property.getVendorExtensions().get("x-ext"));
    }

    @Test
    public void testInlineMapResponseWithObjectSchema() throws Exception {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectSchema()
                .addProperties("name", new StringSchema()));
        schema.setVendorExtension("x-ext", "ext-prop");

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertEquals(1, property.getVendorExtensions().size());
        assertEquals("ext-prop", property.getVendorExtensions().get("x-ext"));
        assertTrue(openapi.getComponents().getSchemas().size() == 1);

        Model inline = openapi.getComponents().getSchemas().get("inline_response_200");
        assertTrue(inline instanceof ObjectSchema);
        ObjectSchema impl = (ObjectSchema) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringSchema);
    }

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