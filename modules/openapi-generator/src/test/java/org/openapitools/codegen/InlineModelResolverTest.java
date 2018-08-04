package org.openapitools.codegen;

import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.core.util.Json;
import org.apache.commons.lang3.StringUtils;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.AssertJUnit.*;
/*
@SuppressWarnings("static-method")
public class InlineModelResolverTest {
    @Test
    public void resolveInlineModelTestWithoutTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("address", new ObjectProperty()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));

        new InlineModelResolver().flatten(openapi);

        ModelImpl user = (ModelImpl)openapi.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)openapi.getDefinitions().get("User_address");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModelTestWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("UserAddressTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));

        new InlineModelResolver().flatten(openapi);

        ModelImpl user = (ModelImpl)openapi.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)openapi.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }    
    
    @Test
    public void resolveInlineModel2EqualInnerModels() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("UserAddressTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));
        
        openapi.addDefinition("AnotherUser", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("lastName", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("UserAddressTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));        

        new InlineModelResolver().flatten(openapi);

        ModelImpl user = (ModelImpl)openapi.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)openapi.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        ModelImpl duplicateAddress = (ModelImpl)openapi.getDefinitions().get("UserAddressTitle_0");
        assertNull(duplicateAddress);
    }        

    @Test
    public void resolveInlineModel2DifferentInnerModelsWIthSameTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("UserAddressTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));
        
        openapi.addDefinition("AnotherUser", new ModelImpl()
                .name("AnotherUser")
                .description("a common user")
                .property("name", new StringProperty())
                .property("lastName", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("UserAddressTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())
                .property("apartment", new StringProperty())));

        new InlineModelResolver().flatten(openapi);

        ModelImpl user = (ModelImpl)openapi.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)openapi.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        ModelImpl duplicateAddress = (ModelImpl)openapi.getDefinitions().get("UserAddressTitle_1");
        assertNotNull(duplicateAddress);
        assertNotNull(duplicateAddress.getProperties().get("city"));
        assertNotNull(duplicateAddress.getProperties().get("street"));
        assertNotNull(duplicateAddress.getProperties().get("apartment"));
    }        
    
    
    @Test
    public void testInlineResponseModel() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectProperty()
                                    .property("name", new StringProperty()).vendorExtension("x-ext", "ext-prop")))))
        .path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(new ObjectProperty()
                                        .property("name", new StringProperty()).vendorExtension("x-ext", "ext-prop")))));
        new InlineModelResolver().flatten(openapi);

        Map<String, Response> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        Property schema = response.getSchema();
        assertTrue(schema instanceof RefProperty);
        assertEquals(1, schema.getVendorExtensions().size());
        assertEquals("ext-prop", schema.getVendorExtensions().get("x-ext"));

        ModelImpl model = (ModelImpl)openapi.getDefinitions().get("inline_response_200");
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringProperty);
    }

    
    @Test
    public void testInlineResponseModelWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        String responseTitle = "GetBarResponse";
    openapi.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectProperty().title(responseTitle)
                                    .property("name", new StringProperty())))))
        .path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(new ObjectProperty()
                                        .property("name", new StringProperty())))));
        new InlineModelResolver().flatten(openapi);

        Map<String, Response> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        assertTrue(response.getSchema() instanceof RefProperty);

        ModelImpl model = (ModelImpl)openapi.getDefinitions().get(responseTitle);
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringProperty);
    }
    
    
    @Test
    public void resolveInlineArrayModelWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        .title("InnerUserTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));

        new InlineModelResolver().flatten(openapi);

        Model model = openapi.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);

        Model user = openapi.getDefinitions().get("InnerUserTitle");
        assertNotNull(user);
        assertEquals("description", user.getDescription());
    }
    
    @Test
    public void resolveInlineArrayModelWithoutTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));

        new InlineModelResolver().flatten(openapi);

        Model model = openapi.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);

        Model user = openapi.getDefinitions().get("User_inner");
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
                                .schema(new ModelImpl()
                                        .property("address", new ObjectProperty()
                                            .property("street", new StringProperty()))
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("address"));
    }

    @Test
    public void resolveInlineBodyParameterWithRequired() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .property("address", new ObjectProperty()
                                                .property("street", new StringProperty()
                                                        .required(true))
                                                .required(true))
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("address"));

        Property addressProperty = impl.getProperties().get("address");
        assertTrue(addressProperty instanceof RefProperty);
        assertTrue(addressProperty.getRequired());

        Model helloAddress = openapi.getDefinitions().get("hello_address");
        assertTrue(helloAddress instanceof ModelImpl);

        ModelImpl addressImpl = (ModelImpl) helloAddress;
        assertNotNull(addressImpl);

        Property streetProperty = addressImpl.getProperties().get("street");
        assertTrue(streetProperty instanceof  StringProperty);
        assertTrue(streetProperty.getRequired());
    }
    
    @Test
    public void resolveInlineBodyParameterWithTitle() throws Exception {
        OpenAPI openapi = new OpenAPI();

        ModelImpl addressModelItem = new ModelImpl();
        String addressModelName = "DetailedAddress";
    addressModelItem.setTitle(addressModelName);
    openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(addressModelItem
                                        .property("address", new ObjectProperty()
                                            .property("street", new StringProperty()))
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getDefinitions().get(addressModelName);
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("address"));
    }    

    @Test
    public void notResolveNonModelBodyParameter() throws Exception {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .type("string")
                                        .format("binary")))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ModelImpl);
        ModelImpl m = (ModelImpl) bp.getSchema();
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
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty()
                                            .property("address", new ObjectProperty()
                                                .property("street", new StringProperty())))))));

        new InlineModelResolver().flatten(openapi);

        Parameter param = openapi.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof ArrayModel);

        ArrayModel am = (ArrayModel) schema;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        RefProperty rp = (RefProperty) inner;

        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/body");
        assertEquals(rp.getSimpleRef(), "body");

        Model inline = openapi.getDefinitions().get("body");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        RefProperty rpAddress = (RefProperty) impl.getProperties().get("address");
        assertNotNull(rpAddress);
        assertEquals(rpAddress.getType(), "ref");
        assertEquals(rpAddress.get$ref(), "#/definitions/hello_address");
        assertEquals(rpAddress.getSimpleRef(), "hello_address");

        Model inlineProp = openapi.getDefinitions().get("hello_address");
        assertNotNull(inlineProp);
        assertTrue(inlineProp instanceof ModelImpl);
        ModelImpl implProp = (ModelImpl) inlineProp;
        assertNotNull(implProp.getProperties().get("street"));
        assertTrue(implProp.getProperties().get("street") instanceof StringProperty);
    }

    @Test
    public void resolveInlineArrayResponse() throws Exception {
        OpenAPI openapi = new OpenAPI();

        ArrayProperty schema = new ArrayProperty()
                .items(new ObjectProperty()
                        .property("name", new StringProperty())
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

        RefProperty rp = (RefProperty) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");
        assertEquals(1, rp.getVendorExtensions().size());
        assertEquals("ext-items", rp.getVendorExtensions().get("x-ext"));

        Model inline = openapi.getDefinitions().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
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
                        .items(new ObjectProperty()
                            .title("FooBar")
                            .property("name", new StringProperty()))))));

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

        RefProperty rp = (RefProperty) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/"+ "FooBar");
        assertEquals(rp.getSimpleRef(), "FooBar");

        Model inline = openapi.getDefinitions().get("FooBar");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }
    
    @Test
    public void testInlineMapResponse() throws Exception {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new StringProperty());
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
        assertTrue(openapi.getDefinitions().size() == 0);
        assertEquals(1, property.getVendorExtensions().size());
        assertEquals("ext-prop", property.getVendorExtensions().get("x-ext"));
    }

    @Test
    public void testInlineMapResponseWithObjectProperty() throws Exception {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectProperty()
                .property("name", new StringProperty()));
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
        assertTrue(openapi.getDefinitions().size() == 1);

        Model inline = openapi.getDefinitions().get("inline_response_200");
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void testArrayResponse() {
        OpenAPI openapi = new OpenAPI();

        ArrayProperty schema = new ArrayProperty();
        schema.setItems(new ObjectProperty()
                .property("name", new StringProperty()));

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
        assertTrue(items instanceof RefProperty);
        RefProperty rp = (RefProperty) items;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = openapi.getDefinitions().get("inline_response_200");
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void testBasicInput() {
        OpenAPI openapi = new OpenAPI();

        ModelImpl user = new ModelImpl()
                .property("name", new StringProperty());

        openapi.path("/foo/baz", new Path()
                .post(new Operation()
                        .parameter(new BodyParameter()
                            .name("myBody")
                            .schema(new RefModel("User")))));

        openapi.addDefinition("User", user);

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
                                .schema(new ModelImpl()))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ModelImpl);
        ModelImpl m = (ModelImpl) bp.getSchema();
        assertNull(m.getType());
    }

    @Test
    public void testArbitraryObjectBodyParamInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .property("arbitrary", new ObjectProperty())))));

        new InlineModelResolver().flatten(openapi);

        Operation operation = openapi.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = openapi.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectProperty);
    }

    @Test
    public void testArbitraryObjectBodyParamWithArray() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty())))));

        new InlineModelResolver().flatten(openapi);

        Parameter param = openapi.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof ArrayModel);

        ArrayModel am = (ArrayModel) schema;
        Property inner = am.getItems();
        assertTrue(inner instanceof ObjectProperty);

        ObjectProperty op = (ObjectProperty) inner;
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
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty()
                                            .property("arbitrary", new ObjectProperty()))))));

        new InlineModelResolver().flatten(openapi);

        Parameter param = openapi.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof ArrayModel);

        ArrayModel am = (ArrayModel) schema;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        RefProperty rp = (RefProperty) inner;

        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/body");
        assertEquals(rp.getSimpleRef(), "body");

        Model inline = openapi.getDefinitions().get("body");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectProperty);
    }

    @Test
    public void testArbitraryObjectResponse() {
        OpenAPI openapi = new OpenAPI();

        openapi.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectProperty()))));
        new InlineModelResolver().flatten(openapi);

        Map<String, Response> responses = openapi.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        assertTrue(response.getSchema() instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) response.getSchema();
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
                                        .items(new ObjectProperty())))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertTrue(response.getSchema() instanceof ArrayProperty);

        ArrayProperty am = (ArrayProperty) response.getSchema();
        Property items = am.getItems();
        assertTrue(items instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) items;
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
                                        .items(new ObjectProperty()
                                            .property("arbitrary", new ObjectProperty()))))));

        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertNotNull(response);

        assertNotNull(response.getSchema());
        Property responseProperty = response.getSchema();
        assertTrue(responseProperty instanceof ArrayProperty);

        ArrayProperty ap = (ArrayProperty) responseProperty;
        Property p = ap.getItems();
        assertNotNull(p);

        RefProperty rp = (RefProperty) p;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = openapi.getDefinitions().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        Property inlineProp = impl.getProperties().get("arbitrary");
        assertNotNull(inlineProp);
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseMapInline() {
        OpenAPI openapi = new OpenAPI();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectProperty());

        openapi.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(openapi);

        Response response = openapi.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertTrue(openapi.getDefinitions().size() == 0);
        Property inlineProp = ((MapProperty) property).getAdditionalProperties();
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelInline() {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("arbitrary", new ObjectProperty()
                        .title("title")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")));

        new InlineModelResolver().flatten(openapi);

        ModelImpl user = (ModelImpl)openapi.getDefinitions().get("User");
        assertNotNull(user);
        Property inlineProp = user.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelWithArrayInlineWithoutTitle() {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("arbitrary", new ObjectProperty())));

        new InlineModelResolver().flatten(openapi);

        Model model = openapi.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);
        ArrayModel am = (ArrayModel) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        ModelImpl userInner = (ModelImpl)openapi.getDefinitions().get("User_inner");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }
    
    @Test
    public void testArbitraryObjectModelWithArrayInlineWithTitle() {
        OpenAPI openapi = new OpenAPI();

        openapi.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        .title("InnerUserTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("arbitrary", new ObjectProperty())));

        new InlineModelResolver().flatten(openapi);

        Model model = openapi.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);
        ArrayModel am = (ArrayModel) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        ModelImpl userInner = (ModelImpl)openapi.getDefinitions().get("InnerUserTitle");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testEmptyExampleOnStrinngTypeModels() {
        OpenAPI openapi = new OpenAPI();

        RefProperty refProperty = new RefProperty();
        refProperty.set$ref("#/definitions/Test");

        openapi.path("/hello", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .schema(new ArrayProperty()
                                        .items(refProperty)))));

        openapi.addDefinition("Test", new ModelImpl()
                .example(StringUtils.EMPTY)
                .type("string"));
        new InlineModelResolver().flatten(openapi);
    }
}
*/