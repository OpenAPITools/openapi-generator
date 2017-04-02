package io.swagger.codegen;

import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.*;
import io.swagger.util.Json;
import org.testng.annotations.Test;

import java.util.Map;

import static org.testng.AssertJUnit.*;

@SuppressWarnings("static-method")
public class InlineModelResolverTest {
    @Test
    public void resolveInlineModelTestWithoutTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
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

        new InlineModelResolver().flatten(swagger);

        ModelImpl user = (ModelImpl)swagger.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)swagger.getDefinitions().get("User_address");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }

    @Test
    public void resolveInlineModelTestWithTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
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

        new InlineModelResolver().flatten(swagger);

        ModelImpl user = (ModelImpl)swagger.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)swagger.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
    }    
    
    @Test
    public void resolveInlineModel2EqualInnerModels() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
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
        
        swagger.addDefinition("AnotherUser", new ModelImpl()
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

        new InlineModelResolver().flatten(swagger);

        ModelImpl user = (ModelImpl)swagger.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)swagger.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        ModelImpl duplicateAddress = (ModelImpl)swagger.getDefinitions().get("UserAddressTitle_0");
        assertNull(duplicateAddress);
    }        

    @Test
    public void resolveInlineModel2DifferentInnerModelsWIthSameTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
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
        
        swagger.addDefinition("AnotherUser", new ModelImpl()
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

        new InlineModelResolver().flatten(swagger);

        ModelImpl user = (ModelImpl)swagger.getDefinitions().get("User");

        assertNotNull(user);
        assertTrue(user.getProperties().get("address") instanceof RefProperty);

        ModelImpl address = (ModelImpl)swagger.getDefinitions().get("UserAddressTitle");
        assertNotNull(address);
        assertNotNull(address.getProperties().get("city"));
        assertNotNull(address.getProperties().get("street"));
        ModelImpl duplicateAddress = (ModelImpl)swagger.getDefinitions().get("UserAddressTitle_1");
        assertNotNull(duplicateAddress);
        assertNotNull(duplicateAddress.getProperties().get("city"));
        assertNotNull(duplicateAddress.getProperties().get("street"));
        assertNotNull(duplicateAddress.getProperties().get("apartment"));
    }        
    
    
    @Test
    public void testInlineResponseModel() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/foo/bar", new Path()
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
        new InlineModelResolver().flatten(swagger);

        Map<String, Response> responses = swagger.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        Property schema = response.getSchema();
        assertTrue(schema instanceof RefProperty);
        assertEquals(1, schema.getVendorExtensions().size());
        assertEquals("ext-prop", schema.getVendorExtensions().get("x-ext"));

        ModelImpl model = (ModelImpl)swagger.getDefinitions().get("inline_response_200");
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringProperty);
    }

    
    @Test
    public void testInlineResponseModelWithTitle() throws Exception {
        Swagger swagger = new Swagger();

        String responseTitle = "GetBarResponse";
    swagger.path("/foo/bar", new Path()
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
        new InlineModelResolver().flatten(swagger);

        Map<String, Response> responses = swagger.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        assertTrue(response.getSchema() instanceof RefProperty);

        ModelImpl model = (ModelImpl)swagger.getDefinitions().get(responseTitle);
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringProperty);
    }
    
    
    @Test
    public void resolveInlineArrayModelWithTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ArrayModel()
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

        new InlineModelResolver().flatten(swagger);

        Model model = swagger.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);

        Model user = swagger.getDefinitions().get("InnerUserTitle");
        assertNotNull(user);
        assertEquals("description", user.getDescription());
    }
    
    @Test
    public void resolveInlineArrayModelWithoutTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("street", new StringProperty())
                        .property("city", new StringProperty())));

        new InlineModelResolver().flatten(swagger);

        Model model = swagger.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);

        Model user = swagger.getDefinitions().get("User_inner");
        assertNotNull(user);
        assertEquals("description", user.getDescription());
    }    
    
    
    

    @Test
    public void resolveInlineBodyParameter() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .property("address", new ObjectProperty()
                                            .property("street", new StringProperty()))
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = swagger.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("address"));
    }
    
    @Test
    public void resolveInlineBodyParameterWithTitle() throws Exception {
        Swagger swagger = new Swagger();

        ModelImpl addressModelItem = new ModelImpl();
        String addressModelName = "DetailedAddress";
    addressModelItem.setTitle(addressModelName);
    swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(addressModelItem
                                        .property("address", new ObjectProperty()
                                            .property("street", new StringProperty()))
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = swagger.getDefinitions().get(addressModelName);
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("address"));
    }    

    @Test
    public void notResolveNonModelBodyParameter() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .type("string")
                                        .format("binary")))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ModelImpl);
        ModelImpl m = (ModelImpl) bp.getSchema();
        assertEquals("string", m.getType());
        assertEquals("binary", m.getFormat());
    }

    @Test
    public void resolveInlineArrayBodyParameter() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty()
                                            .property("address", new ObjectProperty()
                                                .property("street", new StringProperty())))))));

        new InlineModelResolver().flatten(swagger);

        Parameter param = swagger.getPaths().get("/hello").getGet().getParameters().get(0);
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

        Model inline = swagger.getDefinitions().get("body");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        RefProperty rpAddress = (RefProperty) impl.getProperties().get("address");
        assertNotNull(rpAddress);
        assertEquals(rpAddress.getType(), "ref");
        assertEquals(rpAddress.get$ref(), "#/definitions/hello_address");
        assertEquals(rpAddress.getSimpleRef(), "hello_address");

        Model inlineProp = swagger.getDefinitions().get("hello_address");
        assertNotNull(inlineProp);
        assertTrue(inlineProp instanceof ModelImpl);
        ModelImpl implProp = (ModelImpl) inlineProp;
        assertNotNull(implProp.getProperties().get("street"));
        assertTrue(implProp.getProperties().get("street") instanceof StringProperty);
    }

    @Test
    public void resolveInlineArrayResponse() throws Exception {
        Swagger swagger = new Swagger();

        ArrayProperty schema = new ArrayProperty()
                .items(new ObjectProperty()
                        .property("name", new StringProperty())
                        .vendorExtension("x-ext", "ext-items"))
                .vendorExtension("x-ext", "ext-prop");
        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));

        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
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

        Model inline = swagger.getDefinitions().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void resolveInlineArrayResponseWithTitle() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/foo/baz", new Path()
            .get(new Operation()
                .response(200, new Response()
                    .vendorExtension("x-foo", "bar")
                    .description("it works!")
                    .schema(new ArrayProperty()
                        .items(new ObjectProperty()
                            .title("FooBar")
                            .property("name", new StringProperty()))))));

        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
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

        Model inline = swagger.getDefinitions().get("FooBar");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }
    
    @Test
    public void testInlineMapResponse() throws Exception {
        Swagger swagger = new Swagger();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new StringProperty());
        schema.setVendorExtension("x-ext", "ext-prop");

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(swagger);
        Json.prettyPrint(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertTrue(swagger.getDefinitions().size() == 0);
        assertEquals(1, property.getVendorExtensions().size());
        assertEquals("ext-prop", property.getVendorExtensions().get("x-ext"));
    }

    @Test
    public void testInlineMapResponseWithObjectProperty() throws Exception {
        Swagger swagger = new Swagger();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectProperty()
                .property("name", new StringProperty()));
        schema.setVendorExtension("x-ext", "ext-prop");

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertEquals(1, property.getVendorExtensions().size());
        assertEquals("ext-prop", property.getVendorExtensions().get("x-ext"));
        assertTrue(swagger.getDefinitions().size() == 1);

        Model inline = swagger.getDefinitions().get("inline_response_200");
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void testArrayResponse() {
        Swagger swagger = new Swagger();

        ArrayProperty schema = new ArrayProperty();
        schema.setItems(new ObjectProperty()
                .property("name", new StringProperty()));

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertTrue(response.getSchema() instanceof ArrayProperty);

        ArrayProperty am = (ArrayProperty) response.getSchema();
        Property items = am.getItems();
        assertTrue(items instanceof RefProperty);
        RefProperty rp = (RefProperty) items;
        assertEquals(rp.getType(), "ref");
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = swagger.getDefinitions().get("inline_response_200");
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        assertNotNull(impl.getProperties().get("name"));
        assertTrue(impl.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void testBasicInput() {
        Swagger swagger = new Swagger();

        ModelImpl user = new ModelImpl()
                .property("name", new StringProperty());

        swagger.path("/foo/baz", new Path()
                .post(new Operation()
                        .parameter(new BodyParameter()
                            .name("myBody")
                            .schema(new RefModel("User")))));

        swagger.addDefinition("User", user);

        new InlineModelResolver().flatten(swagger);

        Json.prettyPrint(swagger);
    }

    @Test
    public void testArbitraryObjectBodyParam() {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof ModelImpl);
        ModelImpl m = (ModelImpl) bp.getSchema();
        assertNull(m.getType());
    }

    @Test
    public void testArbitraryObjectBodyParamInline() {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ModelImpl()
                                        .property("arbitrary", new ObjectProperty())))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = swagger.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectProperty);
    }

    @Test
    public void testArbitraryObjectBodyParamWithArray() {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty())))));

        new InlineModelResolver().flatten(swagger);

        Parameter param = swagger.getPaths().get("/hello").getGet().getParameters().get(0);
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
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArrayModel()
                                        .items(new ObjectProperty()
                                            .property("arbitrary", new ObjectProperty()))))));

        new InlineModelResolver().flatten(swagger);

        Parameter param = swagger.getPaths().get("/hello").getGet().getParameters().get(0);
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

        Model inline = swagger.getDefinitions().get("body");
        assertNotNull(inline);
        assertTrue(inline instanceof ModelImpl);
        ModelImpl impl = (ModelImpl) inline;
        Property p = impl.getProperties().get("arbitrary");
        assertNotNull(p);
        assertTrue(p instanceof ObjectProperty);
    }

    @Test
    public void testArbitraryObjectResponse() {
        Swagger swagger = new Swagger();

        swagger.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectProperty()))));
        new InlineModelResolver().flatten(swagger);

        Map<String, Response> responses = swagger.getPaths().get("/foo/bar").getGet().getResponses();

        Response response = responses.get("200");
        assertNotNull(response);
        assertTrue(response.getSchema() instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) response.getSchema();
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseArray() {
        Swagger swagger = new Swagger();

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .description("it works!")
                                .schema(new ArrayProperty()
                                        .items(new ObjectProperty())))));
        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
        assertTrue(response.getSchema() instanceof ArrayProperty);

        ArrayProperty am = (ArrayProperty) response.getSchema();
        Property items = am.getItems();
        assertTrue(items instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) items;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectResponseArrayInline() {
        Swagger swagger = new Swagger();

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(new ArrayProperty()
                                        .items(new ObjectProperty()
                                            .property("arbitrary", new ObjectProperty()))))));

        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");
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

        Model inline = swagger.getDefinitions().get("inline_response_200");
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
        Swagger swagger = new Swagger();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectProperty());

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .description("it works!")
                                .schema(schema))));
        new InlineModelResolver().flatten(swagger);

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        assertTrue(swagger.getDefinitions().size() == 0);
        Property inlineProp = ((MapProperty) property).getAdditionalProperties();
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelInline() {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
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

        new InlineModelResolver().flatten(swagger);

        ModelImpl user = (ModelImpl)swagger.getDefinitions().get("User");
        assertNotNull(user);
        Property inlineProp = user.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }

    @Test
    public void testArbitraryObjectModelWithArrayInlineWithoutTitle() {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("arbitrary", new ObjectProperty())));

        new InlineModelResolver().flatten(swagger);

        Model model = swagger.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);
        ArrayModel am = (ArrayModel) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        ModelImpl userInner = (ModelImpl)swagger.getDefinitions().get("User_inner");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }
    
    @Test
    public void testArbitraryObjectModelWithArrayInlineWithTitle() {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        .title("InnerUserTitle")
                        ._default("default")
                        .access("access")
                        .readOnly(false)
                        .required(true)
                        .description("description")
                        .name("name")
                        .property("arbitrary", new ObjectProperty())));

        new InlineModelResolver().flatten(swagger);

        Model model = swagger.getDefinitions().get("User");
        assertTrue(model instanceof ArrayModel);
        ArrayModel am = (ArrayModel) model;
        Property inner = am.getItems();
        assertTrue(inner instanceof RefProperty);

        ModelImpl userInner = (ModelImpl)swagger.getDefinitions().get("InnerUserTitle");
        assertNotNull(userInner);
        Property inlineProp = userInner.getProperties().get("arbitrary");
        assertTrue(inlineProp instanceof ObjectProperty);
        ObjectProperty op = (ObjectProperty) inlineProp;
        assertNull(op.getProperties());
    }    
}
