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
    public void resolveInlineModelTest() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ModelImpl()
                .name("user")
                .description("a common user")
                .property("name", new StringProperty())
                .property("address", new ObjectProperty()
                        .title("title")
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
    public void testInlineResponseModel() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/foo/bar", new Path()
            .get(new Operation()
                    .response(200, new Response()
                            .description("it works!")
                            .schema(new ObjectProperty()
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

        ModelImpl model = (ModelImpl)swagger.getDefinitions().get("inline_response_200");
        assertTrue(model.getProperties().size() == 1);
        assertNotNull(model.getProperties().get("name"));
        assertTrue(model.getProperties().get("name") instanceof StringProperty);
    }

    @Test
    public void resolveInlineArrayModel() throws Exception {
        Swagger swagger = new Swagger();

        swagger.addDefinition("User", new ArrayModel()
                .items(new ObjectProperty()
                        .title("title")
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

        swagger.path("/foo/baz", new Path()
                .get(new Operation()
                        .response(200, new Response()
                                .vendorExtension("x-foo", "bar")
                                .description("it works!")
                                .schema(new ArrayProperty()
                                        .items(
                                                new ObjectProperty()
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
        assertEquals(rp.get$ref(), "#/definitions/inline_response_200");
        assertEquals(rp.getSimpleRef(), "inline_response_200");

        Model inline = swagger.getDefinitions().get("inline_response_200");
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
    }

    @Test
    public void testInlineMapResponseWithObjectProperty() throws Exception {
        Swagger swagger = new Swagger();

        MapProperty schema = new MapProperty();
        schema.setAdditionalProperties(new ObjectProperty()
                .property("name", new StringProperty()));

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
}
