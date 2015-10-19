package io.swagger.codegen;


import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.*;
import org.testng.annotations.Test;

import java.util.Map;

import static org.testng.AssertJUnit.*;

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
                                        .property("name", new StringProperty())))));

        new InlineModelResolver().flatten(swagger);

        Operation operation = swagger.getPaths().get("/hello").getGet();
        BodyParameter bp = (BodyParameter)operation.getParameters().get(0);
        assertTrue(bp.getSchema() instanceof RefModel);

        Model body = swagger.getDefinitions().get("body");
        assertTrue(body instanceof ModelImpl);

        ModelImpl impl = (ModelImpl) body;
        assertNotNull(impl.getProperties().get("name"));
    }

    @Test
    public void resolveInlineArrayBodyParameter() throws Exception {
        Swagger swagger = new Swagger();

        swagger.path("/hello", new Path()
                .get(new Operation()
                        .parameter(new BodyParameter()
                                .name("body")
                                .schema(new ArrayModel()
                                        .items(new StringProperty())))));

        new InlineModelResolver().flatten(swagger);

        Parameter param = swagger.getPaths().get("/hello").getGet().getParameters().get(0);
        assertTrue(param instanceof BodyParameter);

        BodyParameter bp = (BodyParameter) param;
        Model schema = bp.getSchema();

        assertTrue(schema instanceof RefModel);

        Model model = swagger.getDefinitions().get("body");
        assertTrue(model instanceof ArrayModel);
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
        assertTrue(responseProperty instanceof RefProperty);

        Model inline = swagger.getDefinitions().get("inline_response_200");
        assertNotNull(inline);
        assertTrue(inline instanceof ArrayModel);

        ArrayModel am = (ArrayModel) inline;
        assertTrue(am.getItems() instanceof RefProperty);

        Model inlineInner = swagger.getDefinitions().get("inline_response_200_inner");
        assertNotNull(inlineInner);
        assertTrue(inlineInner instanceof ModelImpl);

        ModelImpl innerModel = (ModelImpl) inlineInner;
        assertTrue(innerModel.getProperties().size() == 1);
        assertNotNull(innerModel.getProperties().get("name"));
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

        Response response = swagger.getPaths().get("/foo/baz").getGet().getResponses().get("200");

        Property property = response.getSchema();
        assertTrue(property instanceof RefProperty);

        Model inline = swagger.getDefinitions().get("inline_response_200");
        assertTrue(inline instanceof ArrayModel);
        ArrayModel am = (ArrayModel) inline;
        Property innerProperty = am.getItems();
        assertTrue(innerProperty instanceof StringProperty);
    }
}
