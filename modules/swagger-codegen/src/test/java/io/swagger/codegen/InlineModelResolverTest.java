package io.swagger.codegen;


import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ObjectProperty;
import io.swagger.models.properties.StringProperty;
import io.swagger.util.Json;
import org.junit.Test;

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

        Json.prettyPrint(swagger);
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
                                        .vendorExtension("x-baz", "boo")
                                        .property("name", new StringProperty()
                                            .vendorExtension("x-bars", "bleh"))))));
        new InlineModelResolver().flatten(swagger);

        Json.prettyPrint(swagger);
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

        Json.prettyPrint(swagger);
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

        Json.prettyPrint(swagger);
    }
}
