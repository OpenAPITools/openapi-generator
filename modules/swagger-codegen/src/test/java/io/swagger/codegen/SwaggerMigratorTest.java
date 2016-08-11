package io.swagger.codegen;

import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class SwaggerMigratorTest {

    @Test(description = "read a 1.2 spec")
    public void swaggerMigratorTest() {
        final SwaggerParser loader = new SwaggerParser();
        final Swagger swagger = loader.read("src/test/resources/1_2/petstore-1.2/api-docs");
    }
}
