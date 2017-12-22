package io.swagger.codegen;

import io.swagger.parser.v3.OpenAPIV3Parser;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class SwaggerMigratorTest {

    @Test(description = "read a 1.2 spec", enabled = false) // TODO: update api-docs to oas3 format
    public void swaggerMigratorTest() {
        final OpenAPIV3Parser loader = new OpenAPIV3Parser();
        loader.read("src/test/resources/1_2/petstore-1.2/api-docs");
    }
}
