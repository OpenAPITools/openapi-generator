package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.testng.annotations.Test;

import java.util.*;

import static org.testng.AssertJUnit.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class ExampleGeneratorTest {
    @Test
    public void generateFromResponseSchemaWithPrimitiveType() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_primitive_type")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals("\"primitive type example value\"", examples.get(0).get("example"));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithNoExample() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_no_example")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        assertNull(examples);
    }

    @Test
    public void generateFromResponseSchemaWithArrayOfModel() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                    .getPaths()
                    .get("/generate_from_response_schema_with_array_of_model")
                    .getGet()
                    .getResponses()
                    .get("200")
                    .getContent()
                    .get("application/json")
                    .getSchema(),
                mediaTypeKeys
        );

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals("\"string schema example value\"", examples.get(0).get("example"));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithArrayOfPrimitiveTypes() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_array_of_primitive_types")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals("\"primitive types example value\"", examples.get(0).get("example"));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithModel() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_model")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals(String.format(Locale.ROOT, "{%n  \"example_schema_property\" : \"example schema property value\"%n}"), examples.get(0).get("example"));
        assertEquals("200", examples.get(0).get("statusCode"));
    }
}
