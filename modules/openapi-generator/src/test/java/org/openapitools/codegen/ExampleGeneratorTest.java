package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.testng.annotations.Test;

import java.util.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import static org.testng.AssertJUnit.assertEquals;
import static org.testng.AssertJUnit.assertNull;
import static org.testng.AssertJUnit.assertTrue;

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
    public void generateFromResponseSchemaWithDateFormat() throws Exception {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_date_format")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        ObjectMapper mapper = new ObjectMapper();

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals(mapper.readTree(String.format(Locale.ROOT, "{%n  \"date_with_example\" : \"2024-01-01\",%n  \"date_without_example\" : \"2000-01-23\"%n}")), mapper.readTree(examples.get(0).get("example")));     
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
        assertEquals("[ \"string schema example value\", \"string schema example value\" ]", examples.get(0).get("example"));
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
        assertEquals("[ \"primitive types example value\", \"primitive types example value\" ]", examples.get(0).get("example"));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithArraySchema() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_array_reference")
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
        assertEquals(String.format(Locale.ROOT, "[ {%n  \"example_schema_property\" : \"example schema property value\"%n}, {%n  \"example_schema_property\" : \"example schema property value\"%n} ]"), examples.get(0).get("example"));
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

    @Test
    public void generateFromResponseSchemaWithAllOfComposedModel() throws Exception{
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_allOf_composed_model")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        ObjectMapper mapper = new ObjectMapper();

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals(mapper.readTree(String.format(Locale.ROOT, "{%n  \"example_schema_property_composed\" : \"example schema property value composed\",%n  \"example_schema_property\" : \"example schema property value\"%n}")), mapper.readTree(examples.get(0).get("example")));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithAllOfChildComposedModel() throws Exception {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_allOf_child_composed_model")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema(),
                mediaTypeKeys
        );

        ObjectMapper mapper = new ObjectMapper();

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        assertEquals(mapper.readTree(String.format(Locale.ROOT, "{%n  \"example_schema_property_composed\" : \"example schema property value composed\",%n  \"example_schema_property_composed_parent\" : \"example schema property value composed parent\",%n  \"example_schema_property\" : \"example schema property value\"%n}")), mapper.readTree(examples.get(0).get("example")));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithOneOfComposedModel() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_oneOf_composed_model")
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

    @Test
    public void generateFromResponseSchemaWithAnyOfComposedModel() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_anyOf_composed_model")
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

    @Test
    public void testExamplePropertyOrderPreservation() {
        OpenAPI openAPI = new OpenAPI();
        
        // Create a schema with properties in a specific order
        Schema<?> testSchema = new Schema<>();
        testSchema.setType("object");
        
        // Use LinkedHashMap to preserve property order as defined in spec
        Map<String, Schema> properties = new LinkedHashMap<>();
        
        // Add properties in the specific order: zebra, apple, mango, cherry, banana
        IntegerSchema zebraSchema = new IntegerSchema();
        zebraSchema.setExample(1);
        properties.put("zebra", zebraSchema);
        
        IntegerSchema appleSchema = new IntegerSchema();
        appleSchema.setExample(2);
        properties.put("apple", appleSchema);
        
        IntegerSchema mangoSchema = new IntegerSchema();
        mangoSchema.setExample(3);
        properties.put("mango", mangoSchema);
        
        IntegerSchema cherrySchema = new IntegerSchema();
        cherrySchema.setExample(4);
        properties.put("cherry", cherrySchema);
        
        IntegerSchema bananaSchema = new IntegerSchema();
        bananaSchema.setExample(5);
        properties.put("banana", bananaSchema);
        
        testSchema.setProperties(properties);
        
        // Create examples map
        Map<String, Schema> examples = new HashMap<>();
        examples.put("TestModel", testSchema);
        
        // Generate the example using the model name approach
        ExampleGenerator generator = new ExampleGenerator(examples, openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        
        List<Map<String, String>> generatedExamples = generator.generate(null, new ArrayList<>(mediaTypeKeys), "TestModel");
        
        assertEquals(1, generatedExamples.size());
        String exampleOutput = generatedExamples.get(0).get("example");
        
        System.out.println("Generated example output: " + exampleOutput);

        // Verify the example contains properties in the correct order
        // The order should be: zebra, apple, mango, cherry, banana
        assertTrue(exampleOutput.contains("\"zebra\" : 1"));
        assertTrue(exampleOutput.contains("\"apple\" : 2"));
        assertTrue(exampleOutput.contains("\"mango\" : 3"));
        assertTrue(exampleOutput.contains("\"cherry\" : 4"));
        assertTrue(exampleOutput.contains("\"banana\" : 5"));

        // Verify the order by checking the position of each field in the string
        int zebraPos = exampleOutput.indexOf("\"zebra\"");
        int applePos = exampleOutput.indexOf("\"apple\"");
        int mangoPos = exampleOutput.indexOf("\"mango\"");
        int cherryPos = exampleOutput.indexOf("\"cherry\"");
        int bananaPos = exampleOutput.indexOf("\"banana\"");

        System.out.println("Field positions: zebra=" + zebraPos + ", apple=" + applePos + ", mango=" + mangoPos + ", cherry=" + cherryPos + ", banana=" + bananaPos);
        
        assertTrue("zebra should come before apple", zebraPos < applePos);
        assertTrue("apple should come before mango", applePos < mangoPos);
        assertTrue("mango should come before cherry", mangoPos < cherryPos);
        assertTrue("cherry should come before banana", cherryPos < bananaPos);
    }
}
