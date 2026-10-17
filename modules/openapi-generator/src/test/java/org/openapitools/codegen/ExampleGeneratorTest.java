package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.JsonSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import static org.testng.AssertJUnit.assertEquals;
import static org.testng.AssertJUnit.assertNull;
import static org.testng.AssertJUnit.assertTrue;

public class ExampleGeneratorTest {
    @DataProvider(name = "namedMapSchemas")
    public Object[][] namedMapSchemas() {
        return new Object[][] {
                {new ObjectSchema().additionalProperties(new StringSchema().example("value"))},
                {new Schema<>().type("object").additionalProperties(new StringSchema().example("value"))},
                {new MapSchema().additionalProperties(new StringSchema().example("value"))},
                {new JsonSchema().type("object").additionalProperties(new StringSchema().example("value"))}
        };
    }

    @Test(dataProvider = "namedMapSchemas")
    public void generateNamedMapKeepsAdditionalPropertyValues(Schema<?> mapSchema) throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Labels", mapSchema);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Arrays.asList("application/json", "*/*"), "Labels");
        ObjectMapper mapper = new ObjectMapper();

        assertEquals(2, examples.size());
        for (Map<String, String> example : examples) {
            assertEquals(mapper.readTree("{\"key\":\"value\"}"), mapper.readTree(example.get("example")));
        }
    }

    @Test(dataProvider = "namedMapSchemas")
    public void generateRepeatedReferencesToNamedMap(Schema<?> mapSchema) throws Exception {
        Schema<?> reading = new ObjectSchema()
                .addProperties("first", new Schema<>().$ref("#/components/schemas/Labels"))
                .addProperties("second", new Schema<>().$ref("#/components/schemas/Labels"));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Reading", reading);
        openAPI.getComponents().addSchemas("Labels", mapSchema);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Arrays.asList("application/json", "*/*"), "Reading");
        ObjectMapper mapper = new ObjectMapper();

        assertEquals(2, examples.size());
        for (Map<String, String> generated : examples) {
            JsonNode example = mapper.readTree(generated.get("example"));
            assertEquals(mapper.readTree("{\"key\":\"value\"}"), example.get("first"));
            assertEquals(example.get("first"), example.get("second"));
        }
    }

    @DataProvider(name = "emptyObjectSchemas")
    public Object[][] emptyObjectSchemas() {
        return new Object[][] {
                {new ObjectSchema()},
                {new Schema<>().type("object")},
                {new ObjectSchema().properties(new LinkedHashMap<>())},
                {new Schema<>().type("object").additionalProperties(false)}
        };
    }

    @Test(dataProvider = "emptyObjectSchemas")
    public void generateNestedEmptyObject(Schema<?> emptyObject) throws Exception {
        Schema<?> reading = new ObjectSchema().addProperties("metadata", emptyObject);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Reading", reading);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), "Reading");
        JsonNode metadata = new ObjectMapper().readTree(examples.get(0).get("example")).get("metadata");

        assertTrue(metadata.isObject());
        assertEquals(0, metadata.size());
    }

    @Test(dataProvider = "emptyObjectSchemas")
    public void generateTopLevelEmptyObject(Schema<?> emptyObject) throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Anything", emptyObject);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), "Anything");
        JsonNode example = new ObjectMapper().readTree(examples.get(0).get("example"));

        assertTrue(example.isObject());
        assertEquals(0, example.size());
    }

    @Test
    public void generateRepeatedReferencesToEmptyObject() throws Exception {
        Schema<?> reading = new ObjectSchema()
                .addProperties("first", new Schema<>().$ref("#/components/schemas/Anything"))
                .addProperties("second", new Schema<>().$ref("#/components/schemas/Anything"));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Reading", reading);
        openAPI.getComponents().addSchemas("Anything", new ObjectSchema());
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), "Reading");
        JsonNode example = new ObjectMapper().readTree(examples.get(0).get("example"));

        assertTrue(example.get("first").isObject());
        assertEquals(0, example.get("first").size());
        assertEquals(example.get("first"), example.get("second"));
    }

    @Test
    public void generateArrayOfEmptyObjects() throws Exception {
        Schema<?> array = new ArraySchema().items(new ObjectSchema());
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Array", array);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), "Array");
        JsonNode example = new ObjectMapper().readTree(examples.get(0).get("example"));

        assertTrue(example.isArray());
        assertEquals(2, example.size());
        assertTrue(example.get(0).isObject());
        assertEquals(0, example.get(0).size());
        assertEquals(example.get(0), example.get(1));
    }

    @Test
    public void generateObjectKeepsExplicitExampleAndMapValues() throws Exception {
        Schema<?> reading = new ObjectSchema()
                .addProperties("metadata", new Schema<>().type("object").example(Collections.singletonMap("name", "explicit")))
                .addProperties("labels", new MapSchema().additionalProperties(new StringSchema().example("value")));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Reading", reading);
        ExampleGenerator generator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), "Reading");
        JsonNode example = new ObjectMapper().readTree(examples.get(0).get("example"));

        assertEquals(new ObjectMapper().readTree("{\"name\":\"explicit\"}"), example.get("metadata"));
        assertEquals(new ObjectMapper().readTree("{\"key\":\"value\"}"), example.get("labels"));
    }

    @Test
    public void generateUnresolvedReferenceKeepsExistingFallback() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        ExampleGenerator generator = new ExampleGenerator(Collections.emptyMap(), openAPI);
        Schema<?> reference = new Schema<>().$ref("#/components/schemas/Missing");

        List<Map<String, String>> examples = generator.generate(null, Collections.singletonList("application/json"), reference);
        JsonNode example = new ObjectMapper().readTree(examples.get(0).get("example"));

        // An unresolved reference does not establish that the missing schema is an object.
        assertTrue(example.isTextual());
        assertEquals("{}", example.asText());
    }

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
    public void generateFromResponseSchemaWithAllOfCircularSchema() throws Exception{
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_allOf_circular_model")
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
        assertEquals(mapper.readTree(String.format(Locale.ROOT, "{%n  \"example_schema_property_alloff_circular\" : \"example schema property allOff circular\"%n}")), mapper.readTree(examples.get(0).get("example")));
        assertEquals("200", examples.get(0).get("statusCode"));
    }

    @Test
    public void generateFromResponseSchemaWithAllOfSiblingSchema() throws Exception{
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/example_generator_test.yaml");

        new InlineModelResolver().flatten(openAPI);

        ExampleGenerator exampleGenerator = new ExampleGenerator(openAPI.getComponents().getSchemas(), openAPI);
        Set<String> mediaTypeKeys = new TreeSet<>();
        mediaTypeKeys.add("application/json");
        List<Map<String, String>> examples = exampleGenerator.generateFromResponseSchema(
                "200",
                openAPI
                        .getPaths()
                        .get("/generate_from_response_schema_with_allOf_sibling_model")
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
        assertEquals(mapper.readTree(String.format(Locale.ROOT, "{%n \"base\":{\"example_schema_property\":\"example schema property value\",\"example_schema_property_composed\":\"example schema property value composed\"}, \"sibling\":{\"example_schema_property\":\"example schema property value\",\"example_schema_property_composed\":\"example schema property value composed\"} %n}")), mapper.readTree(examples.get(0).get("example")));
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

    @DataProvider(name = "mapResponseExamples")
    public Object[][] mapResponseExamples() {
        Map<String, Object> objectExample = new LinkedHashMap<>();
        objectExample.put("id", 1);
        objectExample.put("name", "Rex");
        return new Object[][] {
                {objectExample, Collections.emptyMap()},
                {Collections.emptyMap(), Collections.emptyMap()},
                {Collections.singletonMap("application/json", objectExample), Collections.emptyMap()},
                {objectExample, null},
                {Collections.emptyMap(), null},
                {Collections.singletonMap("application/json", objectExample), null}
        };
    }

    @Test(dataProvider = "mapResponseExamples")
    public void generateFromResponseSchemaWithMapExample(Map<String, Object> objectExample, Map<String, Schema> modelSchemas) throws Exception {
        Schema<Object> responseSchema = new Schema<>().type("object");
        responseSchema.setExample(objectExample);
        ExampleGenerator generator = new ExampleGenerator(modelSchemas, new OpenAPI());
        Set<String> mediaTypes = new LinkedHashSet<>(Arrays.asList("application/json", "*/*"));

        List<Map<String, String>> examples = generator.generateFromResponseSchema("200", responseSchema, mediaTypes);

        assertEquals(mediaTypes.size(), examples.size());
        ObjectMapper mapper = new ObjectMapper();
        Iterator<String> expectedMediaTypes = mediaTypes.iterator();
        for (Map<String, String> example : examples) {
            assertEquals(expectedMediaTypes.next(), example.get("contentType"));
            assertEquals(mapper.valueToTree(objectExample), mapper.readTree(example.get("example")));
            assertEquals("application/json", example.get("generatedContentType"));
            assertEquals("200", example.get("statusCode"));
        }
    }

    @Test
    public void generateWithMediaTypeExampleMap() throws Exception {
        Map<String, Object> objectExample = Collections.singletonMap("id", 1);
        Map<String, Object> mediaTypeExamples = Collections.singletonMap("application/json", objectExample);
        ExampleGenerator generator = new ExampleGenerator(Collections.emptyMap(), new OpenAPI());

        List<Map<String, String>> examples = generator.generate(mediaTypeExamples,
                Collections.singletonList("application/json"), new Schema<>().type("object"));

        assertEquals(1, examples.size());
        assertEquals("application/json", examples.get(0).get("contentType"));
        ObjectMapper mapper = new ObjectMapper();
        assertEquals(mapper.valueToTree(objectExample), mapper.readTree(examples.get(0).get("example")));
    }
}
