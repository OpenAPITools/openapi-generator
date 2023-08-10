package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.*;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;

import java.io.IOException;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import java.util.List;

import okio.ByteString;
import org.junit.jupiter.api.*;
import org.openapitools.client.model.Order;

import org.openapitools.client.model.*;

public class JSONTest {
    private ApiClient apiClient = null;
    private JSON json = null;
    private Order order = null;

    @BeforeEach
    public void setup() {
        apiClient = new ApiClient();
        json = apiClient.getJSON();
        order = new Order();
    }

    @Test
    public void testSqlDateTypeAdapter() {
        final String str = "\"2015-11-07\"";
        final java.sql.Date date = java.sql.Date.valueOf("2015-11-07");

        assertEquals(str, json.serialize(date));
        assertEquals(json.deserialize(str, java.sql.Date.class), date);
        assertEquals(
                json.deserialize(
                                "\"2015-11-07T03:49:09.356" + getCurrentTimezoneOffset() + "\"",
                                java.sql.Date.class)
                        .toString(),
                date.toString());

        // custom date format: without day
        DateFormat format = new SimpleDateFormat("yyyy-MM", Locale.ROOT);
        apiClient.setSqlDateFormat(format);
        String dateStr = "\"2015-11\"";
        assertEquals(
                dateStr,
                json.serialize(json.deserialize("\"2015-11-07T03:49:09Z\"", java.sql.Date.class)));
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11\"", java.sql.Date.class)));
    }

    @Test
    public void testDateTypeAdapter() {
        Calendar cal = new GregorianCalendar(2015, 10, 7, 3, 49, 9);
        cal.setTimeZone(TimeZone.getTimeZone("UTC"));

        assertEquals(json.deserialize("\"2015-11-07T05:49:09+02\"", Date.class), cal.getTime());

        cal.set(Calendar.MILLISECOND, 300);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.3Z\"", Date.class), cal.getTime());

        cal.set(Calendar.MILLISECOND, 356);
        Date date = cal.getTime();

        final String utcDate = "\"2015-11-07T03:49:09.356Z\"";
        assertEquals(json.deserialize(utcDate, Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356+00:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T05:49:09.356+02:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T02:49:09.356-01:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356Z\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356+00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T02:49:09.356-0100\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356456789Z\"", Date.class), date);

        assertEquals(utcDate, json.serialize(date));

        // custom datetime format: without milli-seconds, custom time zone
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX", Locale.ROOT);
        format.setTimeZone(TimeZone.getTimeZone("GMT+10"));
        apiClient.setDateFormat(format);

        String dateStr = "\"2015-11-07T13:49:09+10:00\"";
        assertEquals(
                dateStr,
                json.serialize(json.deserialize("\"2015-11-07T03:49:09+00:00\"", Date.class)));
        assertEquals(
                dateStr, json.serialize(json.deserialize("\"2015-11-07T03:49:09Z\"", Date.class)));
        assertEquals(
                dateStr,
                json.serialize(json.deserialize("\"2015-11-07T00:49:09-03:00\"", Date.class)));

        try {
            // invalid time zone format
            json.deserialize("\"2015-11-07T03:49:09+00\"", Date.class);
            fail("json parsing should fail");
        } catch (RuntimeException e) {
            // OK
        }
        try {
            // unexpected milliseconds
            json.deserialize("\"2015-11-07T03:49:09.000Z\"", Date.class);
            fail("json parsing should fail");
        } catch (RuntimeException e) {
            // OK
        }
    }

    @Test
    public void testOffsetDateTimeTypeAdapter() {
        final String str = "\"2016-09-09T08:02:03.123-03:00\"";
        OffsetDateTime date =
                OffsetDateTime.of(2016, 9, 9, 8, 2, 3, 123000000, ZoneOffset.of("-3"));

        assertEquals(str, json.serialize(date));
        // Use toString() instead of isEqual to verify that the offset is preserved
        assertEquals(json.deserialize(str, OffsetDateTime.class).toString(), date.toString());
    }

    @Test
    public void testLocalDateTypeAdapter() {
        final String str = "\"2016-09-09\"";
        final LocalDate date = LocalDate.of(2016, 9, 9);

        assertEquals(str, json.serialize(date));
        assertEquals(json.deserialize(str, LocalDate.class), date);
    }

    @Test
    public void testDefaultDate() throws Exception {
        final DateTimeFormatter datetimeFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(OffsetDateTime.from(datetimeFormat.parse(dateStr)));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() {
        }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateTimeFormatter datetimeFormat =
                DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.of("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(OffsetDateTime.from(datetimeFormat.parse(dateStr)));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() {
        }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.format(o.getShipDate()));
    }

    @Test
    public void testByteArrayTypeAdapterSerialization() {
        // Arrange
        final String expectedBytesAsString = "Let's pretend this a jpg or something";
        final byte[] expectedBytes = expectedBytesAsString.getBytes(StandardCharsets.UTF_8);

        // Act
        String serializedBytesWithQuotes = json.serialize(expectedBytes);

        // Assert
        String serializedBytes =
                serializedBytesWithQuotes.substring(1, serializedBytesWithQuotes.length() - 1);
        if (json.getGson().htmlSafe()) {
            serializedBytes = serializedBytes.replaceAll("\\\\u003d", "=");
        }
        ByteString actualAsByteString = ByteString.decodeBase64(serializedBytes);
        byte[] actualBytes = actualAsByteString.toByteArray();
        assertEquals(expectedBytesAsString, new String(actualBytes, StandardCharsets.UTF_8));
    }

    @Test
    public void testByteArrayTypeAdapterDeserialization() {
        // Arrange
        final String expectedBytesAsString = "Let's pretend this a jpg or something";
        final byte[] expectedBytes = expectedBytesAsString.getBytes(StandardCharsets.UTF_8);
        final ByteString expectedByteString = ByteString.of(expectedBytes);
        final String serializedBytes = expectedByteString.base64();
        final String serializedBytesWithQuotes = "\"" + serializedBytes + "\"";
        Type type = new TypeToken<byte[]>() {
        }.getType();

        // Act
        byte[] actualDeserializedBytes = json.deserialize(serializedBytesWithQuotes, type);

        // Assert
        assertEquals(
                expectedBytesAsString, new String(actualDeserializedBytes, StandardCharsets.UTF_8));
    }

    @Test
    public void testRequiredFieldException() {
        IllegalArgumentException thrown = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            // test json string missing required field(s) to ensure exception is thrown
            Gson gson = json.getGson();
            //Gson gson = new GsonBuilder()
            //        .registerTypeAdapter(Pet.class, new Pet.CustomDeserializer())
            //        .create();
            String json = "{\"id\": 5847, \"name\":\"tag test 1\"}"; // missing photoUrls (required field)
            //String json = "{\"id2\": 5847, \"name\":\"tag test 1\"}";
            //String json = "{\"id\": 5847}";
            Pet p = gson.fromJson(json, Pet.class);
        });

        Assertions.assertEquals("The required field `photoUrls` is not found in the JSON string: {\"id\":5847,\"name\":\"tag test 1\"}", thrown.getMessage());
    }

    @Test
    @Disabled("No longer need the following test as additional field(s) should be stored in `additionalProperties`")
    public void testAdditionalFieldException() {
        IllegalArgumentException thrown = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            // test json string with additional field(s) to ensure exception is thrown
            Gson gson = json.getGson();
            String json = "{\"id\": 5847, \"name\":\"tag test 1\", \"new-field\": true}";
            org.openapitools.client.model.Tag t = gson.fromJson(json, org.openapitools.client.model.Tag.class);
        });

        Assertions.assertEquals("The field `new-field` in the JSON string is not defined in the `Tag` properties. JSON: {\"id\":5847,\"name\":\"tag test 1\",\"new-field\":true}", thrown.getMessage());
    }

    @Test
    public void testCustomDeserializer() {
        // test the custom deserializer to ensure it can deserialize json payload into objects
        Gson gson = json.getGson();
        //Gson gson = new GsonBuilder()
        //        .registerTypeAdapter(Tag.class, new Tag.CustomDeserializer())
        //        .create();
        // id and name
        String json = "{\"id\": 5847, \"name\":\"tag test 1\"}";
        org.openapitools.client.model.Tag t = gson.fromJson(json, org.openapitools.client.model.Tag.class);
        assertEquals(t.getName(), "tag test 1");
        assertEquals(t.getId(), Long.valueOf(5847L));

        // name only
        String json2 = "{\"name\":\"tag test 1\"}";
        org.openapitools.client.model.Tag t2 = gson.fromJson(json2, org.openapitools.client.model.Tag.class);
        assertEquals(t2.getName(), "tag test 1");
        assertEquals(t2.getId(), null);

        // with all required fields 
        String json3 = "{\"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": [\"https://a.com\", \"https://b.com\"]}";
        Pet t3 = gson.fromJson(json3, Pet.class);
        assertEquals(t3.getName(), "pet test 1");
        assertEquals(t3.getId(), Long.valueOf(5847));

        // with all required fields and tags (optional) 
        String json4 = "{\"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": [\"https://a.com\", \"https://b.com\"],\"tags\":[{\"id\":\"tag 123\"}]}";
        Pet t4 = gson.fromJson(json3, Pet.class);
        assertEquals(t4.getName(), "pet test 1");
        assertEquals(t4.getId(), Long.valueOf(5847));
    }

    @Test
    @Disabled("Unknown fields are now correctly deserialized into `additionalProperties`")
    public void testUnknownFields() {
        // test unknown fields in the payload
        Gson gson = json.getGson();
        // test Tag
        String json5 = "{\"unknown_field\": 543, \"id\":\"tag 123\"}";
        Exception exception5 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            org.openapitools.client.model.Tag t5 = gson.fromJson(json5, org.openapitools.client.model.Tag.class);
        });
        assertTrue(exception5.getMessage().contains("The field `unknown_field` in the JSON string is not defined in the `Tag` properties. JSON: {\"unknown_field\":543,\"id\":\"tag 123\"}"));

        // test Pet with invalid tags
        String json6 = "{\"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": [\"https://a.com\", \"https://b.com\"],\"tags\":[{\"unknown_field\": 543, \"id\":\"tag 123\"}]}";
        Exception exception6 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            Pet t6 = gson.fromJson(json6, Pet.class);
        });
        assertTrue(exception6.getMessage().contains("The field `unknown_field` in the JSON string is not defined in the `Tag` properties. JSON: {\"unknown_field\":543,\"id\":\"tag 123\"}"));

        // test Pet with invalid tags (required)
        String json7 = "{\"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": [\"https://a.com\", \"https://b.com\"],\"tags\":[{\"unknown_field\": 543, \"id\":\"tag 123\"}]}";
        Exception exception7 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            PetWithRequiredTags t7 = gson.fromJson(json7, PetWithRequiredTags.class);
        });
        assertTrue(exception7.getMessage().contains("The field `unknown_field` in the JSON string is not defined in the `Tag` properties. JSON: {\"unknown_field\":543,\"id\":\"tag 123\"}"));

        // test Pet with invalid tags (missing reqired)
        String json8 = "{\"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": [\"https://a.com\", \"https://b.com\"]}";
        Exception exception8 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            PetWithRequiredTags t8 = gson.fromJson(json8, PetWithRequiredTags.class);
        });
        assertTrue(exception8.getMessage().contains("The required field `tags` is not found in the JSON string: {\"id\":5847,\"name\":\"pet test 1\",\"photoUrls\":[\"https://a.com\",\"https://b.com\"]}"));
    }

    /**
     * Model tests for Pet
     */
    @Test
    public void testPet() {
        // test Pet
        Pet model = new Pet();
        model.setId(1029L);
        model.setName("Dog");

        Pet model2 = new Pet();
        model2.setId(1029L);
        model2.setName("Dog");

        assertTrue(model.equals(model2));
    }

    // Obtained 22JAN2018 from stackoverflow answer by PuguaSoft
    // https://stackoverflow.com/questions/11399491/java-timezone-offset
    // Direct link https://stackoverflow.com/a/16680815/3166133
    public static String getCurrentTimezoneOffset() {

        TimeZone tz = TimeZone.getDefault();
        Calendar cal = GregorianCalendar.getInstance(tz, Locale.ROOT);
        int offsetInMillis = tz.getOffset(cal.getTimeInMillis());

        String offset =
                String.format(
                        Locale.ROOT,
                        "%02d:%02d",
                        Math.abs(offsetInMillis / 3600000),
                        Math.abs((offsetInMillis / 60000) % 60));
        offset = (offsetInMillis >= 0 ? "+" : "-") + offset;

        return offset;
    }

    /**
     * Validate an anyOf schema can be deserialized into the expected class.
     * The anyOf schema does not have a discriminator.
     */
    @Test
    public void testAnyOfSchemaWithoutDiscriminator() throws Exception {
        {
            String str = "{ \"cultivar\": \"golden delicious\", \"origin\": \"japan\" }";

            // make sure deserialization works for pojo object
            Apple a = json.getGson().fromJson(str, Apple.class);
            assertEquals(a.getCultivar(), "golden delicious");
            assertEquals(a.getOrigin(), "japan");

            GmFruit o = json.getGson().fromJson(str, GmFruit.class);
            assertTrue(o.getActualInstance() instanceof Apple);
            Apple inst = (Apple) o.getActualInstance();
            assertEquals(inst.getCultivar(), "golden delicious");
            assertEquals(inst.getOrigin(), "japan");
            assertEquals(json.getGson().toJson(inst), "{\"cultivar\":\"golden delicious\",\"origin\":\"japan\"}");
            assertEquals(inst.toJson(), "{\"cultivar\":\"golden delicious\",\"origin\":\"japan\"}");
            assertEquals(json.getGson().toJson(o), "{\"cultivar\":\"golden delicious\",\"origin\":\"japan\"}");
            assertEquals(o.toJson(), "{\"cultivar\":\"golden delicious\",\"origin\":\"japan\"}");

            /* comment out the following as we've added "additionalProperties" support
            String str2 = "{ \"origin_typo\": \"japan\" }";
            // no match
            Exception exception = assertThrows(java.lang.IllegalArgumentException.class, () -> {
                Apple o3 = json.getGson().fromJson(str2, Apple.class);
            });

            // no match
            Exception exception3 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
                Banana o2 = json.getGson().fromJson(str2, Banana.class);
            });

            // no match
            Exception exception4 = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                GmFruit o2 = json.getGson().fromJson(str2, GmFruit.class);
            });
             */
        }
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema has a discriminator.
     */
    @Test
    public void testOneOfSchemaWithDiscriminator() throws Exception {
        {
            String str = "{ \"className\": \"whale\", \"hasBaleen\": false, \"hasTeeth\": false }";

            // make sure deserialization works for pojo object
            Whale w = json.getGson().fromJson(str, Whale.class);
            assertEquals(w.getClassName(), "whale");
            assertEquals(w.getHasBaleen(), false);
            assertEquals(w.getHasTeeth(), false);

            Mammal o = json.getGson().fromJson(str, Mammal.class);
            assertTrue(o.getActualInstance() instanceof Whale);
            Whale inst = (Whale) o.getActualInstance();
            assertEquals(inst.getClassName(), "whale");
            assertEquals(inst.getHasBaleen(), false);
            assertEquals(inst.getHasTeeth(), false);
            assertEquals(json.getGson().toJson(inst), "{\"hasBaleen\":false,\"hasTeeth\":false,\"className\":\"whale\"}");
            assertEquals(inst.toJson(), "{\"hasBaleen\":false,\"hasTeeth\":false,\"className\":\"whale\"}");
            assertEquals(json.getGson().toJson(o), "{\"hasBaleen\":false,\"hasTeeth\":false,\"className\":\"whale\"}");
            assertEquals(o.toJson(), "{\"hasBaleen\":false,\"hasTeeth\":false,\"className\":\"whale\"}");

            String str2 = "{ \"className\": \"zebra\", \"type\": \"plains\" }";

            // make sure deserialization works for pojo object
            Zebra z = Zebra.fromJson(str2);
            assertEquals(z.toJson(), "{\"type\":\"plains\",\"className\":\"zebra\"}");

            Mammal o2 = json.getGson().fromJson(str2, Mammal.class);
            assertTrue(o2.getActualInstance() instanceof Zebra);
            Zebra inst2 = (Zebra) o2.getActualInstance();
            assertEquals(json.getGson().toJson(inst2), "{\"type\":\"plains\",\"className\":\"zebra\"}");
            assertEquals(inst2.toJson(), "{\"type\":\"plains\",\"className\":\"zebra\"}");
            assertEquals(json.getGson().toJson(o2), "{\"type\":\"plains\",\"className\":\"zebra\"}");
            assertEquals(o2.toJson(), "{\"type\":\"plains\",\"className\":\"zebra\"}");
        }
        {
            // incorrect payload results in exception
            String str = "{ \"cultivar\": \"golden delicious\", \"mealy\": false, \"garbage_prop\": \"abc\" }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                Mammal o = json.getGson().fromJson(str, Mammal.class);
            });
            assertEquals("java.io.IOException: Failed deserialization for Mammal: 0 classes match result, expected 1. Detailed failure message for oneOf schemas: [Deserialization for Whale failed with `The required field `className` is not found in the JSON string: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`., Deserialization for Zebra failed with `The required field `className` is not found in the JSON string: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`., Deserialization for Pig failed with `The JSON string is invalid for Pig with oneOf schemas: BasquePig, DanishPig. 0 class(es) match the result, expected 1. Detailed failure message for oneOf schemas: [Deserialization for BasquePig failed with `The required field `className` is not found in the JSON string: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`., Deserialization for DanishPig failed with `The required field `className` is not found in the JSON string: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`.]. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`.]. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}", exception.getMessage());
        }
        {
            // Try to deserialize empty object. This should fail 'oneOf' because none will match
            // whale or zebra.
            String str = "{ }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                json.getGson().fromJson(str, Mammal.class);
            });
            assertEquals("java.io.IOException: Failed deserialization for Mammal: 0 classes match result, expected 1. Detailed failure message for oneOf schemas: [Deserialization for Whale failed with `The required field `className` is not found in the JSON string: {}`., Deserialization for Zebra failed with `The required field `className` is not found in the JSON string: {}`., Deserialization for Pig failed with `The JSON string is invalid for Pig with oneOf schemas: BasquePig, DanishPig. 0 class(es) match the result, expected 1. Detailed failure message for oneOf schemas: [Deserialization for BasquePig failed with `The required field `className` is not found in the JSON string: {}`., Deserialization for DanishPig failed with `The required field `className` is not found in the JSON string: {}`.]. JSON: {}`.]. JSON: {}", exception.getMessage());
        }
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema contains primitive Types.
     */
    @Test
    public void testOneOfSchemaWithPrimitives() throws Exception {
        {
            String str = "{\"name\":\"bool_1\",\"value\":false}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "bool_1");
            assertTrue(variable.getValue().getActualInstance() instanceof Scalar);

            Scalar scalar = (Scalar) variable.getValue().getActualInstance();
            assertTrue(scalar.getActualInstance() instanceof Boolean);

            Boolean value = (Boolean) scalar.getActualInstance();
            assertEquals(value, false);

            assertEquals(json.getGson().toJson(variable), str);
        }
        {
            String str = "{\"name\":\"string_1\",\"value\":\"string\"}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "string_1");
            assertTrue(variable.getValue().getActualInstance() instanceof Scalar);

            Scalar scalar = (Scalar) variable.getValue().getActualInstance();
            assertTrue(scalar.getActualInstance() instanceof String);
            
            String value = (String) scalar.getActualInstance();
            assertEquals(value, "string");

            assertEquals(json.getGson().toJson(variable), str);
        }
        {
            String str = "{\"name\":\"decimal_1\",\"value\":124567890.0987654321}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "decimal_1");
            assertTrue(variable.getValue().getActualInstance() instanceof Scalar);

            Scalar scalar = (Scalar) variable.getValue().getActualInstance();
            assertTrue(scalar.getActualInstance() instanceof BigDecimal);
            
            BigDecimal value = (BigDecimal) scalar.getActualInstance();
            assertEquals(value, new BigDecimal("124567890.0987654321"));

            assertEquals(json.getGson().toJson(variable), str);
        }
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema contains an array with elements of primitive types.
     */
    @Test
    public void testOneofSchemaWithArrayOfPrimitives() throws Exception {
        {
            String str = "{\"name\":\"bool_array_1\",\"value\":[false,true,false]}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "bool_array_1");
            assertTrue(variable.getValue().getActualInstance() instanceof List<?>);

            List<Scalar> list = (List<Scalar>) variable.getValue().getActualInstance();
            Scalar item_0 = list.get(0);
            Scalar item_1 = list.get(1);
            Scalar item_2 = list.get(2);

            assertTrue(item_0.getActualInstance() instanceof Boolean);
            assertTrue(item_1.getActualInstance() instanceof Boolean);
            assertTrue(item_2.getActualInstance() instanceof Boolean);

            Boolean bool_0 = (Boolean) item_0.getActualInstance();
            Boolean bool_1 = (Boolean) item_1.getActualInstance();
            Boolean bool_2 = (Boolean) item_2.getActualInstance();
            assertEquals(bool_0, false);
            assertEquals(bool_1, true);
            assertEquals(bool_2, false);

            assertEquals(json.getGson().toJson(variable), str);
        }
        {
            String str = "{\"name\":\"string_array_1\",\"value\":[\"string_0\",\"string_1\",\"string_2\"]}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "string_array_1");
            assertTrue(variable.getValue().getActualInstance() instanceof List<?>);

            List<Scalar> list = (List<Scalar>) variable.getValue().getActualInstance();
            Scalar item_0 = list.get(0);
            Scalar item_1 = list.get(1);
            Scalar item_2 = list.get(2);

            assertTrue(item_0.getActualInstance() instanceof String);
            assertTrue(item_1.getActualInstance() instanceof String);
            assertTrue(item_2.getActualInstance() instanceof String);

            String string_0 = (String) item_0.getActualInstance();
            String string_1 = (String) item_1.getActualInstance();
            String string_2 = (String) item_2.getActualInstance();
            assertEquals(string_0, "string_0");
            assertEquals(string_1, "string_1");
            assertEquals(string_2, "string_2");

            assertEquals(json.getGson().toJson(variable), str);
        }
        {
            String str = "{\"name\":\"decimal_array_1\",\"value\":[124567890.0987654321,987654321.123456789,1112222210.2222222211]}";

            Variable variable = json.getGson().fromJson(str, Variable.class);
            assertEquals(variable.getName(), "decimal_array_1");
            assertTrue(variable.getValue().getActualInstance() instanceof List<?>);

            List<Scalar> list = (List<Scalar>) variable.getValue().getActualInstance();
            Scalar item_0 = list.get(0);
            Scalar item_1 = list.get(1);
            Scalar item_2 = list.get(2);

            assertTrue(item_0.getActualInstance() instanceof BigDecimal);
            assertTrue(item_1.getActualInstance() instanceof BigDecimal);
            assertTrue(item_2.getActualInstance() instanceof BigDecimal);

            BigDecimal decimal_0 = (BigDecimal) item_0.getActualInstance();
            BigDecimal decimal_1 = (BigDecimal) item_1.getActualInstance();
            BigDecimal decimal_2 = (BigDecimal) item_2.getActualInstance();
            assertEquals(decimal_0, new BigDecimal("124567890.0987654321"));
            assertEquals(decimal_1, new BigDecimal("987654321.123456789"));
            assertEquals(decimal_2, new BigDecimal("1112222210.2222222211"));

            assertEquals(json.getGson().toJson(variable), str);
        }
    }

    /**
     * Test JSON validation method
     */
    @Test
    public void testJsonValidation() throws Exception {
        String str = "{ \"cultivar\": [\"golden delicious\"], \"mealy\": false }";
        Exception exception = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            AppleReq a = json.getGson().fromJson(str, AppleReq.class);
        });
        assertTrue(exception.getMessage().contains("Expected the field `cultivar` to be a primitive type in the JSON string but got `[\"golden delicious\"]`"));

        String str2 = "{ \"id\": 5847, \"name\":\"pet test 1\", \"photoUrls\": 123 }";
        Exception exception2 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
            Pet p1 = json.getGson().fromJson(str2, Pet.class);
        });
        assertTrue(exception2.getMessage().contains("Expected the field `photoUrls` to be an array in the JSON string but got `123`"));
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema does not have a discriminator.
     */
    @Test
    public void testOneOfSchemaWithoutDiscriminator() throws Exception {
        // BananaReq and AppleReq have explicitly defined properties that are different by name.
        // There is no discriminator property.
        {
            String str = "{ \"cultivar\": \"golden delicious\", \"mealy\": false }";

            // make sure deserialization works for pojo object
            AppleReq a = json.getGson().fromJson(str, AppleReq.class);
            assertEquals(a.getCultivar(), "golden delicious");
            assertEquals(a.getMealy(), false);

            FruitReq o = json.getGson().fromJson(str, FruitReq.class);
            assertTrue(o.getActualInstance() instanceof AppleReq);
            AppleReq inst = (AppleReq) o.getActualInstance();
            assertEquals(inst.getCultivar(), "golden delicious");
            assertEquals(inst.getMealy(), false);
            assertEquals(json.getGson().toJson(inst), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(inst.toJson(), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(json.getGson().toJson(o), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(o.toJson(), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");

            AppleReq inst2 = o.getAppleReq();
            assertEquals(inst2.getCultivar(), "golden delicious");
            assertEquals(inst2.getMealy(), false);
            assertEquals(json.getGson().toJson(inst2), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(inst2.toJson(), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");

            // test fromJson
            FruitReq o3 = FruitReq.fromJson(str);
            assertTrue(o3.getActualInstance() instanceof AppleReq);
            AppleReq inst3 = (AppleReq) o3.getActualInstance();
            assertEquals(inst3.getCultivar(), "golden delicious");
            assertEquals(inst3.getMealy(), false);
            assertEquals(json.getGson().toJson(inst3), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(inst3.toJson(), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
            assertEquals(o3.toJson(), "{\"cultivar\":\"golden delicious\",\"mealy\":false}");
        }
        {
            // test to ensure the oneOf object can be serialized to "null" correctly
            FruitReq o = new FruitReq();
            assertEquals(o.getActualInstance(), null);
            assertEquals(json.getGson().toJson(o), "null");
            assertEquals(o.toJson(), "null");
            assertEquals(json.getGson().toJson(null), "null");
        }
        {
            // Same test, but this time with additional (undeclared) properties.
            // Since FruitReq has additionalProperties: false, deserialization should fail.
            String str = "{ \"cultivar\": \"golden delicious\", \"mealy\": false, \"garbage_prop\": \"abc\" }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                FruitReq o = json.getGson().fromJson(str, FruitReq.class);
            });
            assertEquals("java.io.IOException: Failed deserialization for FruitReq: 0 classes match result, expected 1. Detailed failure message for oneOf schemas: [Deserialization for AppleReq failed with `The field `garbage_prop` in the JSON string is not defined in the `AppleReq` properties. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`., Deserialization for BananaReq failed with `The field `cultivar` in the JSON string is not defined in the `BananaReq` properties. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}`.]. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}", exception.getMessage());
        }
        {
            String str = "{ \"lengthCm\": 17 }";

            // make sure deserialization works for pojo object
            BananaReq b = json.getGson().fromJson(str, BananaReq.class);
            assertEquals(b.getLengthCm(), new java.math.BigDecimal(17));

            FruitReq o = json.getGson().fromJson(str, FruitReq.class);
            assertTrue(o.getActualInstance() instanceof BananaReq);
            BananaReq inst = (BananaReq) o.getActualInstance();
            assertEquals(inst.getLengthCm(), new java.math.BigDecimal(17));
            assertEquals(json.getGson().toJson(o), "{\"lengthCm\":17}");
            assertEquals(json.getGson().toJson(inst), "{\"lengthCm\":17}");
        }
        {
            // Try to deserialize empty object. This should fail 'oneOf' because none will match
            // AppleReq or BananaReq.
            String str = "{ }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                json.getGson().fromJson(str, FruitReq.class);
            });
            assertTrue(exception.getMessage().contains("Failed deserialization for FruitReq: 0 classes match result, expected 1"));
        }
    }


    /**
     * Test validateJsonElement with null object
     */
    @Test
    public void testValidateJsonElement() throws Exception {
        JsonElement jsonElement = json.getGson().toJsonTree(new JsonObject());
        Exception exception = assertThrows(java.lang.IllegalArgumentException.class, () -> {
                Pet.validateJsonElement(jsonElement);
                });
        assertEquals(exception.getMessage(), "The required field `photoUrls` is not found in the JSON string: {}");
    }

    /**
     * Test additional properties.
     */
    @Test
    public void testAdditionalProperties() throws Exception {
        String str = "{ \"className\": \"zebra\", \"type\": \"plains\", \"from_json\": 4567, \"from_json_map\": {\"nested_string\": \"nested_value\"} }";
        Zebra z = Zebra.fromJson(str);
        z.putAdditionalProperty("new_key", "new_value");
        z.putAdditionalProperty("new_number", 1.23);
        z.putAdditionalProperty("new_boolean", true);
        org.openapitools.client.model.Tag t = new org.openapitools.client.model.Tag();
        t.setId(34L);
        t.setName("just a tag");
        z.putAdditionalProperty("new_object", t);
        assertEquals(z.toJson(), "{\"type\":\"plains\",\"className\":\"zebra\",\"new_key\":\"new_value\",\"new_boolean\":true,\"new_object\":{\"id\":34,\"name\":\"just a tag\"},\"from_json\":4567,\"from_json_map\":{\"nested_string\":\"nested_value\"},\"new_number\":1.23}");
    }

    /**
     * Test the default value in array properties.
     */
    @Test
    public void testDefaultValue() throws Exception {
        // None of these should throw exceptions due to the list being null
        // as the add*Itme method should initialise an empty list if it's set
        // to null
        ArrayDefault ad = new ArrayDefault();
        ad = ad.withDefaultEmptyBracket(null);
        ad.addWithDefaultEmptyBracketItem("test");
        ad = ad.withoutDefault(null);
        ad.addWithoutDefaultItem("hello world");
    }

    /**
     * Test property names collision.
     */
    @Test
    public void testPropertyNamesCollision() throws Exception {
        PropertyNameCollision p = new PropertyNameCollision();
        p.setUnderscoreType("test1");
        p.setType("test2");
        p.setTypeWithUnderscore("test3");
        assertEquals(json.getGson().toJson(p), "{\"_type\":\"test1\",\"type\":\"test2\",\"type_\":\"test3\"}");
    }

    /**
     * Validate a anyOf schema can be deserialized into the expected class.
     * The anyOf schema contains primitive Types.
     */
    @Test
    public void testAnyOfSchemaWithPrimitives() throws Exception {
        {
            // string test
            String str = "\"just a string\"";
            ScalarAnyOf s = json.getGson().fromJson(str, ScalarAnyOf.class);
            assertTrue(s.getActualInstance() instanceof String);
            assertEquals(json.getGson().toJson(s), str);
        }
        {
            // number test
            String str = "123.45";
            ScalarAnyOf s = json.getGson().fromJson(str, ScalarAnyOf.class);
            assertTrue(s.getActualInstance() instanceof BigDecimal);
            assertEquals(json.getGson().toJson(s), str);
        }
        {
            // boolean test
            String str = "true";
            ScalarAnyOf s = json.getGson().fromJson(str, ScalarAnyOf.class);
            assertTrue(s.getActualInstance() instanceof Boolean);
            assertEquals(json.getGson().toJson(s), str);
        }

        // test no match
        com.google.gson.JsonSyntaxException thrown = Assertions.assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
            String str = "{\"id\": 5847, \"name\":\"tag test 1\"}";
            ScalarAnyOf s = json.getGson().fromJson(str, ScalarAnyOf.class);
        });
    }

    /**
     * Test array of number in a model's property
     */
    @Test
    public void testArrayOfNumber() throws Exception {
        // no exception should be thrown
        String str = "{\"ArrayNumber\": null}";
        ArrayOfNumberOnly s = json.getGson().fromJson(str, ArrayOfNumberOnly.class);

        str = "{}";
        s = json.getGson().fromJson(str, ArrayOfNumberOnly.class);

        str = "{\"ArrayNumber\": [1,2,3]}";
        s = json.getGson().fromJson(str, ArrayOfNumberOnly.class);
    }
}
