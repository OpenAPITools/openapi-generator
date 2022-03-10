package org.openapitools.client;

import static org.junit.Assert.*;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import java.io.IOException;
import java.lang.reflect.Type;
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
import okio.ByteString;
import org.junit.*;
import org.openapitools.client.model.Order;

import org.openapitools.client.model.*;

public class JSONTest {
    private ApiClient apiClient = null;
    private JSON json = null;
    private Order order = null;

    @Before
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
            // unexpected miliseconds
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
        Type type = new TypeToken<Order>() {}.getType();
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
        Type type = new TypeToken<Order>() {}.getType();
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
        Type type = new TypeToken<byte[]>() {}.getType();

        // Act
        byte[] actualDeserializedBytes = json.deserialize(serializedBytesWithQuotes, type);

        // Assert
        assertEquals(
                expectedBytesAsString, new String(actualDeserializedBytes, StandardCharsets.UTF_8));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRequiredFieldException() {
        // test json string missing required field(s) to ensure exception is thrown
        Gson gson = json.getGson();
        //Gson gson = new GsonBuilder()
        //        .registerTypeAdapter(Pet.class, new Pet.CustomDeserializer())
        //        .create();
        String json = "{\"id\": 5847, \"name\":\"tag test 1\"}"; // missing photoUrls (required field)
        //String json = "{\"id2\": 5847, \"name\":\"tag test 1\"}";
        //String json = "{\"id\": 5847}";
        Pet p = gson.fromJson(json, Pet.class);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testAdditionalFieldException() {
        // test json string with additional field(s) to ensure exception is thrown
        Gson gson = json.getGson();
        //Gson gson = new GsonBuilder()
        //        .registerTypeAdapter(Tag.class, new Tag.CustomDeserializer())
        //        .create();
        String json = "{\"id\": 5847, \"name\":\"tag test 1\", \"new-field\": true}";
        Tag t = gson.fromJson(json, Tag.class);
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
        Tag t = gson.fromJson(json, Tag.class);
        assertEquals(t.getName(), "tag test 1");
        assertEquals(t.getId(), Long.valueOf(5847L));

        // name only
        String json2 = "{\"name\":\"tag test 1\"}";
        Tag t2 = gson.fromJson(json2, Tag.class);
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

        // test Tag
        String json5 = "{\"unknown_field\": 543, \"id\":\"tag 123\"}";
        Exception exception5 = assertThrows(java.lang.IllegalArgumentException.class, () -> {
                Tag t5 = gson.fromJson(json5, Tag.class);
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

    /** Model tests for Pet */
    @Test
    public void testPet() {
        // test Pet
        Pet model = new Pet();
        model.setId(1029L);
        model.setName("Dog");

        Pet model2 = new Pet();
        model2.setId(1029L);
        model2.setName("Dog");

        Assert.assertTrue(model.equals(model2));
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
            assertEquals(z.toJson(), "{\"className\":\"zebra\",\"type\":\"plains\"}");

            Mammal o2 = json.getGson().fromJson(str2, Mammal.class);
            assertTrue(o2.getActualInstance() instanceof Zebra);
            Zebra inst2 = (Zebra) o2.getActualInstance();
            assertEquals(json.getGson().toJson(inst2), "{\"className\":\"zebra\",\"type\":\"plains\"}");
            assertEquals(inst2.toJson(), "{\"className\":\"zebra\",\"type\":\"plains\"}");
            assertEquals(json.getGson().toJson(o2), "{\"className\":\"zebra\",\"type\":\"plains\"}");
            assertEquals(o2.toJson(), "{\"className\":\"zebra\",\"type\":\"plains\"}");
        }
        {
            // incorrect payload results in exception
            String str = "{ \"cultivar\": \"golden delicious\", \"mealy\": false, \"garbage_prop\": \"abc\" }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                Mammal o = json.getGson().fromJson(str, Mammal.class);
            });
            assertTrue(exception.getMessage().contains("Failed deserialization for Mammal: 0 classes match result, expected 1. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}"));
        }
        {
            // Try to deserialize empty object. This should fail 'oneOf' because none will match
            // whale or zebra.
            String str = "{ }";
            Exception exception = assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
                json.getGson().fromJson(str, Mammal.class);
            });
            assertTrue(exception.getMessage().contains("Failed deserialization for Mammal: 0 classes match result, expected 1"));
        }
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
            assertTrue(exception.getMessage().contains("Failed deserialization for FruitReq: 0 classes match result, expected 1. JSON: {\"cultivar\":\"golden delicious\",\"mealy\":false,\"garbage_prop\":\"abc\"}"));
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
}
