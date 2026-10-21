package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapitools.client.model.FormatTest;

/**
 * Hand-written tests for the JSON-B variant of the okhttp library's JSON helper.
 */
public class JSONTest {
    private JSON json;

    @BeforeEach
    public void setup() {
        json = new ApiClient().getJSON();
    }

    @Test
    public void testByteArrayIsBase64() {
        // OpenAPI 'format: byte' is base64; Yasson's default would emit a JSON array of numbers
        FormatTest model = new FormatTest()
                .number(new BigDecimal("32.1"))
                ._byte("hello".getBytes(StandardCharsets.UTF_8))
                .date(LocalDate.of(2020, 1, 1))
                .password("secret1234");

        String out = json.serialize(model);
        assertTrue(out.contains("\"byte\":\"aGVsbG8=\""), out);

        FormatTest back = json.deserialize(
                "{\"number\":32.1,\"byte\":\"aGVsbG8=\",\"date\":\"2020-01-01\",\"password\":\"secret1234\"}",
                FormatTest.class);
        assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), back.getByte());
    }

    @Test
    public void testDeserializeStringFallsBackToRawBody() {
        // ApiClient routes text responses with a JSON-ish or missing Content-Type through deserialize();
        // for a String return type the raw body must come back instead of a JsonbException
        assertEquals("hello", json.deserialize("hello", String.class));
        assertEquals("quoted", json.deserialize("\"quoted\"", String.class));
        assertThrows(jakarta.json.bind.JsonbException.class, () -> json.deserialize("hello", FormatTest.class));
    }
}
