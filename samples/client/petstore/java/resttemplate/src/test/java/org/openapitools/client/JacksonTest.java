package org.openapitools.client;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapitools.client.model.TestInlineFreeformAdditionalPropertiesRequest;

import static org.junit.jupiter.api.Assertions.*;

class JacksonTest {

    private ObjectMapper mapper;

    @BeforeEach
    void setUp() {
        mapper = JsonMapper.builder()
                // For determinist serialization results
                .enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY)
                .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
                .build();
    }

    @Test
    void testSerializeAdditionalProperties() throws JsonProcessingException {
        // Given
        TestInlineFreeformAdditionalPropertiesRequest model = new TestInlineFreeformAdditionalPropertiesRequest("value")
                .putAdditionalProperty("someString", "someValue")
                .putAdditionalProperty("someNumber", 1.23)
                .putAdditionalProperty("someBoolean", true);

        // When
        String string = mapper.writeValueAsString(model);

        // Then
        String expectedString = "{\"someProperty\":\"value\",\"someBoolean\":true,\"someNumber\":1.23,\"someString\":\"someValue\"}";
        assertEquals(expectedString, string);
    }

    @Test
    void testDeserializeAdditionalProperties() throws JsonProcessingException {
        // Given
        String string = "{\"someProperty\":\"value\",\"someBoolean\":true,\"someNumber\":1.23,\"someString\":\"someValue\"}";

        // When
        TestInlineFreeformAdditionalPropertiesRequest model = mapper.readValue(
                string,
                TestInlineFreeformAdditionalPropertiesRequest.class
        );

        // Then
        TestInlineFreeformAdditionalPropertiesRequest expectedModel = new TestInlineFreeformAdditionalPropertiesRequest("value")
                .putAdditionalProperty("someString", "someValue")
                .putAdditionalProperty("someNumber", 1.23)
                .putAdditionalProperty("someBoolean", true);
        assertEquals(expectedModel, model);

        TestInlineFreeformAdditionalPropertiesRequest invalidModel = new TestInlineFreeformAdditionalPropertiesRequest("value");
        assertNotEquals(invalidModel, model);
    }
}
