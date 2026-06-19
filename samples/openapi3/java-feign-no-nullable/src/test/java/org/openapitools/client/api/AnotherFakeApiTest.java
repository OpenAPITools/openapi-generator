package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for AnotherFakeApi
 */
class AnotherFakeApiTest {

    private AnotherFakeApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(AnotherFakeApi.class);
    }

    
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Test
    void call123testSpecialTagsTest() {
        UUID uuidTest = null;
        Client body = null;
        // Client response = api.call123testSpecialTags(uuidTest, body);

        // TODO: test validations
    }

    
}
