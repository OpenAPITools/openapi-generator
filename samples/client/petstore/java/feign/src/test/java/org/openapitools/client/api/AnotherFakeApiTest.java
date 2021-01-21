package org.openapitools.client.api;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;

/**
 * API tests for AnotherFakeApi
 */
class AnotherFakeApiTest {

    private AnotherFakeApi api;

    @BeforeEach
    void setup() {
        api = new ApiClient().buildClient(AnotherFakeApi.class);
    }

    
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Test
    void call123testSpecialTagsTest() {
        Client body = null;
        // Client response = api.call123testSpecialTags(body);

        // TODO: test validations
    }

    
}
