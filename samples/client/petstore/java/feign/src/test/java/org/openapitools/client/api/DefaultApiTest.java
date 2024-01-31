package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.FooGetDefaultResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for DefaultApi
 */
class DefaultApiTest {

    private DefaultApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(DefaultApi.class);
    }

    
    /**
     * 
     *
     * 
     */
    @Test
    void fooGetTest() {
        // FooGetDefaultResponse response = api.fooGet();

        // TODO: test validations
    }

    
}
