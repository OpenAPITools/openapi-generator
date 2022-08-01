package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.InlineResponseDefault;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

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
        // InlineResponseDefault response = api.fooGet();

        // TODO: test validations
    }

    
}
