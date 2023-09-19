package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for AuthApi
 */
class AuthApiTest {

    private AuthApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(AuthApi.class);
    }

    
    /**
     * To test HTTP basic authentication
     *
     * To test HTTP basic authentication
     */
    @Test
    void testAuthHttpBasicTest() {
        // String response = api.testAuthHttpBasic();

        // TODO: test validations
    }

    
}
