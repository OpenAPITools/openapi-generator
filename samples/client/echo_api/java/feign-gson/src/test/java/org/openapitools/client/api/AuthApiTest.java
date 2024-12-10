package org.openapitools.client.api;

import feign.codec.StringDecoder;
import org.junit.jupiter.api.Assertions;
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

    
    /**
     * To test HTTP bearer authentication
     *
     * To test HTTP bearer authentication
     */
    @Test
    void testAuthHttpBearerTest() {
        ApiClient client = new ApiClient("http_bearer_auth");
        client.getFeignBuilder().decoder(new StringDecoder());
        {
            client.setBearerToken("fixed token");
            AuthApi api = client.buildClient(AuthApi.class);
            String response = api.testAuthHttpBearer();
            Assertions.assertTrue(response.contains("Authorization: Bearer fixed token"));
        }
        {
            client.setBearerToken(() -> "dynamic token");
            AuthApi api = client.buildClient(AuthApi.class);
            String response = api.testAuthHttpBearer();
            Assertions.assertTrue(response.contains("Authorization: Bearer dynamic token"));
        }
    }

    
}
