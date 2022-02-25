package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for FakeClassnameTags123Api
 */
class FakeClassnameTags123ApiTest {

    private FakeClassnameTags123Api api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(FakeClassnameTags123Api.class);
    }

    
    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     */
    @Test
    void testClassnameTest() {
        Client body = null;
        // Client response = api.testClassname(body);

        // TODO: test validations
    }

    
}
