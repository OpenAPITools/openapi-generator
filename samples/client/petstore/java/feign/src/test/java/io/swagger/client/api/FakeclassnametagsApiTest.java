package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.Client;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for FakeclassnametagsApi
 */
public class FakeclassnametagsApiTest {

    private FakeclassnametagsApi api;

    @Before
    public void setup() {
        api = new ApiClient().buildClient(FakeclassnametagsApi.class);
    }

    
    /**
     * To test class name in snake case
     *
     * 
     */
    @Test
    public void testClassnameTest() {
        Client body = null;
        // Client response = api.testClassname(body);

        // TODO: test validations
    }
    
}
