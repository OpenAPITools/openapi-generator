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
 * API tests for FakeClassnameTags123Api
 */
public class FakeClassnameTags123ApiTest {

    private FakeClassnameTags123Api api;

    @Before
    public void setup() {
        api = new ApiClient().buildClient(FakeClassnameTags123Api.class);
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
