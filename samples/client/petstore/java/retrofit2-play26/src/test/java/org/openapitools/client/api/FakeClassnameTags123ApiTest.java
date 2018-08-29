package org.openapitools.client.api;

import org.junit.Before;
import org.junit.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;

/**
 * API tests for FakeClassnameTags123Api
 */
public class FakeClassnameTags123ApiTest {

    private FakeClassnameTags123Api api;

    @Before
    public void setup() {
        api = new ApiClient().createService(FakeClassnameTags123Api.class);
    }

    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     */
    @Test
    public void testClassnameTest() {
        Client client = null;
        // Client response = api.testClassname(client);

        // TODO: test validations
    }
}
