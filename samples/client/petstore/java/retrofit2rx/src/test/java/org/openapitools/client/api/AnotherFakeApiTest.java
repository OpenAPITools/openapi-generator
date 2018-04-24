package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for AnotherFakeApi
 */
public class AnotherFakeApiTest {

    private AnotherFakeApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(AnotherFakeApi.class);
    }

    /**
     * To test special tags
     *
     * To test special tags
     */
    @Test
    public void testSpecialTagsTest() {
        Client client = null;
        // Client response = api.testSpecialTags(client);

        // TODO: test validations
    }
}
