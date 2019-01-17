package org.openapitools.client.api;

import org.junit.Before;
import org.junit.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;

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
     * To test special tags and operation ID starting with number
     */
    @Test
    public void call123testSpecialTagsTest() {
        Client client = null;
        // Client response = api.call123testSpecialTags(client);

        // TODO: test validations
    }
}
