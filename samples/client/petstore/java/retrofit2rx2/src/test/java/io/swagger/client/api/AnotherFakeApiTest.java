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
        Client body = null;
        // Client response = api.testSpecialTags(body);

        // TODO: test validations
    }
}
