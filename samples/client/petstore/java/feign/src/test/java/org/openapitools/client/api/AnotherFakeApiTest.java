package org.openapitools.client.api;
//TODO update packages 

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for AnotherFakeApi
 */
public class AnotherFakeApiTest {

    private AnotherFakeApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(AnotherFakeApi.class);
    }

    
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Test
    void call123testSpecialTagsTest() {
        Client body = null;
        // Client response = api.call123testSpecialTags(body);

        // TODO: test validations
    }

    
}
