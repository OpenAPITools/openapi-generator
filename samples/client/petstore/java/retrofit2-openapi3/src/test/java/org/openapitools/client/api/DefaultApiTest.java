package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.InlineResponseDefault;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for DefaultApi
 */
public class DefaultApiTest {

    private DefaultApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(DefaultApi.class);
    }

    /**
     * 
     *
     * 
     */
    @Test
    public void fooGetTest() {
        // InlineResponseDefault response = api.fooGet();

        // TODO: test validations
    }
}
