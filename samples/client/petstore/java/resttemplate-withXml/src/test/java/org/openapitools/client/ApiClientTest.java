package org.openapitools.client;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.*;
import org.junit.*;
import org.springframework.util.MultiValueMap;
import org.springframework.util.LinkedMultiValueMap;

public class ApiClientTest {
    ApiClient apiClient;

    @Before
    public void setup() {
        apiClient = new ApiClient();
    }

    /**
     * Test uri encoding when params contains comma
     */
    @Test
    public void testUriEncoderWithComma() {
        Map<String,Object> uriParams = new HashMap<>();
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        queryParams.add("key", "val,comma");
        apiClient.generateQueryUri(queryParams, uriParams);

        assertEquals("/key=val%2Ccomma", apiClient.expandPath("/key={key0}", uriParams));
    }
}