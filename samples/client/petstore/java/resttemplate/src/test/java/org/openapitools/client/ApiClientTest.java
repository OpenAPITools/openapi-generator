package org.openapitools.client;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

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
        Map<String, Object> uriParams = new HashMap<>();
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        queryParams.add("ke,&y", "val,?&comma");
        String queryTemplate = apiClient.generateQueryUri(queryParams, uriParams);
        assertEquals("ke%2C%26y=val%2C%3F%26comma", apiClient.expandPath(queryTemplate, uriParams));
    }

    @Test
    public void testPathParamEncoding() {
        Map<String, Object> uriParams = new HashMap<>();
        uriParams.put("username", "user_name,comma&amp space");
        assertEquals("user/user_name%2Ccomma%26amp%20space", apiClient.expandPath("user/{username}", uriParams));
    }

    @Test
    public void testPathAndQueryParamEncoding() {
        Map<String, Object> uriParams = new HashMap<>();
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        queryParams.add("key", "val,comma?q-mark&amp");
        uriParams.put("username", "user_name,comma&amp space");
        String template = "user/{username}?" + apiClient.generateQueryUri(queryParams, uriParams);
        assertEquals("user/user_name%2Ccomma%26amp%20space?key=val%2Ccomma%3Fq-mark%26amp",
                apiClient.expandPath(template, uriParams));
    }

}
