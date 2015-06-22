package io.swagger.client.auth;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import io.swagger.client.Pair;
import org.junit.*;
import static org.junit.Assert.*;


public class ApiKeyAuthTest {
    @Test
    public void testApplyToParamsInQuery() {
        Set<Pair> queryParams = new HashSet<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey("my-api-key");
        auth.applyToParams(queryParams, headerParams);

        assertEquals(1, queryParams.size());
        for (Pair queryParam : queryParams) {
            assertEquals("my-api-key", queryParam.getValue());
        }

        // no changes to header parameters
        assertEquals(0, headerParams.size());
    }

    @Test
    public void testApplyToParamsInHeaderWithPrefix() {
            Set<Pair> queryParams = new HashSet<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams);

        // no changes to query parameters
        assertEquals(0, queryParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN"));
    }
}
