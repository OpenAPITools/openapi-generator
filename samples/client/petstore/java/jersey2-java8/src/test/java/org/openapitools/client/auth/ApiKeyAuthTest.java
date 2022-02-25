package org.openapitools.client.auth;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;

import org.openapitools.client.ApiException;
import org.openapitools.client.Pair;
import org.junit.*;
import static org.junit.Assert.*;


public class ApiKeyAuthTest {
    @Test
    public void testApplyToParamsInQuery() throws ApiException {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey("my-api-key");
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        assertEquals(1, queryParams.size());
        for (Pair queryParam : queryParams) {
            assertEquals("my-api-key", queryParam.getValue());
        }

        // no changes to header or cookie parameters
        assertEquals(0, headerParams.size());
        assertEquals(0, cookieParams.size());
    }

    @Test
    public void testApplyToParamsInHeaderWithPrefix() throws ApiException {
            List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        // no changes to query parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN"));
    }

    @Test
    public void testApplyToParamsInCookieWithPrefix() throws ApiException {
            List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("cookie", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        // no changes to query or header parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, headerParams.size());
        assertEquals(1, cookieParams.size());
        assertEquals("Token my-api-token", cookieParams.get("X-API-TOKEN"));
    }
}
