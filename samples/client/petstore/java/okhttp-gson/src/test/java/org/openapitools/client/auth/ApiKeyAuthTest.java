package org.openapitools.client.auth;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.*;
import org.openapitools.client.ApiException;
import org.openapitools.client.Pair;

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
    public void testApplyToParamsInQueryWithNullValue() throws ApiException {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey(null);
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        // no changes to parameters
        assertEquals(0, queryParams.size());
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

        // no changes to query or cookie parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN"));
    }

    @Test
    public void testApplyToParamsInHeaderWithNullValue() throws ApiException {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey(null);
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        // no changes to parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(0, headerParams.size());
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

    @Test
    public void testApplyToParamsInCookieWithNullValue() throws ApiException {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("cookie", "X-API-TOKEN");
        auth.setApiKey(null);
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams, null, null, null);

        // no changes to parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(0, headerParams.size());
    }
}
