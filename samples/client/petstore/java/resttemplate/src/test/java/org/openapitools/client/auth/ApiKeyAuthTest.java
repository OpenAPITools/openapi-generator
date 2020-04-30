package org.openapitools.client.auth;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;

import org.junit.*;
import org.springframework.http.HttpHeaders;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import static org.junit.Assert.*;

public class ApiKeyAuthTest {
    @Test
    public void testApplyToParamsInQuery() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        HttpHeaders headerParams = new HttpHeaders();
        MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey("my-api-key");
        auth.applyToParams(queryParams, headerParams, cookieParams);

        assertEquals(1, queryParams.size());
        assertEquals("my-api-key", queryParams.get("api_key").get(0));

        // no changes to header or cookie parameters
        assertEquals(0, headerParams.size());
        assertEquals(0, cookieParams.size());
    }

    @Test
    public void testApplyToParamsInHeaderWithPrefix() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        HttpHeaders headerParams = new HttpHeaders();
        MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams);

        // no changes to query or cookie parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN").get(0));
    }

    @Test
    public void testApplyToParamsInCookieWithPrefix() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        HttpHeaders headerParams = new HttpHeaders();
        MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();

        ApiKeyAuth auth = new ApiKeyAuth("cookie", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams, cookieParams);

        // no changes to query or cookie parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, headerParams.size());
        assertEquals(1, cookieParams.size());
        assertEquals("Token my-api-token", cookieParams.get("X-API-TOKEN").get(0));
    }
}
