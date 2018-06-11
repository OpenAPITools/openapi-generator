package io.swagger.client.auth;

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

        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey("my-api-key");
        auth.applyToParams(queryParams, headerParams);

        assertEquals(1, queryParams.size());
        assertEquals("my-api-key", queryParams.get("api_key").get(0));

        // no changes to header parameters
        assertEquals(0, headerParams.size());
    }

    @Test
    public void testApplyToParamsInHeaderWithPrefix() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        HttpHeaders headerParams = new HttpHeaders();

        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams);

        // no changes to query parameters
        assertEquals(0, queryParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN").get(0));
    }
}
