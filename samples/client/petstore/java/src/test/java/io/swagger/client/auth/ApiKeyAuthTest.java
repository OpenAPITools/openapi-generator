package io.swagger.client.auth;

import java.util.Map;
import java.util.HashMap;

import static org.junit.Assert.*;
import org.junit.*;

public class ApiKeyAuthTest {
  @Test
  public void testProcessParamsInQuery() {
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
    auth.setApiKey("my-api-key");
    auth.processParams(queryParams, headerParams);

    assertEquals(1, queryParams.size());
    assertEquals("my-api-key", queryParams.get("api_key"));
    // no changes to header parameters
    assertEquals(0, headerParams.size());
  }

  @Test
  public void testProcessParamsInHeaderWithPrefix() {
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
    auth.setApiKey("my-api-token");
    auth.setApiKeyPrefix("Token");
    auth.processParams(queryParams, headerParams);

    // no changes to query parameters
    assertEquals(0, queryParams.size());
    assertEquals(1, headerParams.size());
    assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN"));
  }
}
