package io.swagger.client;

import io.swagger.client.auth.*;

import static org.junit.Assert.*;
import org.junit.*;

public class ConfigurationTest {
  @Test
  public void testGetAuthentication() {
    Authentication auth = Configuration.getAuthentication("api_key");
    assertNotNull(auth);
    assertTrue(auth instanceof ApiKeyAuth);
    ApiKeyAuth apiKeyAuth = (ApiKeyAuth) auth;
    assertEquals("header", apiKeyAuth.getLocation());
    assertEquals("api_key", apiKeyAuth.getParamName());

    auth = Configuration.getAuthentication("petstore_auth");
    assertTrue(auth instanceof OAuth);

    assertNull(Configuration.getAuthentication("unknown"));
  }

  @Test
  public void testSetUsername() {
    try {
      Configuration.setUsername("my-username");
      fail("should throw RuntimeException");
    } catch (RuntimeException e) {
    }
  }

  @Test
  public void testSetPassword() {
    try {
      Configuration.setPassword("my-password");
      fail("should throw RuntimeException");
    } catch (RuntimeException e) {
    }
  }

  @Test
  public void testSetApiKeyAndPrefix() {
    ApiKeyAuth auth = (ApiKeyAuth) Configuration.getAuthentication("api_key");
    auth.setApiKey(null);
    auth.setApiKeyPrefix(null);

    Configuration.setApiKey("my-api-key");
    Configuration.setApiKeyPrefix("Token");
    assertEquals("my-api-key", auth.getApiKey());
    assertEquals("Token", auth.getApiKeyPrefix());

    auth.setApiKey(null);
    auth.setApiKeyPrefix(null);
  }

  @Test
  public void testDefaultApiClient() {
    ApiClient apiClient = Configuration.getDefaultApiClient();
    assertNotNull(apiClient);
    assertEquals("http://petstore.swagger.io/v2", apiClient.getBasePath());
    assertFalse(apiClient.isDebugging());
  }
}
