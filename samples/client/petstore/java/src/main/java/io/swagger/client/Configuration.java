package io.swagger.client;

import java.util.Map;
import java.util.HashMap;

import io.swagger.client.auth.Authentication;
import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.auth.OAuth;

public class Configuration {
  private static final Map<String, Authentication> AUTH;

  static {
    // setup authentications
    AUTH = new HashMap<String, Authentication>();
    
    
    AUTH.put("api_key", new ApiKeyAuth("header", "api_key"));
    
    
    
    
    AUTH.put("petstore_auth", new OAuth());
    
  }

  public static Authentication getAuthentication(String authName) {
    return AUTH.get(authName);
  }

  /** Set username for the first HTTP basic authentication. */
  public static void setUsername(String username) {
    for (Authentication auth : AUTH.values()) {
      if (auth instanceof HttpBasicAuth) {
        ((HttpBasicAuth) auth).setUsername(username);
        return;
      }
    }
    throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /** Set password for the first HTTP basic authentication. */
  public static void setPassword(String password) {
    for (Authentication auth : AUTH.values()) {
      if (auth instanceof HttpBasicAuth) {
        ((HttpBasicAuth) auth).setPassword(password);
        return;
      }
    }
    throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /** Set API key value for the first API key authentication. */
  public static void setApiKey(String apiKey) {
    for (Authentication auth : AUTH.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKey(apiKey);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /** Set API key prefix for the first API key authentication. */
  public static void setApiKeyPrefix(String apiKeyPrefix) {
    for (Authentication auth : AUTH.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKeyPrefix(apiKeyPrefix);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }
}
