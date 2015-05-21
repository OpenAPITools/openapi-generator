package io.swagger.client;

import java.util.Map;
import java.util.HashMap;

import io.swagger.client.auth.Authentication;
import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.auth.ApiKeyAuth;

public class Configuration {
  private static final Map<String, Authentication> AUTH;

  static {
    AUTH = new HashMap<String, Authentication>();
    
    
    AUTH.put("api_key", new ApiKeyAuth("header", "api_key"));
    
    
    
    
    // TODO: support oauth
    
  }

  public static Authentication getAuthentication(String authName) {
    return AUTH.get(authName);
  }
}
