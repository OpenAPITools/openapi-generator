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
}
