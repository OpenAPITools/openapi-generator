package io.swagger.client.auth;

import java.util.Map;

public class OAuth implements Authentication {
  @Override
  public void processParams(Map<String, String> queryParams, Map<String, String> headerParams) {
    // TODO: support oauth
  }
}
