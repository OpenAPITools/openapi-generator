package io.swagger.client.auth;

import io.swagger.client.QueryParam;

import java.util.Map;
import java.util.Set;

public class OAuth implements Authentication {
  @Override
  public void applyToParams(Set<QueryParam> queryParams, Map<String, String> headerParams) {
    // TODO: support oauth
  }
}
