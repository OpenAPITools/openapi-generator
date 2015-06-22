package io.swagger.client.auth;

import io.swagger.client.Pair;

import java.util.Map;
import java.util.Set;

public interface Authentication {
  /** Apply authentication settings to header and query params. */
  void applyToParams(Set<Pair> queryParams, Map<String, String> headerParams);
}
