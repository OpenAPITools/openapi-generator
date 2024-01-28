package org.openapitools.client.auth;

import feign.RequestInterceptor;
import feign.RequestTemplate;

/**
 * An interceptor that adds the request header needed to use HTTP bearer authentication.
 */
public class HttpBearerAuth implements RequestInterceptor {
  private final String scheme;
  private String bearerToken;

  public HttpBearerAuth(String scheme) {
    this.scheme = scheme;
  }

  /**
   * Gets the token, which together with the scheme, will be sent as the value of the Authorization header.
   */
  public String getBearerToken() {
    return bearerToken;
  }

  /**
   * Sets the token, which together with the scheme, will be sent as the value of the Authorization header.
   */
  public void setBearerToken(String bearerToken) {
    this.bearerToken = bearerToken;
  }

  @Override
  public void apply(RequestTemplate template) {
    if(bearerToken == null) {
      return;
    }

    template.header("Authorization", (scheme != null ? upperCaseBearer(scheme) + " " : "") + bearerToken);
  }

  private static String upperCaseBearer(String scheme) {
    return ("bearer".equalsIgnoreCase(scheme)) ? "Bearer" : scheme;
  }
}
