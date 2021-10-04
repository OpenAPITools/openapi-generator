package org.openapitools.client.auth;

import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import feign.RequestInterceptor;
import feign.RequestTemplate;

import java.util.Collection;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public abstract class OAuth implements RequestInterceptor {

  static final int MILLIS_PER_SECOND = 1000;

  public interface AccessTokenListener {
    void notify(OAuth2AccessToken token);
  }

  private volatile String accessToken;
  private Long expirationTimeMillis;
  private AccessTokenListener accessTokenListener;

  protected OAuth20Service service;
  protected String scopes;
  protected String authorizationUrl;
  protected String tokenUrl;

  public OAuth(String authorizationUrl, String tokenUrl, String scopes) {
    this.scopes = scopes;
    this.authorizationUrl = authorizationUrl;
    this.tokenUrl = tokenUrl;
  }

  @Override
  public void apply(RequestTemplate template) {
    // If the request already have an authorization (eg. Basic auth), do nothing
    if (requestContainsNonOauthAuthorization(template)) {
      return;
    }
    String accessToken = getAccessToken();
    if (accessToken != null) {
      template.header("Authorization", "Bearer " + accessToken);
    }
  }

  private boolean requestContainsNonOauthAuthorization(RequestTemplate template) {
    Collection<String> authorizations = template.headers().get("Authorization");
    if (authorizations == null) {
        return false;
    }
    return !authorizations.stream()
            .anyMatch(authHeader -> !authHeader.equalsIgnoreCase("Bearer"));
  }

  private synchronized void updateAccessToken() {
    OAuth2AccessToken accessTokenResponse;
    accessTokenResponse = getOAuth2AccessToken();
    if (accessTokenResponse != null && accessTokenResponse.getAccessToken() != null) {
      setAccessToken(accessTokenResponse.getAccessToken(), accessTokenResponse.getExpiresIn());
      if (accessTokenListener != null) {
        accessTokenListener.notify(accessTokenResponse);
      }
    }
  }

  abstract OAuth2AccessToken getOAuth2AccessToken();

  abstract OAuthFlow getFlow();

  public synchronized void registerAccessTokenListener(AccessTokenListener accessTokenListener) {
    this.accessTokenListener = accessTokenListener;
  }

  public synchronized String getAccessToken() {
    // If first time, get the token
    if (expirationTimeMillis == null || System.currentTimeMillis() >= expirationTimeMillis) {
      updateAccessToken();
    }
    return accessToken;
  }

  /**
   * Manually sets the access token
   * @param accessToken The access token
   * @param expiresIn Seconds until the token expires
   */
  public synchronized void setAccessToken(String accessToken, Integer expiresIn) {
    this.accessToken = accessToken;
    this.expirationTimeMillis = expiresIn == null ? null : System.currentTimeMillis() + expiresIn * MILLIS_PER_SECOND;
  }

}