package org.openapitools.client.auth;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.model.OAuth2AccessToken;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class OauthPasswordGrant extends OAuth {

  private String username;
  private String password;

  public OauthPasswordGrant(String tokenUrl, String scopes) {
    super(null, tokenUrl, scopes);
  }

  @Override
  protected OAuth2AccessToken getOAuth2AccessToken() {
    try {
      return service.getAccessTokenPasswordGrant(username, password);
    } catch (Exception e) {
      throw new RuntimeException("Failed to get oauth token", e);
    }
  }

  @Override
  protected OAuthFlow getFlow() {
    return OAuthFlow.password;
  }

  /**
   * Configures Oauth password grant flow
   * Note: this flow is deprecated.
   *
   * @param username
   * @param password
   * @param clientId
   * @param clientSecret
   */
  public void configure(String username, String password, String clientId, String clientSecret) {
    this.username = username;
    this.password = password;
    //TODO the clientId and secret are optional according with the RFC
    service = new ServiceBuilder(clientId)
            .apiSecret(clientSecret)
            .defaultScope(scopes)
            .build(new DefaultApi20Impl(authorizationUrl, tokenUrl));
  }
}