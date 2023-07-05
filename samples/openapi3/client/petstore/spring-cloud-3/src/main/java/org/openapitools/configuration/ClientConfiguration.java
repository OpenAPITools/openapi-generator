package org.openapitools.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.oauth2.client.AuthorizedClientServiceOAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.OAuth2AuthorizeRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.security.core.AuthenticationException;

import feign.RequestInterceptor;
import feign.RequestTemplate;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@ConditionalOnProperty( "spring.security.oauth2.client.registration.petstoreAuth.client-id" )
@Configuration
public class ClientConfiguration {

  private static final String CLIENT_PRINCIPAL = "oauth2FeignClient";

  @Bean
  public OAuth2RequestInterceptor requestInterceptor(final OAuth2AuthorizedClientManager oAuth2AuthorizedClientManager ) {
     return new OAuth2RequestInterceptor(OAuth2AuthorizeRequest.withClientRegistrationId("petstoreAuth")
            .principal( new AnonymousAuthenticationToken( CLIENT_PRINCIPAL, CLIENT_PRINCIPAL, AuthorityUtils.createAuthorityList( "ROLE_ANONYMOUS" ) ) )
            .build(), oAuth2AuthorizedClientManager );
  }

  @Bean
  public OAuth2AuthorizedClientManager authorizedClientManager(ClientRegistrationRepository clientRegistrationRepository,
        OAuth2AuthorizedClientService authorizedClientService ) {
    return new AuthorizedClientServiceOAuth2AuthorizedClientManager( clientRegistrationRepository, authorizedClientService );
  }

  public static class OAuth2RequestInterceptor implements RequestInterceptor {

    private final OAuth2AuthorizedClientManager oAuth2AuthorizedClientManager;
    private final OAuth2AuthorizeRequest oAuth2AuthorizeRequest;

    public OAuth2RequestInterceptor(OAuth2AuthorizeRequest oAuth2AuthorizeRequest,OAuth2AuthorizedClientManager oAuth2AuthorizedClientManager){
      this.oAuth2AuthorizeRequest = oAuth2AuthorizeRequest;
      this.oAuth2AuthorizedClientManager = oAuth2AuthorizedClientManager;
    }

    @Override
    public void apply( final RequestTemplate template ) {
      template.header( HttpHeaders.AUTHORIZATION, getBearerToken() );
    }

    public OAuth2AccessToken getAccessToken() {
      final OAuth2AuthorizedClient authorizedClient = oAuth2AuthorizedClientManager.authorize(oAuth2AuthorizeRequest);
      if (authorizedClient == null) {
        throw new FeignAuthenticationException();
      }
     return authorizedClient.getAccessToken();
    }

    public String getBearerToken() {
      final OAuth2AccessToken accessToken = getAccessToken();
      return String.format( "%s %s", accessToken.getTokenType().getValue(), accessToken.getTokenValue() );
    }
  }

  public static class FeignAuthenticationException extends AuthenticationException {

    public FeignAuthenticationException() {
      super( "Client failed to authenticate. Check the setting like authorization-grant-type" );
    }
  }

  @Value("${openapipetstore.security.apiKey.key:}")
  private String apiKeyKey;

  @Bean
  @ConditionalOnProperty(name = "openapipetstore.security.apiKey.key")
  public ApiKeyRequestInterceptor apiKeyRequestInterceptor() {
    return new ApiKeyRequestInterceptor("header", "api_key", this.apiKeyKey);
  }

}
