package org.openapitools.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.oauth2.client.AuthorizedClientServiceOAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.OAuth2AuthorizeRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;

import feign.RequestInterceptor;
import feign.RequestTemplate;

import org.springframework.context.annotation.Configuration;


@Configuration
public class ClientConfiguration {

  private static final String CLIENT_PRINCIPAL_IMPLICIT = "oauth2FeignClient";

  @Bean
  @ConditionalOnProperty( prefix = "spring.security.oauth2.client.registration.petstoreAuthImplicit", name = "enabled", havingValue = "true" )
  public OAuth2RequestInterceptor implicitOAuth2RequestInterceptor(final OAuth2AuthorizedClientManager implicitAuthorizedClientManager ) {
     return new OAuth2RequestInterceptor(OAuth2AuthorizeRequest.withClientRegistrationId("petstoreAuthImplicit")
            .principal( new AnonymousAuthenticationToken( CLIENT_PRINCIPAL_IMPLICIT, CLIENT_PRINCIPAL_IMPLICIT, AuthorityUtils.createAuthorityList( "ROLE_ANONYMOUS" ) ) )
            .build(), implicitAuthorizedClientManager );
  }

  @Bean
  @ConditionalOnProperty( prefix = "spring.security.oauth2.client.registration.petstoreAuthImplicit", name = "enabled", havingValue = "true" )
  public OAuth2AuthorizedClientManager implicitAuthorizedClientManager(ClientRegistrationRepository clientRegistrationRepository,
        OAuth2AuthorizedClientService authorizedClientService ) {
    return new AuthorizedClientServiceOAuth2AuthorizedClientManager( clientRegistrationRepository, authorizedClientService );
  }
  @Value("${openapipetstore.security.apiKey.key:}")
  private String apiKeyKey;

  @Bean
  @ConditionalOnProperty(name = "openapipetstore.security.apiKey.key")
  public ApiKeyRequestInterceptor apiKeyRequestInterceptor() {
    return new ApiKeyRequestInterceptor("header", "api_key", this.apiKeyKey);
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
        throw new OAuth2AuthenticationException( "Client failed to authenticate");
      }
     return authorizedClient.getAccessToken();
    }

    public String getBearerToken() {
      final OAuth2AccessToken accessToken = getAccessToken();
      return String.format( "%s %s", accessToken.getTokenType().getValue(), accessToken.getTokenValue() );
    }
  }

}
