package io.swagger.configuration;

import feign.Logger;
import feign.auth.BasicAuthRequestInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.cloud.security.oauth2.client.feign.OAuth2FeignRequestInterceptor;
import org.springframework.security.oauth2.client.DefaultOAuth2ClientContext;
import org.springframework.security.oauth2.client.resource.BaseOAuth2ProtectedResourceDetails;
import org.springframework.security.oauth2.client.token.grant.client.ClientCredentialsResourceDetails;
import org.springframework.security.oauth2.client.token.grant.code.AuthorizationCodeResourceDetails;
import org.springframework.security.oauth2.client.token.grant.implicit.ImplicitResourceDetails;
import org.springframework.security.oauth2.client.token.grant.password.ResourceOwnerPasswordResourceDetails;
import org.springframework.security.oauth2.common.exceptions.InvalidGrantException;
import org.springframework.security.oauth2.common.exceptions.OAuth2Exception;

@Configuration
@EnableConfigurationProperties
public class ClientConfiguration {

  @Value("${swaggerPetstore.security.apiKey.key:}")
  private String apiKeyKey;

  @Bean
  @ConditionalOnProperty(name = "swaggerPetstore.security.apiKey.key")
  public ApiKeyRequestInterceptor apiKeyRequestInterceptor() {
    return new ApiKeyRequestInterceptor("header", "api_key", this.apiKeyKey);
  }

  @Bean
  @ConditionalOnProperty("swaggerPetstore.security.petstoreAuth.client-id")
  public OAuth2FeignRequestInterceptor petstoreAuthRequestInterceptor() {
    return new OAuth2FeignRequestInterceptor(new DefaultOAuth2ClientContext(), petstoreAuthResourceDetails());
  }

  @Bean
  @ConditionalOnProperty("swaggerPetstore.security.petstoreAuth.client-id")
  @ConfigurationProperties("swaggerPetstore.security.petstoreAuth")
  public ImplicitResourceDetails petstoreAuthResourceDetails() {
    ImplicitResourceDetails details = new ImplicitResourceDetails();
    details.setUserAuthorizationUri("http://petstore.swagger.io/api/oauth/dialog");
    return details;
  }

}
