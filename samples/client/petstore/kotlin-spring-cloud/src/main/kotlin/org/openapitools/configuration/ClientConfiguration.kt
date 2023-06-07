package org.openapitools.configuration

import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty
import org.springframework.boot.context.properties.ConfigurationProperties
import org.springframework.boot.context.properties.EnableConfigurationProperties
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.cloud.openfeign.security.OAuth2FeignRequestInterceptor
import org.springframework.security.oauth2.client.DefaultOAuth2ClientContext
import org.springframework.security.oauth2.client.OAuth2ClientContext
import org.springframework.security.oauth2.client.token.grant.implicit.ImplicitResourceDetails

@Configuration
@EnableConfigurationProperties
class ClientConfiguration {

    @Bean
    @ConditionalOnProperty("openapipetstore.security.petstore_auth.client-id")
    fun petstoreAuthRequestInterceptor(oAuth2ClientContext: OAuth2ClientContext): OAuth2FeignRequestInterceptor {
        return OAuth2FeignRequestInterceptor(oAuth2ClientContext, petstoreAuthResourceDetails())
    }

    @Bean
    @ConditionalOnProperty("openapipetstore.security.petstore_auth.client-id")
    fun oAuth2ClientContext(): OAuth2ClientContext {
        return DefaultOAuth2ClientContext()
    }

    @Bean
    @ConditionalOnProperty("openapipetstore.security.petstore_auth.client-id")
    @ConfigurationProperties("openapipetstore.security.petstore_auth")
    fun petstoreAuthResourceDetails(): ImplicitResourceDetails {
        val details = ImplicitResourceDetails()
        details.userAuthorizationUri= "http://petstore.swagger.io/api/oauth/dialog"
        return details
    }

    @Value("\${openapipetstore.security.api_key.key:}")
    private lateinit var apiKeyKey: String

    @Bean
    @ConditionalOnProperty("openapipetstore.security.api_key.key")
    fun apiKeyRequestInterceptor(): ApiKeyRequestInterceptor {
        return ApiKeyRequestInterceptor("header", "api_key", this.apiKeyKey)
    }

}
