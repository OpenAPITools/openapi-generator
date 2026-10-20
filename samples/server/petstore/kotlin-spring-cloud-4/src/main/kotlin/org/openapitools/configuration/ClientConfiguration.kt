package org.openapitools.configuration

import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty



import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

import org.springframework.security.authentication.AnonymousAuthenticationToken
import org.springframework.security.oauth2.client.AuthorizedClientServiceOAuth2AuthorizedClientManager
import org.springframework.security.oauth2.client.OAuth2AuthorizeRequest
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientManager
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientService
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository
import org.springframework.security.oauth2.core.OAuth2AuthenticationException
import org.springframework.security.oauth2.core.OAuth2AccessToken
import org.springframework.security.core.authority.AuthorityUtils
import org.springframework.http.HttpHeaders

import feign.RequestInterceptor
import feign.RequestTemplate

@Configuration
class ClientConfiguration {


    @Bean
    @ConditionalOnProperty(prefix = "spring.security.oauth2.client.registration.petstore_authImplicit", name = ["enabled"], havingValue = "true")
    fun implicitOAuth2RequestInterceptor(implicitAuthorizedClientManager: OAuth2AuthorizedClientManager): OAuth2RequestInterceptor {
        return OAuth2RequestInterceptor(
            OAuth2AuthorizeRequest.withClientRegistrationId("petstore_authImplicit")
                .principal(AnonymousAuthenticationToken(CLIENT_PRINCIPAL_IMPLICIT, CLIENT_PRINCIPAL_IMPLICIT, AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS")))
                .build(),
            implicitAuthorizedClientManager
        )
    }

    @Bean
    @ConditionalOnProperty(prefix = "spring.security.oauth2.client.registration.petstore_authImplicit", name = ["enabled"], havingValue = "true")
    fun implicitAuthorizedClientManager(
        clientRegistrationRepository: ClientRegistrationRepository,
        authorizedClientService: OAuth2AuthorizedClientService
    ): OAuth2AuthorizedClientManager {
        return AuthorizedClientServiceOAuth2AuthorizedClientManager(clientRegistrationRepository, authorizedClientService)
    }

    @Value("\${openapipetstore.security.api_key.key:}")
    private lateinit var apiKeyKey: String

    @Bean
    @ConditionalOnProperty("openapipetstore.security.api_key.key")
    fun apiKeyRequestInterceptor(): ApiKeyRequestInterceptor {
        return ApiKeyRequestInterceptor("header", "api_key", this.apiKeyKey)
    }

    class OAuth2RequestInterceptor(
        private val oAuth2AuthorizeRequest: OAuth2AuthorizeRequest,
        private val oAuth2AuthorizedClientManager: OAuth2AuthorizedClientManager
    ) : RequestInterceptor {

        override fun apply(template: RequestTemplate) {
            template.header(HttpHeaders.AUTHORIZATION, getBearerToken())
        }

        fun getAccessToken(): OAuth2AccessToken {
            val authorizedClient = oAuth2AuthorizedClientManager.authorize(oAuth2AuthorizeRequest)
                ?: throw OAuth2AuthenticationException("Client failed to authenticate")
            return authorizedClient.accessToken
        }

        fun getBearerToken(): String {
            val accessToken = getAccessToken()
            return String.format(java.util.Locale.ROOT, "%s %s", accessToken.tokenType?.value, accessToken.tokenValue)
        }
    }

    companion object {
        private const val CLIENT_PRINCIPAL_IMPLICIT = "oauth2FeignClient"
    }
}
