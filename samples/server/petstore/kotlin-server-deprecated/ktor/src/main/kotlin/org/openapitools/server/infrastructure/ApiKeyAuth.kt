package org.openapitools.server.infrastructure

import io.ktor.application.ApplicationCall
import io.ktor.application.call
import io.ktor.auth.Authentication
import io.ktor.auth.AuthenticationFailedCause
import io.ktor.auth.AuthenticationPipeline
import io.ktor.auth.AuthenticationProvider
import io.ktor.auth.Credential
import io.ktor.auth.Principal
import io.ktor.auth.UnauthorizedResponse
import io.ktor.http.auth.HeaderValueEncoding
import io.ktor.http.auth.HttpAuthHeader
import io.ktor.request.ApplicationRequest
import io.ktor.response.respond

enum class ApiKeyLocation(val location: String) {
    QUERY("query"),
    HEADER("header")
}
data class ApiKeyCredential(val value: String): Credential
data class ApiPrincipal(val apiKeyCredential: ApiKeyCredential?) : Principal



/**
* Represents a Api Key authentication provider
* @param name is the name of the provider, or `null` for a default provider
*/
class ApiKeyAuthenticationProvider(name: String?) : AuthenticationProvider(name) {
    internal var authenticationFunction: suspend ApplicationCall.(ApiKeyCredential) -> Principal? = { null }

    var apiKeyName: String = "";

    var apiKeyLocation: ApiKeyLocation = ApiKeyLocation.QUERY;

    /**
    * Sets a validation function that will check given [ApiKeyCredential] instance and return [Principal],
    * or null if credential does not correspond to an authenticated principal
    */
    fun validate(body: suspend ApplicationCall.(ApiKeyCredential) -> Principal?) {
        authenticationFunction = body
    }
}

fun Authentication.Configuration.apiKeyAuth(name: String? = null, configure: ApiKeyAuthenticationProvider.() -> Unit) {
    val provider = ApiKeyAuthenticationProvider(name).apply(configure)
    val apiKeyName = provider.apiKeyName
    val apiKeyLocation = provider.apiKeyLocation
    val authenticate = provider.authenticationFunction

    provider.pipeline.intercept(AuthenticationPipeline.RequestAuthentication) { context ->
        val credentials = call.request.apiKeyAuthenticationCredentials(apiKeyName, apiKeyLocation)
        val principal = credentials?.let { authenticate(call, it) }

        val cause = when {
            credentials == null -> AuthenticationFailedCause.NoCredentials
            principal == null -> AuthenticationFailedCause.InvalidCredentials
            else -> null
        }

        if (cause != null) {
            context.challenge(apiKeyName, cause) {
                // TODO: Verify correct response structure here.
                call.respond(UnauthorizedResponse(HttpAuthHeader.Parameterized("API_KEY", mapOf("key" to apiKeyName), HeaderValueEncoding.QUOTED_ALWAYS)))
                it.complete()
            }
        }

        if (principal != null) {
            context.principal(principal)
        }
    }
}

fun ApplicationRequest.apiKeyAuthenticationCredentials(apiKeyName: String, apiKeyLocation: ApiKeyLocation): ApiKeyCredential? {
    val value: String? = when(apiKeyLocation) {
        ApiKeyLocation.QUERY -> this.queryParameters[apiKeyName]
        ApiKeyLocation.HEADER -> this.headers[apiKeyName]
    }
    when (value) {
        null -> return null
        else -> return ApiKeyCredential(value)
    }
}
