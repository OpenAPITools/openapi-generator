package org.openapitools.server.infrastructure

import io.ktor.http.auth.*
import io.ktor.server.application.*
import io.ktor.server.auth.*
import io.ktor.server.request.*
import io.ktor.server.response.*

enum class ApiKeyLocation(val location: String) {
    QUERY("query"),
    HEADER("header")
}

data class ApiKeyCredential(val value: String) : Credential
data class ApiPrincipal(val apiKeyCredential: ApiKeyCredential?) : Principal

/**
* Represents an Api Key authentication provider
*/
class ApiKeyAuthenticationProvider(configuration: Configuration) : AuthenticationProvider(configuration) {

    private val authenticationFunction = configuration.authenticationFunction

    private val apiKeyName: String = configuration.apiKeyName

    private val apiKeyLocation: ApiKeyLocation = configuration.apiKeyLocation

    override suspend fun onAuthenticate(context: AuthenticationContext) {
        val call = context.call
        val credentials = call.request.apiKeyAuthenticationCredentials(apiKeyName, apiKeyLocation)
        val principal = credentials?.let { authenticationFunction.invoke(call, it) }

        val cause = when {
            credentials == null -> AuthenticationFailedCause.NoCredentials
            principal == null -> AuthenticationFailedCause.InvalidCredentials
            else -> null
        }

        if (cause != null) {
            context.challenge(apiKeyName, cause) { challenge, call ->
                call.respond(
                    UnauthorizedResponse(
                        HttpAuthHeader.Parameterized(
                            "API_KEY",
                            mapOf("key" to apiKeyName),
                            HeaderValueEncoding.QUOTED_ALWAYS
                        )
                    )
                )
                challenge.complete()
            }
        }

        if (principal != null) {
            context.principal(principal)
        }
    }

    class Configuration internal constructor(name: String?) : Config(name) {

        internal var authenticationFunction: suspend ApplicationCall.(ApiKeyCredential) -> Principal? = {
            throw NotImplementedError(
                "Api Key auth validate function is not specified. Use apiKeyAuth { validate { ... } } to fix."
            )
        }

        var apiKeyName: String = ""

        var apiKeyLocation: ApiKeyLocation = ApiKeyLocation.QUERY

        /**
        * Sets a validation function that will check given [ApiKeyCredential] instance and return [Principal],
        * or null if credential does not correspond to an authenticated principal
        */
        fun validate(body: suspend ApplicationCall.(ApiKeyCredential) -> Principal?) {
            authenticationFunction = body
        }
    }
}

fun AuthenticationConfig.apiKeyAuth(
    name: String? = null,
    configure: ApiKeyAuthenticationProvider.Configuration.() -> Unit
) {
    val configuration = ApiKeyAuthenticationProvider.Configuration(name).apply(configure)
    val provider = ApiKeyAuthenticationProvider(configuration)
    register(provider)
}

fun ApplicationRequest.apiKeyAuthenticationCredentials(
    apiKeyName: String,
    apiKeyLocation: ApiKeyLocation
): ApiKeyCredential? {
    val value: String? = when (apiKeyLocation) {
        ApiKeyLocation.QUERY -> this.queryParameters[apiKeyName]
        ApiKeyLocation.HEADER -> this.headers[apiKeyName]
    }
    return when (value) {
        null -> null
        else -> ApiKeyCredential(value)
    }
}
