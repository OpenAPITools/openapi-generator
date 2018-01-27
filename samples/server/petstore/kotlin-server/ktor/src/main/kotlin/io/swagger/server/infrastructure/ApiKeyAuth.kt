package io.swagger.server.infrastructure

import io.ktor.application.ApplicationCall
import io.ktor.application.call
import io.ktor.auth.*
import io.ktor.request.ApplicationRequest
import io.ktor.response.respond


import io.ktor.application.*
import io.ktor.pipeline.*
import io.ktor.request.*
import io.ktor.response.*
import java.util.*

enum class ApiKeyLocation(val location: String) {
    QUERY("query"),
    HEADER("header")
}
data class ApiKey(val value: String): Credential
data class ApiPrincipal(val apiKey: ApiKey?) : Principal
fun ApplicationCall.apiKey(key: String, keyLocation: ApiKeyLocation = ApiKeyLocation.valueOf("header")): ApiKey? = request.apiKey(key, keyLocation)
fun ApplicationRequest.apiKey(key: String, keyLocation: ApiKeyLocation = ApiKeyLocation.valueOf("header")): ApiKey? {
    val value: String? = when(keyLocation) {
        ApiKeyLocation.QUERY -> this.queryParameters[key]
        ApiKeyLocation.HEADER -> this.headers[key]
    }
    when (value) {
        null -> return null
        else -> return ApiKey(value)
    }
}

fun AuthenticationPipeline.apiKeyAuth(apiKeyName: String, authLocation: String, validate: suspend (ApiKey) -> ApiPrincipal?) {
    intercept(AuthenticationPipeline.RequestAuthentication) { context ->
        val credentials = call.request.apiKey(apiKeyName, ApiKeyLocation.values().first {  it.location == authLocation })
        val principal = credentials?.let { validate(it) }

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

