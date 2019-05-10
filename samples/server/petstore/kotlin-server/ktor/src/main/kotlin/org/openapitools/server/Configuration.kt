package org.openapitools.server

// Use this file to hold package-level internal functions that return receiver object passed to the `install` method.
import io.ktor.auth.OAuthServerSettings
import io.ktor.features.Compression
import io.ktor.features.HSTS
import io.ktor.features.deflate
import io.ktor.features.gzip
import io.ktor.features.minimumSize
import io.ktor.http.HttpMethod
import io.ktor.util.KtorExperimentalAPI
import java.time.Duration
import java.util.concurrent.Executors

import org.openapitools.server.settings


/**
 * Application block for [HSTS] configuration.
 *
 * This file may be excluded in .openapi-generator-ignore,
 * and application specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/hsts.html
 */
internal fun ApplicationHstsConfiguration(): HSTS.Configuration.() -> Unit {
    return {
        maxAge = Duration.ofDays(365)
        includeSubDomains = true
        preload = false

        // You may also apply any custom directives supported by specific user-agent. For example:
        // customDirectives.put("redirectHttpToHttps", "false")
    }
}

/**
 * Application block for [Compression] configuration.
 *
 * This file may be excluded in .openapi-generator-ignore,
 * and application specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/compression.html
 */
internal fun ApplicationCompressionConfiguration(): Compression.Configuration.() -> Unit {
    return {
        gzip {
            priority = 1.0
        }
        deflate {
            priority = 10.0
            minimumSize(1024) // condition
        }
    }
}

// Defines authentication mechanisms used throughout the application.
@KtorExperimentalAPI
val ApplicationAuthProviders: Map<String, OAuthServerSettings> = listOf<OAuthServerSettings>(
        OAuthServerSettings.OAuth2ServerSettings(
            name = "petstore_auth",
            authorizeUrl = "http://petstore.swagger.io/api/oauth/dialog",
            accessTokenUrl = "",
            requestMethod = HttpMethod.Get,
            clientId = settings.property("auth.oauth.petstore_auth.clientId").getString(),
            clientSecret = settings.property("auth.oauth.petstore_auth.clientSecret").getString(),
            defaultScopes = listOf("write:pets", "read:pets")
        )
//        OAuthServerSettings.OAuth2ServerSettings(
//                name = "facebook",
//                authorizeUrl = "https://graph.facebook.com/oauth/authorize",
//                accessTokenUrl = "https://graph.facebook.com/oauth/access_token",
//                requestMethod = HttpMethod.Post,
//
//                clientId = "settings.property("auth.oauth.facebook.clientId").getString()",
//                clientSecret = "settings.property("auth.oauth.facebook.clientSecret").getString()",
//                defaultScopes = listOf("public_profile")
//        )
).associateBy { it.name }

// Provides an application-level fixed thread pool on which to execute coroutines (mainly)
internal val ApplicationExecutors = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 4)
