package org.openapitools.server

// Use this file to hold package-level internal functions that return receiver object passed to the `install` method.
import io.ktor.http.*
import io.ktor.server.auth.*
import io.ktor.server.config.*
import io.ktor.util.*
import java.time.Duration
import java.util.concurrent.TimeUnit
import io.ktor.server.plugins.compression.*
import io.ktor.server.plugins.hsts.*


/**
 * Application block for [HSTS] configuration.
 *
 * This file may be excluded in .openapi-generator-ignore,
 * and application-specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/hsts.html
 */
internal fun ApplicationHstsConfiguration(): HSTSConfig.() -> Unit {
    return {
        maxAgeInSeconds = TimeUnit.DAYS.toSeconds(365)
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
 * and application-specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/compression.html
 */
internal fun ApplicationCompressionConfiguration(): CompressionConfig.() -> Unit {
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
fun applicationAuthProvider(config: ApplicationConfig): OAuthServerSettings =
    OAuthServerSettings.OAuth2ServerSettings(
        name = "petstore_auth",
        authorizeUrl = "http://petstore.swagger.io/api/oauth/dialog",
        accessTokenUrl = "",
        requestMethod = HttpMethod.Get,
        clientId = config.property("auth.oauth.petstore_auth.clientId").getString(),
        clientSecret = config.property("auth.oauth.petstore_auth.clientSecret").getString(),
        defaultScopes = listOf("write:pets", "read:pets")
    )
