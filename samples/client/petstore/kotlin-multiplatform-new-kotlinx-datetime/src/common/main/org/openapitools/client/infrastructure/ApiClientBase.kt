package org.openapitools.client.infrastructure

import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.engine.HttpClientEngine
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.json.serializer.KotlinxSerializer
import kotlinx.serialization.json.Json
import io.ktor.client.request.*

import org.openapitools.client.auth.*

public abstract class ApiClientBase {
    protected val baseUrl: String
    protected val client: HttpClient
    protected var apiKeyAuth: Map<String, ApiKeyAuth> = mapOf(
        "api_key" to ApiKeyAuth(ApiKeyAuth.Location.Header, "api_key"),
    )
    protected var basicAuth: Map<String, HttpBasicAuth> = mapOf(
    )
    protected var bearerAuth: Map<String, HttpBearerAuth> = mapOf(
    )
    protected var oAuth: Map<String, OAuth> = mapOf(
        "petstore_auth" to OAuth(),
    )

    protected constructor(baseUrl: String, httpClientEngine: HttpClientEngine?, json: Json = Json {}) {
        this.baseUrl = baseUrl
        val serializer = KotlinxSerializer(json)
        val jsonConfig: JsonFeature.Config.() -> Unit = { this.serializer = serializer }
        val clientConfig: (HttpClientConfig<*>) -> Unit = { it.install(JsonFeature, jsonConfig) }
        client = if (httpClientEngine == null) {
            HttpClient(clientConfig)
        } else {
            HttpClient(httpClientEngine, clientConfig)
        }
    }

    protected constructor(baseUrl: String, client: HttpClient) {
        this.baseUrl = baseUrl
        this.client = client
    }

    protected constructor(
        baseUrl: String,
        client: HttpClient,
        apiKeyAuth: Map<String, ApiKeyAuth>,
        basicAuth: Map<String, HttpBasicAuth>,
        bearerAuth: Map<String, HttpBearerAuth>,
        oAuth: Map<String, OAuth>,
    ) {
        this.baseUrl = baseUrl
        this.client = client
        this.apiKeyAuth = apiKeyAuth
        this.basicAuth = basicAuth
        this.bearerAuth = bearerAuth
        this.oAuth = oAuth
    }

    protected fun HttpRequestBuilder.addAuthentication(apiKeyAuths: List<String>, basicAuths: List<String>, bearerAuths: List<String>, oAuths: List<String>) {
        for (name in apiKeyAuths) {
            val auth = apiKeyAuth[name] ?: throw IllegalStateException("ApiKeyAuth \"$name\" was configured, but not found")
            if (auth.isConfigured) {
                auth.configure(this)
                return
            }
        }
        for (name in basicAuths) {
            val auth = basicAuth[name] ?: throw IllegalStateException("HttpBasicAuth \"$name\" was configured, but not found")
            if (auth.isConfigured) {
                auth.configure(this)
                return
            }
        }
        for (name in bearerAuths) {
            val auth = bearerAuth[name] ?: throw IllegalStateException("HttpBearerAuth \"$name\" was configured, but not found")
            if (auth.isConfigured) {
                auth.configure(this)
                return
            }
        }
        for (name in oAuths) {
            val auth = oAuth[name] ?: throw IllegalStateException("OAuth \"$name\" was configured, but not found")
            if (auth.isConfigured) {
                auth.configure(this)
                return
            }
        }
        throw IllegalStateException(
            """
            No valid authentication configured, please configure one of the following:
                API Key Authentication: ${apiKeyAuths.joinToString()}
                HTTP Bearer Authentication: ${apiKeyAuths.joinToString()}
                HTTP Basic Authentication: ${apiKeyAuths.joinToString()}
                OAuth: ${apiKeyAuths.joinToString()}
            """.trimIndent()
        )
    }

    /**
     * Allows configuring of a [ApiKeyAuth]. Please look at the documentation to find out which are available.
     */
    public fun configureApiKey(authName: String, apiKey: String, apiKeyPrefix: String? = null) {
        val auth = apiKeyAuth[authName] ?: throw IllegalArgumentException("Cannot find ApiKeyAuth named \"$authName\"")
        auth.apiKey = apiKey
        auth.apiKeyPrefix = apiKeyPrefix
    }

    /**
     * Allows configuring of a [ApiKeyAuth] via lambda.  Please look at the documentation to find out which are
     * available.
     *
     * It is required that you specify the API key via `key()`.
     *
     * Example:
     * ```kotlin
     * api.configureApiKey("your-authentication") {
     *     key("your-api-key")
     *     keyPrefix("YourPrefix ") // Optional
     * }
     * ```
     *
     * @param authName The name of the authentication to be configured.
     * @exception IllegalArgumentException Thrown when the [ApiKeyAuth] was not found or no API key was given.
     */
    public fun configureApiKey(authName: String, block: ApiKeyAuth.Configurer.() -> Unit) {
        val auth = apiKeyAuth[authName] ?: throw IllegalArgumentException("Cannot find ApiKeyAuth named \"$authName\"")
        val configurer = ApiKeyConfigurer()
        block(configurer)
        val key = configurer.key ?: throw IllegalArgumentException("Specifying \"key\" is required")
        auth.apiKey = key
        auth.apiKeyPrefix = configurer.keyPrefix
    }

    /**
     * Allows configuring of a [HttpBearerAuth]. Please look at the documentation to find out which are available.
     */
    public fun configureBearer(authName: String, token: String) {
        val auth =
            bearerAuth[authName] ?: throw IllegalArgumentException("Cannot find HttpBearerAuth named \"$authName\"")
        auth.bearerToken = token
    }

    /**
     * Allows configuring of a [HttpBearerAuth] via lambda. Please look at the documentation to find out which are
     * available.
     *
     * It is required that you specify the bearer token via `token()`.
     *
     * Example:
     * ```kotlin
     * api.configureBearer("your-authentication") {
     *     token("your-token")
     * }
     * ```
     *
     * @param authName The name of the authentication to be configured.
     * @exception IllegalArgumentException Thrown when the [HttpBasicAuth] was not found or no bearer token was given.
     */
    public fun configureBearer(authName: String, block: HttpBearerAuth.Configurer.() -> Unit) {
        val auth =
            bearerAuth[authName] ?: throw IllegalArgumentException("Cannot find HttpBearerAuth named \"$authName\"")
        val configurer = BearerConfigurer()
        block(configurer)
        val token = configurer.token ?: throw IllegalArgumentException("Specifying \"token\" is required")
        auth.bearerToken = token
    }

    /**
     * Allows configuring of a [HttpBasicAuth]. Please look at the documentation to find out which are available.
     * At the minimum either [username] or [password] must be specified.
     *
     * @exception IllegalArgumentException Thrown if neither [username] nor [password] is specified.
     */
    public fun configureBasic(authName: String, username: String?, password: String?) {
        val auth =
            basicAuth[authName] ?: throw IllegalArgumentException("Cannot find HttpBasicAuth named \"$authName\"")
        if (username == null && password == null) {
            throw IllegalArgumentException("One of \"username\" or \"password\" is required")
        }
        auth.username = username
        auth.password = password
    }

    /**
     * Allows configuring of a [HttpBasicAuth] via lambda.  Please look at the documentation to find out which are
     * available.
     *
     * At the minimum either `username()` or `password()` must be called.
     *
     * Example:
     * ```kotlin
     * api.configureBasic("your-authentication") {
     *     // At minimum one has to be specified
     *     username("your-username")
     *     password("your-password")
     * }
     * ```
     *
     * @param authName The name of the authentication to be configured.
     * @exception IllegalArgumentException Thrown when the [HttpBasicAuth] was not found or neither username nor password
     * was given.
     */
    public fun configureBasic(authName: String, block: HttpBasicAuth.Configurer.() -> Unit) {
        val auth =
            basicAuth[authName] ?: throw IllegalArgumentException("Cannot find HttpBasicAuth named \"$authName\"")
        val configurer = BasicConfigurer()
        block(configurer)
        val username = configurer.username
        val password = configurer.password
        if (username == null && password == null) {
            throw IllegalArgumentException("One of \"username\" or \"password\" is required")
        }
        auth.username = username
        auth.password = password
    }

    /**
     * Allows configuring of a [OAuth]. Please look at the documentation to find out which are available.
     */
    public fun configureOAuth(authName: String, token: String) {
        val auth = oAuth[authName] ?: throw IllegalArgumentException("Cannot find OAuth named \"$authName\"")
        auth.accessToken = token
    }

    /**
     * Allows configuring of a [OAuth] via lambda.  Please look at the documentation to find out which are available.
     *
     * It is required that you specify the OAuth token via `token()`.
     *
     * Example:
     * ```kotlin
     * api.configureOAuth("your-authentication") {
     *     token("your-OAuth-token")
     * }
     * ```
     *
     * @param authName The name of the authentication to be configured.
     * @exception IllegalArgumentException Thrown when the [OAuth] was not found or no OAuth token was given.
     */
    public fun configureOAuth(authName: String, block: OAuth.Configurer.() -> Unit) {
        val auth = oAuth[authName] ?: throw IllegalArgumentException("Cannot find OAuth named \"$authName\"")
        val configurer = OAuthConfigurer()
        block(configurer)
        val token = configurer.token ?: throw IllegalArgumentException("Specifying \"token\" is required")
        auth.accessToken = token
    }

    private data class ApiKeyConfigurer(var key: String? = null, var keyPrefix: String? = null) :
        ApiKeyAuth.Configurer {
        override fun key(value: String) {
            key = value
        }

        override fun keyPrefix(value: String?) {
            keyPrefix = value
        }
    }

    private data class BearerConfigurer(var token: String? = null) :
        HttpBearerAuth.Configurer {
        override fun token(value: String) {
            token = value
        }
    }

    private data class BasicConfigurer(var username: String? = null, var password: String? = null) :
        HttpBasicAuth.Configurer {
        override fun username(value: String?) {
            username = value
        }

        override fun password(value: String?) {
            password = value
        }
    }

    private data class OAuthConfigurer(var token: String? = null) :
        OAuth.Configurer {
        override fun token(value: String) {
            token = value
        }
    }
}
