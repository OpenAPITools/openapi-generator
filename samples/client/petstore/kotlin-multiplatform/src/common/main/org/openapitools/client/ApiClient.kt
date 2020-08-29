package org.openapitools.client

import io.ktor.client.engine.HttpClientEngine
import kotlinx.serialization.json.Json
import org.openapitools.client.apis.*
import org.openapitools.client.infrastructure.ApiClientBase

class ApiClient(
    baseUrl: String = "http://petstore.swagger.io/v2",
    httpClientEngine: HttpClientEngine? = null,
    json: Json = Json {}
) {
    val `pet` = PetApi(baseUrl, httpClientEngine, json)
    val `store` = StoreApi(baseUrl, httpClientEngine, json)
    val `user` = UserApi(baseUrl, httpClientEngine, json)

    val allClients: Set<ApiClientBase> = setOf(
        `pet`,
        `store`,
        `user`,
    )

    /**
     * Set the username for the first HTTP basic authentication for all apis.
     *
     * @param username Username
     */
    fun setUsername(username: String) {
        for (client in allClients) {
            client.setUsername(username)
        }
    }

    /**
     * Set the password for the first HTTP basic authentication for all apis.
     *
     * @param password Password
     */
    fun setPassword(password: String) {
        for (client in allClients) {
            client.setPassword(password)
        }
    }

    /**
     * Set the API key value for the first API key authentication for all apis.
     *
     * @param apiKey API key
     * @param paramName The name of the API key parameter, or null or set the first key.
     */
    fun setApiKey(apiKey: String, paramName: String? = null) {
        for (client in allClients) {
            client.setApiKey(apiKey, paramName)
        }
    }

    /**
     * Set the API key prefix for the first API key authentication for all apis.
     *
     * @param apiKeyPrefix API key prefix
     * @param paramName The name of the API key parameter, or null or set the first key.
     */
    fun setApiKeyPrefix(apiKeyPrefix: String, paramName: String? = null) {
        for (client in allClients) {
            client.setApiKeyPrefix(apiKeyPrefix, paramName)
        }
    }

    /**
     * Set the access token for the first OAuth2 authentication for all apis.
     *
     * @param accessToken Access token
     */
    fun setAccessToken(accessToken: String) {
        for (client in allClients) {
            client.setAccessToken(accessToken)
        }
    }

    /**
     * Set the access token for the first Bearer authentication for all apis.
     *
     * @param bearerToken The bearer token.
     */
    fun setBearerToken(bearerToken: String) {
        for (client in allClients) {
            client.setBearerToken(bearerToken)
        }
    }
}
