package org.openapitools.client

import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.engine.HttpClientEngine
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.json.serializer.KotlinxSerializer
import kotlinx.serialization.json.Json
import org.openapitools.client.apis.*
import org.openapitools.client.infrastructure.ApiClientBase

@Suppress("RemoveRedundantBackticks", "MemberVisibilityCanBePrivate", "unused")
public open class ApiClient(
    baseUrl: String = "http://petstore.swagger.io/v2",
    client: HttpClient
) {
    public constructor(baseUrl: String, httpClientEngine: HttpClientEngine? = null, json: Json = Json {}) :
        this(baseUrl, createHttpClient(httpClientEngine, KotlinxSerializer(json)))

    public val `pet`: PetApi by lazy { PetApi(baseUrl, client) }
    public val `store`: StoreApi by lazy { StoreApi(baseUrl, client) }
    public val `user`: UserApi by lazy { UserApi(baseUrl, client) }

    public val allClients: Set<ApiClientBase> by lazy {
        setOf(
            `pet`,
            `store`,
            `user`,
        )
    }
}

internal fun createHttpClient(httpClientEngine: HttpClientEngine? = null, serializer: KotlinxSerializer): HttpClient {
    val jsonConfig: JsonFeature.Config.() -> Unit = { this.serializer = serializer }
    val clientConfig: (HttpClientConfig<*>) -> Unit = { it.install(JsonFeature, jsonConfig) }
    return if (httpClientEngine == null) {
        HttpClient(clientConfig)
    } else {
        HttpClient(httpClientEngine, clientConfig)
    }
}
