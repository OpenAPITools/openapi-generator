package org.openapitools.client

import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.engine.HttpClientEngine
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.json.serializer.KotlinxSerializer
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.GlobalScope
import kotlinx.serialization.json.Json
import org.openapitools.client.apis.*
import org.openapitools.client.infrastructure.ApiClientBase

@Suppress("RemoveRedundantBackticks", "MemberVisibilityCanBePrivate", "unused")
public open class ApiClientAsync(
    baseUrl: String = "http://petstore.swagger.io/v2",
    client: HttpClient,
    coroutineScope: CoroutineScope = GlobalScope,
) {
    public constructor(baseUrl: String, httpClientEngine: HttpClientEngine? = null, json: Json = Json {}, coroutineScope: CoroutineScope = GlobalScope) :
        this(baseUrl, createHttpClient(httpClientEngine, KotlinxSerializer(json)), coroutineScope)

    public val `pet`: PetApiAsync by lazy {
        PetApiAsync(baseUrl, client, coroutineScope)
    }
    public val `store`: StoreApiAsync by lazy {
        StoreApiAsync(baseUrl, client, coroutineScope)
    }
    public val `user`: UserApiAsync by lazy {
        UserApiAsync(baseUrl, client, coroutineScope)
    }

    public val allClients: Set<ApiClientBase> by lazy {
        setOf(
            `pet`,
            `store`,
            `user`,
        )
    }
}
