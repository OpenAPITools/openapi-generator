package org.openapitools.client

import io.ktor.client.engine.HttpClientEngine
import kotlinx.serialization.json.JsonConfiguration
import org.openapitools.client.apis.*

class ApiClient(
    baseUrl: String = "http://petstore.swagger.io/v2",
    httpClientEngine: HttpClientEngine? = null,
    jsonConfiguration: JsonConfiguration = JsonConfiguration.Stable
) {
    val pet = PetApi(baseUrl, httpClientEngine, jsonConfiguration)
    val store = StoreApi(baseUrl, httpClientEngine, jsonConfiguration)
    val user = UserApi(baseUrl, httpClientEngine, jsonConfiguration)
}
