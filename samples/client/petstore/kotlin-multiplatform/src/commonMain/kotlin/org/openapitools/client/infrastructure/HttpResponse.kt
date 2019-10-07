package org.openapitools.client.infrastructure

import io.ktor.client.call.TypeInfo
import io.ktor.client.call.typeInfo
import io.ktor.http.Headers
import io.ktor.http.isSuccess

public open class HttpResponse<T : Any>(val response: io.ktor.client.response.HttpResponse, val provider: BodyProvider<T>) {
    val status: Int = response.status.value
    val success: Boolean = response.status.isSuccess()
    val headers: Map<String, List<String>> = response.headers.mapEntries()
    suspend fun body(): T = provider.body(response)
    suspend fun <V : Any> typedBody(type: TypeInfo): V = provider.typedBody(response, type)

    public companion object {
        private fun Headers.mapEntries(): Map<String, List<String>> {
            val result = mutableMapOf<String, List<String>>()
            entries().forEach { result[it.key] = it.value }
            return result
        }
    }
}

public interface BodyProvider<T : Any> {
    suspend fun body(response: io.ktor.client.response.HttpResponse): T
    suspend fun <V : Any> typedBody(response: io.ktor.client.response.HttpResponse, type: TypeInfo): V
}

public class TypedBodyProvider<T : Any>(private val type: TypeInfo) : BodyProvider<T> {
    @Suppress("UNCHECKED_CAST")
    override suspend fun body(response: io.ktor.client.response.HttpResponse): T =
            response.call.receive(type) as T

    @Suppress("UNCHECKED_CAST")
    override suspend fun <V : Any> typedBody(response: io.ktor.client.response.HttpResponse, type: TypeInfo): V =
            response.call.receive(type) as V
}

public class MappedBodyProvider<S : Any, T : Any>(private val provider: BodyProvider<S>, private val block: S.() -> T) : BodyProvider<T> {
    override suspend fun body(response: io.ktor.client.response.HttpResponse): T =
            block(provider.body(response))

    override suspend fun <V : Any> typedBody(response: io.ktor.client.response.HttpResponse, type: TypeInfo): V =
            provider.typedBody(response, type)
}

public inline fun <reified T : Any> io.ktor.client.response.HttpResponse.wrap(): HttpResponse<T> =
        HttpResponse(this, TypedBodyProvider(typeInfo<T>()))

public fun <T : Any, V : Any> HttpResponse<T>.map(block: T.() -> V): HttpResponse<V> =
        HttpResponse(response, MappedBodyProvider(provider, block))
