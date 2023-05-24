package org.openapitools.client.infrastructure;

import org.springframework.http.HttpMethod
import org.springframework.web.reactive.function.client.WebClient
import org.springframework.http.ResponseEntity
import reactor.core.publisher.Mono

open class ApiClient(protected val client: WebClient) {

    companion object {
        protected const val ContentType = "Content-Type"
        protected const val Accept = "Accept"
        protected const val JsonMediaType = "application/json"
    }

    protected inline fun <reified I : Any, reified T: Any?> request(requestConfig: RequestConfig<I>): Mono<ResponseEntity<T>> {
        return prepare(defaults(requestConfig))
            .retrieve()
            .toEntity(T::class.java)
    }

    protected fun <I : Any> prepare(requestConfig: RequestConfig<I>) =
        client.method(requestConfig)
            .uri(requestConfig)
            .headers(requestConfig)
            .body(requestConfig)

    protected fun <I> defaults(requestConfig: RequestConfig<I>) =
        requestConfig.apply {
            if (body != null && headers[ContentType].isNullOrEmpty()) {
                headers[ContentType] = JsonMediaType
            }
            if (headers[Accept].isNullOrEmpty()) {
                headers[Accept] = JsonMediaType
            }
        }

    private fun <I> WebClient.method(requestConfig: RequestConfig<I>)=
        method(HttpMethod.valueOf(requestConfig.method.name))

    private fun <I> WebClient.RequestBodyUriSpec.uri(requestConfig: RequestConfig<I>) =
        uri { builder ->
            builder.path(requestConfig.path).apply {
                requestConfig.query.forEach { (name, value) ->
                    queryParam(name, value)
                }
            }.build()
        }

    private fun <I> WebClient.RequestBodySpec.headers(requestConfig: RequestConfig<I>) =
        apply { requestConfig.headers.forEach { (name, value) -> header(name, value) } }

    private fun <I : Any> WebClient.RequestBodySpec.body(requestConfig: RequestConfig<I>) =
        apply { if (requestConfig.body != null) bodyValue(requestConfig.body) }
}