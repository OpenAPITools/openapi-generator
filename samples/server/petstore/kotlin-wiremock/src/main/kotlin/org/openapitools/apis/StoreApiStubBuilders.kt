@file:Suppress(
    "RemoveRedundantQualifierName",
    "UnusedImport",
    "unused",
)

package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

/**
 *  Builder for WireMock stubs of operation deleteOrder.
 */
class DeleteOrderStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for deleteOrder respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

        /**
        * Let the stub for deleteOrder respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith404(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for deleteOrder respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation getInventory.
 */
class GetInventoryStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for getInventory respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: kotlin.collections.Map<kotlin.String, kotlin.Int>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

    /**
     * Let the stub for getInventory respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation getOrderById.
 */
class GetOrderByIdStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for getOrderById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for getOrderById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

        /**
        * Let the stub for getOrderById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith404(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for getOrderById respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation placeOrder.
 */
class PlaceOrderStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for placeOrder respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for placeOrder respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for placeOrder respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

