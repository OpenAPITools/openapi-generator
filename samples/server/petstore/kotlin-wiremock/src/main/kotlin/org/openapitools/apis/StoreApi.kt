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

open class StoreApiStubs(protected val objectMapper: ObjectMapper) {

    fun deleteOrder(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteOrderStubBuilder =
        DeleteOrderStubBuilder(delete("/store/order/{orderId}")
            .withPathParam("orderId", orderId)
            .configurer()
        )

    inner class DeleteOrderStubBuilder(private val stub: MappingBuilder) {

        fun respondWith400(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(400)
                .configurer()
            )

        fun respondWith404(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(404)
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun getInventory(configurer: MappingBuilder.() -> MappingBuilder = { this }): GetInventoryStubBuilder =
        GetInventoryStubBuilder(get("/store/inventory")
            .configurer()
        )

    inner class GetInventoryStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.collections.Map<kotlin.String, kotlin.Int>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun getOrderById(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetOrderByIdStubBuilder =
        GetOrderByIdStubBuilder(get("/store/order/{orderId}")
            .withPathParam("orderId", orderId)
            .configurer()
        )

    inner class GetOrderByIdStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith400(
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith404(
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(404)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun placeOrder(configurer: MappingBuilder.() -> MappingBuilder = { this }): PlaceOrderStubBuilder =
        PlaceOrderStubBuilder(post("/store/order")
            .configurer()
        )

    inner class PlaceOrderStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith400(
            body: Order,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }
}
