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
 * WireMock stub request builder.
 */
open class StoreApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation deleteOrder.
     *
     * @param orderId path parameter orderId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [DeleteOrderStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun deleteOrder(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteOrderStubBuilder =
        DeleteOrderStubBuilder(objectMapper, delete(urlPathTemplate("/store/order/{orderId}"))
            .withPathParam("orderId", orderId)
            .configurer()
        )

    /**
     * Construct a stub for the operation getInventory.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [GetInventoryStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun getInventory(configurer: MappingBuilder.() -> MappingBuilder = { this }): GetInventoryStubBuilder =
        GetInventoryStubBuilder(objectMapper, get(urlPathTemplate("/store/inventory"))
            .configurer()
        )

    /**
     * Construct a stub for the operation getOrderById.
     *
     * @param orderId path parameter orderId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [GetOrderByIdStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun getOrderById(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetOrderByIdStubBuilder =
        GetOrderByIdStubBuilder(objectMapper, get(urlPathTemplate("/store/order/{orderId}"))
            .withPathParam("orderId", orderId)
            .configurer()
        )

    /**
     * Construct a stub for the operation placeOrder.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [PlaceOrderStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun placeOrder(configurer: MappingBuilder.() -> MappingBuilder = { this }): PlaceOrderStubBuilder =
        PlaceOrderStubBuilder(objectMapper, post(urlPathTemplate("/store/order"))
            .configurer()
        )
}
