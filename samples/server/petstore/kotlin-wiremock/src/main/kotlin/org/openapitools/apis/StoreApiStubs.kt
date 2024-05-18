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

open class StoreApiStubs(private val objectMapper: ObjectMapper) {

    fun deleteOrder(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteOrderStubBuilder =
        DeleteOrderStubBuilder(objectMapper, delete(urlPathTemplate("/store/order/{orderId}"))
            .withPathParam("orderId", orderId)
            .configurer()
        )

    fun getInventory(configurer: MappingBuilder.() -> MappingBuilder = { this }): GetInventoryStubBuilder =
        GetInventoryStubBuilder(objectMapper, get(urlPathTemplate("/store/inventory"))
            .configurer()
        )

    fun getOrderById(orderId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetOrderByIdStubBuilder =
        GetOrderByIdStubBuilder(objectMapper, get(urlPathTemplate("/store/order/{orderId}"))
            .withPathParam("orderId", orderId)
            .configurer()
        )

    fun placeOrder(configurer: MappingBuilder.() -> MappingBuilder = { this }): PlaceOrderStubBuilder =
        PlaceOrderStubBuilder(objectMapper, post(urlPathTemplate("/store/order"))
            .configurer()
        )
}
