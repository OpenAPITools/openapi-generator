package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order
import okhttp3.Headers

interface StoreApi {

    fun deleteOrder(@PathParam("orderId") orderId: kotlin.String) {

    fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int> {

    fun getOrderById(@Min(1L) @Max(5L) @PathParam("orderId") orderId: kotlin.Long): Order {

    fun placeOrder(@Valid @RequestBody order: Order): Order {
}
