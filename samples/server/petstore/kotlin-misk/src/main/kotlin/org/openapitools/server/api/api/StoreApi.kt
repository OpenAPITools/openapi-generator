package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order
import okhttp3.Headers

interface StoreApi {

    fun deleteOrder(orderId: kotlin.String) 

    fun getInventory() : kotlin.collections.Map<kotlin.String, kotlin.Int>

    fun getOrderById(orderId: kotlin.Long) : Order

    fun placeOrder(order: Order) : Order
}
