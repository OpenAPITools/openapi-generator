package org.openapitools.api

import org.openapitools.model.Order

interface StoreApiService {

    fun deleteOrder(orderId: String): Unit

    fun getInventory(): Map<String, Int>

    fun getOrderById(orderId: Long): Order

    fun placeOrder(body: Order): Order
}
