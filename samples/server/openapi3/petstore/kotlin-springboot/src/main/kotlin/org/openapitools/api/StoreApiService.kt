package org.openapitools.api

import org.openapitools.model.Order
interface StoreApiService {

	fun deleteOrder(orderId: kotlin.String): Unit

	fun getInventory(): Map<String, kotlin.Int>

	fun getOrderById(orderId: kotlin.Long): Order

	fun placeOrder(order: Order): Order
}
