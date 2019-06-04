package org.openapitools.api

import org.openapitools.model.Order
import kotlinx.coroutines.flow.Flow;
interface StoreApiService {

	suspend fun deleteOrder(orderId: String): Unit

	suspend fun getInventory(): Map<String, Int>

	suspend fun getOrderById(orderId: Long): Order

	suspend fun placeOrder(order: Order): Order
}
