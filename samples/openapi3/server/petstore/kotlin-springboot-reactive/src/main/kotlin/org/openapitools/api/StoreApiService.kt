package org.openapitools.api

import org.openapitools.model.Order
import kotlinx.coroutines.flow.Flow;
interface StoreApiService {

	suspend fun deleteOrder(orderId: kotlin.String): Unit

	suspend fun getInventory(): Map<String, kotlin.Int>

	suspend fun getOrderById(orderId: kotlin.Long): Order

	suspend fun placeOrder(order: Order): Order
}
