package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order

import jakarta.inject.Inject
import jakarta.inject.Singleton
import okhttp3.Headers

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class StoreApiImpl @Inject constructor(
): StoreApi {

    override fun deleteOrder(orderId: kotlin.String) {
        TODO()
    }

    override fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int> {
        TODO()
    }

    override fun getOrderById(orderId: kotlin.Long): Order {
        TODO()
    }

    override fun placeOrder(order: Order): Order {
        TODO()
    }
}
