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

    override fun deleteOrder(@PathParam("orderId") orderId: kotlin.String) {
        TODO()
    }

    override fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int> {
        TODO()
    }

    override fun getOrderById(@Min(1L) @Max(5L) @PathParam("orderId") orderId: kotlin.Long): Order {
        TODO()
    }

    override fun placeOrder(@Valid @RequestBody order: Order): Order {
        TODO()
    }
}
