package org.openapitools.server.apis

import org.openapitools.server.models.Order

class StoreApiServiceImpl : StoreApiService {

    override fun deleteOrder(orderId: kotlin.String): Unit {
        TODO("Implement me")
    }

    override fun getInventory(): Map<String, kotlin.Int> {
        TODO("Implement me")
    }

    override fun getOrderById(orderId: kotlin.Long): Order {
        TODO("Implement me")
    }

    override fun placeOrder(order: Order): Order {
        TODO("Implement me")
    }
}
