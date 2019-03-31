package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.stereotype.Service
@Service
class StoreApiServiceImpl : StoreApiService {

    override fun deleteOrder(orderId: String): Unit {
        TODO("Implement me")
    }

    override fun getInventory(): Map<String, Int> {
        TODO("Implement me")
    }

    override fun getOrderById(orderId: Long): Order {
        TODO("Implement me")
    }

    override fun placeOrder(body: Order): Order {
        TODO("Implement me")
    }
}
