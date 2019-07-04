package org.openapitools.api

import org.openapitools.model.Order
import kotlinx.coroutines.flow.Flow;
import org.springframework.stereotype.Service
@Service
class StoreApiServiceImpl : StoreApiService {

    override suspend fun deleteOrder(orderId: String): Unit {
        TODO("Implement me")
    }

    override suspend fun getInventory(): Map<String, Int> {
        TODO("Implement me")
    }

    override suspend fun getOrderById(orderId: Long): Order {
        TODO("Implement me")
    }

    override suspend fun placeOrder(order: Order): Order {
        TODO("Implement me")
    }
}
