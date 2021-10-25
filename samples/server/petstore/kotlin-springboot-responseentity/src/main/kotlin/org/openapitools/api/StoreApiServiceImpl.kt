package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Service


@Service
class StoreApiServiceImpl : StoreApiService {

    override fun deleteOrder(orderId: kotlin.String): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun getInventory(): ResponseEntity<Map<String, kotlin.Int>> {
        TODO("Implement me")
    }

    override fun getOrderById(orderId: kotlin.Long): ResponseEntity<Order> {
        TODO("Implement me")
    }

    override fun placeOrder(body: Order): ResponseEntity<Order> {
        TODO("Implement me")
    }
}
