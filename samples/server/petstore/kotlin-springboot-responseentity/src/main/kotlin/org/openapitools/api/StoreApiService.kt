package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.ResponseEntity


interface StoreApiService {

    fun deleteOrder(orderId: kotlin.String): ResponseEntity<Unit>

    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>>

    fun getOrderById(orderId: kotlin.Long): ResponseEntity<Order>

    fun placeOrder(body: Order): ResponseEntity<Order>
}
