

package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order
import jakarta.inject.Inject
import misk.testing.MiskTest
import okhttp3.Headers

import org.junit.jupiter.api.Test

@MiskTest(startService = true)
internal class StoreApiTest {

    @Inject private lateinit var StoreApi : StoreApiController

    /**
     * To test StoreApiController.deleteOrder
     */
    @Test
    fun `should handle deleteOrder`() {
        val orderId: kotlin.String = TODO()
        val response = StoreApi.deleteOrder(orderId)
    }

    /**
     * To test StoreApiController.getInventory
     */
    @Test
    fun `should handle getInventory`() {
        val response: kotlin.collections.Map<kotlin.String, kotlin.Int> = StoreApi.getInventory()
    }

    /**
     * To test StoreApiController.getOrderById
     */
    @Test
    fun `should handle getOrderById`() {
        val orderId: kotlin.Long = TODO()
        val response: Order = StoreApi.getOrderById(orderId)
    }

    /**
     * To test StoreApiController.placeOrder
     */
    @Test
    fun `should handle placeOrder`() {
        val order: Order = TODO()
        val response: Order = StoreApi.placeOrder(order)
    }

}
