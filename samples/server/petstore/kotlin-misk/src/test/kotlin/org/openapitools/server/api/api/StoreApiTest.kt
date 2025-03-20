package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order
import jakarta.inject.Inject
import misk.testing.MiskTest
import misk.testing.MiskTestModule
import misk.web.WebTestClient


import org.junit.jupiter.api.Test

@MiskTest(startService = true)
internal class StoreApiTest {

    @MiskTestModule val module = GambitTestingModule()

    @Inject private lateinit var webTestClient: WebTestClient

    /**
     * To test StoreApiController.deleteOrder
     */
    @Test
    fun `should handle deleteOrder`() {
        val orderId: kotlin.String = TODO()
        val response:  = webTestClient.deleteOrder(orderId)

        TODO()
    }
    /**
     * To test StoreApiController.getInventory
     */
    @Test
    fun `should handle getInventory`() {
        val response: : kotlin.collections.Map<kotlin.String, kotlin.Int> = webTestClient.getInventory()

        TODO()
    }
    /**
     * To test StoreApiController.getOrderById
     */
    @Test
    fun `should handle getOrderById`() {
        val orderId: kotlin.Long = TODO()
        val response: : Order = webTestClient.getOrderById(orderId)

        TODO()
    }
    /**
     * To test StoreApiController.placeOrder
     */
    @Test
    fun `should handle placeOrder`() {
        val order: Order = TODO()
        val response: : Order = webTestClient.placeOrder(order)

        TODO()
    }
}
