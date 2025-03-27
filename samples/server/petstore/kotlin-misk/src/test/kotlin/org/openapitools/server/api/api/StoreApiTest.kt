package org.openapitools.server.api.api

import jakarta.inject.Inject
import misk.testing.MiskTest
import org.junit.jupiter.api.Test

import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader

import org.openapitools.server.api.model.Order

@MiskTest(startService = true)
internal class StoreApiTest {

    @Inject private lateinit var storeApi: StoreApi

    /**
     * To test StoreApiAction.deleteOrder
     */
    @Test
    fun `should handle deleteOrder`() {
        val orderId = TODO()
        val response = storeApi.deleteOrder(orderId)
    }

    /**
     * To test StoreApiAction.getInventory
     */
    @Test
    fun `should handle getInventory`() {
        val response: kotlin.collections.Map<kotlin.String, kotlin.Int> = storeApi.getInventory()
    }

    /**
     * To test StoreApiAction.getOrderById
     */
    @Test
    fun `should handle getOrderById`() {
        val orderId = TODO()
        val response: Order = storeApi.getOrderById(orderId)
    }

    /**
     * To test StoreApiAction.placeOrder
     */
    @Test
    fun `should handle placeOrder`() {
        val order = TODO()
        val response: Order = storeApi.placeOrder(order)
    }

}
