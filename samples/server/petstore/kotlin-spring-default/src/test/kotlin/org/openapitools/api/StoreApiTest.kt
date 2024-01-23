package org.openapitools.api

import org.openapitools.model.Order
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class StoreApiTest {

    private val api: StoreApiController = StoreApiController()

    /**
     * To test StoreApiController.deleteOrder
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun deleteOrderTest() {
        val orderId: kotlin.String = TODO()
        val response: ResponseEntity<Unit> = api.deleteOrder(orderId)

        // TODO: test validations
    }

    /**
     * To test StoreApiController.getInventory
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun getInventoryTest() {
        val response: ResponseEntity<Map<String, kotlin.Int>> = api.getInventory()

        // TODO: test validations
    }

    /**
     * To test StoreApiController.getOrderById
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun getOrderByIdTest() {
        val orderId: kotlin.Long = TODO()
        val response: ResponseEntity<Order> = api.getOrderById(orderId)

        // TODO: test validations
    }

    /**
     * To test StoreApiController.placeOrder
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun placeOrderTest() {
        val order: Order = TODO()
        val response: ResponseEntity<Order> = api.placeOrder(order)

        // TODO: test validations
    }
}
