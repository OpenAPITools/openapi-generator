package org.openapitools.api

import org.openapitools.model.Order
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Disabled
import org.springframework.http.ResponseEntity

class StoreApiTest {

    private val service: StoreApiService = StoreApiServiceImpl()
    private val api: StoreApiController = StoreApiController(service)

    /**
     * To test StoreApiController.deleteOrder
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
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
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
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
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
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
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun placeOrderTest() {
        val order: Order = TODO()
        
        
        val response: ResponseEntity<Order> = api.placeOrder(order)

        // TODO: test validations
    }
}
