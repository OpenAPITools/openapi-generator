package org.openapitools.api

import org.openapitools.model.Order
import org.junit.jupiter.api.Test

import org.springframework.http.ResponseEntity

class StoreApiTest {

    private val service: StoreApiService = StoreApiServiceImpl()
    private val api: StoreApiController = StoreApiController(service)

    
    /**
    * Delete purchase order by ID
    *
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun deleteOrderTest() {
        val orderId:kotlin.String? = null
        val response: ResponseEntity<Unit> = api.deleteOrder(orderId!!)

        // TODO: test validations
    }
    
    /**
    * Returns pet inventories by status
    *
    * Returns a map of status codes to quantities
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
    * Find purchase order by ID
    *
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun getOrderByIdTest() {
        val orderId:kotlin.Long? = null
        val response: ResponseEntity<Order> = api.getOrderById(orderId!!)

        // TODO: test validations
    }
    
    /**
    * Place an order for a pet
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun placeOrderTest() {
        val order:Order? = null
        val response: ResponseEntity<Order> = api.placeOrder(order!!)

        // TODO: test validations
    }
    
}
