package org.openapitools.server.apis

import org.openapitools.server.models.Order
import io.javalin.http.Context

interface StoreApiService {

    /**
     * DELETE /store/order/{orderId} : Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     * @param orderId ID of the order that needs to be deleted (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#deleteOrder
     */
    fun deleteOrder(orderId: kotlin.String, ctx: Context): Unit

    /**
     * GET /store/inventory : Returns pet inventories by status
     * Returns a map of status codes to quantities
     *
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see StoreApi#getInventory
     */
    fun getInventory(ctx: Context): Map<String, kotlin.Int>

    /**
     * GET /store/order/{orderId} : Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     *
     * @param orderId ID of pet that needs to be fetched (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#getOrderById
     */
    fun getOrderById(orderId: kotlin.Long, ctx: Context): Order

    /**
     * POST /store/order : Place an order for a pet
     * 
     *
     * @param order order placed for purchasing the pet (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     * @see StoreApi#placeOrder
     */
    fun placeOrder(order: Order, ctx: Context): Order
}
