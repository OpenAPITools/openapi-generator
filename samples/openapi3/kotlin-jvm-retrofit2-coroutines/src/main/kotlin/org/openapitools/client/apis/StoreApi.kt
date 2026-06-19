package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.Order

interface StoreApi {
    /**
     * DELETE store/order/{orderId}
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * Responses:
     *  - 400: Invalid ID supplied
     *  - 404: Order not found
     *
     * @param orderId ID of the order that needs to be deleted
     * @return [Unit]
     */
    @DELETE("store/order/{orderId}")
    suspend fun deleteOrder(@Path("orderId") orderId: kotlin.String)

    /**
     * GET store/inventory
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * Responses:
     *  - 200: successful operation
     *
     * @return [kotlin.collections.Map<kotlin.String, kotlin.Int>]
     */
    @GET("store/inventory")
    suspend fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int>

    /**
     * GET store/order/{orderId}
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid ID supplied
     *  - 404: Order not found
     *
     * @param orderId ID of pet that needs to be fetched
     * @return [Order]
     */
    @GET("store/order/{orderId}")
    suspend fun getOrderById(@Path("orderId") orderId: kotlin.Long): Order

    /**
     * POST store/order
     * Place an order for a pet
     * 
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid Order
     *
     * @param order order placed for purchasing the pet
     * @return [Order]
     */
    @POST("store/order")
    suspend fun placeOrder(@Body order: Order): Order

}
