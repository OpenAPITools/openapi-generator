package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import rx.Observable

import org.openapitools.client.models.Order

interface StoreApi {
    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * Responses:
     *  - 400: Invalid ID supplied
     *  - 404: Order not found
     * 
     * @param orderId ID of the order that needs to be deleted 
    * @return [Call]<[Unit]>
     */
    @DELETE("store/order/{order_id}")
    fun deleteOrder(@Path("order_id") orderId: kotlin.String): Observable<Unit>

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * Responses:
     *  - 200: successful operation
     * 
    * @return [Call]<[kotlin.collections.Map<kotlin.String, kotlin.Int>]>
     */
    @GET("store/inventory")
    fun getInventory(): Observable<kotlin.collections.Map<kotlin.String, kotlin.Int>>

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid ID supplied
     *  - 404: Order not found
     * 
     * @param orderId ID of pet that needs to be fetched 
    * @return [Call]<[Order]>
     */
    @GET("store/order/{order_id}")
    fun getOrderById(@Path("order_id") orderId: kotlin.Long): Observable<Order>

    /**
     * Place an order for a pet
     * 
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid Order
     * 
     * @param order order placed for purchasing the pet 
    * @return [Call]<[Order]>
     */
    @POST("store/order")
    fun placeOrder(@Body order: Order): Observable<Order>

}
