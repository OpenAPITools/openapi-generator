package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import rx.Observable

import org.openapitools.client.models.Order

interface StoreApi {
    @DELETE("/store/order/{order_id}")
    fun deleteOrder(@Path("order_id") orderId: kotlin.String): Observable<Unit>

    @GET("/store/inventory")
    fun getInventory(): Observable<kotlin.collections.Map<kotlin.String, kotlin.Int>>

    @GET("/store/order/{order_id}")
    fun getOrderById(@Path("order_id") orderId: kotlin.Long): Observable<Order>

    @POST("/store/order")
    fun placeOrder(@Body order: Order): Observable<Order>

}
