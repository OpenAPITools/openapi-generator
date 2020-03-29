package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import retrofit2.http.*

import org.openapitools.client.models.Order

interface StoreApi {
    @DELETE("/store/order/{order_id}")
    suspend fun deleteOrder(@Path("order_id") orderId: kotlin.String): Unit

    @GET("/store/inventory")
    suspend fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int>

    @GET("/store/order/{order_id}")
    suspend fun getOrderById(@Path("order_id") orderId: kotlin.Long): Order

    @POST("/store/order")
    suspend fun placeOrder(@Body order: Order): Order

}
