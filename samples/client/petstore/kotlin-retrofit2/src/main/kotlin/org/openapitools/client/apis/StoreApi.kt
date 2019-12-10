package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call

import org.openapitools.client.models.Order

interface StoreApi {
    @DELETE("/store/order/{orderId}")
    fun deleteOrder(@Path("orderId") orderId: kotlin.String): Call<Unit>

    @GET("/store/inventory")
    fun getInventory(): Call<kotlin.collections.Map<kotlin.String, kotlin.Int>>

    @GET("/store/order/{orderId}")
    fun getOrderById(@Path("orderId") orderId: kotlin.Long): Call<Order>

    @POST("/store/order")
    fun placeOrder(@Body body: Order): Call<Order>

}
