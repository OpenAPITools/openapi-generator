package org.openapitools.server.apis

import org.openapitools.server.models.Order
import io.javalin.http.Context

class StoreApiServiceImpl : StoreApiService {

    override fun deleteOrder(orderId: kotlin.String, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun getInventory(ctx: Context): Map<String, kotlin.Int> {
        TODO("Implement me")
    }

    override fun getOrderById(orderId: kotlin.Long, ctx: Context): Order {
        TODO("Implement me")
    }

    override fun placeOrder(order: Order, ctx: Context): Order {
        TODO("Implement me")
    }
}
