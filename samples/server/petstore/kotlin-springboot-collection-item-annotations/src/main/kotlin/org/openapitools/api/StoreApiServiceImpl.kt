package org.openapitools.api

import org.openapitools.model.Order
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import org.springframework.stereotype.Service
@Service
class StoreApiServiceImpl : StoreApiService {

    override fun deleteOrder(orderId: kotlin.String): Unit {
        TODO("Implement me")
    }

    override fun getInventory(): Map<String, kotlin.Int> {
        TODO("Implement me")
    }

    override fun getOrderById(orderId: kotlin.Long): Order {
        TODO("Implement me")
    }

    override fun placeOrder(order: Order): Order {
        TODO("Implement me")
    }
}
