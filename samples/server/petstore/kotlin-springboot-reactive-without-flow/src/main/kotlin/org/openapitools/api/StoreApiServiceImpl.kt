package org.openapitools.api

import org.openapitools.model.Order
import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import kotlinx.coroutines.flow.Flow
import org.springframework.stereotype.Service
@Service
class StoreApiServiceImpl : StoreApiService {

    override suspend fun deleteOrder(orderId: kotlin.String): Unit {
        TODO("Implement me")
    }

    override suspend fun getInventory(): Map<String, kotlin.Int> {
        TODO("Implement me")
    }

    override suspend fun getOrderById(orderId: kotlin.Long): Order {
        TODO("Implement me")
    }

    override suspend fun placeOrder(order: Order): Order {
        TODO("Implement me")
    }
}
