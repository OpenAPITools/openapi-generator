package org.openapitools.server.api.api

import jakarta.inject.Inject
import jakarta.inject.Singleton
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader
import org.openapitools.server.api.model.Order

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class StoreApiImpl @Inject constructor(
): StoreApi {

    override fun deleteOrder(@PathParam("orderId") orderId: kotlin.String) {
        TODO()
    }

    override fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int> {
        TODO()
    }

    override fun getOrderById(@Min(1L) @Max(5L) @PathParam("orderId") orderId: kotlin.Long): Order {
        TODO()
    }

    override fun placeOrder(@Valid @RequestBody order: Order): Order {
        TODO()
    }
}
