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
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import kotlinx.coroutines.flow.Flow

import java.util.Optional

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.28.0-SNAPSHOT")
interface StoreApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see StoreApi#deleteOrder
     */
    suspend fun deleteOrder(orderId: kotlin.String,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see StoreApi#getInventory
     */
    suspend fun getInventory(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Map<String, kotlin.Int>>


    /**
     * @see StoreApi#getOrderById
     */
    suspend fun getOrderById(orderId: kotlin.Int,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Order>


    /**
     * @see StoreApi#placeOrder
     */
    suspend fun placeOrder(order: Order,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Order>

}
