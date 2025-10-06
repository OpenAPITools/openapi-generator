package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.17.0-SNAPSHOT")
interface StoreApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see StoreApi#deleteOrder
     */
    fun deleteOrder(orderId: kotlin.String): ResponseEntity<Unit>


    /**
     * @see StoreApi#getInventory
     */
    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>>


    /**
     * @see StoreApi#getOrderById
     */
    fun getOrderById(orderId: kotlin.Long): ResponseEntity<Order>


    /**
     * @see StoreApi#placeOrder
     */
    fun placeOrder(order: Order): ResponseEntity<Order>

}
