package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.core.io.Resource
import org.springframework.http.server.reactive.ServerHttpRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.9.0-SNAPSHOT")
interface StoreApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see StoreApi#deleteOrder
     */
    fun deleteOrder(orderId: kotlin.String,
        serverHttpRequest: ServerHttpRequest): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see StoreApi#getInventory
     */
    fun getInventory(serverHttpRequest: ServerHttpRequest): ResponseEntity<Map<String, kotlin.Int>> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see StoreApi#getOrderById
     */
    fun getOrderById(orderId: kotlin.Long,
        serverHttpRequest: ServerHttpRequest): ResponseEntity<Order> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>aeiou</status>  <complete>true</complete></Order>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see StoreApi#placeOrder
     */
    fun placeOrder(order: Order,
        serverHttpRequest: ServerHttpRequest): ResponseEntity<Order> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>aeiou</status>  <complete>true</complete></Order>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }

}
