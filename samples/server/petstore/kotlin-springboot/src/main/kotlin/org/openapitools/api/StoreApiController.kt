package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
@RequestMapping("\${api.base-path:/v2}")
class StoreApiController(@Autowired(required = true) val service: StoreApiService) {


    @RequestMapping(
        method = [RequestMethod.DELETE],
        // "/store/order/{orderId}"
        value = [PATH_DELETE_ORDER]
    )
    fun deleteOrder(
        @PathVariable("orderId") orderId: kotlin.String
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteOrder(orderId), HttpStatus.valueOf(400))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/store/inventory"
        value = [PATH_GET_INVENTORY],
        produces = ["application/json"]
    )
    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>> {
        return ResponseEntity(service.getInventory(), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/store/order/{orderId}"
        value = [PATH_GET_ORDER_BY_ID],
        produces = ["application/xml", "application/json"]
    )
    fun getOrderById(
        @Min(value=1L) @Max(value=5L) @PathVariable("orderId") orderId: kotlin.Long
    ): ResponseEntity<Order> {
        return ResponseEntity(service.getOrderById(orderId), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        // "/store/order"
        value = [PATH_PLACE_ORDER],
        produces = ["application/xml", "application/json"]
    )
    fun placeOrder(
        @Valid @RequestBody body: Order
    ): ResponseEntity<Order> {
        return ResponseEntity(service.placeOrder(body), HttpStatus.valueOf(200))
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
        const val PATH_DELETE_ORDER: String = "/store/order/{orderId}"
        const val PATH_GET_INVENTORY: String = "/store/inventory"
        const val PATH_GET_ORDER_BY_ID: String = "/store/order/{orderId}"
        const val PATH_PLACE_ORDER: String = "/store/order"
    }
}
