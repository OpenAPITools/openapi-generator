package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
@RequestMapping("\${api.base-path:/v2}")
class StoreApiController(@Autowired(required = true) val service: StoreApiService) {


    @RequestMapping(
        method = [RequestMethod.DELETE],
        value = ["/store/order/{orderId}"]
    )
    fun deleteOrder( @PathVariable("orderId") orderId: kotlin.String): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteOrder(orderId), HttpStatus.valueOf(400))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/store/inventory"],
        produces = ["application/json"]
    )
    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>> {
        return ResponseEntity(service.getInventory(), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/store/order/{orderId}"],
        produces = ["application/xml", "application/json"]
    )
    fun getOrderById(@Min(1L) @Max(5L)  @PathVariable("orderId") orderId: kotlin.Long): ResponseEntity<Order> {
        return ResponseEntity(service.getOrderById(orderId), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/store/order"],
        produces = ["application/xml", "application/json"],
        consumes = ["application/json"]
    )
    fun placeOrder( @Valid @RequestBody order: Order): ResponseEntity<Order> {
        return ResponseEntity(service.placeOrder(order), HttpStatus.valueOf(200))
    }
}
