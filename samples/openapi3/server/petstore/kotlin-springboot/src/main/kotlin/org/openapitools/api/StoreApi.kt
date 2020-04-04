package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
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
class StoreApiController() {


    @RequestMapping(
        value = ["/store/order/{orderId}"],
        method = [RequestMethod.DELETE])
    fun deleteOrder( @PathVariable("orderId") orderId: kotlin.String
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/store/inventory"],
        produces = ["application/json"], 
        method = [RequestMethod.GET])
    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/store/order/{orderId}"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun getOrderById(@Min(1L) @Max(5L)  @PathVariable("orderId") orderId: kotlin.Long
): ResponseEntity<Order> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/store/order"],
        produces = ["application/xml", "application/json"], 
        consumes = ["application/json"],
        method = [RequestMethod.POST])
    fun placeOrder( @Valid @RequestBody order: Order
): ResponseEntity<Order> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
}
