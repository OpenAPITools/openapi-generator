package org.openapitools.api

import org.openapitools.model.Order
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType

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

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @RequestMapping(
        method = [RequestMethod.DELETE],
        value = [PATH_DELETE_ORDER /* "/store/order/{orderId}" */]
    )
    fun deleteOrder(
        @PathVariable("orderId") orderId: kotlin.String
    ): Unit {
        return service.deleteOrder(orderId)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_GET_INVENTORY /* "/store/inventory" */],
        produces = ["application/json"]
    )
    fun getInventory(): Map<String, kotlin.Int> {
        return service.getInventory()
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_GET_ORDER_BY_ID /* "/store/order/{orderId}" */],
        produces = ["application/xml", "application/json"]
    )
    fun getOrderById(
        @Min(value=1L) @Max(value=5L) @PathVariable("orderId") orderId: kotlin.Long
    ): Order {
        return service.getOrderById(orderId)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_PLACE_ORDER /* "/store/order" */],
        produces = ["application/xml", "application/json"]
    )
    fun placeOrder(
        @Valid @RequestBody body: Order
    ): Order {
        return service.placeOrder(body)
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
