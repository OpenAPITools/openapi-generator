package org.openapitools.api

import org.openapitools.model.Order
import io.swagger.annotations.*
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.web.multipart.MultipartFile
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.*

import kotlin.collections.List
import kotlin.collections.Map

@Controller
@Validated
@Api(value = "Store", description = "The Store API")
@RequestMapping("\${api.base-path:/v2}")
class StoreApiController(@Autowired(required = true) val service: StoreApiService) {

    @ApiOperation(
            value = "Delete purchase order by ID",
            nickname = "deleteOrder",
            notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors")
    @ApiResponses(
            value = [ApiResponse(code = 400, message = "Invalid ID supplied"),ApiResponse(code = 404, message = "Order not found")])
    @RequestMapping(
            value = ["/store/order/{orderId}"],
            method = [RequestMethod.DELETE])
    fun deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted", required=true, defaultValue="null") @PathVariable("orderId") orderId: String): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteOrder(orderId), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Returns pet inventories by status",
            nickname = "getInventory",
            notes = "Returns a map of status codes to quantities",
            response = Int::class,
            responseContainer = "Map",
            authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation", response = Map::class, responseContainer = "Map")])
    @RequestMapping(
            value = ["/store/inventory"],
            produces = ["application/json"], 
            method = [RequestMethod.GET])
    fun getInventory(): ResponseEntity<Map<String, Int>> {
        return ResponseEntity(service.getInventory(), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Find purchase order by ID",
            nickname = "getOrderById",
            notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions",
            response = Order::class)
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation", response = Order::class),ApiResponse(code = 400, message = "Invalid ID supplied"),ApiResponse(code = 404, message = "Order not found")])
    @RequestMapping(
            value = ["/store/order/{orderId}"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun getOrderById(@Min(1L) @Max(5L) @ApiParam(value = "ID of pet that needs to be fetched", required=true, defaultValue="null") @PathVariable("orderId") orderId: Long): ResponseEntity<Order> {
        return ResponseEntity(service.getOrderById(orderId), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Place an order for a pet",
            nickname = "placeOrder",
            notes = "",
            response = Order::class)
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation", response = Order::class),ApiResponse(code = 400, message = "Invalid Order")])
    @RequestMapping(
            value = ["/store/order"],
            produces = ["application/xml", "application/json"], 
            consumes = ["application/json"],
            method = [RequestMethod.POST])
    fun placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true ) @Valid @RequestBody order: Order): ResponseEntity<Order> {
        return ResponseEntity(service.placeOrder(order), HttpStatus.OK)
    }
}
