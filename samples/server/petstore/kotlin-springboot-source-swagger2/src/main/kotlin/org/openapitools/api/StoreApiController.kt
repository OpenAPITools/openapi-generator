package org.openapitools.api

import org.openapitools.model.Order
import io.swagger.v3.oas.annotations.*
import io.swagger.v3.oas.annotations.enums.*
import io.swagger.v3.oas.annotations.media.*
import io.swagger.v3.oas.annotations.responses.*
import io.swagger.v3.oas.annotations.security.*
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

    @Operation(
        summary = "Delete purchase order by ID",
        operationId = "deleteOrder",
        description = """For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors""",
        responses = [
            ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
            ApiResponse(responseCode = "404", description = "Order not found") ]
    )
    @RequestMapping(
        method = [RequestMethod.DELETE],
        value = ["/store/order/{orderId}"]
    )
    fun deleteOrder(@Parameter(description = "ID of the order that needs to be deleted", required = true) @PathVariable("orderId") orderId: kotlin.String): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteOrder(orderId), HttpStatus.valueOf(400))
    }

    @Operation(
        summary = "Returns pet inventories by status",
        operationId = "getInventory",
        description = """Returns a map of status codes to quantities""",
        responses = [
            ApiResponse(responseCode = "200", description = "successful operation", content = [Content(schema = Schema(implementation = kotlin.collections.Map::class))]) ],
        security = [ SecurityRequirement(name = "api_key") ]
    )
    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/store/inventory"],
        produces = ["application/json"]
    )
    fun getInventory(): ResponseEntity<Map<String, kotlin.Int>> {
        return ResponseEntity(service.getInventory(), HttpStatus.valueOf(200))
    }

    @Operation(
        summary = "Find purchase order by ID",
        operationId = "getOrderById",
        description = """For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions""",
        responses = [
            ApiResponse(responseCode = "200", description = "successful operation", content = [Content(schema = Schema(implementation = Order::class))]),
            ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
            ApiResponse(responseCode = "404", description = "Order not found") ]
    )
    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/store/order/{orderId}"],
        produces = ["application/xml", "application/json"]
    )
    fun getOrderById(@Min(1L) @Max(5L) @Parameter(description = "ID of pet that needs to be fetched", required = true) @PathVariable("orderId") orderId: kotlin.Long): ResponseEntity<Order> {
        return ResponseEntity(service.getOrderById(orderId), HttpStatus.valueOf(200))
    }

    @Operation(
        summary = "Place an order for a pet",
        operationId = "placeOrder",
        description = """""",
        responses = [
            ApiResponse(responseCode = "200", description = "successful operation", content = [Content(schema = Schema(implementation = Order::class))]),
            ApiResponse(responseCode = "400", description = "Invalid Order") ]
    )
    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/store/order"],
        produces = ["application/xml", "application/json"]
    )
    fun placeOrder(@Parameter(description = "order placed for purchasing the pet", required = true) @Valid @RequestBody body: Order): ResponseEntity<Order> {
        return ResponseEntity(service.placeOrder(body), HttpStatus.valueOf(200))
    }
}
