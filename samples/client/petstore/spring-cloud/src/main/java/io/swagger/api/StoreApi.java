package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;


@Api(value = "store", description = "the store API")
public interface StoreApi {

    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class, tags={ "store", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    @RequestMapping(value = "/store/order/{orderId}",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.DELETE)
    ResponseEntity<Void> deleteOrder(
@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("orderId") String orderId

);


    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        @Authorization(value = "api_key")
    }, tags={ "store", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Integer.class) })
    @RequestMapping(value = "/store/inventory",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.GET)
    ResponseEntity<Map<String, Integer>> getInventory();


    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Order.class),
        @ApiResponse(code = 404, message = "Order not found", response = Order.class) })
    @RequestMapping(value = "/store/order/{orderId}",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.GET)
    ResponseEntity<Order> getOrderById(
@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("orderId") Long orderId

);


    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Order.class) })
    @RequestMapping(value = "/store/order",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.POST)
    ResponseEntity<Order> placeOrder(

@ApiParam(value = "order placed for purchasing the pet" ,required=true ) @RequestBody Order body
);

}
