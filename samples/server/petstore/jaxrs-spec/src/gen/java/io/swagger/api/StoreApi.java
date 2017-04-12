package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/store")

@Api(description = "the store API")




public class StoreApi  {

    @DELETE
    @Path("/order/{order_id}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = void.class, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = void.class),
        @ApiResponse(code = 404, message = "Order not found", response = void.class) })
    public Response deleteOrder(@PathParam("order_id") @ApiParam("ID of the order that needs to be deleted") String orderId) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        @Authorization(value = "api_key")
    }, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Integer.class, responseContainer = "Map") })
    public Response getInventory() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/order/{order_id}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Order.class),
        @ApiResponse(code = 404, message = "Order not found", response = Order.class) })
    public Response getOrderById(@PathParam("order_id") @Min(1) @Max(5) @ApiParam("ID of pet that needs to be fetched") Long orderId) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Order.class) })
    public Response placeOrder(Order body) {
        return Response.ok().entity("magic!").build();
    }
}

