package com.ibm.ws.petstoresample.api;

import com.ibm.ws.petstoresample.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;

@Path("/stores")

@Api(description = "the stores API")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen", date = "2016-06-06T11:04:02.369-04:00")

public class StoresApi  {

    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = void.class, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = void.class),
        @ApiResponse(code = 404, message = "Order not found", response = void.class) })
    public Response deleteOrder(@PathParam("orderId") String orderId) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Order.class),
        @ApiResponse(code = 404, message = "Order not found", response = Order.class) })
    public Response getOrderById(@PathParam("orderId") String orderId) {
    	return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/order")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Order.class) })
    public Response placeOrder(Order body) {
    	return Response.ok().entity("magic!").build();
    }
}

