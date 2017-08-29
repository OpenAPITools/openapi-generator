package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import java.util.Map;
import io.swagger.model.Order;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/Store")


@io.swagger.annotations.Api(description = "the Store API")

public class StoreApi  {
   private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();

    @DELETE
    
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class, tags={ "store",  })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    public Response deleteOrder(
        @ApiParam(value = "ID of the order that needs to be deleted",required=true) @PathParam("order_id") String orderId,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key")
    }, tags={ "store",  })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    public Response getInventory(
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }
    @GET
    
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store",  })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    public Response getOrderById(
        @ApiParam(value = "ID of pet that needs to be fetched",required=true) @PathParam("order_id") Long orderId,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId,securityContext);
    }
    @POST
    
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid Order", response = Void.class) })
    public Response placeOrder(
        @ApiParam(value = "order placed for purchasing the pet" ,required=true) Order body,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(body,securityContext);
    }
}
