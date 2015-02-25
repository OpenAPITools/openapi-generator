package io.swagger.api;

import io.swagger.model.*;

import com.wordnik.swagger.annotations.ApiParam;

import com.sun.jersey.multipart.FormDataParam;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/store")
@com.wordnik.swagger.annotations.Api(value = "/store", description = "the store API")
public class StoreApi {
  
  @GET
  @Path("/inventory")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "map")
  @com.wordnik.swagger.annotations.ApiResponses(value = {  })

  public Response getInventory()
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  @Path("/order")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid Order") })

  public Response placeOrder(@ApiParam(value = "order placed for purchasing the pet"  ) Order body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/order/{orderId}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "Order not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied") })

  public Response getOrderById(@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathParam("orderId") String orderId)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @DELETE
  @Path("/order/{orderId}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "Order not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied") })

  public Response deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathParam("orderId") String orderId)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
}
