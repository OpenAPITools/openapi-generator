package com.wordnik.api;

import com.wordnik.swagger.annotations.*;

import com.wordnik.client.model.Order;
import java.util.List;
import com.wordnik.api.NotFoundException;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/store.json")
@Api(value = "/store", description = "the store API")
@Produces({"application/json"})
public class StoreApi {
  @GET
  @Path("/order/{orderId}")
  @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors", responseClass = "Order")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response getOrderById(
    @ApiParam(value = "ID of pet that needs to be fetched"
    ,required=true)@PathParam("orderId") String orderId
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @DELETE
  @Path("/order/{orderId}")
  @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response deleteOrder(
    @ApiParam(value = "ID of the order that needs to be deleted"
    ,required=true)@PathParam("orderId") String orderId
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @POST
  @Path("/order")
  @ApiOperation(value = "Place an order for a pet", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response placeOrder(
    @ApiParam(value = "order placed for purchasing the pet"
    ,required=true) Order body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  }

