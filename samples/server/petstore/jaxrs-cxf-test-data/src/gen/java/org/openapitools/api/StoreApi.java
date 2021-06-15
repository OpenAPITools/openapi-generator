package org.openapitools.api;

import java.util.List;
import java.util.Map;
import org.openapitools.model.Order;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/v2/store")
@Api(value = "/", description = "")
public interface StoreApi  {

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     */
    @DELETE
    @Path("/order/{order_id}")
    @ApiOperation(value = "Delete purchase order by ID", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Order not found") })
    public void deleteOrder(@PathParam("order_id") String orderId);

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     */
    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @ApiOperation(value = "Returns pet inventories by status", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    public Map<String, Integer> getInventory();

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     *
     */
    @GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find purchase order by ID", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Order not found") })
    public Order getOrderById(@PathParam("order_id") @Min(1L) @Max(5L) Long orderId);

    /**
     * Place an order for a pet
     *
     */
    @POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Place an order for a pet", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order") })
    public Order placeOrder(@Valid Order body);
}

