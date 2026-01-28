package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/store")
@Tag(name = "store")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class StoreApi {

    @DELETE
    @Path("/order/{order_id}")
    @Operation(summary = "Delete purchase order by ID", description = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found")
    })
    public Response deleteOrder(@PathParam("order_id") String orderId) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @Operation(summary = "Returns pet inventories by status", description = "Returns a map of status codes to quantities")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response getInventory() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find purchase order by ID", description = "For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found")
    })
    public Response getOrderById(@PathParam("order_id") @Min(1L) @Max(5L) Long orderId) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Place an order for a pet", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid Order")
    })
    public Response placeOrder(@Valid @NotNull Order order) {
        return Response.ok().entity("magic!").build();
    }
}
