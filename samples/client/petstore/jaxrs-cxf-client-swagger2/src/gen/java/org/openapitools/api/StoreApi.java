package org.openapitools.api;

import org.openapitools.model.Order;

import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

/**
 * OpenAPI Petstore
 *
 * <p>This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 */
@Path("/store")
@OpenAPIDefinition(
    info = @Info(
        title = "OpenAPI Petstore",
        description = "This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.",
        version = "1.0.0"
    )
)
public interface StoreApi  {

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     */
    @DELETE
    @Path("/order/{orderId}")
    @Operation(operationId = "deleteOrder", summary = "Delete purchase order by ID",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found") })
    public void deleteOrder(@PathParam("orderId") String orderId);

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     */
    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @Operation(operationId = "getInventory", summary = "Returns pet inventories by status",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Map.class)))) })
    public Map<String, Integer> getInventory();

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     *
     */
    @GET
    @Path("/order/{orderId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "getOrderById", summary = "Find purchase order by ID",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found") })
    public Order getOrderById(@PathParam("orderId") Long orderId);

    /**
     * Place an order for a pet
     *
     * 
     *
     */
    @POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "placeOrder", summary = "Place an order for a pet",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        @ApiResponse(responseCode = "400", description = "Invalid Order") })
    public Order placeOrder(Order order);
}
