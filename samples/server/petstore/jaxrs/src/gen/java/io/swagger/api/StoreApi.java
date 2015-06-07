package io.swagger.api;

import io.swagger.annotations.ApiParam;
import io.swagger.api.NotFoundException;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;
import io.swagger.model.Order;

import javax.ws.rs.core.Response;

@Path("/store")


@io.swagger.annotations.Api(value = "/store", description = "the store API")
public class StoreApi {

    private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();

    @GET
    @Path("/inventory")

    @Produces({"application/json", "application/xml"})
    @io.swagger.annotations.ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "map")
    @io.swagger.annotations.ApiResponses(value = {
            @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation")})

    public Response getInventory()
            throws NotFoundException {
        return delegate.getInventory();
    }

    @POST
    @Path("/order")

    @Produces({"application/json", "application/xml"})
    @io.swagger.annotations.ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class)
    @io.swagger.annotations.ApiResponses(value = {
            @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),

            @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid Order")})

    public Response placeOrder(@ApiParam(value = "order placed for purchasing the pet") Order body)
            throws NotFoundException {
        return delegate.placeOrder(body);
    }

    @GET
    @Path("/order/{orderId}")

    @Produces({"application/json", "application/xml"})
    @io.swagger.annotations.ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class)
    @io.swagger.annotations.ApiResponses(value = {
            @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found"),

            @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),

            @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied")})

    public Response getOrderById(@ApiParam(value = "ID of pet that needs to be fetched", required = true) @PathParam("orderId") String orderId)
            throws NotFoundException {
        return delegate.getOrderById(orderId);
    }

    @DELETE
    @Path("/order/{orderId}")

    @Produces({"application/json", "application/xml"})
    @io.swagger.annotations.ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class)
    @io.swagger.annotations.ApiResponses(value = {
            @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found"),

            @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied")})

    public Response deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted", required = true) @PathParam("orderId") String orderId)
            throws NotFoundException {
        return delegate.deleteOrder(orderId);
    }
}

