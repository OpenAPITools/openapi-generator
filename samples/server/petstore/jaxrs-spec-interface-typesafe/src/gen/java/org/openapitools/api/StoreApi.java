package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/store")
@Api(description = "the Store API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface StoreApi {

    @DELETE
    @Path("/order/{order_id}")
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    DeleteOrderResponse deleteOrder(@PathParam("order_id") @ApiParam("ID of the order that needs to be deleted") String orderId);

    public static class DeleteOrderResponse extends org.openapitools.api.support.ResponseWrapper {
        private DeleteOrderResponse(Response delegate) {
            super(delegate);
        }
        public static DeleteOrderResponse with400() {
            return new DeleteOrderResponse(Response.status(400).build());
        }
        public static DeleteOrderResponse with404() {
            return new DeleteOrderResponse(Response.status(404).build());
        }
        public static DeleteOrderResponse withCustomResponse(Response response) {
            return new DeleteOrderResponse(response);
        }
    }

    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    GetInventoryResponse getInventory();

    public static class GetInventoryResponse extends org.openapitools.api.support.ResponseWrapper {
        private GetInventoryResponse(Response delegate) {
            super(delegate);
        }
        public static GetInventoryResponse with200ApplicationJson(Map<String, Integer> entity) {
            return new GetInventoryResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static GetInventoryResponse withCustomResponse(Response response) {
            return new GetInventoryResponse(response);
        }
    }

    @GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    GetOrderByIdResponse getOrderById(@PathParam("order_id") @Min(1L) @Max(5L) @ApiParam("ID of pet that needs to be fetched") Long orderId);

    public static class GetOrderByIdResponse extends org.openapitools.api.support.ResponseWrapper {
        private GetOrderByIdResponse(Response delegate) {
            super(delegate);
        }
        public static GetOrderByIdResponse with200ApplicationXml(Order entity) {
            return new GetOrderByIdResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static GetOrderByIdResponse with200ApplicationJson(Order entity) {
            return new GetOrderByIdResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static GetOrderByIdResponse with400() {
            return new GetOrderByIdResponse(Response.status(400).build());
        }
        public static GetOrderByIdResponse with404() {
            return new GetOrderByIdResponse(Response.status(404).build());
        }
        public static GetOrderByIdResponse withCustomResponse(Response response) {
            return new GetOrderByIdResponse(response);
        }
    }

    @POST
    @Path("/order")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Place an order for a pet", notes = "", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Void.class) })
    PlaceOrderResponse placeOrder(@Valid @NotNull Order body);

    public static class PlaceOrderResponse extends org.openapitools.api.support.ResponseWrapper {
        private PlaceOrderResponse(Response delegate) {
            super(delegate);
        }
        public static PlaceOrderResponse with200ApplicationXml(Order entity) {
            return new PlaceOrderResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static PlaceOrderResponse with200ApplicationJson(Order entity) {
            return new PlaceOrderResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static PlaceOrderResponse with400() {
            return new PlaceOrderResponse(Response.status(400).build());
        }
        public static PlaceOrderResponse withCustomResponse(Response response) {
            return new PlaceOrderResponse(response);
        }
    }
}
