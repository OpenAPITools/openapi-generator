package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

@Path("/v2")
public interface StoreApi  {
    @GET
    @Path("/store/inventory")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventory();
    @POST
    @Path("/store/order")
    
    @Produces({ "application/json", "application/xml" })
    public Response placeOrder(Order body);
    @GET
    @Path("/store/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getOrderById(@PathParam("orderId") String orderId);
    @DELETE
    @Path("/store/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteOrder(@PathParam("orderId") String orderId);
}

