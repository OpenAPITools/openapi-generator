package io.swagger.api;

import io.swagger.model.Order;
import java.util.Map;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/")
public interface StoreApi  {
    @DELETE
    @Path("/store/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteOrder(@PathParam("orderId") String orderId);
    @GET
    @Path("/store/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    public Response findOrdersByStatus(@QueryParam("status") String status);
    @GET
    @Path("/store/inventory")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventory();
    @GET
    @Path("/store/inventory?response=arbitrary_object")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventoryInObject();
    @GET
    @Path("/store/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getOrderById(@PathParam("orderId") String orderId);
    @POST
    @Path("/store/order")
    
    @Produces({ "application/json", "application/xml" })
    public Response placeOrder(Order body);
}

