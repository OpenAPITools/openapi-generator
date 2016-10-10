package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/v2")
public interface StoreApi  {
    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    Response deleteOrder(@PathParam("orderId") String orderId);
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    Response getInventory();
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    Response getOrderById(@PathParam("orderId") Long orderId);
    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    Response placeOrder(Order body);
}

