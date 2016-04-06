package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;

import io.swagger.model.Order;
import java.util.Map;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/store")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-03-16T14:27:58.108+08:00")
public class StoreApi  {
   private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();

    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteOrder( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId,securityContext);
    }
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    public Response findOrdersByStatus( @QueryParam("status") String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findOrdersByStatus(status,securityContext);
    }
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }
    @GET
    @Path("/inventory?response=arbitrary_object")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventoryInObject(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventoryInObject(securityContext);
    }
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getOrderById( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId,securityContext);
    }
    @POST
    @Path("/order")
    
    @Produces({ "application/json", "application/xml" })
    public Response placeOrder( Order body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(body,securityContext);
    }
}
