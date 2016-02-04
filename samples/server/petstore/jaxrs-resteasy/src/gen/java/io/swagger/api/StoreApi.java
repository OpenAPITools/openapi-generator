package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;

import java.util.Map;
import io.swagger.model.Order;


import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/store")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public class StoreApi  {
   private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();


    @GET
    @Path("/inventory")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }

    @POST
    @Path("/order")
    
    @Produces({ "application/json", "application/xml" })
    public Response placeOrder( Order body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(body,securityContext);
    }

    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getOrderById( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId,securityContext);
    }

    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteOrder( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId,securityContext);
    }

}

