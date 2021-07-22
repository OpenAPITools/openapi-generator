package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.StoreApiService;



import java.util.Map;
import org.openapitools.model.Order;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.inject.Inject;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/store")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class StoreApi  {

    @Inject StoreApiService service;

    @DELETE
    @Path("/order/{orderId}")
    
    
    public Response deleteOrder( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deleteOrder(orderId,securityContext);
    }
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getInventory(securityContext);
    }
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    public Response getOrderById( @Min(1L) @Max(5L) @PathParam("orderId") Long orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getOrderById(orderId,securityContext);
    }
    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    public Response placeOrder( @NotNull @Valid Order body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.placeOrder(body,securityContext);
    }
}
