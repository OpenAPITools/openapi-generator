package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.util.Map;
import io.swagger.model.Order;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;

@Path("/store")


@io.swagger.annotations.Api(description = "the store API")

public class StoreApi  {
   private final StoreApiService delegate;

   public StoreApi(@Context ServletConfig servletContext) {
      StoreApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("StoreApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (StoreApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = StoreApiServiceFactory.getStoreApi();
      }

      this.delegate = delegate;
   }

    @DELETE
    @Path("/order/{order_id}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class, tags={ "store", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    public Response deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true) @PathParam("order_id") String orderId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId,securityContext);
    }
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key")
    }, tags={ "store", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }
    @GET
    @Path("/order/{order_id}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Order.class),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    public Response getOrderById(@ApiParam(value = "ID of pet that needs to be fetched",required=true) @PathParam("order_id") Long orderId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId,securityContext);
    }
    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Order.class),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid Order", response = Void.class) })
    public Response placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true) Order body
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(body,securityContext);
    }
}
