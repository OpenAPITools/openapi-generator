package org.openapitools.api;

import org.openapitools.api.StoreApiService;
import org.openapitools.api.factories.StoreApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.util.Map;
import org.openapitools.model.Order;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import jakarta.servlet.ServletConfig;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.*;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

@Path("/store")


@Tag(description = "the store API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class StoreApi  {

   private final StoreApiService delegate;

   public StoreApi(@Context ServletConfig servletContext) {

      StoreApiService delegate = null;
      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("StoreApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (StoreApiService) Class.forName(implClass).getDeclaredConstructor().newInstance();
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


    @jakarta.ws.rs.DELETE
    @Path("/order/{order_id}")
    @Operation(summary = "Delete purchase order by ID", description = "", responses = {
            @ApiResponse(responseCode = "400", description = "Invalid ID supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "Order not found", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "store", }) 
    public Response deleteOrder(@Schema(description= "ID of the order that needs to be deleted", required = true) @PathParam("order_id") @NotNull  String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId, securityContext);
    }

    @jakarta.ws.rs.GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @Operation(summary = "Returns pet inventories by status", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Map.class))),
            }, tags={ "store", }) 
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }

    @jakarta.ws.rs.GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find purchase order by ID", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Order.class))),
            @ApiResponse(responseCode = "400", description = "Invalid ID supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "Order not found", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "store", }) 
    public Response getOrderById(@Schema(description= "ID of pet that needs to be fetched", required = true) @PathParam("order_id") @NotNull  @Min(1L) @Max(5L) Long orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId, securityContext);
    }

    @jakarta.ws.rs.POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Place an order for a pet", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Order.class))),
            @ApiResponse(responseCode = "400", description = "Invalid Order", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "store", }) 
    public Response placeOrder(@Schema(description = "order placed for purchasing the pet", required = true) @NotNull @Valid  Order order,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(order, securityContext);
    }
}
