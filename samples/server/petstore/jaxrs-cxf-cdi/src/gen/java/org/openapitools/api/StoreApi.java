package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;
import org.openapitools.api.StoreApiService;

import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
@Path("/store")
@RequestScoped

@Tag(name = "the store API")




public class StoreApi  {

  @Context SecurityContext securityContext;

  @Inject StoreApiService delegate;


    @DELETE
    @Path("/order/{orderId}")
    
    
    @Operation(summary = "Delete purchase order by ID", description = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors" , tags={ "store",  },
    responses = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "Order not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response deleteOrder(@Parameter(in = ParameterIn.PATH,description = "ID of the order that needs to be deleted",required=true) @PathParam("orderId") String orderId) {
        return delegate.deleteOrder(orderId, securityContext);
    }

    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    @Operation(summary = "Returns pet inventories by status", description = "Returns a map of status codes to quantities" 
        , security = {
             
        }
    , tags={ "store",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content( schema = @Schema(implementation = Map.class) )}  )  })
    public Response getInventory() {
        return delegate.getInventory(securityContext);
    }

    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find purchase order by ID", description = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions" , tags={ "store",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Order.class))  )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "Order not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response getOrderById( @Min(1L) @Max(5L)@Parameter(in = ParameterIn.PATH,description = "ID of pet that needs to be fetched",required=true) @PathParam("orderId") Long orderId) {
        return delegate.getOrderById(orderId, securityContext);
    }

    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Place an order for a pet", description = "" , tags={ "store" },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Order.class))  )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid Order" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response placeOrder(@Parameter(description = "order placed for purchasing the pet" ,required=true) Order body) {
        return delegate.placeOrder(body, securityContext);
    }
}
