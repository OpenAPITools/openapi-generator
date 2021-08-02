package org.openapitools.api;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import org.openapitools.model.Order;

@Path("/store")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyEapServerCodegen")
public interface StoreApi  {

    @DELETE
    @Path("/order/{orderId}") Response deleteOrder( @PathParam("orderId") String orderId,@Context SecurityContext securityContext);
    @GET
    @Path("/inventory")

    @Produces({ "application/json" }) Response getInventory(@Context SecurityContext securityContext);
    @GET
    @Path("/order/{orderId}")

    @Produces({ "application/xml", "application/json" }) Response getOrderById( @Min(1L) @Max(5L) @PathParam("orderId") Long orderId,@Context SecurityContext securityContext);
    @POST
    @Path("/order")

    @Produces({ "application/xml", "application/json" }) Response placeOrder( @NotNull @Valid Order body,@Context SecurityContext securityContext);
}
