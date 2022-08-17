package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/store")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface StoreApi {

    @DELETE
    @Path("/order/{order_id}")
    void deleteOrder(@PathParam("order_id") String orderId);

    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    Map<String, Integer> getInventory();

    @GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    Order getOrderById(@PathParam("order_id") @Min(1L) @Max(5L) Long orderId);

    @POST
    @Path("/order")
    @Produces({ "application/xml", "application/json" })
    Order placeOrder(@Valid @NotNull Order body);
}
