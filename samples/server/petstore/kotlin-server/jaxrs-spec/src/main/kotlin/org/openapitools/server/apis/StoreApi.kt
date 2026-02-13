package org.openapitools.server.apis;

import org.openapitools.server.models.Order

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/store")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.20.0-SNAPSHOT")
class StoreApi {

    @DELETE
    @Path("/order/{orderId}")
    suspend fun deleteOrder(@PathParam("orderId") orderId: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/inventory")
    @Produces("application/json")
    suspend fun getInventory(): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/order/{orderId}")
    @Produces("application/xml", "application/json")
    suspend fun getOrderById(@PathParam("orderId") orderId: kotlin.Long): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/order")
    @Produces("application/xml", "application/json")
    suspend fun placeOrder( body: Order): Response {
        return Response.ok().entity("magic!").build();
    }
}
