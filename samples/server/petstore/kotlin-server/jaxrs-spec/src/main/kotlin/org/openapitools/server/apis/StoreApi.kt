package org.openapitools.server.apis;

import org.openapitools.server.models.Order

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream
import java.util.Map
import java.util.List



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))class StoreApi {

    @DELETE
    suspend fun deleteOrder(@PathParam("orderId") orderId: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/json")
    suspend fun getInventory(): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun getOrderById(@PathParam("orderId") orderId: kotlin.Long): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Produces("application/xml", "application/json")
    suspend fun placeOrder( body: Order): Response {
        return Response.ok().entity("magic!").build();
    }
}
