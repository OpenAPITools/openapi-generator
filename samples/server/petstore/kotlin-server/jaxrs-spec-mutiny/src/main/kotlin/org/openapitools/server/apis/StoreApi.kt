package org.openapitools.server.apis;

import org.openapitools.server.models.Order

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))
interface StoreApi {

    @DELETE
    @Path("/store/order/{orderId}")
    fun deleteOrder(@PathParam("orderId") orderId: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/store/inventory")
    @Produces("application/json")
    fun getInventory(): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/store/order/{orderId}")
    @Produces("application/xml", "application/json")
    fun getOrderById(@PathParam("orderId") orderId: kotlin.Long): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/store/order")
    @Produces("application/xml", "application/json")
    fun placeOrder( body: Order): io.smallrye.mutiny.Uni<Response>
}
