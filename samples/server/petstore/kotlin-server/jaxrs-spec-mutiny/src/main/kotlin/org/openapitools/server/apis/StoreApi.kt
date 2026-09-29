package org.openapitools.server.apis;

import org.openapitools.server.models.Order

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/store")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.26.0-SNAPSHOT")
interface StoreApi {

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * @param orderId ID of the order that needs to be deleted
     * @return Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     */
    @DELETE
    @Path("/order/{orderId}")
    fun deleteOrder(@PathParam("orderId") orderId: kotlin.String): io.smallrye.mutiny.Uni<Response>

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     * @return successful operation (status code 200)
     */
    @GET
    @Path("/inventory")
    @Produces("application/json")
    fun getInventory(): io.smallrye.mutiny.Uni<Response>

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * @param orderId ID of pet that needs to be fetched
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     */
    @GET
    @Path("/order/{orderId}")
    @Produces("application/xml", "application/json")
    fun getOrderById(@PathParam("orderId") orderId: kotlin.Long): io.smallrye.mutiny.Uni<Response>

    /**
     * Place an order for a pet
     * @param body order placed for purchasing the pet
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     */
    @POST
    @Path("/order")
    @Produces("application/xml", "application/json")
    fun placeOrder( body: Order): io.smallrye.mutiny.Uni<Response>
}
