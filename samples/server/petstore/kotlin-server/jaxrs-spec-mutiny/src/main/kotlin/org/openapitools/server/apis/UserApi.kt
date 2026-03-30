package org.openapitools.server.apis;

import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/user")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.22.0-SNAPSHOT")
interface UserApi {

    @POST
    @Path("")
    fun createUser( body: User): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/createWithArray")
    fun createUsersWithArrayInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/createWithList")
    fun createUsersWithListInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    @DELETE
    @Path("/{username}")
    fun deleteUser(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/{username}")
    @Produces("application/xml", "application/json")
    fun getUserByName(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/login")
    @Produces("application/xml", "application/json")
    fun loginUser(@QueryParam("username")   username: kotlin.String,@QueryParam("password")   password: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/logout")
    fun logoutUser(): io.smallrye.mutiny.Uni<Response>

    @PUT
    @Path("/{username}")
    fun updateUser(@PathParam("username") username: kotlin.String, body: User): io.smallrye.mutiny.Uni<Response>
}
