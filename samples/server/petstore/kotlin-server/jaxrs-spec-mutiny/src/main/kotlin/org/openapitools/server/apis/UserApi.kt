package org.openapitools.server.apis;

import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))
interface UserApi {

    @POST
    @Path("/user")
    fun createUser( body: User): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/user/createWithArray")
    fun createUsersWithArrayInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/user/createWithList")
    fun createUsersWithListInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    @DELETE
    @Path("/user/{username}")
    fun deleteUser(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/user/{username}")
    @Produces("application/xml", "application/json")
    fun getUserByName(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/user/login")
    @Produces("application/xml", "application/json")
    fun loginUser(@QueryParam("username")   username: kotlin.String,@QueryParam("password")   password: kotlin.String): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/user/logout")
    fun logoutUser(): io.smallrye.mutiny.Uni<Response>

    @PUT
    @Path("/user/{username}")
    fun updateUser(@PathParam("username") username: kotlin.String, body: User): io.smallrye.mutiny.Uni<Response>
}
