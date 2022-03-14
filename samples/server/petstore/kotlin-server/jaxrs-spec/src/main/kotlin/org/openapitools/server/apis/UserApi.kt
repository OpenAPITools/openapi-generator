package org.openapitools.server.apis;

import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream
import java.util.Map
import java.util.List



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))class UserApi {

    @POST
    suspend fun createUser( body: User): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    suspend fun createUsersWithArrayInput( body: kotlin.collections.List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    suspend fun createUsersWithListInput( body: kotlin.collections.List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    suspend fun deleteUser(@PathParam("username") username: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun getUserByName(@PathParam("username") username: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun loginUser(@QueryParam("username")   username: kotlin.String,@QueryParam("password")   password: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    suspend fun logoutUser(): Response {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    suspend fun updateUser(@PathParam("username") username: kotlin.String, body: User): Response {
        return Response.ok().entity("magic!").build();
    }
}
