package org.openapitools.server.apis;

import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/user")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.20.0-SNAPSHOT")
class UserApi {

    @POST
    suspend fun createUser( body: User): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    suspend fun createUsersWithArrayInput( body: kotlin.collections.List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    suspend fun createUsersWithListInput( body: kotlin.collections.List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    suspend fun deleteUser(@PathParam("username") username: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    @Produces("application/xml", "application/json")
    suspend fun getUserByName(@PathParam("username") username: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    @Produces("application/xml", "application/json")
    suspend fun loginUser(@QueryParam("username")   username: kotlin.String,@QueryParam("password")   password: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    suspend fun logoutUser(): Response {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    suspend fun updateUser(@PathParam("username") username: kotlin.String, body: User): Response {
        return Response.ok().entity("magic!").build();
    }
}
