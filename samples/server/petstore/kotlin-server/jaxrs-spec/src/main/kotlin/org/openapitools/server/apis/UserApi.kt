package org.openapitools.server.apis;

import java.time.OffsetDateTime
import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.10.0-SNAPSHOT")
class UserApi {

    @POST
    suspend fun createUser( body: User): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    suspend fun createUsersWithArrayInput( body: List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    suspend fun createUsersWithListInput( body: List<User>): Response {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    suspend fun deleteUser(@PathParam("username") username: String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun getUserByName(@PathParam("username") username: String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun loginUser(@QueryParam("username")   username: String,@QueryParam("password")   password: String): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    suspend fun logoutUser(): Response {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    suspend fun updateUser(@PathParam("username") username: String, body: User): Response {
        return Response.ok().entity("magic!").build();
    }
}
