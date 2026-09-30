package org.openapitools.server.apis;

import org.openapitools.server.models.User

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/user")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.26.0-SNAPSHOT")
interface UserApi {

    /**
     * Create user
     *
     * This can only be done by the logged in user.
     * @param body Created user object
     * @return successful operation (status code 0)
     */
    @POST
    fun createUser( body: User): io.smallrye.mutiny.Uni<Response>

    /**
     * Creates list of users with given input array
     * @param body List of user object
     * @return successful operation (status code 0)
     */
    @POST
    @Path("/createWithArray")
    fun createUsersWithArrayInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    /**
     * Creates list of users with given input array
     * @param body List of user object
     * @return successful operation (status code 0)
     */
    @POST
    @Path("/createWithList")
    fun createUsersWithListInput( body: kotlin.collections.List<User>): io.smallrye.mutiny.Uni<Response>

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     */
    @DELETE
    @Path("/{username}")
    fun deleteUser(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    /**
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return successful operation (status code 200)
     *         or Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     */
    @GET
    @Path("/{username}")
    @Produces("application/xml", "application/json")
    fun getUserByName(@PathParam("username") username: kotlin.String): io.smallrye.mutiny.Uni<Response>

    /**
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return successful operation (status code 200)
     *         or Invalid username/password supplied (status code 400)
     */
    @GET
    @Path("/login")
    @Produces("application/xml", "application/json")
    fun loginUser(@QueryParam("username") username: kotlin.String,@QueryParam("password") password: kotlin.String): io.smallrye.mutiny.Uni<Response>

    /**
     * Logs out current logged in user session
     * @return successful operation (status code 0)
     */
    @GET
    @Path("/logout")
    fun logoutUser(): io.smallrye.mutiny.Uni<Response>

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     * @param username name that need to be deleted
     * @param body Updated user object
     * @return Invalid user supplied (status code 400)
     *         or User not found (status code 404)
     */
    @PUT
    @Path("/{username}")
    fun updateUser(@PathParam("username") username: kotlin.String, body: User): io.smallrye.mutiny.Uni<Response>
}
