package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.User

interface UserApi {
    /**
     * POST user
     * Create user
     * This can only be done by the logged in user.
     * Responses:
     *  - 0: successful operation
     *
     * @param user Created user object
     * @return [Unit]
     */
    @POST("user")
    suspend fun createUser(@Body user: User)

    /**
     * POST user/createWithArray
     * Creates list of users with given input array
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @param user List of user object
     * @return [Unit]
     */
    @POST("user/createWithArray")
    suspend fun createUsersWithArrayInput(@Body user: kotlin.collections.List<User>)

    /**
     * POST user/createWithList
     * Creates list of users with given input array
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @param user List of user object
     * @return [Unit]
     */
    @POST("user/createWithList")
    suspend fun createUsersWithListInput(@Body user: kotlin.collections.List<User>)

    /**
     * DELETE user/{username}
     * Delete user
     * This can only be done by the logged in user.
     * Responses:
     *  - 400: Invalid username supplied
     *  - 404: User not found
     *
     * @param username The name that needs to be deleted
     * @return [Unit]
     */
    @DELETE("user/{username}")
    suspend fun deleteUser(@Path("username") username: kotlin.String)

    /**
     * GET user/{username}
     * Get user by user name
     * 
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid username supplied
     *  - 404: User not found
     *
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return [User]
     */
    @GET("user/{username}")
    suspend fun getUserByName(@Path("username") username: kotlin.String): User

    /**
     * GET user/login
     * Logs user into the system
     * 
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid username/password supplied
     *
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return [kotlin.String]
     */
    @GET("user/login")
    suspend fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): kotlin.String

    /**
     * GET user/logout
     * Logs out current logged in user session
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @return [Unit]
     */
    @GET("user/logout")
    suspend fun logoutUser()

    /**
     * PUT user/{username}
     * Updated user
     * This can only be done by the logged in user.
     * Responses:
     *  - 400: Invalid user supplied
     *  - 404: User not found
     *
     * @param username name that need to be deleted
     * @param user Updated user object
     * @return [Unit]
     */
    @PUT("user/{username}")
    suspend fun updateUser(@Path("username") username: kotlin.String, @Body user: User)

}
