package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiUser

interface UserApi {
    /**
     * Create user
     * This can only be done by the logged in user.
     * Responses:
     *  - 0: successful operation
     *
     * @param apiUser Created user object
     * @return [Unit]
     */
    @POST("user")
    suspend fun createUser(@Body apiUser: ApiUser): Response<Unit>

    /**
     * Creates list of users with given input array
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @param apiUser List of user object
     * @return [Unit]
     */
    @POST("user/createWithArray")
    suspend fun createUsersWithArrayInput(@Body apiUser: kotlin.collections.List<ApiUser>): Response<Unit>

    /**
     * Creates list of users with given input array
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @param apiUser List of user object
     * @return [Unit]
     */
    @POST("user/createWithList")
    suspend fun createUsersWithListInput(@Body apiUser: kotlin.collections.List<ApiUser>): Response<Unit>

    /**
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
    suspend fun deleteUser(@Path("username") username: kotlin.String): Response<Unit>

    /**
     * Get user by user name
     * 
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid username supplied
     *  - 404: User not found
     *
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return [ApiUser]
     */
    @GET("user/{username}")
    suspend fun getUserByName(@Path("username") username: kotlin.String): Response<ApiUser>

    /**
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
    suspend fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): Response<kotlin.String>

    /**
     * Logs out current logged in user session
     * 
     * Responses:
     *  - 0: successful operation
     *
     * @return [Unit]
     */
    @GET("user/logout")
    suspend fun logoutUser(): Response<Unit>

    /**
     * Updated user
     * This can only be done by the logged in user.
     * Responses:
     *  - 400: Invalid user supplied
     *  - 404: User not found
     *
     * @param username name that need to be deleted
     * @param apiUser Updated user object
     * @return [Unit]
     */
    @PUT("user/{username}")
    suspend fun updateUser(@Path("username") username: kotlin.String, @Body apiUser: ApiUser): Response<Unit>

}
