package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import retrofit2.http.*

import org.openapitools.client.models.User

interface UserApi {
    @POST("/user")
    suspend fun createUser(@Body user: User): Unit

    @POST("/user/createWithArray")
    suspend fun createUsersWithArrayInput(@Body user: kotlin.Array<User>): Unit

    @POST("/user/createWithList")
    suspend fun createUsersWithListInput(@Body user: kotlin.Array<User>): Unit

    @DELETE("/user/{username}")
    suspend fun deleteUser(@Path("username") username: kotlin.String): Unit

    @GET("/user/{username}")
    suspend fun getUserByName(@Path("username") username: kotlin.String): User

    @GET("/user/login")
    suspend fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): kotlin.String

    @GET("/user/logout")
    suspend fun logoutUser(): Unit

    @PUT("/user/{username}")
    suspend fun updateUser(@Path("username") username: kotlin.String, @Body user: User): Unit

}
