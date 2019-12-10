package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call

import org.openapitools.client.models.User

interface UserApi {
    @POST("/user")
    fun createUser(@Body body: User): Call<Unit>

    @POST("/user/createWithArray")
    fun createUsersWithArrayInput(@Body body: kotlin.Array<User>): Call<Unit>

    @POST("/user/createWithList")
    fun createUsersWithListInput(@Body body: kotlin.Array<User>): Call<Unit>

    @DELETE("/user/{username}")
    fun deleteUser(@Path("username") username: kotlin.String): Call<Unit>

    @GET("/user/{username}")
    fun getUserByName(@Path("username") username: kotlin.String): Call<User>

    @GET("/user/login")
    fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): Call<kotlin.String>

    @GET("/user/logout")
    fun logoutUser(): Call<Unit>

    @PUT("/user/{username}")
    fun updateUser(@Path("username") username: kotlin.String, @Body body: User): Call<Unit>

}
