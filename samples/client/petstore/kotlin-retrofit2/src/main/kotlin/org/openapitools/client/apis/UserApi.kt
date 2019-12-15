package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.User

interface UserApi {
    @POST("/user")
    fun createUser(@Body body: User): Call<Unit>

    @POST("/user/createWithArray")
    fun createUsersWithArrayInput(@Body body: Array<User>): Call<Unit>

    @POST("/user/createWithList")
    fun createUsersWithListInput(@Body body: Array<User>): Call<Unit>

    @DELETE("/user/{username}")
    fun deleteUser(@Path("username") username: String): Call<Unit>

    @GET("/user/{username}")
    fun getUserByName(@Path("username") username: String): Call<User>

    @GET("/user/login")
    fun loginUser(@Query("username") username: String, @Query("password") password: String): Call<String>

    @GET("/user/logout")
    fun logoutUser(): Call<Unit>

    @PUT("/user/{username}")
    fun updateUser(@Path("username") username: String, @Body body: User): Call<Unit>

}
