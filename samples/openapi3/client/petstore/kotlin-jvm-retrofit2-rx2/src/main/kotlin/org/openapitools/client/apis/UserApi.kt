package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import io.reactivex.Single
import io.reactivex.Completable

import org.openapitools.client.models.User

interface UserApi {
    @POST("/user")
    fun createUser(@Body user: User): Completable

    @POST("/user/createWithArray")
    fun createUsersWithArrayInput(@Body user: kotlin.Array<User>): Completable

    @POST("/user/createWithList")
    fun createUsersWithListInput(@Body user: kotlin.Array<User>): Completable

    @DELETE("/user/{username}")
    fun deleteUser(@Path("username") username: kotlin.String): Completable

    @GET("/user/{username}")
    fun getUserByName(@Path("username") username: kotlin.String): Single<User>

    @GET("/user/login")
    fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): Single<kotlin.String>

    @GET("/user/logout")
    fun logoutUser(): Completable

    @PUT("/user/{username}")
    fun updateUser(@Path("username") username: kotlin.String, @Body user: User): Completable

}
