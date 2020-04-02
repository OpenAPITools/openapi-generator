package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import rx.Observable

import org.openapitools.client.models.User

interface UserApi {
    @POST("/user")
    fun createUser(@Body user: User): Observable<Unit>

    @POST("/user/createWithArray")
    fun createUsersWithArrayInput(@Body user: kotlin.Array<User>): Observable<Unit>

    @POST("/user/createWithList")
    fun createUsersWithListInput(@Body user: kotlin.Array<User>): Observable<Unit>

    @DELETE("/user/{username}")
    fun deleteUser(@Path("username") username: kotlin.String): Observable<Unit>

    @GET("/user/{username}")
    fun getUserByName(@Path("username") username: kotlin.String): Observable<User>

    @GET("/user/login")
    fun loginUser(@Query("username") username: kotlin.String, @Query("password") password: kotlin.String): Observable<kotlin.String>

    @GET("/user/logout")
    fun logoutUser(): Observable<Unit>

    @PUT("/user/{username}")
    fun updateUser(@Path("username") username: kotlin.String, @Body user: User): Observable<Unit>

}
