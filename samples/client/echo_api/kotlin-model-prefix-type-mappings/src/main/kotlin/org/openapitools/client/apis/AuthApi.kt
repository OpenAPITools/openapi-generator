package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName


interface AuthApi {
    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * Responses:
     *  - 200: Successful operation
     *
     * @return [kotlin.String]
     */
    @POST("auth/http/basic")
    suspend fun testAuthHttpBasic(): Response<kotlin.String>

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * Responses:
     *  - 200: Successful operation
     *
     * @return [kotlin.String]
     */
    @POST("auth/http/bearer")
    suspend fun testAuthHttpBearer(): Response<kotlin.String>

}
