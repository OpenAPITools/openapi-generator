package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import okhttp3.ResponseBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiPet
import org.openapitools.client.models.ApiTag

import okhttp3.MultipartBody

interface BodyApi {
    /**
     * POST binary/gif
     * Test binary (gif) response body
     * Test binary (gif) response body
     * Responses:
     *  - 200: Successful operation
     *
     * @return [ResponseBody]
     */
    @POST("binary/gif")
    suspend fun testBinaryGif(): Response<ResponseBody>

    /**
     * POST body/application/octetstream/binary
     * Test body parameter(s)
     * Test body parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param body  (optional)
     * @return [kotlin.String]
     */
    @POST("body/application/octetstream/binary")
    suspend fun testBodyApplicationOctetstreamBinary(@Body body: RequestBody? = null): Response<kotlin.String>

    /**
     * POST body/application/octetstream/array_of_binary
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * Responses:
     *  - 200: Successful operation
     *
     * @param files 
     * @return [kotlin.String]
     */
    @Multipart
    @POST("body/application/octetstream/array_of_binary")
    suspend fun testBodyMultipartFormdataArrayOfBinary(@Part files: MultipartBody.Part): Response<kotlin.String>

    /**
     * POST body/application/octetstream/single_binary
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * Responses:
     *  - 200: Successful operation
     *
     * @param myFile  (optional)
     * @return [kotlin.String]
     */
    @Multipart
    @POST("body/application/octetstream/single_binary")
    suspend fun testBodyMultipartFormdataSingleBinary(@Part myFile: MultipartBody.Part? = null): Response<kotlin.String>

    /**
     * POST echo/body/FreeFormObject/response_string
     * Test free form object
     * Test free form object
     * Responses:
     *  - 200: Successful operation
     *
     * @param body Free form object (optional)
     * @return [kotlin.String]
     */
    @POST("echo/body/FreeFormObject/response_string")
    suspend fun testEchoBodyFreeFormObjectResponseString(@Body body: kotlin.Any? = null): Response<kotlin.String>

    /**
     * POST echo/body/Pet
     * Test body parameter(s)
     * Test body parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return [ApiPet]
     */
    @POST("echo/body/Pet")
    suspend fun testEchoBodyPet(@Body apiPet: ApiPet? = null): Response<ApiPet>

    /**
     * POST echo/body/Pet/response_string
     * Test empty response body
     * Test empty response body
     * Responses:
     *  - 200: Successful operation
     *
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return [kotlin.String]
     */
    @POST("echo/body/Pet/response_string")
    suspend fun testEchoBodyPetResponseString(@Body apiPet: ApiPet? = null): Response<kotlin.String>

    /**
     * POST echo/body/Tag/response_string
     * Test empty json (request body)
     * Test empty json (request body)
     * Responses:
     *  - 200: Successful operation
     *
     * @param apiTag Tag object (optional)
     * @return [kotlin.String]
     */
    @POST("echo/body/Tag/response_string")
    suspend fun testEchoBodyTagResponseString(@Body apiTag: ApiTag? = null): Response<kotlin.String>

}
